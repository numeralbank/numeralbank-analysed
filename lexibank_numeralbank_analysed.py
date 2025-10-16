import json
import pathlib
import subprocess
from collections import defaultdict

import attr
import pycldf
from cldfzenodo.api import API as ZenodoAPI
from cldfzenodo.record import GithubRepos
from clldutils.misc import slug
from cltoolkit import Wordlist
from git import Repo, GitCommandError
from pylexibank import Dataset as BaseDataset
from pylexibank import Language, Lexeme
from pylexibank import progressbar
from unidecode import unidecode


def simple_chars(chars):
    return slug(unidecode(chars).replace("@", "a"))


@attr.s
class CustomLexeme(Lexeme):
    NumberValue = attr.ib(default=None, metadata={"datatype": "integer"})


@attr.s
class CustomLanguage(Language):
    BaseAnnotation = attr.ib(default=None)
    BaseAnnotator = attr.ib(default=None)
    BaseComment = attr.ib(default=None)
    Coverage = attr.ib(
        default=None,
        metadata={
            "datatype": "float",
            "dc:description": "Coverage of the language in comparison with our master concept list.",
        },
    )
    OneToThirty = attr.ib(default=None, metadata={"datatype": "float"})
    BaseInSource = attr.ib(default=None)


def coverage(language, concepts):
    return len([c.id for c in language.concepts if c.id in concepts]) / len(concepts)


def git_last_commit_date(p, git_command="git"):
    p = pathlib.Path(p)
    if not p.exists():
        raise ValueError("cannot read from non-existent directory")
    p = p.resolve()
    cmd = [
        git_command,
        "--git-dir={0}".format(p.joinpath(".git")),
        "--no-pager",
        "log",
        "-1",
        '--format="%ai"',
    ]
    try:
        p = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        stdout, stderr = p.communicate()
        if p.returncode == 0:
            res = stdout.strip()  # pragma: no cover
        else:
            raise ValueError(stderr)
    except (ValueError, FileNotFoundError):
        return ""
    if not isinstance(res, str):
        res = res.decode("utf8")
    return res.replace('"', "")


class Dataset(BaseDataset):
    dir = pathlib.Path(__file__).parent
    id = "numeralbank-analysed"
    language_class = CustomLanguage
    lexeme_class = CustomLexeme

    def cmd_download(self, args):
        self.dataset_meta = {
            r["ID"]: r["URL"]
            for r in self.etc_dir.read_csv("datasets.tsv", delimiter="\t", dicts=True)
        }

        github_info = {
            rec.doi: rec.github_repos
            for rec in ZenodoAPI.iter_records(community='lexibank', allversions=True)}

        for dataset, src in self.dataset_meta.items():
            ghinfo = github_info[src] if src in github_info else GithubRepos.from_url(src)
            args.log.info("Checking {}".format(dataset))
            dest = self.raw_dir / dataset

            # download data
            if dest.exists():
                args.log.info("... dataset already exists, pulling changes")
                for remote in Repo(str(dest)).remotes:
                    remote.fetch()
            else:
                args.log.info("... cloning {}".format(dataset))
                try:
                    Repo.clone_from(ghinfo.clone_url, str(dest))
                except GitCommandError as e:
                    args.log.error("... download failed\n{}".format(str(e)))
                    continue

            # check out release (fall back to master branch)
            repo = Repo(str(dest))
            if ghinfo.tag:
                args.log.info("... checking out tag {}".format(ghinfo.tag))
                repo.git.checkout(ghinfo.tag)
            else:
                args.log.warning("... could not determine tag to check out")
                args.log.info("... checking out master")
                try:
                    branch = repo.branches.main
                    branch.checkout()
                except AttributeError:
                    try:
                        branch = repo.branches.master
                        branch.checkout()
                    except AttributeError:
                        args.log.error("found neither main nor master branch")
                repo.git.merge()

    def cmd_makecldf(self, args):
        args.writer.add_sources()
        all_concepts = {
            concept["CONCEPTICON_GLOSS"]: concept["NUMBER_VALUE"] for concept in self.concepts
        }
        datasets = {
            r["ID"]: (r["Source"], r["URL"])
            for r in self.etc_dir.read_csv("datasets.tsv", delimiter="\t", dicts=True)
        }

        args.writer.cldf.add_component(
            "ContributionTable",
            {
                "datatype": "string",
                "name": "Metadata",
                "dc:description": "JSON encoded metadata of used datasets",
            },
        )
        for c in ["Description", "Contributor"]:
            args.writer.cldf.remove_columns("ContributionTable", c)

        for ds, (src, url) in datasets.items():
            cldf_path = self.raw_dir.joinpath(ds, "cldf", "cldf-metadata.json")
            with open(cldf_path) as f:
                js = json.load(f)
            doi = None
            git_version = None
            if "github.com" in url.lower():
                accessURL = url
                git_version = git_last_commit_date(cldf_path.parent.parent)
            else:
                doi = url
                accessURL = "https://doi.org/{0}".format(doi)

            args.writer.objects["contributions.csv"].append(
                dict(
                    ID=js["rdf:ID"],
                    Name=js["dc:title"],
                    Citation=js["dc:bibliographicCitation"],
                    Metadata=json.dumps(
                        {
                            "dcat:accessURL": accessURL,
                            "dc:description": js.get("dc:description", None),
                            "dc:license": js.get("dc:license", None),
                            "aboutUrl": js.get("aboutUrl", None),
                            "doi": doi,
                            "git_version": git_version,
                            "source": src,
                        }
                    ),
                )
            )

        wl = Wordlist(
            [
                pycldf.Dataset.from_metadata(
                    self.raw_dir.joinpath(ds, "cldf", "cldf-metadata.json")
                )
                for ds in datasets
            ]
        )

        # get base info from the external document
        base_info = {}
        for row in self.etc_dir.read_csv("bases.tsv", delimiter="\t", dicts=True):
            if row["Annotator"] == "Russell Barlow":
                row["Comment"] = ""
            if row["Language_ID"]:
                base_info[row["Language_ID"]] = row
            else:
                base_info[row["Glottocode"]] = row
        args.log.info("loaded base info")

        # filter languages (only those with glottocodes)
        #  select first all language which occur only in one dataset
        #  otherwise select languages having the best coverage excluding googleuninum
        map_glottocode_nr_of_sources = defaultdict(int)
        for language in wl.languages:
            if language.glottocode is not None:
                map_glottocode_nr_of_sources[language.glottocode] += 1

        selected_languages = []
        visited = set()

        for language in sorted(wl.languages, key=lambda x: coverage(x, all_concepts), reverse=True):
            #if language.glottocode is not None and (
            #    map_glottocode_nr_of_sources[language.glottocode] == 1
            #    or (language.glottocode not in visited and language.dataset != "googleuninum")
            #):
            if language.glottocode is not None:
                visited.add(language.glottocode)
                selected_languages += [language]

        one_to_forty = [
            concept["CONCEPTICON_GLOSS"]
            for concept in self.concepts
            if concept["TEST"] in ["1", "2"]
        ]
        one_to_thirty = [
            concept["CONCEPTICON_GLOSS"] for concept in self.concepts if concept["TEST"] in ["1"]
        ]

        for concept in wl.concepts:
            args.writer.add_concept(
                ID=slug(concept.id),
                Name=concept.id,
                Concepticon_ID=concept.concepticon_id,
                Concepticon_Gloss=concept.id,
            )

        with open(self.raw_dir.joinpath("unique_relations.json")) as f:
            relations = json.load(f)

        # relations conversion for our detection method
        convert = {
            "Tener": "decimal",
            "Twoer": "binary",
            "Twentier": "vigesimal",
            "Fiver": "quinary",
            "Unknown": "unknown",
        }

        # how to represent basic relations in Chan, which are frequent enough
        # in the data
        target_bases = {
            "Decimal": "decimal",
            "decimal": "decimal",
            "Decimal-Vigesimal": "decimal/vigesimal",
            "Vigesimal": "vigesimal",
            "Restricted": "restricted",
            "vigesimal": "vigesimal",
            "quinary": "quinary",
            "quinary AND decimal": "quinary",
            "quinary AND vigesimal": "quinary",
            "quinary AND decimal AND vigesimal": "quinary",
            "binary": "binary",
            "decimal AND vigesimal": "decimal/vigesimal",
            "decimal OR vigesimal": "decimal/vigesimal",
            "duodecimal": "duodecimal",
            "octal": "octal",
            "quinary OR decimal": "quinary/decimal",
            "quinary AND vigesimal OR decimal": "quinary/vigesimal",
            "quinary AND double decimal": "quinary/decimal",
            "octal AND decimal": "octal",
            "octal AND duodecimal AND hexadecimal AND vigesimal AND tetravigesimal": "octal",
            "octogesimal": "octogesimal",
            "trigesimal": "trigesimal",
            "quinquagesimal": "quinquagesimal",
            "pentadecimal OR pentavigesimal": "pentadecimal/pentavigesimal",
            "pentavigesimal": "pentavigesimal",
        }

        valid_bases = set(
            [
                "binary",
                "decimal",
                "mixed",
                "decimal/vigesimal",
                "duodecimal",
                "octal",
                "octogesimal",
                "pentadecimal/pentavigesimal",
                "pentavigesimal",
                "quaternary",
                "quinary",
                "quinary/decimal",
                "quinquagesimal",
                "restricted",
                "senary",
                "trigesimal",
                "vigesimal",
            ]
        )

        base_errors = set()
        for language in progressbar(
            sorted(selected_languages, key=lambda x: x.glottocode),
            desc="Processing {} selected languages".format(len(selected_languages)),
        ):
            # check for sufficient coverage
            cov_ = coverage(language, all_concepts)
            cov1 = coverage(language, one_to_forty)
            cov2 = coverage(language, one_to_thirty)

            # retrieve annotated base
            if language.id in base_info:
                annotated_base, annotator, cmt = (
                    target_bases.get(
                        base_info[language.id]["Base"], base_info[language.id]["Base"]
                    ),
                    base_info[language.id]["Annotator"],
                    base_info[language.id]["Comment"],
                )
            elif language.glottocode in base_info:
                annotated_base, annotator, cmt = (
                    target_bases.get(
                        base_info[language.glottocode]["Base"],
                        base_info[language.glottocode]["Base"],
                    ),
                    base_info[language.glottocode]["Annotator"],
                    base_info[language.glottocode]["Comment"],
                )
            else:
                if language.dataset == "numerals":
                    annotated_base = target_bases.get(language.data["Base"], language.data["Base"])
                    annotator, cmt = "Eugene Chan", ""
                elif language.dataset == "sand":
                    annotated_base = target_bases.get(language.data["Base"], language.data["Base"])
                    annotator, cmt = "Mamta Kumari", ""
                else:
                    annotated_base, annotator, cmt = "", "", ""

            # check for type
            if annotated_base and annotated_base not in valid_bases:
                if annotated_base.lower() != "unknown":
                    base_errors.add((language.id, annotated_base, annotator))
                annotated_base, annotator, cmt = "", "", ""

            if 'Comment' in language.data and language.data['Comment']:
                if cmt:
                    cmt += '; '
                cmt += language.data['Comment']

            args.writer.add_language(
                ID=language.id,
                Name=language.name,
                Glottocode=language.glottocode,
                Latitude=language.latitude,
                Longitude=language.longitude,
                Macroarea=language.macroarea,
                BaseAnnotation=annotated_base,
                BaseAnnotator=annotator,
                BaseComment=cmt,
                Coverage=cov_,
                OneToThirty=cov2,
            )
            for concept in language.concepts:
                if concept.id in all_concepts:
                    for form in concept.forms:
                        args.writer.add_form(
                            Language_ID=language.id,
                            Parameter_ID=slug(concept.id),
                            Value=form.value,
                            Form=simple_chars(form.form),
                            Loan=form.data["Loan"],
                            Source=datasets[language.dataset][0],
                            NumberValue=all_concepts[concept.id],
                            Comment=form.data["Comment"].strip() if form.data["Comment"] is not None else None,
                        )

        counts = defaultdict(int)
        with open(self.dir.joinpath("base_errors.md"), "w") as f:
            f.write("Language | Annotation | Annotator\n--- | --- | ---\n")
            for a, b, c in sorted(base_errors):
                f.write("{0} | {1} | {2}\n".format(a, b, c))
                counts[b, c] += 1

        for (a, b), c in counts.items():
            args.log.info(
                "Problematic annotation {0:40} by {1:20} occurs {2} times.".format(a, b, c)
            )
