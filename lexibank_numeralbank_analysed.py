import pathlib

import pycldf
from pylexibank import Dataset as BaseDataset
from pylexibank import Language, Lexeme
from cltoolkit import Wordlist
from pylexibank import progressbar
from cldfzenodo import oai_lexibank
from git import Repo, GitCommandError
import json
import attr
from lingpy import sw_align
from collections import defaultdict
from tabulate import tabulate
from unidecode import unidecode

from clldutils.misc import slug


def simple_chars(chars):

    return slug(unidecode(chars).replace("@", "a"))


def common_substring(seqA, seqB):
    almA, almB, _ = sw_align(seqA, seqB)
    alm_a, alm_b = almA[1], almB[1]
    start = False
    subs = []
    for a, b in zip(alm_a, alm_b):
        if a != b and not start:
            pass
        elif a == b and not start:
            start = True
        if start and a == b:
            subs += [a]
        elif start and a != b:
            break
    return len(subs)




def find_system(language, relations):
    scores = {}
    coverage = {}
    colexis = {}
    for system in relations:
        score, count = 0, 0
        coverage[system] = 0
        colexis[system] = {}
        for relation, conceptA, conceptB in relations[system]:
            hit = False
            if relation == "partial":
                try:
                    for formA in language.concepts[conceptA].forms:
                        for formB in language.concepts[conceptB].forms:
                            if unidecode(formA.form) in unidecode(formB.form):
                                hit = True
                                colexis[system][conceptA] = conceptB
                                break
                    if hit:
                        score += 1
                    count += 1
                    coverage[system] += 1
                except KeyError:
                    pass
        if count:
            scores[system] = score / count
        else:
            scores[system] = 0
    return scores, coverage, colexis


@attr.s
class CustomLexeme(Lexeme):
    NumberValue = attr.ib(default=None, metadata={"datatype": "integer"})


@attr.s
class CustomLanguage(Language):
    BestBase = attr.ib(default=None)
    Bases = attr.ib(default=None)
    Base = attr.ib(default=None)
    Coverage = attr.ib(
        default=None,
        metadata={
            'datatype': 'float',
            'dc:description': 'Coverage of the language in comparison with our master concept list.'}
    )
    OneToThirty = attr.ib(default=None)
    BaseInSource = attr.ib(default=None)


def coverage(language, concepts):
    return len([c.id for c in language.concepts if c.id in concepts]) / len(concepts)


class Dataset(BaseDataset):
    dir = pathlib.Path(__file__).parent
    id = "numeralbank-analysed"
    language_class = CustomLanguage
    lexeme_class = CustomLexeme


    def cmd_download(self, args):
        github_info = {rec.doi: rec.github_repos for rec in oai_lexibank()}

        for dataset, row in self.dataset_meta.items():
            ghinfo = github_info[row['Zenodo']]
            args.log.info("Checking {}".format(dataset))
            dest = self.raw_dir / dataset

            # download data
            if dest.exists():
                args.log.info("... dataset already exists.  pulling changes.")
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
                args.log.info('... checking out tag {}'.format(ghinfo.tag))
                repo.git.checkout(ghinfo.tag)
            else:
                args.log.warning('... could not determine tag to check out')
                args.log.info('... checking out master')
                try:
                    branch = repo.branches.main
                    branch.checkout()
                except AttributeError:
                    try:
                        branch = repo.branches.master
                        branch.checkout()
                    except AttributeError:
                        args.log.error('found neither main nor master branch')
                repo.git.merge()


    def cmd_makecldf(self, args):

        all_concepts = {concept["CONCEPTICON_GLOSS"]: concept["NUMBER_VALUE"] for concept in self.concepts}
        datasets = [ds["ID"] for ds in self.etc_dir.read_csv(
            "datasets.tsv",
            delimiter="\t", dicts=True
            )]
        wl = Wordlist([
            pycldf.Dataset.from_metadata(self.raw_dir.joinpath(
                ds, "cldf", "cldf-metadata.json")) for ds in datasets
            ])

        for concept in wl.concepts:
            args.writer.add_concept(
                    ID=slug(concept.id),
                    Name=concept.id,
                    Concepticon_ID=concept.concepticon_id,
                    Concepticon_Gloss=concept.id
                    )

        with open(self.raw_dir.joinpath("unique_relations.json")) as f:
            relations = json.load(f)

        # relations conversion for our detection method
        convert = {
            "Tener": "decimal",
            "Eighter": "octal",
            "Twentier": "vigesimal",
            "Fiver": "quinary",
            "Unknown": "unknown"
        }

        # how to represent basic relations in Chan, which are frequent enough
        # in the data
        target_bases = {
                "decimal": "decimal",
                "vigesimal": "vigesimal",
                "quinary": "quinary",
                "quinary AND decimal": "quinary/decimal",
                "quinary AND vigesimal": "quinary/vigesimal",
                "binary": "binary",
                "decimal AND vigesimal": "decimal/vigesimal",
                "duodecimal": "duodecimal",
                "octal": "octal",
                "quinary OR decimal": "quinary/decimal",
                "quinary AND vigesimal OR decimal": "quinary/vigesimal",
                "quinary AND double decimal": "quinary/decimal",
                "octal AND decimal": "octal",
                "octal AND duodecimal AND hexadecimal AND vigesimal AND tetravigesimal": "octal"
                }
        

        all_scores = []
        errors = defaultdict(list)
        for language in progressbar(wl.languages):
            scores, cov, colexis = find_system(language, relations)
            # check for sufficient coverage
            cov_ = coverage(language, all_concepts)
            # TODO adjust coverage here
            if cov_ < 0.25:
                pass
                #args.log.info("Ignoring {0} with low coverage".format(language.name))
            else:
                # TODO: check for other annotations on numerals
                if language.dataset == "numerals":
                    real_base = language.data["Base"]
                elif language.dataset == "sand":
                    real_base = language.data["Base"]
                else:
                    real_base = "unknown"
                scoreS = " ".join(["{0}:{1:.2f}".format(k, v) for k, v in scores.items()])
                bestSystems = [k for k, v in sorted(scores.items(), key=lambda x: x[1],
                        reverse=True)]
                bestSystem = bestSystems[0]
                #secondSystem = bestSystems[1]

                if len(set(cov.values())) == 1 and list(cov.values())[0] == 0:
                    bestSystem = ""
                    args.log.info("Ignoring {0} due to low coverage in concepts".format(language.name))

                if bestSystem and scores[bestSystem] < 0.05:
                    bestSystem = "Unknown"

                if bestSystem:
                    # check for correctness, can be expanded when more systems available
                    base_in_source = target_bases.get(real_base, "")
                    if real_base in ["quinary", "octal", "decimal", "vigesimal"]:
                        if real_base == convert[bestSystem]:
                            all_scores += [1]
                        elif convert[bestSystem] in real_base:
                            all_scores += [0.5]
                        else:
                            all_scores += [0]
                            errors[real_base, bestSystem] += [[
                                language,
                                scoreS,
                                colexis
                            ]]
                    else:
                        real_base = "unknown"
                    args.writer.add_language(
                        ID=language.id,
                        Name=language.name,
                        Glottocode=language.glottocode,
                        Latitude=language.latitude,
                        Longitude=language.longitude,
                        Macroarea=language.macroarea,
                        Bases=scoreS,
                        BestBase=convert[bestSystem],
                        Base=real_base,
                        Coverage=cov_,
                        BaseInSource=base_in_source
                    )
                    for concept in language.concepts:
                        if concept.id in all_concepts:
                            for form in concept.forms:
                                args.writer.add_form(
                                        Language_ID=language.id,
                                        Parameter_ID=slug(concept.id),
                                        Value=form.value,
                                        Form=simple_chars(form.form),
                                        Source="",
                                        NumberValue=all_concepts[concept.id]
                                        )
        args.log.info("Tests: {0}".format(len(all_scores)))
        args.log.info("Hits:  {0}".format(all_scores.count(1)))
        args.log.info("Fails: {0}".format(all_scores.count(0)))
        args.log.info("Props: {0:.2f}".format(sum(all_scores)/len(all_scores)))

        estring = ""
        for (gold, test), results in errors.items():
            args.log.info("{0:10} / {1:10} : {2}".format(gold, test, len(results)))
            estring += "# {0} / {1}\n".format(gold, test)
            for language, scoreS, colexis in results:
                estring += "## {0} / {1} / {2}\n\n".format(language.id, language.name, scoreS)
                table = []
                for concept in language.concepts:
                    if concept.id in all_concepts:
                        row = []
                        for system in ["Fiver", "Eighter", "Tener", "Twentier"]:
                            row += [colexis[system].get(concept.id, "")]
                        table += [[concept.id, " / ".join([unidecode(f.form) for f in concept.forms])]+row]
                estring += tabulate(table, tablefmt="pipe", headers=["Concept", "Forms", "Fiver", "Eighter", "Tener", "Twentier"])+"\n\n"
        with open(self.dir.joinpath("errors.md"), "w") as f:
            f.write(estring)
