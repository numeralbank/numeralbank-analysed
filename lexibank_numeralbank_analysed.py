import pathlib
import zipfile
import itertools
import collections

import pycldf
from cldfbench import CLDFSpec
from pylexibank import Dataset as BaseDataset
from pylexibank import Language
from cltoolkit import Wordlist
from cltoolkit.features import FEATURES
from cldfzenodo import oai_lexibank
from pyclts import CLTS
from git import Repo, GitCommandError
from tqdm import tqdm
from csvw.dsv import reader
import json
import attr

from clldutils.misc import slug


def find_system(language, relations):
    scores = {}
    coverage = {}
    for system in relations:
        score, count = 0, 0
        coverage[system] = 0
        for conceptA, conceptB in relations[system]:
            hit = False
            try:
                for formA in language.concepts[conceptA].forms:
                    for formB in language.concepts[conceptB].forms:
                        if formA.form in formB.form:
                            hit = True
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
    return scores, coverage


@attr.s
class CustomLanguage(Language):
    BestBase = attr.ib(default=None)
    Bases = attr.ib(default=None)
    Base = attr.ib(default=None)



class Dataset(BaseDataset):
    dir = pathlib.Path(__file__).parent
    id = "numeralbank-analysed"
    language_class = CustomLanguage


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

        #with self.raw_dir.temp_download(CLTS_2_1[0], 'ds.zip', log=args.log) as zipp:
        #    zipfile.ZipFile(str(zipp)).extractall(self.raw_dir)


    def cmd_makecldf(self, args):

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

        with open(self.raw_dir.joinpath("unique_systems.json")) as f:
            relations = json.load(f)

        for language in wl.languages:
            scores, coverage = find_system(language, relations)
            # check for sufficient coverage
            if len(set(coverage.values())) == 1 and list(coverage.values())[0] == 0:
                args.log.info("Ignoring {0} with low coverage".format(language.name))
            else:
                if language.dataset == "numerals":
                    real_base = language.data["Base"]
                else:
                    real_base = "unknown"
                scoreS = " ".join(["{0}:{1:.2f}".format(k, v) for k, v in scores.items()])
                bestSystem = [k for k, v in sorted(scores.items(), key=lambda x: x[1],
                        reverse=True)][0]
                if len(set(scores.values())) == 1 and list(scores.values())[0] == 0:
                    bestSystem = ""
                if bestSystem and scores[bestSystem] < 0.1:
                    bestSystem = "Unknown"
                if bestSystem:
                    args.log.info("{0} / {1}".format(language.name, bestSystem))
                    args.writer.add_language(
                        ID=language.id,
                        Name=language.name,
                        Glottocode=language.glottocode,
                        Latitude=language.latitude,
                        Longitude=language.longitude,
                        Bases=scoreS,
                        BestBase=bestSystem,
                        Base=real_base
                    )
                    for concept in language.concepts:
                        for form in concept.forms:
                            args.writer.add_form(
                                    Language_ID=language.id,
                                    Parameter_ID=slug(concept.id),
                                    Value=form.value,
                                    Form=form.form,
                                    Source=""
                                    )

