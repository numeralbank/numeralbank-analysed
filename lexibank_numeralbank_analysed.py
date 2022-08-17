import pathlib
import zipfile
import itertools
import collections

import pycldf
from cldfbench import CLDFSpec
from pylexibank import Dataset as BaseDataset
from cltoolkit import Wordlist
from cltoolkit.features import FEATURES
from cldfzenodo import oai_lexibank
from pyclts import CLTS
from git import Repo, GitCommandError
from tqdm import tqdm
from csvw.dsv import reader

from clldutils.misc import slug



class Dataset(BaseDataset):
    dir = pathlib.Path(__file__).parent
    id = "numeralbank-analysed"


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

        with self.raw_dir.temp_download(CLTS_2_1[0], 'ds.zip', log=args.log) as zipp:
            zipfile.ZipFile(str(zipp)).extractall(self.raw_dir)


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
        for language in wl.languages:
            args.writer.add_language(
                    ID=language.id,
                    Name=language.name,
                    Glottocode=language.glottocode,
                    Latitude=language.latitude,
                    Longitude=language.longitude)
            for concept in language.concepts:
                print(language.name, language.family, concept)
                for form in concept.forms:
                    args.writer.add_form(
                            Language_ID=language.id,
                            Parameter_ID=slug(concept.id),
                            Value=form.value,
                            Form=form.form,
                            Source=""
                            )

