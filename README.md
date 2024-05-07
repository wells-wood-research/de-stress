# de-stress
### DEsigned STRucture Evaluation ServiceS

![Front End Tests](https://github.com/wells-wood-research/de-stress/workflows/Front%20End%20Tests/badge.svg)
![Big Structure Tests](https://github.com/wells-wood-research/de-stress/workflows/Big%20Structure%20Tests/badge.svg)

DE-STRESS is a web application that provides a suite of tools for evaluating protein
designs. Our aim is to help make protein design more reliable, by providing tools to
help you select the most promising designs to take into the lab.

The application is available for non-commercial use through the following URL:

https://pragmaticproteindesign.bio.ed.ac.uk/de-stress/

## Citing DE-STRESS

If you use DE-STRESS, please cite the following article:

[Stam MJ and Wood CW (2021) DE-STRESS: A user-friendly web application for the evaluation of protein designs, Protein Engineering, Design and Selection, 34, gzab029.](https://academic.oup.com/peds/article/doi/10.1093/protein/gzab029/6462357)

## Contacting Us

If you find a bug or would like to request a feature, we'd really appreciate it if you
report it as an [issue](https://github.com/wells-wood-research/de-stress/issues). If
you're stuck and need help or have any general feedback, please create a post on the
[discussion page](https://github.com/wells-wood-research/de-stress/discussions).

For more information about our research group, check out our
[group website](https://www.wellswoodresearchgroup.com).

## Local Deployment

DE-STRESS can be installed locally as a web server (https://pragmaticproteindesign.bio.ed.ac.uk/de-stress/) or as a command line tool (headless DE-STRESS).

The DE-STRESS webserver has a few limitations which are there to ensure the stability of the webserver. These limitations are listed below.

* Only proteins with 500 residues or less can be uploaded.
* Only 30 files can be uploaded at a time.
* There is a max run time of 20 seconds for all the DE-STRESS metrics.

The headless version of DE-STRESS can be ran using the command line interface and the user can change the settings to run DE-STRESS on a larger set of PDB files. The code has been written to allow multiprocessing so that large amounts of files can be ran in a reasonable amount of time. The .env-headless file can be used to update the MAX_RUN_TIME, HEADLESS_DESTRESS_BATCH_SIZE and HEADLESS_DESTRESS_WORKERS variables to change the amount of seconds the DE-STRESS metrics are allowed to run, how many PDB files are in a batch, and how many CPUs should be used respectively.

Before installing either of these versions of DE-STRESS, make sure you have all the relevant licenses for the dependencies in
`de-stress/dependencies_for_de-stress/`. The current dependencies used by DE-STRESS are shown below.

* Aggrescan3D
* DFIRE 2 pair
* DSSP
* EvoEF2 (source)
* Rosetta (source)

Rosetta requires a commercial licence to install. In the future, we will offer a version of DE-STRESS without Rosetta but that is not available yet. 

## Local install of headless DE-STRESS

Run the setup.sh bash script to install a local version of headless DE-STRESS. This script will ask you which version of DE-STRESS you want, and after selecting headless DE-STRESS it will begin the install. After this, it will ask you if you want to install Rosetta and whether you have a licence for this software. If yes is selected, then it will begin an automatic install of Rosetta from the git repo https://github.com/RosettaCommons/rosetta. Once this has been installed, the dependencies(EvoEF2 and Rosetta) will be compiled from source code. Rosetta can take a long time to compile and this script will ask you how many CPUs to use for the compilation (if using 2 CPUs the compilation of Rosetta can take around 3 hours). 

```bash
./setup.sh
```

Once this script has finished running, the installation of headless DE-STRESS will be complete and you can run DE-STRESS on a set of PDB files using the below docker run command. 

```bash
docker run -it --rm --env-file .env-headless -v /absolute/path/to/de-stress/dependencies_for_de-stress/:/dependencies_for_de-stress -v /absolute/path/to/input_path/:/input_path de-stress-big-structure:latest poetry run headless_destress /input_path
```

You can change the settings in the .env-headless file to change the max run time, number of CPUs used and the batch size for the runs. Once this docker command has ran, a CSV file called design_data.csv will be saved in the input path which has the DE-STRESS metric for the set of PDB files. Also a logging.txt file is saved in the same folder. 

## Local install of the DE-STRESS web server

Firstly, download `big_structure.dump` and place it in `de-stress/database`. This is a .dump file of a PostgreSQL database that contains the pre-calculated DE-STRESS metrics for a set of structures from the Protein Data Bank (PDB). This database is used for the reference set functionality in DE-STRESS, which allows users to compare their designed proteins against a set of known proteins. 

After this run the setup.sh bash script to install a local version of the DE-STRESS webserver and follow the same steps as described above. This script wil lask you if you want to install the webserver in a development or production environment as well. The settings for the DE-STRESS webserver can be chaned in the .env file as well. 

```bash
./setup.sh
```

Next, navigate to /de-stress/front-end and run the below command to launch the user interface for the web server. Note npm needs to be installed locally to be able to do this. 

```bash
npm start
```

Finally, after this command has ran there will be a url link that can be clicked to view the user interface for the DE-STRESS web server.

