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

Make sure you have all the relevant dependencies in
`de-stress/dependencies_for_de-stress/`. Currently, these are:

* Aggrescan3D
* DFIRE 2 pair
* DSSP
* EvoEF2 (source)
* Rosetta (source)

Create a `.env` file in the top level `de-stress` folder. You can copy
`de-stress/.env-testing` and update that. This 

Download `big_structure.dump` and place it in `de-stress/database`.

Next, from within `de-stress/`, build all the containers:

```bash
# use production-compose.yml if you're deploying in a production environment
docker-compose -f development-compose.yml build
```

Compile the dependencies in the container:

```bash
docker run \
    -it \
    --rm \
    -v /absolute/path/to/de-stress/dependencies_for_de-stress/:/dependencies_for_de-stress \
    de-stress_big-structure:latest \
    sh build_dependencies.sh
```

This will compile the software, but the output will be stored on the host machine as a
volume is used. This means that you cannot move or delete this folder while the
application is being served or it will break.

Launch the application:

```bash
# Change rq-worker to however many processes you want to use for analysis
docker-compose -f development-compose.yml --env-file .env up -d --scale rq-worker=4
```

Navigate to `de-stress/database` and run `import_db_dump.sh`.

## Headless DE-STRESS

The DE-STRESS webserver has a few limitations which are there to ensure the stability of the webserver. These limitations are listed below. 

* Only proteins with 500 residues or less can be uploaded.
* Only 30 files can be uploaded at a time.
* There is a max run time of 20 seconds for all the DE-STRESS metrics.

The headless version of DE-STRESS can be ran locally and the user can change the settings to run a larger set of PDB files. The code has been written to allow multiprocessing so that large amounts of files can be ran in a reasonable amount of time. The `.env-headless` file can be used to update the MAX_RUN_TIME, HEADLESS_DESTRESS_WORKERS and HEADLESS_DESTRESS_BATCH_SIZE variables to change the amount of seconds the DE-STRESS metrics are allowed to run, how many PDB files are in a batch, and how many processers should be used respectively. 

Firstly the docker image needs to be built. There is a different docker compose file called `headless-compose.yml` that needs to be used instead of the `development-compose.yml` file.  

```bash 
docker compose -f headless-compose.yml build
```

After this, make sure the dependencies have been built. The path `/absolute/path/to/de-stress/dependencies_for_de-stress/` needs to be replaced with the user's local path to the DE-STRESS dependencies. 

```bash
docker run -it --rm -v /absolute/path/to/de-stress/dependencies_for_de-stress/:/dependencies_for_de-stress de-stress-big-structure:latest sh build_dependencies.sh
```

Finally, run headless DE-STRESS with the following command and change the `/absolute/path/to/` to the the local file path to these folders. 

```bash
docker run -it --rm --env-file .env-headless -v /absolute/path/to/de-stress/dependencies_for_de-stress/:/dependencies_for_de-stress -v /absolute/path/to/input_path/:/input_path de-stress-big-structure:latest poetry run headless_destress /input_path
