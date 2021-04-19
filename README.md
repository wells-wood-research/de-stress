# de-stress
DEsigned STRucture Evaluation ServiceS

![Front End Tests](https://github.com/wells-wood-research/de-stress/workflows/Front%20End%20Tests/badge.svg)
![Big Structure Tests](https://github.com/wells-wood-research/de-stress/workflows/Big%20Structure%20Tests/badge.svg)

DE-STRESS is a web application that provides a suite of tools for evaluating protein
designs. Our aim is to help make protein design more reliable, by providing tools to
help you select the most promising designs to take into the lab.

# Citing DE-STRESS

If you use DE-STRESS, please cite the following article:

Stam MJ and Wood CW (2021)...

# Contacting Us

If you find a bug or would like to request a feature, we'd really appreciate it if you
report it as an [issue](https://github.com/wells-wood-research/de-stress/issues). If
you're stuck and need help or have any general feedback, please create a post on the
[discussion page](https://github.com/wells-wood-research/de-stress/discussions).

For more information about our research group, check out our
[group website](https://www.wellswoodresearchgroup.com).

# Local Deployment

Make sure you have all the relevant dependencies in
`de-stress/dependencies_for_de-stress/`. Currently, these are:

* Aggrescan3D
* DFire 2 pair
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
