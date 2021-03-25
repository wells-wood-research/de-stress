# de-stress
DEsigned STRucture Evaluation ServiceS

![Front End Tests](https://github.com/wells-wood-research/de-stress/workflows/Front%20End%20Tests/badge.svg)
![Big Structure Tests](https://github.com/wells-wood-research/de-stress/workflows/Big%20Structure%20Tests/badge.svg)

# Deployment

Make sure you have all the relevant dependencies in
`de-stress/dependencies_for_de-stress/`. Currently, these are:

* EvoEF2 (source)
* DeFire
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
docker-compose -f development-compose.yml up -d
```

Navigate to `de-stress/database` and run `import_db_dump.sh`.
