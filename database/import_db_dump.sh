docker exec --user postgres \
    de-stress_database_1 \
    bash -c "createdb bigstructure && pg_restore -d bigstructure data/bigstructure.dump"
