docker exec --user postgres \
    lilis-web-stack_destress-database_1 \
    bash -c "createdb bigstructure && pg_restore -d bigstructure data/bigstructure.dump"
