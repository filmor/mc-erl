DROP TABLE IF EXISTS worlds;
DROP TABLE IF EXISTS chunks;
DROP TABLE IF EXISTS selected_chunks;

CREATE TABLE worlds
(
        name text NOT NULL,
        id serial NOT NULL,
        CONSTRAINT worlds_pkey PRIMARY KEY (name )
);

CREATE TABLE chunks
(
        world integer NOT NULL,
        coord int8 NOT NULL,
        "timestamp" timestamp without time zone NOT NULL DEFAULT now(),
        selected boolean NOT NULL,
        data bytea,
        CONSTRAINT chunks_pkey PRIMARY KEY (world, coord, "timestamp" )
);

