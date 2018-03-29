CREATE TABLE mta_bus_data
(
    vehicle                INTEGER     NOT NULL, 
    time_stamp             TEXT        NOT NULL,
    route                  TEXT,
    historical             FLOAT,
    recent                 FLOAT,
    schedule               FLOAT,
    hist_cum               FLOAT,
    rece_cum               FLOAT,
    sche_cum               FLOAT,
    predicted_t            FLOAT,
    measured_t             FLOAT       CHECK(measured_t >= 0),
    residual               FLOAT,
    abs_residual           FLOAT       CHECK(abs_residual >= 0),
    stop_gtfs_seq          INTEGER,
    phase                  TEXT,
    direction              INTEGER,
    dist_covered           FLOAT,
    dist_from_origin       FLOAT,
    total_trip_dist        FLOAT,
    depot                  TEXT,
    is_express             BOOLEAN,
    is_invalid             BOOLEAN,
    shape                  TEXT,
    stop_id                TEXT,
    block                  TEXT,
    service_date           FLOAT, 
    predicted_arrival      FLOAT,
    tail_stop_arr_time     FLOAT,

    PRIMARY KEY(vehicle, time_stamp, stop_gtfs_seq)
);

