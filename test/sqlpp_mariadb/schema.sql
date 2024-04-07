CREATE DATABASE sw;
USE sw;

CREATE TABLE planets (
    planet_id INT PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    climate VARCHAR(255),
    population BIGINT
);
