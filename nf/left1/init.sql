DROP TABLE IF EXISTS P;
DROP TABLE IF EXISTS Q;

CREATE TABLE P(x INT, y INT);
CREATE TABLE Q(x INT, y INT);

COPY P FROM '/home/rcsql/nf/left1/p.csv' WITH (FORMAT csv);
COPY Q FROM '/home/rcsql/nf/left1/q.csv' WITH (FORMAT csv);
