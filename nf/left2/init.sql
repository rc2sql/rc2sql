DROP TABLE IF EXISTS P;
DROP TABLE IF EXISTS Q;

CREATE TABLE P(x INT, y INT);
CREATE TABLE Q(x INT);

COPY P FROM '/home/rcsql/nf/left2/p.csv' WITH (FORMAT csv);
COPY Q FROM '/home/rcsql/nf/left2/q.csv' WITH (FORMAT csv);
