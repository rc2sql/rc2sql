DROP TABLE IF EXISTS P;
DROP TABLE IF EXISTS Q;
DROP TABLE IF EXISTS R;

CREATE TABLE P(x INT, y INT);
CREATE TABLE Q(x INT, y INT);
CREATE TABLE R(y INT);

COPY P FROM '/home/rcsql/nf/enf2/p.csv' WITH (FORMAT csv);
COPY Q FROM '/home/rcsql/nf/enf2/q.csv' WITH (FORMAT csv);
COPY R FROM '/home/rcsql/nf/enf2/r.csv' WITH (FORMAT csv);
