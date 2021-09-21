This repository is the supplementary material for the paper

Practical Relational Calculus Query Evaluation

RC2SQL is a tool for translating relational calculus (RC) queries to SQL.
RC2SQL takes as input a RC query and a training database
and outputs a pair of RANF queries:
- one characterizing the query's relative safety
- and one equivalent to the query if the query is relatively safe.

VGTrans is our implementation of the translation by Van Gelder & Topor.
VGTrans has the same interface as RC2SQL.

---

# Directory Structure:

- `paper.pdf` - paper on RC2SQL
- `Dockerfile` - Dockerfile for this supplementary material
- `run_tests.sh` - script to run correctness tests
- `run.sh` - script to run the experiments
- `bold.*` - scripts for postprocessing evaluation results (e.g., highlighting fastest execution)
- `main.tex` - template of a LaTeX document with evaluation results
- `exps*.sh` - scripts for the individual experiments
- `rw*.sh` - scripts invoking a database engine to evaluate a pair of queries
after translation
- `mpreg.sh` - script evaluating a query using MonPoly-REG
- `radb.sh` - script for the translation from relational algebra (RA) to SQL
- `test_rw.sh` - script to test our translation and the implementation of VGTrans
against the verified implementation (whose output is guaranteed to be correct)
of the approach by Ailamazyan et al.
- `test_ranf.sh` - script to test the optimization of RANF queries
and their translation to SQL by comparing PostgreSQL, MySQL, SQLite, and VeriMon
- `cnt.py` - script for converting an SQL query Q produced by *radb*
into an SQL query computing the *query cost* of Q.
- `functions.sh` - helper bash functions
- `amazon/` - contains scripts for experiments in Figure 7
- `examples/` - example queries from this README
- `nf/` - benchmark for Example 22 and Section E.3.
- `src/` - RC2SQL's source code (in OCaml)
- `tools/` - tools for generating pseudorandom queries, Data Golf structures,
and for checking the translation's correctness and Data Golf properties

Further tools:

- `ailamazyan/` - our implementation of the approach by Ailamazyan et al.,
including the formally verified core (`verified.ml`)
and the underlying theories (in subdirectory `thys`)
- `dddlib/` - the implementation of Difference Decision Diagrams (DDD)
- `ddd-rc/` - our implementation of a tool using DDDs for evaluating RC queries
- `ldd-r6438/` - the implementation of Linear Decision Diagrams (LDD)
- `ldd-rc/` - our implementation of a tool using LDDs for evaluating RC queries
- `monpoly/` - the *MonPoly* tool and its verified core (*VeriMon*)
- `monpoly-reg-1.0/` - the *MonPoly-Reg* tool
- `radb/` - our modification of the *radb* tool

---

# Build

We recommend running the experiments using `docker` and the provided `Dockerfile`.
Please set up at least 8 GiB of main memory for your Docker container.
Note that the first command below will take some time to finish (roughly 20 minutes).
```
sudo docker build -t rc2sql .
sudo docker run -it rc2sql
```
Once you run the second command above you will
obtain a shell with all the tools installed.

We observed that several queries (e.g., the 7th query in the MEDIUM experiment) time out
if the setting `enable_nestloop = off` is omitted in the PostgreSQL configuration.
Hence, we set `enable_nestloop = off` in all our experiments (l. 77 in Dockerfile).

---

# Usage

To run RC2SQL type:

```

$ ./src/rtrans.native ${prefix}

```

and to run VGTrans type:

```
$ ./src/vgtrans.native ${prefix}
```

where

prefix  = prefix of the path to a text file with an RC query (`${prefix}.fo`)
and training database (`${prefix}.tdb`).

RC2SQL outputs a pair of queries (`${prefix}.afin`, `${prefix}.ainf`) using
the optimizations and a pair of queries (`${prefix}.sfin`, `${prefix}.sinf`)
without optimizations.

VGTrans outputs queries of the form `${prefix}.v{afin, ainf, sfin, sinf}`.

RC Syntax

```
{f} ::=   TRUE
        | FALSE
        | {ID}({s})
        | {ID} = {t}
        | NOT {f}
        | {f} AND {f}
        | {f} OR  {f}
        | EXISTS {ID} . {f}
        | FORALL {ID} . {f}

{t} ::=   {NUM}
        | {ID}

{s} ::=   %empty
        | {t}
        | {t} , {s}
```

where `{NUM}` is a nonnegative integer (constant)
and `{ID}` is an identifier (variable).
Non-terminals are enclosed in curly braces.

Database Syntax

```
{d} :=   %empty
       | {ID}({s})
       | {ID}({s}) {d}

{s} ::=   %empty
        | {NUM}
        | {NUM} , {s}
```

where `{NUM}` is a nonnegative integer and `{ID}` is an identifier.

---

# Example

An example is provided in the script `examples/ex.sh`.

A warning of the form `NOTICE:  table "tbl_*" does not exist, skipping`
is produced when the table `tbl_*` is dropped, but does not exist.
It is not an error and can safely be ignored.

The script contains example invocations of all the tools used in the artifact.

The script creates a random query using the tool `./tools/gen_test`. (You can invoke the tool without any arguments to see its usage.)
Then the script translates the query to RANF queries using RC2SQL and VGTrans tools and then to SQL using the *radb* tool. It creates both optimized and unoptimized translations.

It then sets up PostgreSQL and MySQL databases to contain an example database and evaluates the queries using Verimon, PostgreSQL, MySQL, tool by Ailamazyan et al, DDDs, LDDs, and MonPoly-Reg.
Results of the evaluation of all tools are then compared to the expected result provided by the Ailamazyan et al.'s tool (hence the "OK"s in the output).

Finally it computes the cost of each translated query using the `cnt.py` script.
Hence the output of the form:

```
 cost
------
 8660
(1 row)

 cost
------
    1
(1 row)

cost
8660
cost
1
```

We remark that a query evaluation's time can be influenced by other queries,
e.g., the one computing the query cost, that are evaluated before
if the database is not reinitialized (all relevant tables dropped and reloaded).
Hence, we reinitialize the relevant tables (i.e., those appearing in a query)
in the database before every query evaluation in our experiments.

---

# Evaluation

To reproduce the experiments from the paper, run

```
$ ./run.sh
```

The individual experiments are described in Section 5.
After the script `run.sh` finishes,
the results are contained in the files `exps*.tex`
used to plot Figure 6 and 7.

A PDF with the evaluation results can be obtained by executing
```
pdflatex main.tex
```

The timeout for the individual experiments can be set
in the scripts `exps*.sh`. With the unmodified timeouts of 300s and 600s, respectively,
the script would take overnight to recreate the tables.

Note. If you want to run the benchmarks with a tool
that is omitted in the paper because the tool always times out or crashes,
please uncomment the corresponding line in the script `exps*.sh`, e.g.,
to execute LDDs in the experiment LARGE, uncomment the following line
```
#line "\\ldd" run04LDD
```
in the script `exps_large.sh`.

---

# Query Cost

The following sequence of steps computes the query cost for RC2SQL and VGT
on the query Q^{susp} and dataset MI on which RC2SQL performs worse than VGT:
```
$ ./exps_susp.sh > exps_susp.tex
$ psql < z_7.psql
$ python3 cnt.py z_7.apsqlfin | psql
   cost
----------
 64277358
(1 row)

$ python3 cnt.py z_7.vapsqlfin | psql
   cost
----------
 76784956
(1 row)
```
Note that the queries `*inf` are FALSE for Q^{susp} and can thus be neglected in terms of their cost.
The query cost for VGT is actually higher than that for RC2SQL by a factor of 1.2x although
PostrgreSQL evaluates the query produced by VGT faster by a factor of 0.9x.

To confirm that the count aggregations optimize the inefficiency of VGT\-
compared to RC2SQL\-, we compute the query cost for Q^{susp} on a Data Golf structure
with `n = 20`. We choose this size of the Data Golf structure
to be able to compute the query cost within a reasonable time.

```
$ ./amazon/gen_test "/home/rcsql/z_0" 1 0 20 2 1 0
$ ./src/rtrans.native z_0
$ ./src/vgtrans.native z_0
$ ./radb.sh z_0
$ psql < z_0.psql
$ python3 cnt.py z_0.apsqlfin | psql     # cost of RC2SQL
 cost
------
 3287
(1 row)

$ python3 cnt.py z_0.spsqlfin | psql     # cost of RC2SQL-
  cost  
--------
 116140
(1 row)

$ python3 cnt.py z_0.vapsqlfin | psql    # cost of VGT
 cost 
------
 3605
(1 row)

$ python3 cnt.py z_0.vspsqlfin | psql    # cost of VGT-
  cost   
---------
 9293760
(1 row)
```

---

# Comparing query normal forms

Folder `nf/` contains empirical results of our comparison of safe-range normal form (SRNF) and existential normal form (ENF).

These results support our observations in Example 22.

Furthermore, the folder `nf/` contains an empirical comparison of `LEFT JOIN` vs `EXCEPT`.

Please refer to the `README.md` file in the folder for more details.

---

# Unsound results produced by MySQL 8.0.25

Running `for i in {0..9}; do ./test_ranf.sh 14 4 2 1 10 2 1 ${i}; done`
tests the translation of the queries used in the SMALL experiment
from RANF to SQL by comparing PostgreSQL, MySQL, SQLite, and VeriMon.
Here we use `n = 10` as the script `test_ranf.sh` cannot be used with a higher value of the parameter `n`.

The output (in the file `log.txt`)
```
RE: /home/rcsql/z_14_4_2_1_10_2_1_2/vam
RE: /home/rcsql/z_14_4_2_1_10_2_1_4/vam
DIFF(p-m): /home/rcsql/z_14_4_2_1_10_2_1_5/a
RE: /home/rcsql/z_14_4_2_1_10_2_1_7/vam
RE: /home/rcsql/z_14_4_2_1_10_2_1_9/vam
```
reports 5 inconsistencies between MySQL and PostgreSQL.
The verdict `DIFF` stands for unsound output while `RE` denotes a runtime error.
For instance, the last line reports a runtime error with MySQL (suffix `m`)
on the optimized query (infix `a`) produced by VGT (prefix `v`).
`DIFF(p-m)` denotes an output inconsistency between PostgreSQL and MySQL.
`DIFF(p-*.l)` denotes an output inconsistency between PostgreSQL and SQLite.
`DIFF(p-v)` denotes an output inconsistency between PostgreSQL and VeriMon.
`DIFF(v-v)` denotes an output inconsistency between optimized and unoptimized RANF queries.
`DIFF(dg)` denotes a violation of Data Golf properties (positive set is not contained
in the result or negative set has a nonempty intersection with the result)

Note. We remark that the output of MySQL can be nondeterministic, i.e.,
some of the above inconsistencies might not be reported at every execution
of `test_ranf.sh`.

---

# Tests

To test RC2SQL/VGTrans against the formally verified implementation
of the approach by Ailamazyan et al. run

```
$ ./run_tests.sh
```

In total, 40 tests are executed (they take roughly 5 minutes to finish).
The errors are printed into `log.txt` (the file does not exist if no error
has been found).
The tests are conducted on pseudorandom queries and databases.
The parameters are summarized here and can be easily adjusted in the file
`run_tests.sh`.

- Number of subqueries: 10
- Maximum arity of a subquery: 4
- Precise arity of the query: 2
- Free variables must be *generated*: 0/1
- Minimum number of tuples in a table: 4
- Minimum number of tuples in a training table: 2
- Data Golf strategy: 0/1

---

# Formalization of the approach by Ailamazyan et al. in Isabelle/HOL

The formal development can be browsed as a generated HTML page
(open `ailamazyan/html/index.html`).
An alternative way to study the theory files is to open them in Isabelle/jEdit.

The raw Isabelle sources are included in the directory `ailamazyan/thys`.

The formalization can been processed with the development version of Isabelle, which can be downloaded from

https://isabelle-dev.sketis.net/source/isabelle/

and the development version of AFP, which can be downloaded from

https://isabelle-dev.sketis.net/source/afp-devel/

To build the theories, export the verified OCaml code,
and regenerate the HTML page, run
`isabelle build -d '$AFP' -o browser_info -c -e -v -D thys/`
in the directory `ailamazyan/`.
This should take roughly 20 minutes.
Instructions where to find the verified OCaml code
and the generated html sources are printed in the console.

To build the pdf document, run
`~/isabelle/bin/isabelle document -d '$AFP' -d thys/ -O thys/ -v Eval_FO`
in the directory `ailamazyan/`.
Instructions where to find the pdf document are printed in the console.
