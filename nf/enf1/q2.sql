EXPLAIN ANALYZE
SELECT P.x, P.y FROM P
LEFT JOIN (SELECT Q.x, R.y FROM Q, R) AS S
ON (P.x = S.x AND P.y = S.y)
WHERE S.x IS NULL