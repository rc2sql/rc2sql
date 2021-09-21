EXPLAIN ANALYZE
((SELECT P.x, P.y FROM P)
EXCEPT
(SELECT Q.x, Q.y FROM Q))
UNION
(SELECT P.x, P.y FROM P
LEFT JOIN R ON (P.y = R.y)
WHERE R.y IS NULL)
