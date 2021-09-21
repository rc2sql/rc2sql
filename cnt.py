import re
import sys

fname=sys.argv[1]
lines=open(fname, "r").readlines()
subs=[]
for l in lines[1:-1]:
  m=re.search(".....rat([0-9]*)\(([a0-9, ]*)\)", l)
  assert m is not None
  args=m.group(2).split(", ")
  n=len(args) if "" not in args else 0
  subs.append((m.group(1), n))
cnt="SELECT ({}) AS cost FROM {}".format(" + ".join(["{} * c{}.count".format(n, i) for (i, n) in subs]), ", ".join(["(SELECT COUNT(*) AS count FROM rat{}) AS c{}".format(i, i) for (i, n) in subs]))
print("".join(lines[1:-1]) + cnt)
