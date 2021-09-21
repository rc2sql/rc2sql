import random
import sys

n=int(sys.argv[1])
m=int(sys.argv[2])

p=[[2*i, 2*i] for i in range(n)]
q=[[2*i] for i in range(max(n,m))]

random.shuffle(p)
random.shuffle(q)

f=open("p.csv", "w")
for x in p:
  print(",".join(map(str, x)), file=f)
f.close()

f=open("q.csv", "w")
for x in q[:m]:
  print(",".join(map(str, x)), file=f)
f.close()
