import gzip
import json
import sys
import random

pref = sys.argv[1]
random.seed(42)

def parse(path):
  g = gzip.open(path, 'r')
  for l in g:
    yield json.loads(l)

db = []
fresh = 0
conv = {}
revs = {}

def idx(v):
  global fresh
  if not v in conv:
    conv[v] = fresh
    fresh += 1
  return conv[v]

def loadR():
  fr = open(pref + "_R.csv", "w")
  ft = open(pref + "_T.csv", "w")
  for review in parse(pref + ".json.gz"):
    if not 'summary' in review:
      continue
    r = review['reviewerID']
    s = review['overall']
    t = review['summary']
    p = review['asin']
    if len(r) > 0 and len(t) > 0 and len(p) > 0:
      r = idx(r)
      s = idx(s)
      t = idx(t)
      p = idx(p)
      if not p in revs:
        revs[p] = []
      if not (r, s) in revs[p]:
        revs[p].append((r, s))
      db.append("P0A3({},{},{})".format(r, s, p))
      db.append("P1A3({},{},{})".format(r, t, p))
      print("{},{},{}".format(r, s, p), file = fr)
      print("{},{},{}".format(r, t, p), file = ft)
  fr.close()
  ft.close()

brands = {}

def loadP():
  fp = open(pref + "_P.csv", "w")
  for prod in parse("meta_" + pref + ".json.gz"):
    if not 'brand' in prod or not 'asin' in prod:
      continue
    b = prod['brand']
    p = prod['asin']
    if b == "" or p == "":
      continue
    b = idx(b)
    p = idx(p)
    if not b in brands:
      brands[b] = []
    if not p in brands[b]:
      brands[b].append(p)
    db.append("P0A2({},{})".format(b, p))
    print("{},{}".format(b, p), file = fp)
  fp.close()

def loadB():
  fb = open(pref + "_B.csv", "w")
  for b, ps in brands.items():
    if len(ps) > 2:
      db.append("P0A1({})".format(b))
      print("{}".format(b), file = fb)
  fb.close()

def printDB():
  fdb = open(pref + ".db", "w")
  print(" ".join(db), file = fdb)
  fdb.close()
  flog = open(pref + ".log", "w")
  print("@0 " + " ".join(db), file = flog)
  flog.close()

loadR()
loadP()
loadB()
printDB()
