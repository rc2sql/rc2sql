#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <map>
#include <set>
#include <string>
#include <vector>

int read_res(FILE *f, std::vector<std::string> &rel, std::set<std::map<std::string, int> > &data) {
  char *buf = NULL;
  size_t len_buf = 0;
  size_t len = getline(&buf, &len_buf, f);
  if (len == -1) {
    printf("UNKNOWN\n");
    exit(EXIT_SUCCESS);
  }
  if (!strcmp(buf, "Infinite\n")) return 1;
  if (strcmp(buf, "Finite\n")) {
    printf("UNKNOWN\n");
    exit(EXIT_SUCCESS);
  }

  len = getline(&buf, &len_buf, f);
  if (len == -1) {
    printf("UNKNOWN\n");
    exit(EXIT_SUCCESS);
  }

  assert(len >= 2);
  assert(buf[0] == '(');
  assert(buf[len - 1] == ')' || (buf[len - 1] == '\n' && buf[len - 2] == ')'));
  int pos = 1;
  while (buf[pos] != ')') {
    std::string n;
    while (buf[pos] != ',' && buf[pos] != ')') {
      n.push_back(buf[pos++]);
    }
    rel.push_back(n);
    if (buf[pos] == ',') pos++;
  }

  while ((len = getline(&buf, &len_buf, f)) != -1) {
    if (!strcmp(buf, "\n")) continue;
    assert(len >= 2);
    assert(buf[0] == '(');
    assert(buf[len - 1] == ')' || (buf[len - 1] == '\n' && buf[len - 2] == ')'));
    int pos = 1;
    std::vector<int> tup;
    while (buf[pos] != ')') {
      std::string n;
      while (buf[pos] != ',' && buf[pos] != ')') {
        n.push_back(buf[pos++]);
      }
      tup.push_back(atoi(n.c_str()));
      if (buf[pos] == ',') pos++;
    }
    assert(rel.size() == tup.size());
    std::map<std::string, int> m;
    for (unsigned i = 0; i < tup.size(); i++) m[rel[i]] = tup[i];
    data.insert(m);
  }

  free(buf);

  return 0;
}

void usage() {
  fprintf(stderr, "\
Usage: cmp res1 res2 [res2neg]\n");
    exit(EXIT_FAILURE);
}

int main(int argc, char **argv) {
  if (argc != 3 && argc != 4) {
    usage();
  }

  std::vector<std::string> rel1;
  std::set<std::map<std::string, int> > data1;
  FILE *f1 = fopen(argv[1], "r");
  int inf1 = read_res(f1, rel1, data1);
  fclose(f1);

  std::vector<std::string> rel2;
  std::set<std::map<std::string, int> > data2;
  FILE *f2 = fopen(argv[2], "r");
  int inf2 = read_res(f2, rel2, data2);
  fclose(f2);

  if (argc == 4) {
    std::vector<std::string> rel3;
    std::set<std::map<std::string, int> > data3;
    FILE *f3 = fopen(argv[3], "r");
    int inf3 = read_res(f3, rel3, data3);
    fclose(f3);

    int ok = 1;
    for (auto it : data2) {
      if (data1.find(it) == data1.end()) {
        printf("POS WRONG;");
        ok = 0;
        break;
      }
    }
    for (auto it : data3) {
      if (data1.find(it) != data1.end()) {
        printf("NEG WRONG;");
        ok = 0;
        break;
      }
    }
    if (ok) printf("OK");
    printf("\n");
  } else {
    if (inf1 == inf2 && data1 == data2) {
      printf("OK\n");
    } else {
      printf("DIFF\n");
      /*
      for (auto it : data1) {
        if (data2.find(it) == data2.end()) {
          printf("ONLY IN data1:");
          for (auto it2 : it) printf(" %s->%d", it2.first.c_str(), it2.second);
          printf("\n");
        }
      }
      for (auto it : data2) {
        if (data1.find(it) == data1.end()) {
          printf("ONLY IN data2:");
          for (auto it2 : it) printf(" %s->%d", it2.first.c_str(), it2.second);
          printf("\n");
        }
      }
      */
    }
  }

  return 0;
}
