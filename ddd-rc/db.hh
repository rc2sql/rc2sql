#ifndef __LOG_H__
#define __LOG_H__

#include <climits>
#include <map>
#include <set>
#include <string>

static int let(char c) {
    return ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z');
}

static int num(char c) {
    return ('0' <= c && c <= '9');
}

static int anum(char c) {
    return let(c) || num(c);
}

static int ws(char c) {
    return c == ' ' || c == '\n' || c == '\r';
}

std::map<std::string, std::vector<std::vector<int> > > parse_db(const char *db_buf, size_t db_len) {
    std::map<std::string, std::vector<std::vector<int> > > res;
    size_t pos = 0;
    while (pos < db_len && db_len != -1) {
        while (pos < db_len && ws(db_buf[pos])) pos++;
        if (pos == db_len) break;
        if (!let(db_buf[pos])) throw std::runtime_error("corrupted database");
        std::string name;
        while (pos < db_len && anum(db_buf[pos])) name.push_back(db_buf[pos++]);
        while (pos < db_len && ws(db_buf[pos])) pos++;
        if (pos == db_len || db_buf[pos] != '(') throw std::runtime_error("corrupted database");
        pos++;
        std::vector<int> tuple;
        while (pos < db_len && db_buf[pos] != ')') {
            while (pos < db_len && ws(db_buf[pos])) pos++;
            if (pos < db_len && db_buf[pos] == ')') break;
            if (!num(db_buf[pos])) throw std::runtime_error("corrupted database");
            std::string tmp;
            while (pos < db_len && num(db_buf[pos])) tmp.push_back(db_buf[pos++]);
            errno = 0;
            long n = strtol (tmp.c_str(), NULL, 10);
            if (!(INT_MIN <= n && n <= INT_MAX && errno != ERANGE)) throw std::runtime_error("corrupted database");
            tuple.push_back(n);
            while (pos < db_len && ws(db_buf[pos])) pos++;
            if (pos < db_len && db_buf[pos] == ')') break;
            if (pos == db_len || db_buf[pos] != ',') throw std::runtime_error("corrupted database");
            pos++;
        }
        res[name].push_back(tuple);
        if (pos < db_len) pos++;
    }
    return res;
}

#endif
