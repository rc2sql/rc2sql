#ifndef TGC_H
#define TGC_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "dddcpp.h"

class command {
public:
  command();
  command(const varlist& v, const varlist& vp, const ddd& expr);
  command(const command& cmd);
  ~command();
  command& operator=(const command& cmd);
  void print() const;

  varlist m_v;
  varlist m_vp;
  ddd m_expr;
};


class command_list {
public:
  command_list();
  ~command_list();
  command_list(const command_list& c);
  command_list& operator=(const command_list& c);
  void print() const;
  unsigned int length() const;
  command& operator[](unsigned int i) const;
  void append(const command& c);
private:
  command *m_cmd;
  unsigned int m_max;
  unsigned int m_num;
};


typedef void (*ProgressFunc)(unsigned int i, const ddd& R);
void default_progress_func(unsigned int i, const ddd& R);

typedef ddd (*ReductionFunc)(const ddd& R);
ddd default_reduction_func(const ddd& R);


class tgc {
public:
  ddd m_init;
  command_list m_cmd;
  void set_progress_func(ProgressFunc fn);
  void set_reduction_func(ReductionFunc fn);
  tgc();
  ~tgc();
  void print() const;
  void run(void) const;

  void add_initial(const ddd& ini);
  void add_cmd(const command& c);
  void add_cmd(const varlist& v, const varlist& vp, const ddd& expr);

  ddd pre(const ddd& R, const command& c) const;
  ddd pre(const ddd& R) const;
  ddd pre_all(const ddd& R) const;
  ddd pre_i(const ddd& R) const;
  ddd prestar(const ddd& R) const;

  ddd post(const ddd& R, const command& c) const;
  ddd post(const ddd& R) const;
  ddd post_i(const ddd& R) const;
  ddd poststar(const ddd& R) const;

  // Returns true if property holds
  bool check_property(const ddd& prop) const;
  bool check_property_frontset(const ddd& prop) const;
  ddd reachable_state_space(void) const;
  ddd reachable_state_space_frontset(void) const;

private:
  ProgressFunc m_progress_func;
  ReductionFunc m_reduction_func;
};

#endif
