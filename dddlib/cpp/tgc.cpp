#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>

#include "tgc.h"

command::command() {
  m_expr = False;
}

command::command(const command& cmd) {
  m_v = cmd.m_v;
  m_vp = cmd.m_vp;
  m_expr = cmd.m_expr;
}

command::command(const varlist& v, const varlist& vp, const ddd& expr) {
  m_v = v;
  m_vp = vp;
  m_expr = expr;
}

command::~command() {
}

command& command::operator=(const command& cmd) {
  m_v = cmd.m_v;
  m_vp = cmd.m_vp;
  m_expr = cmd.m_expr;
  return *this;
}

void command::print() const {
  m_v.print();
  printf(":= any");
  m_vp.print();
  printf(" . ");
  PrintINFpretty(m_expr);
}


command_list::command_list() {
  m_cmd = new command[m_max=2];
  m_num = 0;
}

command_list::~command_list() {
  delete[] m_cmd;
}

command_list::command_list(const command_list& c) {
  m_cmd = new command[m_max=c.m_max];
  m_num = c.m_num;
  for (unsigned int i=0; i<m_num; i++)
    m_cmd[i] = c.m_cmd[i];
}

command_list& command_list::operator=(const command_list& c) {
  m_cmd = new command[m_max=c.m_max];
  m_num = c.m_num;
  for (unsigned int i=0; i<m_num; i++)
    m_cmd[i] = c.m_cmd[i];
  return *this;
}

void command_list::print() const {
  for (unsigned i=0; i<m_num; i++) {
    printf("  ");
    m_cmd[i].print();
    printf("\n");
  }
}

unsigned int command_list::length() const {
  return m_num;
}

command& command_list::operator[](unsigned int i) const {
  assert(i<m_num);
  return m_cmd[i];
}

void command_list::append(const command& c) {
  if (m_num==m_max) {
    command* tmp = new command[m_max*=2];
    for (unsigned int i=0; i<m_num; i++) 
      tmp[i] = m_cmd[i];
    delete[] m_cmd;
    m_cmd = tmp;
  }
  m_cmd[m_num++] = c;
}

void default_progress_func(unsigned int i, const ddd& R) {
  printf("[%d:%d,%g] ", i, NodeCount(R),PathCount(R));
  fflush(stdout);
}

ddd default_reduction_func(const ddd& R) {
  return R;
}

void tgc::set_progress_func(ProgressFunc fn) {
  m_progress_func = fn;
}

void tgc::set_reduction_func(ReductionFunc fn) {
  m_reduction_func = fn;
}

tgc::tgc() {
  m_init = True;
  set_progress_func(default_progress_func);
  set_reduction_func(default_reduction_func);
}

tgc::~tgc() {
}

void tgc::add_initial(const ddd& ini) {
  m_init &= ini;
}

void tgc::add_cmd(const command& c) {
  m_cmd.append(c);
}

void tgc::add_cmd(const varlist& v, const varlist& vp, const ddd& expr) {
  add_cmd(command(v,vp,expr));
}

void tgc::print() const {
  printf("initially:\n");
  PrintINFpretty(m_init);
  printf("commands:\n");
  m_cmd.print();
}

#define FP(X,F,I)                     \
  {                                   \
    X = I;                            \
    int __i = 0;                      \
    ddd _##X##_;                      \
    do {                              \
      m_progress_func(++__i,X);       \
      _##X##_ = X;                    \
      X = F;                          \
    } while (!Equivalent(X,_##X##_)); \
    X = _##X##_;                      \
  }

#define LFP(X,F) FP(X,F,False)
#define GFP(X,F) FP(X,F,True)

ddd tgc::pre(const ddd& R, const command& c) const {
  return Exists(c.m_vp, Replace(R,c.m_vp,c.m_v) & c.m_expr);
}

ddd tgc::pre(const ddd& R) const {
  ddd Ri = False;
  for (unsigned int i=0; i<m_cmd.length(); i++) {
    Ri |= pre(R, m_cmd[i]);
  }
  return Ri;
}

ddd tgc::pre_all(const ddd& R) const {
  ddd Ri = False;
  for (unsigned int i=0; i<m_cmd.length(); i++) {
    Ri |= pre(R, m_cmd[i]) & !pre(!R, m_cmd[i]);
  }
  return Ri;
}

ddd tgc::pre_i(const ddd& R) const {
  ddd Ri = R;
  for (unsigned int i=0; i<m_cmd.length(); i++) {
    Ri |= pre(Ri, m_cmd[i]);
  }
  return Ri;
}

ddd tgc::prestar(const ddd& R) const {
  ddd Ri;
  LFP(Ri, m_reduction_func(R | pre_i(Ri)));
  return Ri;
}


ddd tgc::post(const ddd& R, const command& c) const {
  return Replace(Exists(c.m_v, c.m_expr & R), c.m_v, c.m_vp);
}

ddd tgc::post(const ddd& R) const {
  ddd Ri = False;
  for (unsigned int i=0; i<m_cmd.length(); i++) {
    Ri |= post(R, m_cmd[i]);
  }
  return Ri;
}

ddd tgc::post_i(const ddd& R) const {
  ddd Ri = R;
  for (unsigned int i=0; i<m_cmd.length(); i++) {
    Ri |= post(Ri, m_cmd[i]);
  }
  return Ri;
}

ddd tgc::poststar(const ddd& R) const {
  ddd Ri;
  LFP(Ri, m_reduction_func(R | post_i(Ri)));
  return Ri;
}

bool tgc::check_property(const ddd& prop) const {
  ddd R = !prop; // set of bad states
  ddd RP;
  int i = 1;
  do {
    m_progress_func(i++,R);
    RP = R;
    R = m_reduction_func(R | pre_i(R));

    // If R and initial intersect, we can reach R from I
    if (Satisfiable(R & m_init))
      return false;
  }
  while (!Equivalent(R,RP));
  return true;
}

bool tgc::check_property_frontset(const ddd& prop) const {
  ddd R = !prop; // set of bad states
  ddd F = R;
  int i = 1;
  while (Satisfiable(F)) {
    m_progress_func(i++,R);
    if (Satisfiable(F & m_init))
      return false;
    F = m_reduction_func(pre_i(F) - R);
    R = m_reduction_func(R | F);
  }
  return true;
}

ddd tgc::reachable_state_space(void) const {
  return poststar(m_init);
}

ddd tgc::reachable_state_space_frontset(void) const {
  ddd R = m_init;
  ddd F = R;
  int i = 1;
  while (Satisfiable(F)) {
    m_progress_func(i++,R);
    F = m_reduction_func(post_i(F) - R);
    R = m_reduction_func(R | F);
  }
  return R;
}


/*
void tgc::run(void) const {
  ddd R = m_init;

  bool* cmd_enabled = new bool[m_cmd.length()];
  bool  time_enabled = false;
  bool  one_cmd_enabled = false;
  unsigned int i;

  system("stty erase ^?");

  printf("\nRunning program from the initial state.\n");
  for(;;) {
    printf("\n");
    for (i=0; i<m_cmd.length(); i++) {
      if ( (cmd_enabled[i] = (Satisfiable(R & m_cmd[i].m_guard) &&
			      !Consequence(post_d(R,m_cmd[i]), R)) ) ) {
	one_cmd_enabled = true;
	printf("\033[0;%dm", 32);
      }
      printf("%2d. ", i);
      m_cmd[i].print();
      printf("\n");
      printf("\033[0;%dm", 39);
    }
    if ( (time_enabled = !Consequence(post_t(R),R)) ) {
      printf("\033[0;%dm", 32);
    }
    printf("%2d. ", m_cmd.length());
    printf("Advance time");
    printf("\n");
    printf("\033[0;%dm", 39);

    if (!one_cmd_enabled && !time_enabled) {
      printf("Program is deadlocked.  Goodbye...\n\n");
      return;
    }
    
    for(;;) {
      int j;
      char buffer[1024];
      printf("Command (q to quit) > ");
      scanf("%s", buffer);
      if (!strcasecmp(buffer, "q")) { 
	delete [] cmd_enabled;
	return;
      }
      j = strtol(buffer, NULL, 10);
      if (j>=0 && (unsigned int)j<m_cmd.length()) {
	if (cmd_enabled[j]) {
	  R |= post_d(R,m_cmd[j]);
	  printf("State space = %d\n",NodeCount(R));
	  break;
	}
      }
      else if ((unsigned int)j==m_cmd.length()) {
	if (time_enabled) {
	  R |= post_t(R);
	  printf("State space = %d\n",NodeCount(R));
	  break;
	}
      }
    }
  }
}
*/


