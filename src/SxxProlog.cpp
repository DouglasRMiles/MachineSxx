// ConsoleApplication2.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"

/* prolog.c: a simple Prolog interpreter written in C++,               */
/*           including an example test run as main().                  */
/* Copyright (c) Alan Mycroft, University of Cambridge, 2000.          */
/*

        original from https://www.cl.cam.ac.uk/~am21/research/funnel/prolog.c

        Dmiles made trhe trail non static

*/

// #define LISP90 1
#define DEBUGGING 1
#define USE_CUT 1

void myBreak() {
  ;;;
}


int test_program_append(bool interactive);

#ifdef LISP90
#include "lisp90.cpp"
#else
#define nonvar char*
#define read_atom(s) s
// #define to_string(s) s
#endif

#define C_TEXT( text ) ((char*)std::string( text ).c_str())
#undef C_TEXT
#define C_TEXT( text ) ((char*)text)

#define MKATOM(TEXT) SymTable::GetAtom(TEXT) 
#define FACT(TEXT) new Clause(TEXT)
#define RULE(HEAD,...) new Clause(HEAD,__VA_ARGS__)

/*Psuedovars for source translation */
#define PSUEDOVAR(NAME)  Var* V(NAME) = new Var()
#define V(NAME) VAR_ ## NAME


/* Create a Callable Compound*/
#define CC(...) new Fun(__VA_ARGS__) 
/* Create a List*/
#define LL(...) CC(ATOM_dot,__VA_ARGS__)

#ifdef DEBUGGING
//#define DEBUG(mask,CPP) if ((mask & debugflags)!=0) CPP 
#define DEBUG(mask,CPP) CPP 
#else
#define DEBUG(mask,CPP) 
#endif

#include <iostream>
using namespace std;
#include <string.h>
#include <string>
#include <map>
#include <unordered_map>
#include <vector>

void indent(ostream& str, int n) {
  for (int i = 0; i < n; i++) str << "    ";
}
void indent(int n) {
  indent(cout, n);
}

class Prolog; class Term;

class Prog1Pred;
#define HashMap std::unordered_map<std::string, Prog1Pred*>

class PredTable
{
public:
  std::vector<HashMap> tables;

  PredTable()
  {
    tables = std::vector<HashMap>(33);
  }


  void InsertNameArity(const std::string& N, const int& A, Prog1Pred* Adr)
  {
    HashMap T = tables[A];
    if (T.empty())
    {
      tables[A] = T = HashMap();
    }
    T.emplace(N, Adr);
  }

  Prog1Pred* IsInPredTable(const std::string& N, const int& A)
  {
    if (tables[A].empty())
    {
      return nullptr;
    }
    return static_cast<Prog1Pred*>(tables[A][N]);
  }
};

class Prog1Pred;

class Sym {

#ifdef LISP90
  nonvar atomname;
#else
  std::string atomname;
#endif

public:
  Sym(const char* s) :
#ifdef LISP90
    atomname(read_atom(s)) {
  }
#else
    atomname(s) {
    // cout << "made '" << atomname << "' from '" << s << "'\n";         
  }
#endif
  virtual ostream& operator<<(ostream& str) {
    print(str); return(str);
  }
  virtual void print(ostream& str) {
    // str <<  "'" ; 
    str << atomname;
    // str << "'";
  }

  virtual Sym* copyAtom(Prolog* m) {
    return(this);
  }

  const char* c_str() {
    return(atomname.c_str());
  }

  bool eqatom(Sym* t) {
    return(strcmp(c_str(), t->c_str()) == 0);
  }

  // Might store preds here?
  // std::vector<Prog1Pred*> preds = std::vector<Prog1Pred*>(33);

};

#define AtomHashMap std::unordered_map<std::string,Sym*>
class SymTable
{
public:
  static AtomHashMap symtable;

  static Sym* GetAtom(const std::string N)
  {
    Sym* A = symtable[N];
    if (A == NULL) {
      symtable[N] = A = new Sym(N.c_str());
    }
    return A;
  }
};
AtomHashMap SymTable::symtable = AtomHashMap();


class Fun;
class Term {
public:
  void print() {
    print(cout);
  }
  virtual ostream& operator<<(ostream& str) {
    print(str); return(str);
  }
  virtual void print(ostream& str) = 0;
  virtual bool unifyTerm(Prolog*, Term*) = 0;
  virtual bool unifyStructure(Prolog*, Fun*) = 0;
  virtual Term* copyTerm(bool fresh, Prolog*) = 0;
  virtual void reset(Prolog*) = 0;
};


Sym* ATOM_nil = MKATOM("[]");
Sym* ATOM_dot = MKATOM(".");

class Fun : public Term {
  Sym* fsym; int arity; Term** Arguments;
private:

public:
  Fun(const char* f) : fsym(MKATOM(f)), arity(0), Arguments(NULL) {}
  Fun(Sym* f) : fsym(f), arity(0), Arguments(NULL) {}
  Fun(Sym* f, Term* a1) : fsym(f), arity(1), Arguments(new Term*[1]) { Arguments[0] = a1; };
  Fun(Sym* f, Term* a1, Term* a2) : fsym(f), arity(2), Arguments(new Term*[2]) { Arguments[0] = a1, Arguments[1] = a2; };
  Fun(Sym* f, Term* a1, Term* a2, Term* a3) : fsym(f), arity(3), Arguments(new Term*[3]) { Arguments[0] = a1, Arguments[1] = a2, Arguments[2] = a3; };
  Sym* name() { return fsym; }

  virtual void print(ostream& str) {
    if (fsym == ATOM_dot) {
      str << "[";
      for (int i = 0; i < arity; ) {
        Arguments[i++]->print(str);
        if (Arguments[i] == NULL)				continue;
        if (i < arity)	str << "|";
      }
      str << "]";
      return;
    }
    fsym->print(str);
    if (arity > 0) {
      str << "(";
      for (int i = 0; i < arity; ) {
        Arguments[i]->print(str);
        if (++i < arity)				str << ",";
      }
      str << ")";
    }
  }
  bool unifyTerm(Prolog* m, Term* t) {
    return(t->unifyStructure(m, this));
  }
  Term* copyTerm(bool fresh, Prolog* m) {
    return(copyStructure(fresh, m));
  }
  Fun* copyStructure(bool fresh, Prolog* m) {
    return(new Fun(fresh, m, this));
  }
  void reset(Prolog* m) {}

private:
  Fun(bool fresh, Prolog* m, Fun* p)
    : fsym(p->fsym->copyAtom(m)), arity(p->arity),
    Arguments(p->arity == 0 ? NULL : new Term*[p->arity]) {
    for (int i = 0; i < arity; i++) Arguments[i] = p->Arguments[i]->copyTerm(fresh, m);
  }
  virtual bool unifyStructure(Prolog* m, Fun* t);
};

class Var : public Term {
private:
  Term * instance;
  int varno;
  static int timestamp;
public:
  Var() : instance(this), varno(++timestamp) {
  }
  virtual void print(ostream& str) {
    if (instance != this) instance->print(str);
    else str << "_" << varno;
  };
  bool unifyTerm(Prolog* m, Term* t);
  Term* copyTerm(bool fresh, Prolog* m);
  virtual void reset(Prolog* m) {
    instance = this;
  }
  virtual bool unifyStructure(Prolog* m, Fun* t) {
    return(this->unifyTerm(m, t));
  }
};

int Var::timestamp = 0;

class Prog1Pred;
class TermVarMapping;

class Goal {
private:
  Fun * head; Goal* tail;
public:
  Goal(Fun* h) : head(h), tail(NULL) {}
  Goal(Fun* h, Goal* t) : head(h), tail(t) {}

  Goal* copyGoal(bool fresh, Prolog* m) {
    return(new Goal(head->copyStructure(fresh, m), tail == NULL ? NULL : tail->copyGoal(fresh, m)));
  }
  Goal* appendGoal(Goal* l) {
    return(new Goal(head, tail == NULL ? NULL : tail->appendGoal(l)));
  }
  virtual void print(ostream& str) { head->print(str); if (tail != NULL) { str << "; "; tail->print(str); } }
  virtual bool unifyGoal_Unused(Prolog* m, Goal* c2);
  int solve(Prolog* m, Prog1Pred* p, bool interactive, int results, int level, TermVarMapping* map);
};

class Prolog {
private:
  Var * tcar; Prolog* sofar;
  Prolog(Var* h, Prolog* t) : tcar(h), sofar(t) {
  }

public:
  Prolog() : tcar(NULL), sofar(NULL) {
  }
  Prolog* Note() {
    return(sofar);
  }
  Prolog* Push(Var* x) {
    return(sofar = new Prolog(x, sofar));
  }
  void Undo(Prolog* whereto) {
    for (; sofar != whereto; sofar = sofar->sofar)			sofar->tcar->reset(this);
  }
};

class Clause {
public:
  Fun * head; Goal* body;
  Clause(Fun* h, Goal* t) : head(h), body(t) {}
  Clause(Fun* h, Fun* t) : head(h), body(new Goal(t)) {}
  Clause(Fun* h) : head(h), body(NULL) {}
  virtual ostream& operator<<(ostream& str) {
    print(str); return(str);
  }

  virtual Clause* copyClause(bool fresh, Prolog* m) {
    return(new Clause(head->copyStructure(fresh, m), body == NULL ? NULL : body->copyGoal(fresh, m)));
  }

  bool unifyTerm_Unused(Prolog* m, Clause* c2) {
    Prolog* mark = m->Note();
    if (!(head->unifyTerm(m, c2->head) && body->unifyGoal_Unused(m, c2->body))) {
      m->Undo(mark);
      return false;
    }
    return true;
  }

  virtual Clause* copyForEdit() {
    auto tv = new Prolog();
    auto tvm = tv->Note();
    Clause* c3 = copyClause(false, tv);
    auto newtvm = tv->Note();
    if (newtvm != tvm) {
      // secretly this shouldnt really happen with copyClause(false, tv);
      tv->Undo(tvm);
    }
    return(c3);
  }

  virtual void print(ostream& str) {
    head->print(str);  str << " :- ";
    if (body == NULL) {
      str << "true";
    }
    else {
      body->print(str);
    }
  }

  Goal* appendBody(Fun* l) {
    Goal* item = new Goal(l);
    if (body == NULL) {
      body = item;
    }
    else {
      body->appendGoal(item);
    }
    return(item);
  }

};

class Prog1Pred {
public:
  Clause * pcar; Prog1Pred* pcdr;
  Prog1Pred(Clause* h) : pcar(h), pcdr(NULL) {}
  Prog1Pred(Clause* h, Prog1Pred* t) : pcar(h), pcdr(t) {}
  Prog1Pred(Clause* h, Clause* t) : pcar(h), pcdr(new Prog1Pred(t)) {}
  Prog1Pred(Clause* h, Clause* h2, Clause* t) : pcar(h), pcdr(new Prog1Pred(h2, t)) {}

  /* returns the newly appended Prog1Pred*/
  Prog1Pred* appendStatement(Clause* l) {
    if (pcdr != NULL) return(pcdr->appendStatement(l));
    pcdr = new Prog1Pred(l);
    return(pcdr);
  }

  /* returns itself*/
  Prog1Pred* prependStatement(Clause* h) {
    pcdr = new Prog1Pred(pcar, pcdr);
    pcar = h;
    return(this);
  }

};

bool Goal::unifyGoal_Unused(Prolog* m, Goal* c2) {
  Prolog* mark = m->Note();
  if (!(head->unifyTerm(m, c2->head) && tail->unifyGoal_Unused(m, c2->tail))) {
    m->Undo(mark);
    return false;
  }
  return true;
}


bool Fun::unifyStructure(Prolog* m, Fun* t) {
  Prolog* mark = m->Note();
  if (!(arity == t->arity && fsym->eqatom(t->fsym))) { m->Undo(mark); return(false); }
  for (int i = 0; i < arity; i++) if (!Arguments[i]->unifyTerm(m, t->Arguments[i])) { m->Undo(mark); return(false); }
  return(true);
};

bool Var::unifyTerm(Prolog* m, Term* t) {
  if (instance != this) return(instance->unifyTerm(m, t)); m->Push(this); instance = t; return(true);
}
Term* Var::copyTerm(bool fresh, Prolog* m) {
  if (instance == this) {
    if (!fresh) return this;
    m->Push(this);
    instance = new Var();
    return(instance);
  }
  return(instance);
}


class TermVarMapping {
private:
  Var * * varvar;
  const char** vartext;
  int size;
public:
  TermVarMapping(Var* vv[], const char* vt[], int vs)
    :varvar(vv), vartext(vt), size(vs) {
  }
  void showanswer(ostream& str) {
    if (size == 0)			str << "yes\n";
    else {
      for (int i = 0; i < size; i++) {
        str << vartext[i] << " = "; varvar[i]->print(str); str << "\n";
      }
    }
  }
};

Fun* NIL = CC(ATOM_nil);
Sym* ATOM_cut = MKATOM("!");
Fun* CUT = CC(ATOM_cut);

int Goal::solve(Prolog* m, Prog1Pred* p, bool interactive, int results, int level, TermVarMapping* map) {
  std::string line = "";
  ostream& str = cout;
  bool cutted = false;
  bool success = false;
  DEBUG(SOLVE, { indent(level); str << "solve@" << level << ": ";
       this->print(str); str << "\n"; });

  for (Prog1Pred* q = p; q != NULL; q = q->pcdr) {
    Prolog* t = m->Note();
    Clause* c = q->pcar->copyClause(true, m);
    int nextLevel = 1;
    m->Undo(t);
    DEBUG(SOLVE, { indent(level); str << "  try:"; c->print(str); str << "\n"; });
    if (head->unifyTerm(m, c->head)) {
      Goal* gdash = c->body == NULL ?  tail :  (nextLevel = 0, 
        c->body->appendGoal(tail));
      if (gdash != NULL) {
        if (gdash->head->name() == ATOM_cut) {
          gdash = NULL;
          cutted = true;
        }
      }
      if (gdash == NULL) {
        success = true;
        results++;
        map->showanswer(str);        
        if (interactive) {
          std::cout << "(ENTER) - next solution \t (stop+ENTER) - stop finding solution" << std::endl;
          std::getline(std::cin, line);
        }
      }
      else {
        int waz = gdash->solve(m, p,interactive,results, level + nextLevel, map);
        if (waz == 0) {
          DEBUG(SOLVE, { indent(level); str << "  parent fails.\n"; });
        }        
      }
    }
    else {
      DEBUG(SOLVE, { indent(level); str << "  nomatch.\n"; });
      if (interactive) {
        if(results!=0) {
        std::cout << "(ENTER) no more solutions" << std::endl; 
         std::getline(std::cin, line);
        }
      }
    }
    if (cutted) break;
    m->Undo(t);
    if (line == "stop") break;
  }
  return results;
}


/* A sample test program: append*/
int test_program_append_prev(bool interactive) {
  ostream& str = cout;

  Sym* at_app = MKATOM("append_3");
  Sym* at_cons = MKATOM(".");
  Fun* f_nil = new Fun(MKATOM("[]"));
  Fun* f_1 = new Fun(MKATOM("1"));
  Fun* f_2 = new Fun(MKATOM("2"));
  Fun* f_3 = new Fun(MKATOM("3"));

  Term* v_x = new Var();
  Fun* lhs1 = new Fun(at_app, f_nil, v_x, v_x);
  Clause* c1 = new Clause(lhs1);

  Term* v_l = new Var();
  Term* v_m = new Var();
  Term* v_n = new Var();
  Fun* rhs2 = new Fun(at_app, v_l, v_m, v_n);
  Fun* lhs2 = new Fun(at_app, new Fun(at_cons, v_x, v_l),
    v_m,
    new Fun(at_cons, v_x, v_n));
  Clause* c2 = new Clause(lhs2, new Goal(rhs2, NULL));

  Var* v_i = new Var();
  Var* v_j = new Var();
  Fun* rhs3 = new Fun(at_app, v_i, v_j,
    new Fun(at_cons, f_1,
      new Fun(at_cons, f_2,
        new Fun(at_cons, f_3, f_nil))));

  Goal* g1 = new Goal(rhs3, NULL);

  Prog1Pred* test_p = new Prog1Pred(c1, new Prog1Pred(c2));
  Prog1Pred* test_p2 = new Prog1Pred(c2, new Prog1Pred(c1));

  Var* varvar[] = { v_i, v_j };
  const char* varname[] = { "I", "J" };
  TermVarMapping* var_name_map = new TermVarMapping(varvar, varname, 2);

  Prolog* m = new Prolog();
  str << "=D=======Append with normal clause order:\n";
  g1->solve(m, test_p,true, 0,0, var_name_map);
  str << "\n=E=======Append with reversed normal clause order:\n";
  g1->solve(m, test_p2,true, 0,0, var_name_map);
  return(0);
}

#ifndef LISP90
int main(int argc, char* argv[]) {
  test_program_append( true);
  test_program_append_prev( true);
}
#endif


/* A sample test program: append*/

int test_program_append(bool interactive) {
  ostream& str = cout;
  /*Psuedovars for source translation */
  PSUEDOVAR(X);
  PSUEDOVAR(L);
  PSUEDOVAR(M);
  PSUEDOVAR(N);
  PSUEDOVAR(I);
  PSUEDOVAR(J);


  Sym* APPEND3 = MKATOM("append_3");




  /* append_3([],X,X). */
  Clause* c1 = FACT(CC(APPEND3, NIL, VAR_X, VAR_X));
  /*  append([X|LL],M,[X|N]):- append(LL,M,N). */
  Clause* c2 = RULE(CC(APPEND3, LL(VAR_X, VAR_L), VAR_M, LL(VAR_X, VAR_N)), CC(APPEND3, VAR_L, VAR_M, VAR_N));


  /*
           Test normally:

                  append_3([],X,X).
                  append([X|LL],M,[X|N]):- append(LL,M,N).
  */
  Prog1Pred* test_program_normally = new Prog1Pred(c1, c2);


  /*
          Test reversed:

                  append([X|LL],M,[X|N]):- append(LL,M,N).
                  append_3([],X,X).
  */
  Prog1Pred* test_program_reversed = new Prog1Pred(c2, c1);


  /*
           Test Cat:

                  append_3([],X,X):- !.
                  append([X|LL],M,[X|N]):- append(LL,M,N).
  */

  // #undef USE_CUT


  cout << "\n"; cout << "\n"; cout << "\n"; cout << "\n";
  c1->print(cout);
  cout << "\n"; cout << "\n"; cout << "\n"; cout << "\n";

#ifdef USE_CUT
  Clause* c3 = c1->copyForEdit();

  c3->appendBody(CUT);

  Prog1Pred* test_program_cut = new Prog1Pred(c3, c2);

  cout << "\n"; cout << "\n"; cout << "\n"; cout << "\n";
  c1->print(cout);
  cout << "\n"; cout << "\n"; cout << "\n"; cout << "\n";

  cout << "\n"; cout << "\n"; cout << "\n"; cout << "\n";
  c3->print(cout);
  cout << "\n"; cout << "\n"; cout << "\n"; cout << "\n";

#endif


  Var* varvar[] = { VAR_I,VAR_J };
  const char* varname[] = { "I", "J" };
  TermVarMapping* var_name_map = new TermVarMapping(varvar, varname, 2);

  /*
           ?- append_3(I,J,[1,2,3]).
  */
  Goal* g1 = new Goal(CC(APPEND3, VAR_I, VAR_J, LL(CC("1"), LL(CC("2"), LL(CC("3"), NIL)))));

  Prolog* m = new Prolog();

  str << "=A=======Append with normal clause order:\n";
  g1->solve(m, test_program_normally, true, 0,0, var_name_map);

  str << "\n=B========Append with reversed normal clause order:\n";
  g1->solve(m, test_program_reversed, true, 0,0, var_name_map);

#ifdef USE_CUT
  str << "\n=C=======Append with a cut:\n";
  g1->solve(m, test_program_cut, true, 0, 0,var_name_map);
#endif
  return(0);
}




