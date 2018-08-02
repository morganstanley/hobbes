
#include <hobbes/parse/parser.H>
#include <hobbes/parse/lalr.H>
#include <hobbes/eval/cc.H>
#include <hobbes/lang/pat/pattern.H>

// useful for debugging generated expressions
#include <hobbes/lang/pat/print.H>

namespace hobbes {

// convert a user-defined parser definition to a CFG that can be used to derive LR parse tables
terminals extractGrammarRule(const ParseRule::Bindings& bs) {
  terminals r;
  for (const auto& b : bs) {
    r.push_back(b.second);
  }
  return r;
}

grammar extractGrammar(const Parser& p) {
  grammar g;
  for (const auto& r : p) {
    g[r.symbol].push_back(extractGrammarRule(r.bindings));
  }
  return g;
}

std::string constructorName(terminal* t) {
  std::ostringstream ss;
  t->show(ss);
  return ss.str();
}

// differentiate terminals and non-terminals, map rules to corresponding expressions
struct ParserEvalInfo {
  cc*               c;
  LexicalAnnotation la;
  MonoTypePtr       arrty;

  parserdef   pdef;
  lrtable     table;

  typedef std::map<terminal*, Exprs> ReduceExprs;
  ReduceExprs reduceExprs;
};

void prepareEvalInfo(cc* c, const Parser& p, terminal* root, const precedence& prec, ParserEvalInfo* pei, const LexicalAnnotation& la) {
  pei->c     = c;
  pei->la    = la;
  pei->arrty = freshTypeVar();

  // remember reduce expressions (translating bound names to canonical names)
  for (const ParseRule& pr : p) {
    VarMapping vm;
    for (size_t i = 0; i < pr.bindings.size(); ++i) {
      if (pr.bindings[i].first != "_") {
        vm[pr.bindings[i].first] = var(".v" + str::from(i), la);
      }
    }
    pei->reduceExprs[pr.symbol].push_back(substitute(vm, pr.reducer));
  }

  // compile an LALR(1) parser and table from this parser definition
  pei->pdef  = lalr1parser(extractGrammar(p), root);
  pei->table = lalrTable(pei->pdef, prec);
}

ExprPtr evalExpr(const ParserEvalInfo& pei, terminal* s, size_t rule) {
  auto re = pei.reduceExprs.find(s);
  if (re == pei.reduceExprs.end() || rule >= re->second.size()) {
    throw std::runtime_error("Internal error, reduce expr doesn't exist for " + constructorName(s) + "/" + str::from(rule));
  } else {
    return re->second[rule];
  }
}

MonoTypePtr parseVariant(const ParserEvalInfo& pei) {
  Variant::Members ms;
  ms.push_back(Variant::Member(".failure", primty("unit"), 0));
  ms.push_back(Variant::Member(".success", TVar::make("..r"), 1));
  size_t i = 2;
  for (const auto& re : pei.reduceExprs) {
    ms.push_back(Variant::Member(constructorName(re.first), TVar::make("." + constructorName(re.first)), i++));
  }
  return MonoTypePtr(Variant::make(ms));
}

MonoTypePtr parseResultType(const ParserEvalInfo& pei) {
  Variant::Members ms;
  ms.push_back(Variant::Member(".f0", primty("unit"), 0));
  ms.push_back(Variant::Member(".f1", TVar::make("..r"), 1));
  return MonoTypePtr(Variant::make(ms));
}

ExprPtr lengthOf(const ExprPtr& arr, const LexicalAnnotation& la) {
  return fncall(var("size", la), list(arr), la);
}

ExprPtr elementOf(const ExprPtr& arr, const ExprPtr& i, const LexicalAnnotation& la) {
  return fncall(var("element", la), list(arr, i), la);
}

size_t stateCount(const ParserEvalInfo& pei) {
  return pei.table.size();
}

bool isNonTerminal(const ParserEvalInfo& pei, terminal* t) {
  return pei.pdef.g.find(t) != pei.pdef.g.end();
}

bool needsOutputFunction(const ParserEvalInfo& pei, size_t i) {
  for (const auto& td : pei.table[i]) {
    if (td.second.isGoTo()) {
      return true;
    }
  }
  return false;
}

str::seq varNames(size_t n) {
  str::seq r;
  for (size_t i = 0; i < n; ++i) {
    r.push_back(".v" + str::from(i));
  }
  return r;
}

size_t parseDepth(const ParserEvalInfo& pei, size_t i) {
  auto sd = pei.pdef.state_defs.find(i);
  if (sd == pei.pdef.state_defs.end() || sd->second.size() == 0) {
    throw std::runtime_error("Internal error, can't find depth for invalid state #" + str::from(i));
  } else {
    size_t d = 0;
    for (const auto& itm : sd->second) {
      d = std::max<size_t>(d, itm.p);
    }
    return d;
  }
}

str::seq shiftVarNames(const ParserEvalInfo& pei, size_t fromS, size_t toS) {
  str::seq r;
  size_t m = parseDepth(pei, fromS) + 1;
  size_t n = parseDepth(pei, toS);
  for (size_t i = 0; i < n ;++i) {
    r.push_back(".v" + str::from((m - n) + i));
  }
  return r;
}

ExprPtr parseFailure(const ParserEvalInfo& pei, size_t i) {
  return assume(ExprPtr(new MkVariant(".failure", mktunit(pei.la), pei.la)), parseVariant(pei), pei.la);
}

// generate local function definitions for each parser state
ExprPtr makeOutputParserState(const ParserEvalInfo& pei, size_t i) {
  PatternRows prs;
  for (const auto& sp : pei.table[i]) {
    if (isNonTerminal(pei, sp.first)) {
      if (!sp.second.isGoTo()) {
        throw std::runtime_error("Internal error, non-goto non-terminal transition");
      }
      size_t j = sp.second.goToState();

      // |S=(i, vN)| -> sIo(arr, i, v0..vN, sJi(arr, i, v0..vN))
      prs.push_back(PatternRow(
        list(PatternPtr(
          new MatchVariant(
            constructorName(sp.first),
            PatternPtr(
              new MatchRecord(
                list(
                  MatchRecord::Field(".f0", PatternPtr(new MatchAny(".i", pei.la))),
                  MatchRecord::Field(".f1", PatternPtr(new MatchAny(".sdp", pei.la))),
                  MatchRecord::Field(".f2", PatternPtr(new MatchAny(".v" + str::from(parseDepth(pei, i)), pei.la)))
                ),
                pei.la
              )
            ),
            pei.la
          )
        )),

        fncall(var("ieq", pei.la), list(var(".sd", pei.la), var(".sdp", pei.la)), pei.la),

        fncall(
          var(".s" + str::from(i) + "o", pei.la),
          append(
            vars(append(str::strings(".arr", ".sd"), varNames(parseDepth(pei, i))), pei.la),
            list(fncall(var(".s" + str::from(j) + "i", pei.la), append(vars(str::strings(".arr", ".i"), pei.la), append(list(fncall(var("iadd", pei.la), list(var(".sd", pei.la), constant(static_cast<int>(1), pei.la)), pei.la)), vars(shiftVarNames(pei, i, j), pei.la))), pei.la))
          ),
          pei.la
        )
      ));
    }
  }
  prs.push_back(PatternRow(list(PatternPtr(new MatchAny("z", pei.la))), var("z", pei.la)));

  // \a i v0..vi r.match r with | |S0=(j, vi+1)| -> ... | _ -> fail
  return
    fn(append(str::strings(".arr", ".sd"), append(varNames(parseDepth(pei, i)), str::strings(".x"))),
      compileMatch(pei.c, list(var(".x", pei.la)), prs, pei.la),
      pei.la
    );
}

ExprPtr makeReduction(const ParserEvalInfo& pei, size_t i, terminal* sym, size_t rule) {
  return
    assume(
      ExprPtr(
        new MkVariant(
          constructorName(sym),
          ExprPtr(new MkRecord(
            list(
              MkRecord::FieldDef(".f0", var(".i", pei.la)),
              MkRecord::FieldDef(".f1", fncall(var("isub", pei.la), list(var(".sd", pei.la), constant(static_cast<int>(parseDepth(pei, i)), pei.la)), pei.la)),
              MkRecord::FieldDef(".f2", evalExpr(pei, sym, rule))
            ),
            pei.la
          )),
          pei.la
      )),
      parseVariant(pei),
      pei.la
    );
}

ExprPtr doAction(const ParserEvalInfo& pei, size_t i, const action& act) {
  if (act.isShift()) {
    // S -> E0 ... Ei #Ei+1 ... EN to S -> E0 ... Ei Ei+1 #... EN
    // becomes:
    // let
    //   j = i+1;
    //   sd' = sd+1
    // in
    //   sIo(arr, sd, v0..vN, sSi(arr, j, sd', v0..vN))
    ExprPtr shiftExpr =
      let(".j", fncall(var("ladd", pei.la), list(var(".i", pei.la), constant(static_cast<size_t>(1), pei.la)), pei.la),
        let(".sdp", fncall(var("iadd", pei.la), list(var(".sd", pei.la), constant(static_cast<int>(1), pei.la)), pei.la),
          fncall(var(".s" + str::from(act.shiftState()) + "i", pei.la), vars(append(str::strings(".arr", ".j", ".sdp"), shiftVarNames(pei, i, act.shiftState())), pei.la), pei.la),
          pei.la
        ),
        pei.la
      );

    if (needsOutputFunction(pei, i)) {
      return fncall(var(".s" + str::from(i) + "o", pei.la), append(vars(append(str::strings(".arr", ".sd"), varNames(parseDepth(pei, i))), pei.la), list(shiftExpr)), pei.la);
    } else {
      return shiftExpr;
    }
  } else if (act.isReduce()) {
    // S -> E0 E1 ... EN#
    // becomes:
    // |S=f(E0, E1, ..., EN)|
    return makeReduction(pei, i, act.reduceSym(), act.reduceRule());
  } else if (act.isAccept()) {
    // S -> E $#
    // becomes:
    // E
    return assume(ExprPtr(new MkVariant(".success", var(".v0", pei.la), pei.la)), parseVariant(pei), pei.la);
  } else {
    throw std::runtime_error("Internal error, can't perform unexpected parse action");
  }
}

ExprPtr makeInputParserState(const ParserEvalInfo& pei, size_t i) {
  const action* eofAct = 0;
  PatternRows prs;
  for (const auto& sp : pei.table[i]) {
    if (sp.first == endOfFile::value()) {
      eofAct = &sp.second;
    } else if (!isNonTerminal(pei, sp.first)) {
      prs.push_back(PatternRow(list(sp.first->matchPattern()), let(".v" + str::from(parseDepth(pei, i)), sp.first->matchRefExpr(), doAction(pei, i, sp.second), pei.la)));
    }
  }
  prs.push_back(PatternRow(list(PatternPtr(new MatchAny("_", pei.la))), parseFailure(pei, i)));

  // \a i sd ... match arr[i] with | $ -> ... | c0 -> ... | _ -> fail
  return
    fn(append(str::strings(".arr", ".i", ".sd"), varNames(parseDepth(pei, i))),
      // if (i == size(arr)) then eval-$ else eval-char
      fncall(var("if", pei.la), list(fncall(var("leq", pei.la), list(var(".i", pei.la), lengthOf(assume(var(".arr", pei.la), pei.arrty, pei.la), pei.la)), pei.la),
        eofAct ? doAction(pei, i, *eofAct) : parseFailure(pei, i),
        compileMatch(pei.c, list(elementOf(var(".arr", pei.la), var(".i", pei.la), pei.la)), prs, pei.la)
      ), pei.la),
      pei.la
    );
}

LetRec::Bindings makeParserStates(const ParserEvalInfo& pei) {
  LetRec::Bindings bs;
  for (size_t i = 0; i < stateCount(pei); ++i) {
    if (needsOutputFunction(pei, i)) {
      bs.push_back(LetRec::Binding(".s" + str::from(i) + "o", makeOutputParserState(pei, i)));
    }
    bs.push_back(LetRec::Binding(".s" + str::from(i) + "i", makeInputParserState(pei, i)));
  }
  return bs;
}

// generate parser code from a parser definition
ExprPtr makeParser(cc* c, const Parser& p, terminal* root, const precedence& prec, const LexicalAnnotation& la) {
  // first we need to derive the LALR(1) parse table
  // and derive secondary indexes for producing expressions for this table
  ParserEvalInfo pei;
  prepareEvalInfo(c, p, root, prec, &pei, la);

  // then we translate the parse table to an equivalent expression
  LetRec::Bindings sbs = makeParserStates(pei);

  // and a parser is just a function that takes a terminal array and evaluates the parse table expression
  ExprPtr e(
    new LetRec(sbs,
      compileMatch(
        pei.c,
        list(fncall(var(".s0o", la), list(var(".arr", la), constant(static_cast<int>(0), la), fncall(var(".s0i", la), list(assume(var(".arr", la), pei.arrty, la), constant(static_cast<size_t>(0), la), constant(static_cast<int>(0), la)), la)), la)),
        list(
          PatternRow(list(PatternPtr(new MatchVariant(".success", PatternPtr(new MatchAny("r", la)), la))), assume(ExprPtr(new MkVariant(".f1", ExprPtr(new Var("r", la)), la)), parseResultType(pei), la)),
          PatternRow(list(PatternPtr(new MatchAny("_", la))), assume(ExprPtr(new MkVariant(".f0", ExprPtr(new Unit(la)), la)), parseResultType(pei), la))
        ),
        pei.la
      ),
      pei.la
    )
  );

  return
    assume(
      fn(str::strings(".arr"), e, la),
      qualtype(list(ConstraintPtr(new Constraint("Array", list(pei.arrty, primty("char"))))), functy(list(pei.arrty), parseResultType(pei))),
      la
    );
}

ExprPtr makeParser(cc* c, const Parser& p, const precedence& prec, const LexicalAnnotation& la) {
  if (p.size() > 0) {
    return makeParser(c, p, p[0].symbol, prec, la);
  } else {
    throw std::runtime_error("Internal error, cannot produce parser from empty definition");
  }
}

void show(std::ostream& out, const Parser& p) {
  for (const auto& pr : p) {
    out << show(pr.symbol) << " ->";
    for (const auto& bv : pr.bindings) {
      out << " ";
      if (bv.first != "_") {
        out << bv.first << ":";
      }
      out << show(bv.second);
    }
    out << " { " << show(pr.reducer) << " }";
    out << std::endl;
  }
}

}

