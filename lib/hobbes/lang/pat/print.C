
#include <hobbes/lang/pat/print.H>
#include <hobbes/util/str.H>

namespace hobbes {

// "pretty print" an expression produced by pattern match compilation
void printMatchExp(std::ostream& out, const ExprPtr& e) {
  if (const App* ap = is<App>(e)) {
    if (const Var* fn = is<Var>(ap->fn())) {
      std::string fns = fn->value();
      if (fns == "leq") { fns = "=="; }

      if (fns == "==" || fns == "===" || fns == "and" || fns == "or") {
        printMatchExp(out, ap->args()[0]);
        out << " " << fns << " ";
        printMatchExp(out, ap->args()[1]);
        return;
      }
    }
  }

  e->show(out);
}

void printMatchResult(const std::string& indent, std::ostream& out, const ExprPtr& e) {
  if (const Let* le = is<Let>(e)) {
    out << indent << "let " << le->var() << " = ";
    if (is<Fn>(le->varExpr())) {
      out << "\n";
      printMatchResult(indent + "  ", out, le->varExpr());
      out << indent;
    } else {
      le->varExpr()->show(out);
      out << " ";
    }
    out << "in\n";
    printMatchResult(indent, out, le->bodyExpr());
  } else if (const LetRec* lr = is<LetRec>(e)) {
    out << indent << "letrec\n";
    for (const auto& b : lr->bindings()) {
      out << indent << "  " << b.first << " =" << std::endl;
      printMatchResult(indent + "    ", out, b.second);
    }
    out << indent << "in\n";
    printMatchResult(indent, out, lr->bodyExpr());
  } else if (const Fn* f = is<Fn>(e)) {
    out << indent << "\\(" << str::cdelim(f->varNames(), ", ") << ").\n";
    printMatchResult(indent + "  ", out, f->body());
  } else if (const App* ap = is<App>(e)) {
    if (const Var* fn = is<Var>(ap->fn())) {
      if (fn->value() == "if") {
        out << indent << "if (";
        printMatchExp(out, ap->args()[0]);
        out << ") then\n";
        printMatchResult(indent + "  ", out, ap->args()[1]);
        out << indent << "else\n";
        printMatchResult(indent + "  ", out, ap->args()[2]);
      } else {
        out << indent;
        e->show(out);
        out << "\n";
      }
    } else {
      out << indent;
      e->show(out);
      out << "\n";
    }
  } else if (const Switch* sw = is<Switch>(e)) {
    out << indent << "switch (";
    printMatchExp(out, sw->expr());
    out << ") {\n";
    for (auto b : sw->bindings()) {
      out << indent;
      b.value->show(out);
      out << " =>\n";
      printMatchResult(indent + "  ", out, b.exp);
    }
    out << indent << "default =>\n";
    printMatchResult(indent + "  ", out, sw->defaultExpr());
    out << indent << "}\n";
  } else {
    out << indent;
    e->show(out);
    out << "\n";
  }
}

void printMatchResult(std::ostream& out, const ExprPtr& e) {
  printMatchResult("", out, e);
}

// pretty-print a pattern match table
void printMatchTable(std::ostream& out, const PatternRows& prs) {
  if (prs.size() == 0) {
    out << "{}";
  } else {
    str::seqs ps;
    size_t    pc = prs[0].patterns.size();
    ps.resize(pc + 1);

    for (size_t c = 0; c < pc; ++c) {
      ps[c].resize(prs.size());

      for (size_t r = 0; r < prs.size(); ++r) {
        ps[c][r] = show(prs[r].patterns[c]);
      }
    }

    ps[pc].resize(prs.size());
    for (size_t r = 0; r < prs.size(); ++r) {
      if (prs[r].guard) {
        ps[pc][r] = "when " + show(prs[r].guard);
      } else {
        ps[pc][r] = "";
      }
    }

    str::seqs nps = str::rightAlign(ps);

    for (size_t r = 0; r < prs.size(); ++r) {
      out << "{ ";
      out << nps[0][r];
      for (size_t c = 1; c < nps.size(); ++c) {
        out << " | " << nps[c][r];
      }
      out << " } -> ";
      prs[r].result->show(out);
      out << "\n";
    }
  }
}

}

