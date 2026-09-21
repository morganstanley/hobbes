
#include <hobbes/hobbes.H>
#include <hobbes/ipc/nbindings.H>
#include <hobbes/ipc/net.H>
#include <hobbes/lang/preds/class.H>
#include <hobbes/lang/tyunqualify.H>
#include <map>
#include <memory>
#include <set>

namespace hobbes {

/******************
 * global set of client connections
 ******************/
using Connections = std::set<Client *>;
static Connections connections;

bool isAllocatedConnection(Client* c) {
  return connections.find(c) != connections.end();
}

Client* makeConnection(const std::string& hp) {
  auto* r = new Client(hp);
  connections.insert(r);
  return r;
}


/************
 * connection :: (Connection hostport c) => c
 *
 *   establish a compile-time connection to a remote process
 ************/
Client* decodeConnType(const MonoTypePtr& t) {
  if (const TApp* ap = is<TApp>(t)) {
    if (const Prim* apn = is<Prim>(ap->fn())) {
      if (apn->name() == "connection" && ap->args().size() == 1) {
        if (const TLong* ptr = is<TLong>(ap->args()[0])) {
          return reinterpret_cast<Client*>(ptr->value());
        }
      }
    }
  }
  return nullptr;
}

bool isPartialConnection(const MonoTypePtr& t) {
  if (const TApp* ap = is<TApp>(t)) {
    if (ap->args().size() == 1) {
      if (const Prim* apn = is<Prim>(ap->fn())) {
        return apn->name() == "connection";
      } else {
        return is<TVar>(ap->fn()) != nullptr;
      }
    }
  }
  return false;
}

MonoTypePtr makeConnType(Client* c) {
  return tapp(primty("connection"), list(tlong(reinterpret_cast<long>(c))));
}

// connect to a remote process at compile-time
class ConnectionP : public Unqualifier {
public:
  static std::string constraintName() { return "Connect"; }
  static std::string connectVar() { return "connection"; }

  // Resolving a (Connect "host:port" c) constraint opens a live outbound
  // TCP connection as a side effect of type-constraint resolution, not
  // evaluation -- merely type-checking untrusted text (a net REPL
  // prepare(), a web GET /?<expr>, ':t' on pasted code) can make the host
  // running the compiler connect out to an arbitrary network target,
  // before any decision to evaluate anything. Disabled unless the
  // embedding application opts in with an exact-match allowlist of
  // "host:port" strings.
  void enableConnecting(const std::set<std::string>& allowedHostPorts) {
    this->anyHostPort     = false;
    this->allowedHostPorts = allowedHostPorts;
  }
  void enableConnecting() {
    this->anyHostPort = true;
    this->allowedHostPorts.clear();
  }
  bool connectingAllowed(const std::string& hostport) const {
    return this->anyHostPort || this->allowedHostPorts.count(hostport) > 0;
  }

  bool refine(const TEnvPtr&, const ConstraintPtr& cst, MonoTypeUnifier* u, Definitions*) override {
    MonoTypePtr hostport;
    MonoTypePtr handle;

    if (decodeConstraint(cst, &hostport, &handle)) {
      if (Client* c = decodeConnType(handle)) {
        if (isAllocatedConnection(c)) {
          size_t uc = u->size();
          mgu(hostport, TString::make(c->remoteHost()), u);
          return uc != u->size();
        }
      } else if (const TString* hp = is<TString>(hostport)) {
        if (!connectingAllowed(hp->value())) {
          throw std::runtime_error(
            "Connect constraint rejected: refusing to open a connection to '" + hp->value() +
            "' during type-constraint resolution (remote connections are disabled by default; "
            "the embedding application must call cc::enableRemoteConnections to permit this "
            "target -- note that hi does not, for the compiler behind -p or -w)");
        }
        size_t uc = u->size();
        mgu(handle, makeConnType(makeConnection(hp->value())), u);
        return uc != u->size();
      }
    }
    return false;
  }

  bool satisfied(const TEnvPtr&, const ConstraintPtr& cst, Definitions*) const override {
    MonoTypePtr hostport;
    MonoTypePtr handle;

    if (decodeConstraint(cst, &hostport, &handle)) {
      if (Client* c = decodeConnType(handle)) {
        // 'connections' is process-global, so without this an already-open
        // connection made through some other cc would satisfy the constraint
        // here even though this cc allows no targets at all. Prune rather
        // than throw, the way ProcessP does -- only refine() reports the
        // rejection.
        return isAllocatedConnection(c) && connectingAllowed(c->remoteHost());
      }
    }
    return false;
  }

  bool satisfiable(const TEnvPtr&, const ConstraintPtr& cst, Definitions*) const override {
    MonoTypePtr hostport;
    MonoTypePtr handle;

    if (!decodeConstraint(cst, &hostport, &handle)) {
      return false;
    }
    if (const TString* hp = is<TString>(hostport)) {
      if (!connectingAllowed(hp->value())) {
        return false;
      }
    } else if (is<TVar>(hostport) == nullptr) {
      return false;
    }
    return (is<TVar>(handle) != nullptr) || isPartialConnection(handle);
  }

  // satisfied() prunes silently when the target is not allowed (so a
  // satisfiability probe does not abort the compile), which would otherwise
  // leave a bare "unsatisfied constraint Connect ..." with no hint that the
  // allowlist is the reason -- say so here
  void explain(const TEnvPtr&, const ConstraintPtr& cst, const ExprPtr& e, Definitions*, annmsgs* msgs) override {
    MonoTypePtr hostport, handle;
    if (decodeConstraint(cst, &hostport, &handle)) {
      std::string hp;
      if (Client* c = decodeConnType(handle)) {
        if (isAllocatedConnection(c)) { hp = c->remoteHost(); }
      } else if (const TString* s = is<TString>(hostport)) {
        hp = s->value();
      }
      if (!hp.empty() && !connectingAllowed(hp)) {
        msgs->push_back(annmsg("connecting to '" + hp + "' is not allowed here (see cc::enableRemoteConnections)", e->la()));
      }
    }
  }

  struct StripConnQual : public switchExprTyFn {
    const ConstraintPtr& constraint;
  
    StripConnQual(const ConstraintPtr& cst) : constraint(cst) {
    }
  
    ExprPtr wrapWithTy(const QualTypePtr& qty, Expr* e) const override {
      ExprPtr result(e);
      result->type(removeConstraint(this->constraint, qty));
      return result;
    }

    ExprPtr with(const Var* v) const override {
      if (v->value() == connectVar() && hasConstraint(this->constraint, v->type())) {
        return mktunit(v->la());
      } else {
        return wrapWithTy(v->type(), new Var(v->value(), v->la()));
      }
    }
  };

  ExprPtr unqualify(const TEnvPtr&, const ConstraintPtr& cst, const ExprPtr& e, Definitions*) const override {
    return switchOf(e, StripConnQual(cst));
  }

  PolyTypePtr lookup(const std::string& vn) const override {
    if (vn == connectVar()) {
      return polytype(2, qualtype(list(std::make_shared<Constraint>(constraintName(), list(tgen(0), tgen(1)))), tgen(1)));
    } else {
      return PolyTypePtr();
    }
  }

  SymSet bindings() const override {
    SymSet r;
    r.insert(connectVar());
    return r;
  }

  FunDeps dependencies(const ConstraintPtr&) const override {
    return list(FunDep(list(0), 1), FunDep(list(1), 0));
  }
private:
  bool                  anyHostPort = false;
  std::set<std::string> allowedHostPorts;

  static bool decodeConstraint(const ConstraintPtr& c, MonoTypePtr* hostport, MonoTypePtr* handle) {
    if (c->name() == constraintName() && c->arguments().size() == 2) {
      *hostport = c->arguments()[0];
      *handle   = c->arguments()[1];
      return true;
    }
    return false;
  }
};

/*******************************************
 * class Invoke ch expr inty outty | ch expr inty -> outty where
 *   invoke :: (connection ch, quote expr, inty) -> (promise ch outty)
 *******************************************/
class InvokeP : public Unqualifier {
public:
  static std::string constraintName() { return "Invoke"; }
  static std::string netInvoke() { return "invoke"; }

  // Resolving an (Invoke ch expr inty outty) constraint ships "expr" to
  // the remote peer at the other end of "ch" and has it evaluate/type it
  // to learn outty -- remote code execution triggered by local
  // type-constraint resolution, not evaluation, reachable from both
  // refine() and satisfied() (see remoteOutputType below). Disabled
  // unless the embedding application opts in with an exact-match
  // allowlist of the "host:port" strings (matching Client::remoteHost())
  // it trusts to invoke code on.
  void enableInvoking(const std::set<std::string>& allowedHostPorts) {
    this->anyHostPort      = false;
    this->allowedHostPorts = allowedHostPorts;
  }
  void enableInvoking() {
    this->anyHostPort = true;
    this->allowedHostPorts.clear();
  }
  bool invokingAllowed(const std::string& hostport) const {
    return this->anyHostPort || this->allowedHostPorts.count(hostport) > 0;
  }

  bool refine(const TEnvPtr&, const ConstraintPtr& cst, MonoTypeUnifier* u, Definitions*) override {
    MonoTypePtr ch, expr, inty, outty;
    if (decodeConstraint(cst, &ch, &expr, &inty, &outty)) {
      if (const TLong* chv = is<TLong>(ch)) {
        if (auto* conn = reinterpret_cast<Client*>(chv->value())) {
          if (const TExpr* exprv = is<TExpr>(expr)) {
            if (isAllocatedConnection(conn) && !hasFreeVariables(inty)) {
              size_t uc = u->size();
              mgu(outty, remoteOutputType(conn, exprv->expr(), inty), u);
              return uc != u->size();
            }
          }
        }
      }
    }
    return false;
  }

  bool satisfied(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const override {
    MonoTypePtr ch, expr, inty, outty;
    if (decodeConstraint(cst, &ch, &expr, &inty, &outty)) {
      if (const TLong* chv = is<TLong>(ch)) {
        if (auto* conn = reinterpret_cast<Client*>(chv->value())) {
          if (const TExpr* exprv = is<TExpr>(expr)) {
            if (isAllocatedConnection(conn) && !hasFreeVariables(inty)) {
              // like ProcessP, prune rather than throw out of a satisfaction
              // probe (satisfiable() lands here too) -- only refine() reports
              // the rejection
              if (!invokingAllowed(conn->remoteHost())) {
                return false;
              }
              return *outty == *remoteOutputType(conn, exprv->expr(), inty) &&
                     hobbes::satisfied(tenv, std::make_shared<Constraint>("BlockCodec", list(inty)), ds) &&
                     hobbes::satisfied(tenv, std::make_shared<Constraint>("BlockCodec", list(outty)), ds);
            }
          }
        }
      }
    }
    return false;
  }

  bool satisfiable(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const override {
    MonoTypePtr ch, expr, inty, outty;
    if (!decodeConstraint(cst, &ch, &expr, &inty, &outty)) { return false; }

    auto* chv = is<TLong>(ch);
    if (chv == nullptr) { return is<TVar>(ch) != nullptr; }
    auto* conn = reinterpret_cast<Client*>(chv->value());
    if ((conn == nullptr) || !isAllocatedConnection(conn)) { return false; }

    auto* exprv = is<TExpr>(expr);
    if (exprv == nullptr) { return is<TVar>(expr) != nullptr; }

    return hasFreeVariables(inty) || satisfied(tenv, cst, ds);
  }

  void explain(const TEnvPtr&, const ConstraintPtr& cst, const ExprPtr& e, Definitions*, annmsgs* msgs) override {
    MonoTypePtr ch, expr, inty, outty;
    if (decodeConstraint(cst, &ch, &expr, &inty, &outty)) {
      if (const TLong* chv = is<TLong>(ch)) {
        if (auto* conn = reinterpret_cast<Client*>(chv->value())) {
          if (isAllocatedConnection(conn) && !invokingAllowed(conn->remoteHost())) {
            msgs->push_back(annmsg("invoking code on '" + conn->remoteHost() + "' is not allowed here (see cc::enableRemoteInvocation)", e->la()));
          }
        }
      }
    }
  }

  struct RewriteInvokes : public switchExprTyFn {
    const ConstraintPtr& constraint;
    std::string          invokeFn;
  
    RewriteInvokes(const ConstraintPtr& cst, const std::string& invokeFn) : constraint(cst), invokeFn(invokeFn) {
    }
  
    ExprPtr wrapWithTy(const QualTypePtr& qty, Expr* e) const override {
      ExprPtr result(e);
      result->type(removeConstraint(this->constraint, qty));
      return result;
    }
  
    ExprPtr with(const Var* v) const override {
      if (v->value() == netInvoke() && hasConstraint(this->constraint, v->type())) {
        return var(this->invokeFn, removeConstraint(this->constraint, v->type()), v->la());
      } else {
        return wrapWithTy(v->type(), new Var(v->value(), v->la()));
      }
    }
  };

  ExprPtr unqualify(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds) const override {
    std::string invokeFn = makeInvokeFn(tenv, cst, ds, e->la());
    return switchOf(e, RewriteInvokes(cst, invokeFn));
  }

  PolyTypePtr lookup(const std::string& vn) const override {
    if (vn == netInvoke()) {
      // invoke :: (Invoke ch expr inty outty) => (connection ch, quote expr, inty) -> (promise ch outty)
      return polytype(4,
        qualtype(list(std::make_shared<Constraint>(constraintName(), list(tgen(0), tgen(1), tgen(2), tgen(3)))),
          functy(
            list(
              tapp(primty("connection"), list(tgen(0))),
              tapp(primty("quote"), list(tgen(1))),
              tgen(2)
            ),
            tapp(primty("promise"), list(tgen(0), tgen(3)))
          )
        )
      );
    } else {
      return PolyTypePtr();
    }
  }

  SymSet bindings() const override {
    SymSet r;
    r.insert(netInvoke());
    return r;
  }

  FunDeps dependencies(const ConstraintPtr&) const override {
    return list(FunDep(list(0, 1, 2), 3));
  }
private:
  bool                  anyHostPort = false;
  std::set<std::string> allowedHostPorts;

  // every path that ships an expression to the peer goes through this check
  // first: refine() and satisfied() via remoteOutputType below, unqualify()
  // via a direct call before remoteExpr
  void checkInvokingAllowed(Client* conn) const {
    if (!invokingAllowed(conn->remoteHost())) {
      throw std::runtime_error(
        "Invoke constraint rejected: refusing to invoke code on '" + conn->remoteHost() +
        "' during type-constraint resolution (remote invocation is disabled by default; "
        "the embedding application must call cc::enableRemoteInvocation to permit this "
        "target -- note that hi does not, for the compiler behind -p or -w)");
    }
  }

  MonoTypePtr remoteOutputType(Client* conn, const ExprPtr& e, const MonoTypePtr& inty) const {
    checkInvokingAllowed(conn);
    return conn->output(e, inty);
  }

  static bool decodeConstraint(const ConstraintPtr& c, MonoTypePtr* ch, MonoTypePtr* expr, MonoTypePtr* inty, MonoTypePtr* outty) {
    if (c->name() == constraintName() && c->arguments().size() == 4) {
      *ch    = c->arguments()[0];
      *expr  = c->arguments()[1];
      *inty  = c->arguments()[2];
      *outty = c->arguments()[3];
      return true;
    }
    return false;
  }

  std::string makeInvokeFn(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds, const LexicalAnnotation& la) const {
    MonoTypePtr ch, expr, inty, outty;
    if (decodeConstraint(cst, &ch, &expr, &inty, &outty)) {
      if (const TLong* chv = is<TLong>(ch)) {
        if (auto* c = reinterpret_cast<Client*>(chv->value())) {
          if (const TExpr* exprv = is<TExpr>(expr)) {
            if (isAllocatedConnection(c) && !hasFreeVariables(inty)) {
              MonoTypePtr retty = tapp(primty("promise"), list(ch, outty));
              // unqualify only runs after satisfied() returned true, so this
              // is already gated -- re-check anyway rather than leave a path
              // to the peer that depends on that ordering holding
              checkInvokingAllowed(c);
              uint32_t invid = c->remoteExpr(exprv->expr(), inty);

              MonoTypePtr unitt=primty("unit"), bytet=primty("byte"), intt=primty("int"), longt=primty("long");

              MonoTypePtr urfnty = functy(list(intt), opaqueptr<char>(false));
              MonoTypePtr rfnty  = functy(list(intt), tuplety(list(outty)));

              ConstraintPtr incst  = std::make_shared<Constraint>("BlockCodec", list(inty));
              ConstraintPtr outcst = std::make_shared<Constraint>("BlockCodec", list(tuplety(list(outty))));
              ExprPtr qinvokeFn =
                fn(str::strings(".ch", ".expr", ".x"),
                  // write the 'invoke expression' indicator byte
                  let(".i0", fncall(var("fdWriteByte", qualtype(functy(list(intt, bytet), unitt)), la), list(constant(static_cast<int>(c->fd()), la), constant(static_cast<uint8_t>(2), la)), la),

                  // write the ID of the remote expression to invoke
                  let(".i1", fncall(var("fdWriteInt", qualtype(functy(list(intt, longt), unitt)), la), list(constant(static_cast<int>(c->fd()), la), constant(static_cast<int>(invid), la)), la),

                  // write argument data for this invocation
                  let(".i2", fncall(var("writeTo", qualtype(list(incst), functy(list(intt, inty), unitt)), la), list(constant(static_cast<int>(c->fd()), la), var(".x", inty, la)), la),

                  // enqueue the read function for this expected result
                  let("r", fncall(var(".unsafeAppendClientReadFn", functy(list(longt, urfnty), longt), la), list(
                              constant(static_cast<long>(chv->value()), la),
                              fncall(var("unsafeCast", functy(list(rfnty), urfnty), la), list(var("readFrom", qualtype(list(outcst), rfnty), la)), la)
                           ), la),

                  // and then return the ID of this enqueued read function
                  assume(fncall(var("unsafeCast", functy(list(longt), retty), la), list(var("r", longt, la)), la), retty, la), la), la), la), la), la);

              qinvokeFn->type(qualtype(list(incst, outcst), functy(list(tapp(primty("connection"), list(ch)), tapp(primty("quote"), list(expr)), inty), retty)));
              ExprPtr invokeFn = unqualifyTypes(tenv, assume(qinvokeFn, qinvokeFn->type(), la), ds);

              std::string invokeFnName = ".cxn.invokeFn." + freshName();
              ds->push_back(Definition(invokeFnName, invokeFn));
              return invokeFnName;
            }
          }
        }
      }
    }
    throw std::runtime_error("Cannot produce codec function for invalid constraint: " + show(cst));
  }
};

/*******************************************
 * class Receive ch ty where
 *   receive :: (promise ch ty) -> ty
 *******************************************/
class ReceiveP : public Unqualifier {
public:
  static std::string constraintName() { return "Receive"; }
  static std::string receive()        { return "receive"; }

  bool refine(const TEnvPtr&, const ConstraintPtr&, MonoTypeUnifier*, Definitions*) override {
    return false;
  }

  bool satisfied(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const override {
    MonoTypePtr ch, rty;
    if (decodeConstraint(cst, &ch, &rty)) {
      if (const TLong* chv = is<TLong>(ch)) {
        if (auto* conn = reinterpret_cast<Client*>(chv->value())) {
          if (isAllocatedConnection(conn) && !hasFreeVariables(rty)) {
            return hobbes::satisfied(tenv, std::make_shared<Constraint>("BlockCodec", list(rty)), ds);
          }
        }
      }
    }
    return false;
  }

  bool satisfiable(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const override {
    MonoTypePtr ch, rty;
    if (!decodeConstraint(cst, &ch, &rty)) { return false; }

    auto* chv = is<TLong>(ch);
    if (chv == nullptr) { return is<TVar>(ch) != nullptr; }
    auto* conn = reinterpret_cast<Client*>(chv->value());
    if ((conn == nullptr) || !isAllocatedConnection(conn)) { return false; }

    return hasFreeVariables(rty) || satisfied(tenv, cst, ds);
  }

  void explain(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*, annmsgs*) override {
  }

  struct RewriteReceives : public switchExprTyFn {
    const ConstraintPtr& constraint;
    std::string          receiveFn;
  
    RewriteReceives(const ConstraintPtr& cst, const std::string& receiveFn) : constraint(cst), receiveFn(receiveFn) {
    }
  
    ExprPtr wrapWithTy(const QualTypePtr& qty, Expr* e) const override {
      ExprPtr result(e);
      result->type(removeConstraint(this->constraint, qty));
      return result;
    }
  
    ExprPtr with(const Var* v) const override {
      if (v->value() == receive() && hasConstraint(this->constraint, v->type())) {
        return var(this->receiveFn, removeConstraint(this->constraint, v->type()), v->la());
      } else {
        return wrapWithTy(v->type(), new Var(v->value(), v->la()));
      }
    }
  };

  ExprPtr unqualify(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds) const override {
    std::string receiveFn = makeReceiveFn(tenv, cst, ds, e->la());
    return switchOf(e, RewriteReceives(cst, receiveFn));
  }

  PolyTypePtr lookup(const std::string& vn) const override {
    if (vn == receive()) {
      // receive :: (Receive ch outty) => (promise ch ty) -> ty
      return polytype(2,
        qualtype(list(std::make_shared<Constraint>(constraintName(), list(tgen(0), tgen(1)))),
          functy(
            list(
              tapp(primty("promise"), list(tgen(0), tgen(1)))
            ),
            tgen(1)
          )
        )
      );
    } else {
      return PolyTypePtr();
    }
  }

  SymSet bindings() const override {
    SymSet r;
    r.insert(receive());
    return r;
  }

  FunDeps dependencies(const ConstraintPtr&) const override {
    return FunDeps();
  }
private:
  static bool decodeConstraint(const ConstraintPtr& c, MonoTypePtr* ch, MonoTypePtr* ty) {
    if (c->name() == constraintName() && c->arguments().size() == 2) {
      *ch = c->arguments()[0];
      *ty = c->arguments()[1];
      return true;
    }
    return false;
  }

  static std::string makeReceiveFn(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds, const LexicalAnnotation& la) {
    MonoTypePtr ch, ty;
    if (decodeConstraint(cst, &ch, &ty)) {
      if (const TLong* chv = is<TLong>(ch)) {
        if (auto* c = reinterpret_cast<Client*>(chv->value())) {
          if (isAllocatedConnection(c) && !hasFreeVariables(ty)) {
            // our receive function is uniquely determined by its connection and result type
            std::string recvFnName = ".cxn.recvFn." + str::from(chv->value()) + "." + str::from(reinterpret_cast<long>(ty.get()));

            // we only need to generate this function if we've never seen this return type before
            try {
              tenv->lookup(recvFnName);
              return recvFnName;
            } catch (std::exception&) {
              // this function is not defined
            }

            // we need to make it, just generate code to defer to Client::unsafeRead
            ExprPtr recvFn =
              fn("x",
                proj(
                  assume(
                    fncall(
                      var("unsafeCast", functy(list(opaqueptr<char>(false)), tuplety(list(ty))), la), list(
                        fncall(
                          var(".unsafeClientRead", functy(list(primty("long"), primty("long")), opaqueptr<char>(false)), la),
                          list(
                            constant(static_cast<long>(chv->value()), la),
                            var("x", primty("long"), la)
                          ),
                          la
                        )
                      ),
                      la
                    ),
                    tuplety(list(ty)),
                    la
                  ),
                  ".f0",
                  la
                ),
                la
              );
            recvFn->type(qualtype(functy(list(primty("long")), ty)));

            ds->push_back(Definition(recvFnName, recvFn));
            return recvFnName;
          }
        }
      }
    }
    throw std::runtime_error("Cannot produce codec function for invalid constraint: " + show(cst));
  }
};

// show a connection state
void printConnectionUF(long x) {
  auto* c = reinterpret_cast<Client*>(x);
  if (!isAllocatedConnection(c)) {
    throw std::runtime_error(".printConnection: handle is not a live connection");
  }
  c->show(std::cout);
}

// the handle in a 'connection N' type is just the number N, which an
// expression can write for itself -- the type is reachable from source, and
// nothing about it says the number came from makeConnection. Every use that
// dereferences the handle must therefore ask the registry whether it names a
// live connection first: these run while an expression is compiled, so a
// forged handle faulted the process during a net REPL 'prepare' or a :t,
// before any decision to evaluate anything.
Client* decodeLiveConnType(const MonoTypePtr& t) {
  Client* c = decodeConnType(t);
  if (c != nullptr && !isAllocatedConnection(c)) {
    throw std::runtime_error("not a live connection: " + show(t));
  }
  return c;
}

struct printConnectionF : public op {
  std::string showf;

  printConnectionF(const std::string& showf) : showf(showf) {
  }

  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr&, const Exprs& es) override {
    if (Client* conn = decodeLiveConnType(tys[0])) {
      ExprPtr wfrtfn = var(this->showf, functy(list(primty("long")), primty("unit")), es[0]->la());
      return c->compile(fncall(wfrtfn, list(constant(reinterpret_cast<long>(conn), es[0]->la())), es[0]->la()));
    } else {
      throw std::runtime_error("Internal error, invalid connection type: " + show(tys[0]));
    }
  }

  PolyTypePtr type(typedb&) const override {
    return polytype(1, qualtype(functy(list(tapp(primty("connection"), list(tgen(0)))), primty("unit"))));
  }
};

struct remoteHostF : public op {
  remoteHostF() = default;

  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr&, const Exprs& es) override {
    if (Client* conn = decodeLiveConnType(tys[0])) {
      return c->compile(ExprPtr(mkarray(conn->remoteHost(), es[0]->la())));
    } else {
      throw std::runtime_error("Internal error, invalid connection type: " + show(tys[0]));
    }
  }

  PolyTypePtr type(typedb&) const override {
    return polytype(1, qualtype(functy(list(tapp(primty("connection"), list(tgen(0)))), arrayty(primty("char")))));
  }
};

void initNetworkDefs(cc& c) {
  // we'll want to be able to make connections
  c.typeEnv()->bind(ConnectionP::constraintName(), UnqualifierPtr(new ConnectionP()));

  // remotely invoke functions
  c.typeEnv()->bind(InvokeP::constraintName(), UnqualifierPtr(new InvokeP()));

  // and read results. These raw-pointer bridges are only for generated code
  // (include/hobbes/ipc/net.H): bind them under dot-prefixed names that the
  // parser cannot produce, so user expressions cannot name them directly.
  // Client::unsafeAppendReadFn/unsafeRead additionally validate the handle
  // via isAllocatedConnection as defense in depth against serialized-AST Var
  // references reaching these bindings.
  c.typeEnv()->bind(ReceiveP::constraintName(), UnqualifierPtr(new ReceiveP()));
  c.bind(".unsafeAppendClientReadFn", &Client::unsafeAppendReadFn);
  c.bind(".unsafeClientRead",         &Client::unsafeRead);

  // some basic utility functions
  c.bind(".printConnection", &printConnectionUF);
  c.bindLLFunc("printConnection", new printConnectionF(".printConnection"));
  c.bindLLFunc("remoteHost", new remoteHostF());
}

static std::shared_ptr<ConnectionP> connectionUnqualifier(cc& c) {
  auto cp = std::dynamic_pointer_cast<ConnectionP>(c.typeEnv()->lookupUnqualifier(ConnectionP::constraintName()));
  if (!cp) {
    throw std::runtime_error("cannot allow remote connections: '" + ConnectionP::constraintName() + "' is bound to a replacement unqualifier, not the built-in one");
  }
  return cp;
}

static std::shared_ptr<InvokeP> invokeUnqualifier(cc& c) {
  auto ip = std::dynamic_pointer_cast<InvokeP>(c.typeEnv()->lookupUnqualifier(InvokeP::constraintName()));
  if (!ip) {
    throw std::runtime_error("cannot allow remote invocation: '" + InvokeP::constraintName() + "' is bound to a replacement unqualifier, not the built-in one");
  }
  return ip;
}

void enableRemoteConnections(cc& c, const std::set<std::string>& allowedHostPorts) {
  hlock _;
  connectionUnqualifier(c)->enableConnecting(allowedHostPorts);
}

void enableRemoteConnections(cc& c) {
  hlock _;
  connectionUnqualifier(c)->enableConnecting();
}

void enableRemoteInvocation(cc& c, const std::set<std::string>& allowedHostPorts) {
  hlock _;
  invokeUnqualifier(c)->enableInvoking(allowedHostPorts);
}

void enableRemoteInvocation(cc& c) {
  hlock _;
  invokeUnqualifier(c)->enableInvoking();
}

}

