
#include <hobbes/hobbes.H>
#include <hobbes/lang/preds/class.H>
#include <hobbes/lang/tyunqualify.H>
#include <hobbes/ipc/nbindings.H>
#include <hobbes/ipc/net.H>
#include <map>

namespace hobbes {

/******************
 * global set of client connections
 ******************/
typedef std::set<Client*> Connections;
static Connections connections;

bool isAllocatedConnection(Client* c) {
  return connections.find(c) != connections.end();
}

Client* makeConnection(const std::string& hp) {
  Client* r = new Client(hp);
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
  return 0;
}

bool isPartialConnection(const MonoTypePtr& t) {
  if (const TApp* ap = is<TApp>(t)) {
    if (ap->args().size() == 1) {
      if (const Prim* apn = is<Prim>(ap->fn())) {
        return apn->name() == "connection";
      } else {
        return is<TVar>(ap->fn());
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

  bool refine(const TEnvPtr&, const ConstraintPtr& cst, MonoTypeUnifier* u, Definitions*) {
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
        size_t uc = u->size();
        mgu(handle, makeConnType(makeConnection(hp->value())), u);
        return uc != u->size();
      }
    }
    return false;
  }

  bool satisfied(const TEnvPtr&, const ConstraintPtr& cst, Definitions*) const {
    MonoTypePtr hostport;
    MonoTypePtr handle;

    if (decodeConstraint(cst, &hostport, &handle)) {
      if (Client* c = decodeConnType(handle)) {
        return isAllocatedConnection(c);
      }
    }
    return false;
  }

  bool satisfiable(const TEnvPtr&, const ConstraintPtr& cst, Definitions*) const {
    MonoTypePtr hostport;
    MonoTypePtr handle;
    
    return decodeConstraint(cst, &hostport, &handle) &&
           (is<TVar>(hostport) || is<TString>(hostport)) &&
           (is<TVar>(handle) || isPartialConnection(handle));
  }

  void explain(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*, annmsgs*) {
  }

  struct StripConnQual : public switchExprTyFn {
    const ConstraintPtr& constraint;
  
    StripConnQual(const ConstraintPtr& cst) : constraint(cst) {
    }
  
    ExprPtr wrapWithTy(const QualTypePtr& qty, Expr* e) const {
      ExprPtr result(e);
      result->type(removeConstraint(this->constraint, qty));
      return result;
    }

    ExprPtr with(const Var* v) const {
      if (v->value() == connectVar() && hasConstraint(this->constraint, v->type())) {
        return mktunit(v->la());
      } else {
        return wrapWithTy(v->type(), new Var(v->value(), v->la()));
      }
    }
  };

  ExprPtr unqualify(const TEnvPtr&, const ConstraintPtr& cst, const ExprPtr& e, Definitions*) const {
    return switchOf(e, StripConnQual(cst));
  }

  PolyTypePtr lookup(const std::string& vn) const {
    if (vn == connectVar()) {
      return polytype(2, qualtype(list(ConstraintPtr(new Constraint(constraintName(), list(tgen(0), tgen(1))))), tgen(1)));
    } else {
      return PolyTypePtr();
    }
  }

  SymSet bindings() const {
    SymSet r;
    r.insert(connectVar());
    return r;
  }

  FunDeps dependencies(const ConstraintPtr&) const {
    return list(FunDep(list(0), 1), FunDep(list(1), 0));
  }
private:
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

  bool refine(const TEnvPtr&, const ConstraintPtr& cst, MonoTypeUnifier* u, Definitions*) {
    MonoTypePtr ch, expr, inty, outty;
    if (decodeConstraint(cst, &ch, &expr, &inty, &outty)) {
      if (const TLong* chv = is<TLong>(ch)) {
        if (Client* conn = reinterpret_cast<Client*>(chv->value())) {
          if (const TExpr* exprv = is<TExpr>(expr)) {
            if (isAllocatedConnection(conn) && !hasFreeVariables(inty)) {
              size_t uc = u->size();
              mgu(outty, conn->output(exprv->expr(), inty), u);
              return uc != u->size();
            }
          }
        }
      }
    }
    return false;
  }

  bool satisfied(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const {
    MonoTypePtr ch, expr, inty, outty;
    if (decodeConstraint(cst, &ch, &expr, &inty, &outty)) {
      if (const TLong* chv = is<TLong>(ch)) {
        if (Client* conn = reinterpret_cast<Client*>(chv->value())) {
          if (const TExpr* exprv = is<TExpr>(expr)) {
            if (isAllocatedConnection(conn) && !hasFreeVariables(inty)) {
              return *outty == *conn->output(exprv->expr(), inty);
                     hobbes::satisfied(tenv, ConstraintPtr(new Constraint("BlockCodec", list(inty))), ds) &&
                     hobbes::satisfied(tenv, ConstraintPtr(new Constraint("BlockCodec", list(outty))), ds);
            }
          }
        }
      }
    }
    return false;
  }

  bool satisfiable(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const {
    MonoTypePtr ch, expr, inty, outty;
    if (!decodeConstraint(cst, &ch, &expr, &inty, &outty)) { return false; }

    TLong* chv = is<TLong>(ch);
    if (!chv) { return is<TVar>(ch); }
    Client* conn = reinterpret_cast<Client*>(chv->value());
    if (!conn || !isAllocatedConnection(conn)) { return false; }

    TExpr* exprv = is<TExpr>(expr);
    if (!exprv) { return is<TVar>(expr); }

    return hasFreeVariables(inty) || satisfied(tenv, cst, ds);
  }

  void explain(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*, annmsgs*) {
  }

  struct RewriteInvokes : public switchExprTyFn {
    const ConstraintPtr& constraint;
    std::string          invokeFn;
  
    RewriteInvokes(const ConstraintPtr& cst, const std::string& invokeFn) : constraint(cst), invokeFn(invokeFn) {
    }
  
    ExprPtr wrapWithTy(const QualTypePtr& qty, Expr* e) const {
      ExprPtr result(e);
      result->type(removeConstraint(this->constraint, qty));
      return result;
    }
  
    ExprPtr with(const Var* v) const {
      if (v->value() == netInvoke() && hasConstraint(this->constraint, v->type())) {
        return var(this->invokeFn, removeConstraint(this->constraint, v->type()), v->la());
      } else {
        return wrapWithTy(v->type(), new Var(v->value(), v->la()));
      }
    }
  };

  ExprPtr unqualify(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds) const {
    std::string invokeFn = makeInvokeFn(tenv, cst, ds, e->la());
    return switchOf(e, RewriteInvokes(cst, invokeFn));
  }

  PolyTypePtr lookup(const std::string& vn) const {
    if (vn == netInvoke()) {
      // invoke :: (Invoke ch expr inty outty) => (connection ch, quote expr, inty) -> (promise ch outty)
      return polytype(4,
        qualtype(list(ConstraintPtr(new Constraint(constraintName(), list(tgen(0), tgen(1), tgen(2), tgen(3))))),
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

  SymSet bindings() const {
    SymSet r;
    r.insert(netInvoke());
    return r;
  }

  FunDeps dependencies(const ConstraintPtr&) const {
    return list(FunDep(list(0, 1, 2), 3));
  }
private:
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

  static std::string makeInvokeFn(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds, const LexicalAnnotation& la) {
    MonoTypePtr ch, expr, inty, outty;
    if (decodeConstraint(cst, &ch, &expr, &inty, &outty)) {
      if (const TLong* chv = is<TLong>(ch)) {
        if (Client* c = reinterpret_cast<Client*>(chv->value())) {
          if (const TExpr* exprv = is<TExpr>(expr)) {
            if (isAllocatedConnection(c) && !hasFreeVariables(inty)) {
              MonoTypePtr retty = tapp(primty("promise"), list(ch, outty));
              uint32_t invid = c->remoteExpr(exprv->expr(), inty);

              MonoTypePtr unitt=primty("unit"), bytet=primty("byte"), intt=primty("int"), longt=primty("long");

              MonoTypePtr urfnty = functy(list(intt), opaqueptr<char>(false));
              MonoTypePtr rfnty  = functy(list(intt), tuplety(list(outty)));

              ConstraintPtr incst  = ConstraintPtr(new Constraint("BlockCodec", list(inty)));
              ConstraintPtr outcst = ConstraintPtr(new Constraint("BlockCodec", list(tuplety(list(outty)))));
              ExprPtr qinvokeFn =
                fn(str::strings(".ch", ".expr", ".x"),
                  // write the 'invoke expression' indicator byte
                  let(".i0", fncall(var("fdWriteByte", qualtype(functy(list(intt, bytet), unitt)), la), list(constant(static_cast<int>(c->fd()), la), constant(static_cast<uint8_t>(2), la)), la),

                  // write the ID of the remote expression to invoke
                  let(".i1", fncall(var("fdWriteInt", qualtype(functy(list(intt, longt), unitt)), la), list(constant(static_cast<int>(c->fd()), la), constant(static_cast<int>(invid), la)), la),

                  // write argument data for this invocation
                  let(".i2", fncall(var("writeTo", qualtype(list(incst), functy(list(intt, inty), unitt)), la), list(constant(static_cast<int>(c->fd()), la), var(".x", inty, la)), la),

                  // enqueue the read function for this expected result
                  let("r", fncall(var("unsafeAppendClientReadFn", functy(list(longt, urfnty), longt), la), list(
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

  bool refine(const TEnvPtr&, const ConstraintPtr&, MonoTypeUnifier*, Definitions*) {
    return false;
  }

  bool satisfied(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const {
    MonoTypePtr ch, rty;
    if (decodeConstraint(cst, &ch, &rty)) {
      if (const TLong* chv = is<TLong>(ch)) {
        if (Client* conn = reinterpret_cast<Client*>(chv->value())) {
          if (isAllocatedConnection(conn) && !hasFreeVariables(rty)) {
            return hobbes::satisfied(tenv, ConstraintPtr(new Constraint("BlockCodec", list(rty))), ds);
          }
        }
      }
    }
    return false;
  }

  bool satisfiable(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const {
    MonoTypePtr ch, rty;
    if (!decodeConstraint(cst, &ch, &rty)) { return false; }

    TLong* chv = is<TLong>(ch);
    if (!chv) { return is<TVar>(ch); }
    Client* conn = reinterpret_cast<Client*>(chv->value());
    if (!conn || !isAllocatedConnection(conn)) { return false; }

    return hasFreeVariables(rty) || satisfied(tenv, cst, ds);
  }

  void explain(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*, annmsgs*) {
  }

  struct RewriteReceives : public switchExprTyFn {
    const ConstraintPtr& constraint;
    std::string          receiveFn;
  
    RewriteReceives(const ConstraintPtr& cst, const std::string& receiveFn) : constraint(cst), receiveFn(receiveFn) {
    }
  
    ExprPtr wrapWithTy(const QualTypePtr& qty, Expr* e) const {
      ExprPtr result(e);
      result->type(removeConstraint(this->constraint, qty));
      return result;
    }
  
    ExprPtr with(const Var* v) const {
      if (v->value() == receive() && hasConstraint(this->constraint, v->type())) {
        return var(this->receiveFn, removeConstraint(this->constraint, v->type()), v->la());
      } else {
        return wrapWithTy(v->type(), new Var(v->value(), v->la()));
      }
    }
  };

  ExprPtr unqualify(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds) const {
    std::string receiveFn = makeReceiveFn(tenv, cst, ds, e->la());
    return switchOf(e, RewriteReceives(cst, receiveFn));
  }

  PolyTypePtr lookup(const std::string& vn) const {
    if (vn == receive()) {
      // receive :: (Receive ch outty) => (promise ch ty) -> ty
      return polytype(2,
        qualtype(list(ConstraintPtr(new Constraint(constraintName(), list(tgen(0), tgen(1))))),
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

  SymSet bindings() const {
    SymSet r;
    r.insert(receive());
    return r;
  }

  FunDeps dependencies(const ConstraintPtr&) const {
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
        if (Client* c = reinterpret_cast<Client*>(chv->value())) {
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
                          var("unsafeClientRead", functy(list(primty("long"), primty("long")), opaqueptr<char>(false)), la),
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
  reinterpret_cast<Client*>(x)->show(std::cout);
}

struct printConnectionF : public op {
  std::string showf;

  printConnectionF(const std::string& showf) : showf(showf) {
  }

  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr&, const Exprs& es) {
    if (Client* conn = decodeConnType(tys[0])) {
      ExprPtr wfrtfn = var(this->showf, functy(list(primty("long")), primty("unit")), es[0]->la());
      return c->compile(fncall(wfrtfn, list(constant(reinterpret_cast<long>(conn), es[0]->la())), es[0]->la()));
    } else {
      throw std::runtime_error("Internal error, invalid connection type: " + show(tys[0]));
    }
  }

  PolyTypePtr type(typedb&) const {
    return polytype(1, qualtype(functy(list(tapp(primty("connection"), list(tgen(0)))), primty("unit"))));
  }
};

struct remoteHostF : public op {
  remoteHostF() {
  }

  llvm::Value* apply(jitcc* c, const MonoTypes& tys, const MonoTypePtr&, const Exprs& es) {
    if (Client* conn = decodeConnType(tys[0])) {
      return c->compile(ExprPtr(mkarray(conn->remoteHost(), es[0]->la())));
    } else {
      throw std::runtime_error("Internal error, invalid connection type: " + show(tys[0]));
    }
  }

  PolyTypePtr type(typedb&) const {
    return polytype(1, qualtype(functy(list(tapp(primty("connection"), list(tgen(0)))), arrayty(primty("char")))));
  }
};

void initNetworkDefs(cc& c) {
  // we'll want to be able to make connections
  c.typeEnv()->bind(ConnectionP::constraintName(), UnqualifierPtr(new ConnectionP()));

  // remotely invoke functions
  c.typeEnv()->bind(InvokeP::constraintName(), UnqualifierPtr(new InvokeP()));

  // and read results
  c.typeEnv()->bind(ReceiveP::constraintName(), UnqualifierPtr(new ReceiveP()));
  c.bind("unsafeAppendClientReadFn", &Client::unsafeAppendReadFn);
  c.bind("unsafeClientRead",         &Client::unsafeRead);

  // some basic utility functions
  c.bind(".printConnection", &printConnectionUF);
  c.bindLLFunc("printConnection", new printConnectionF(".printConnection"));
  c.bindLLFunc("remoteHost", new remoteHostF());
}

}

