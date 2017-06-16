
#include <hobbes/lang/tyunqualify.H>
#include <hobbes/lang/typepreds.H>
#include <stdexcept>

namespace hobbes {

ExprPtr unqualifyTypes(const TEnvPtr& tenv, const ExprPtr& e, Definitions* ds) {
  ExprPtr result = e;
  bool    changed = true;

  while (changed) {
    changed = false;

    QualTypePtr eqt = result->type();
  
    if (eqt == QualTypePtr()) {
      throw annotated_error(
        *e,
        "Internal compiler error, cannot unqualify expression without explicit type annotations "
        "(did you forget to perform type-inference first?): " + show(result)
      );
    } else if (eqt->constraints().size() > 0) {
      // resolve satisfiable, satisfied predicates in this expression
      const Constraints& cs = eqt->constraints();
      for (const auto& c : cs) {
        UnqualifierPtr uq = tenv->lookupUnqualifier(c);

        if (!satisfiable(uq, tenv, c, ds)) {
          annmsgs msgs;
          uq->explain(tenv, c, e, ds, &msgs);
          if (msgs.size() > 0) {
            throw annotated_error(msgs);
          } else {
            throw annotated_error(*e, "Unsatisfiable predicate: " + show(c));
          }
        } else if (satisfied(uq, tenv, c, ds)) {
          result = uq->unqualify(tenv, c, result, ds);
          changed = true;
        }
      }
    }
  }

  return result;
}

}

