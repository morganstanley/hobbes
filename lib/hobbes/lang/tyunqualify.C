
#include <hobbes/lang/tyunqualify.H>
#include <hobbes/lang/typepreds.H>
#include <hobbes/lang/preds/class.H>
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
    } else if (!eqt->constraints().empty()) {
      // resolve satisfiable, satisfied predicates in this expression
      // class constraints with a unique instance are gathered and eliminated in one
      // traversal per contiguous run, since a rewrite per constraint (each copying
      // and then discarding the entire expression) dominates compile time for large
      // generated expressions like match tables
      //
      // so are the constraints that an unqualifier says it can eliminate together
      // (Unqualifier::batchable): an expression has a field constraint for each
      // field it reads, and a constructor constraint for each case it tests. The
      // parser made for a `parse {}` grammar has three of the first for each of
      // its states that follows a nonterminal and one of the second for each
      // nonterminal, and at two rewrites of the whole parser for each of them,
      // unqualifying a grammar of one long rule took a minute (OSS-Fuzz 586582947)
      //
      // (a run is eliminated when it ends, so a constraint after it that can't be
      // satisfied is reported before an error in eliminating the run would be)
      const Constraints& cs = eqt->constraints();
      TCInstConstraints batch;
      Constraints       uqBatch;
      UnqualifierPtr    uqBatchBy;

      auto flushBatch = [&]() {
        if (!batch.empty()) {
          result = unqualifyClassConstraints(tenv, batch, result, ds);
          batch.clear();
        }
        if (!uqBatch.empty()) {
          result = uqBatchBy->unqualifyBatch(tenv, uqBatch, result, ds);
          uqBatch.clear();
        }
      };

      for (const auto& c : cs) {
        UnqualifierPtr uq = tenv->lookupUnqualifier(c);

        if (!satisfiable(uq, tenv, c, ds)) {
          annmsgs msgs;
          uq->explain(tenv, c, e, ds, &msgs);
          if (!msgs.empty()) {
            throw annotated_error(msgs);
          } else {
            throw annotated_error(*e, "Unsatisfiable predicate: " + show(c));
          }
        } else if (satisfied(uq, tenv, c, ds)) {
          TCInstancePtr inst;
          if (const auto* tc = dynamic_cast<const TClass*>(uq.get())) {
            inst = tc->uniqueInstance(tenv, c, ds);
          }

          if (inst != TCInstancePtr()) {
            if (!uqBatch.empty()) {
              flushBatch();
            }
            batch.push_back(std::make_pair(c, inst));
          } else if (uq->batchable(tenv, c, ds)) {
            if (!batch.empty() || uq != uqBatchBy) {
              flushBatch();
            }
            uqBatchBy = uq;
            uqBatch.push_back(c);
          } else {
            flushBatch();
            result = uq->unqualify(tenv, c, result, ds);
          }
          changed = true;
        }
      }
      flushBatch();
    }
  }

  return result;
}

}

