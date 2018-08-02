
#include <hobbes/lang/constraints.H>
#include <hobbes/lang/typeinf.H>
#include <hobbes/util/array.H>
#include <hobbes/util/perf.H>

#include <hobbes/lang/preds/class.H>
#include <hobbes/lang/preds/not.H>

namespace hobbes {

// normalize constraints into a single representation as a sequence of mono types
void typeSeqForm(const ConstraintPtr& c, MonoTypes* mts) {
  const MonoTypes& args = c->arguments();

  mts->push_back(MonoTypePtr(TString::make(c->name())));
  mts->insert(mts->end(), args.begin(), args.end());
}

ConstraintSet::ConstraintSet() {
}

MaybePathPoints focusOnFundep(const VarIDs& vs, const MonoTypes& mts) {
  // we have to fudge the input IDs with a +1 offset for constraint names
  MaybePathPoints result;
  result.push_back(mts[0]);
  for (size_t i = 1; i < mts.size(); ++i) {
    if (in(int(i-1), vs)) {
      result.push_back(mts[i]);
    } else {
      result.push_back(MaybePathPoint());
    }
  }
  return result;
}

void ConstraintSet::insert(const TEnvPtr& tenv, const ConstraintPtr& c, MonoTypeUnifier* s) {
  // convert this constraint to a normalized form as a sequence of types
  MonoTypes cpath;
  typeSeqForm(c, &cpath);

  // if this constraint is already stored, we don't need to insert
  if (this->csts.lookup(cpath)) {
    return;
  }

  // OK, the constraint seems not to be stored
  // let's apply functional dependencies from this constraint across other constraints in this set
  // that may produce unifications that can further eliminate this constraint
  for (auto fd : tenv->lookupUnqualifier(c)->dependencies(c)) {
    Constraints mcs;
    this->csts.find(focusOnFundep(fd.first, cpath), &mcs);

    for (auto mc : mcs) {
      MonoTypes mcpath;
      typeSeqForm(mc, &mcpath);
      mgu(cpath[fd.second+1], mcpath[fd.second+1], s);
    }
  }

  // apply any substitutions that might have been made
  for (size_t i = 1; i < cpath.size(); ++i) {
    cpath[i] = s->substitute(cpath[i]);
  }

  // now only store the constraint if it's still unique
  if (!this->csts.lookup(cpath)) {
    this->csts.insert(cpath, c);
  }
}

Constraints ConstraintSet::constraints() const {
  return this->csts.values();
}

}

