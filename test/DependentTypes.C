
#include <hobbes/hobbes.H>
#include "test.H"

using namespace hobbes;
static cc& c() { static __thread cc* x = 0; if (!x) { x = new cc(); } return *x; }

TEST(DependentTypes, SPrintf) {
  compile(&c(), c().readModule(R"END(
primty x = roll(|Prim=(x, nothing :: (() + Type))|) :: Type
arrayty x = roll(|Array=x|) :: Type
tstringty x = roll(|TString=x|) :: Type
mktuplety x = roll(|Record=map(\v.(".f"++show(v.0), v.1), zip([0L..size(x)-1], x))|) :: Type

process :: ([char], int) -> ^x.(() + ((Type * Type * Type) * x))
process x idx = match x with
 | '(?<pre>[^%]*)(?<fmt>%s)(?<suf>.*)' -> cons((tstringty(pre), tstringty(".f" ++ show(idx)), arrayty(primty("char"))), process(suf, idx+1))
 | '(?<pre>[^%]*)(?<fmt>%d)(?<suf>.*)' -> cons((tstringty(pre), tstringty(".f" ++ show(idx)), primty("int")), process(suf, idx+1))
 | _ -> cons((tstringty(x), primty("unit"), primty("unit")), nil())

doSPrintf x = let t = toArray(process(x, 0)) in mktuplety(map(\x.mktuplety([x.0, x.1, x.2]), t))


class SPrintF a t where
  sprintf_ :: (a, t) -> [char]
instance (a=((str * () * ()) * ())) => SPrintF a _ where
  sprintf_ r t = typeValueLower(unsafeCast(())::str)
instance (a=((str * acc * [char]) * t)) => SPrintF a _ where
  sprintf_ r t = concat([typeValueLower(unsafeCast(())::str), fieldValue(t)::(_/acc::_)=>_, sprintf_(tupleTail(r), t)])
instance (a=((str * acc * int) * t)) => SPrintF a _ where
  sprintf_ r t = concat([typeValueLower(unsafeCast(())::str), showInt(fieldValue(t)::(_/acc::_)=>_), sprintf_(tupleTail(r), t)])

mkSPrintFType :: (TypeApply a `doSPrintf` x) => x -> a
mkSPrintFType x = unsafeCast()

sprintf fmt args = sprintf_(mkSPrintFType(fmt), args)
  )END"));

  EXPECT_EQ(makeStdString(c().compileFn<const array<char>*()>("sprintf(`\"Hello %s %d!\"`, (\"World\", 3))")()), "Hello World 3!");
}

