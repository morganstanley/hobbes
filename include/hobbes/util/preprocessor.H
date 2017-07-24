/*
 * preprocessor : very basic preprocessor metaprogramming
 */

#ifndef HOBBES_LANG_PREPROCESSOR_HPP_INCLUDED
#define HOBBES_LANG_PREPROCESSOR_HPP_INCLUDED

namespace hobbes {

#define HOBPP_FIRST(a, ...) a
#define HOBPP_SECOND(a, b, ...) b
#define HOBPP_CAT(a,b) a ## b
#define HOBPP_IS_PROBE(...) HOBPP_SECOND(__VA_ARGS__, 0)
#define HOBPP_PROBE() ~, 1
#define HOBPP_NOT(x) HOBPP_IS_PROBE(HOBPP_CAT(HOBPP_SNOT_, x))
#define HOBPP_SNOT_0 HOBPP_PROBE()
#define HOBPP_BOOL(x) HOBPP_NOT(HOBPP_NOT(x))
#define HOBPP_IF_ELSE(condition) HOBPP_SIF_ELSE(HOBPP_BOOL(condition))
#define HOBPP_SIF_ELSE(condition) HOBPP_CAT(HOBPP_SIF_, condition)
#define HOBPP_SIF_1(...) __VA_ARGS__ HOBPP_SIF_1_ELSE
#define HOBPP_SIF_0(...)             HOBPP_SIF_0_ELSE
#define HOBPP_SIF_1_ELSE(...)
#define HOBPP_SIF_0_ELSE(...) __VA_ARGS__
#define HOBPP_EMPTY()
#define HOBPP_EVAL(...) HOBPP_EVAL256(__VA_ARGS__)
#define HOBPP_EVAL256(...) HOBPP_EVAL128(HOBPP_EVAL128(__VA_ARGS__))
#define HOBPP_EVAL128(...) HOBPP_EVAL64(HOBPP_EVAL64(__VA_ARGS__))
#define HOBPP_EVAL64(...) HOBPP_EVAL32(HOBPP_EVAL32(__VA_ARGS__))
#define HOBPP_EVAL32(...) HOBPP_EVAL16(HOBPP_EVAL16(__VA_ARGS__))
#define HOBPP_EVAL16(...) HOBPP_EVAL8(HOBPP_EVAL8(__VA_ARGS__))
#define HOBPP_EVAL8(...) HOBPP_EVAL4(HOBPP_EVAL4(__VA_ARGS__))
#define HOBPP_EVAL4(...) HOBPP_EVAL2(HOBPP_EVAL2(__VA_ARGS__))
#define HOBPP_EVAL2(...) HOBPP_EVAL1(HOBPP_EVAL1(__VA_ARGS__))
#define HOBPP_EVAL1(...) __VA_ARGS__
#define HOBPP_DEFER2(m) m HOBPP_EMPTY HOBPP_EMPTY()()
#define HOBPP_HAS_PARGS(...) HOBPP_BOOL(HOBPP_FIRST(HOBPP_SEOAP_ __VA_ARGS__)())
#define HOBPP_SEOAP_(...) HOBPP_BOOL(HOBPP_FIRST(HOBPP_SEOA_ __VA_ARGS__)())
#define HOBPP_SEOA_() 0
#define HOBPP_MAP(f, VS...) HOBPP_EVAL(HOBPP_MAPP(f, VS))
#define HOBPP_MAPP(f, H, T...)        \
  f H                                 \
  HOBPP_IF_ELSE(HOBPP_HAS_PARGS(T))(  \
    HOBPP_DEFER2(HOBPP_SMAPP)()(f, T) \
  )(                                  \
  )
#define HOBPP_SMAPP() HOBPP_MAPP

}

#endif

