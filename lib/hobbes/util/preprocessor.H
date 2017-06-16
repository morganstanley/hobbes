/*
 * preprocessor : very basic preprocessor metaprogramming
 */

#ifndef HOBBES_LANG_PREPROCESSOR_HPP_INCLUDED
#define HOBBES_LANG_PREPROCESSOR_HPP_INCLUDED

namespace hobbes {

#define _HOBPP_FIRST(a, ...) a
#define _HOBPP_SECOND(a, b, ...) b
#define _HOBPP_CAT(a,b) a ## b
#define _HOBPP_IS_PROBE(...) _HOBPP_SECOND(__VA_ARGS__, 0)
#define _HOBPP_PROBE() ~, 1
#define _HOBPP_NOT(x) _HOBPP_IS_PROBE(_HOBPP_CAT(_HOBPP__NOT_, x))
#define _HOBPP__NOT_0 _HOBPP_PROBE()
#define _HOBPP_BOOL(x) _HOBPP_NOT(_HOBPP_NOT(x))
#define _HOBPP_IF_ELSE(condition) _HOBPP__IF_ELSE(_HOBPP_BOOL(condition))
#define _HOBPP__IF_ELSE(condition) _HOBPP_CAT(_HOBPP__IF_, condition)
#define _HOBPP__IF_1(...) __VA_ARGS__ _HOBPP__IF_1_ELSE
#define _HOBPP__IF_0(...)             _HOBPP__IF_0_ELSE
#define _HOBPP__IF_1_ELSE(...)
#define _HOBPP__IF_0_ELSE(...) __VA_ARGS__
#define _HOBPP_EMPTY()
#define _HOBPP_EVAL(...) _HOBPP_EVAL256(__VA_ARGS__)
#define _HOBPP_EVAL256(...) _HOBPP_EVAL128(_HOBPP_EVAL128(__VA_ARGS__))
#define _HOBPP_EVAL128(...) _HOBPP_EVAL64(_HOBPP_EVAL64(__VA_ARGS__))
#define _HOBPP_EVAL64(...) _HOBPP_EVAL32(_HOBPP_EVAL32(__VA_ARGS__))
#define _HOBPP_EVAL32(...) _HOBPP_EVAL16(_HOBPP_EVAL16(__VA_ARGS__))
#define _HOBPP_EVAL16(...) _HOBPP_EVAL8(_HOBPP_EVAL8(__VA_ARGS__))
#define _HOBPP_EVAL8(...) _HOBPP_EVAL4(_HOBPP_EVAL4(__VA_ARGS__))
#define _HOBPP_EVAL4(...) _HOBPP_EVAL2(_HOBPP_EVAL2(__VA_ARGS__))
#define _HOBPP_EVAL2(...) _HOBPP_EVAL1(_HOBPP_EVAL1(__VA_ARGS__))
#define _HOBPP_EVAL1(...) __VA_ARGS__
#define _HOBPP_DEFER2(m) m _HOBPP_EMPTY _HOBPP_EMPTY()()
#define _HOBPP_HAS_PARGS(...) _HOBPP_BOOL(_HOBPP_FIRST(_HOBPP__EOAP_ __VA_ARGS__)())
#define _HOBPP__EOAP_(...) _HOBPP_BOOL(_HOBPP_FIRST(_HOBPP__EOA_ __VA_ARGS__)())
#define _HOBPP__EOA_() 0
#define _HOBPP_MAP(f, VS...) _HOBPP_EVAL(_HOBPP_MAPP(f, VS))
#define _HOBPP_MAPP(f, H, T...)        \
  f H                                 \
  _HOBPP_IF_ELSE(_HOBPP_HAS_PARGS(T))(  \
    _HOBPP_DEFER2(_HOBPP__MAPP)()(f, T) \
  )(                                  \
  )
#define _HOBPP__MAPP() _HOBPP_MAPP

}

#endif

