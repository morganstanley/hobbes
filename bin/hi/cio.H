
#ifndef HI_REPL_CIO_HPP_INCLUDED
#define HI_REPL_CIO_HPP_INCLUDED

#include <iostream>

namespace hi {

// controls display options
typedef unsigned char color;

DEFINE_STRUCT(ConsoleColors,
  (color, promptfg),
  (color, stdtextfg),
  (color, errorfg),
  (color, hlfg),
  (color, divfg),
  (color, evalfg),
  (color, unsweetfg),
  (color, typefg),
  (color, llvmfg),

  (color, instfg),
  (color, argdelimfg),
  (color, registerfg),
  (color, xnumfg),
  (color, xvalfg),
  (color, linenumfg),
  (color, linenumdelimfg),
  (color, evenlinebg),
  (color, oddlinebg)
);

// controls whether or not we bother to send terminal control sequences
bool extConsoleCmdsEnabled();

extern ConsoleColors colors;

inline char cchar(uint8_t x) { return static_cast<char>(x); }

inline void sendCmd(std::ostream& out, char c) {
  if (extConsoleCmdsEnabled()) {
    out << cchar(0x1B) << '[' << c << std::flush;
  }
}

inline void sendCmd(std::ostream& out, int n0, char c) {
  if (extConsoleCmdsEnabled()) {
    out << cchar(0x1B) << '[' << n0 << c << std::flush;
  }
}

inline void sendCmd(std::ostream& out, int n0, int n1, char c) {
  if (extConsoleCmdsEnabled()) {
    out << cchar(0x1B) << '[' << n0 << ';' << n1 << c << std::flush;
  }
}

inline void sendCmd(std::ostream& out, int n0, int n1, int n2, char c) {
  if (extConsoleCmdsEnabled()) {
    out << cchar(0x1B) << '[' << n0 << ';' << n1 << ';' << n2 << c << std::flush;
  }
}

struct setbold { setbold() { } };
inline std::ostream& operator<<(std::ostream& lhs, const setbold&) {
  sendCmd(lhs, 1, 'm');
  return lhs;
}

struct setunderline { setunderline() { } };
inline std::ostream& operator<<(std::ostream& lhs, const setunderline&) {
  sendCmd(lhs, 4, 'm');
  return lhs;
}

struct setfont {
  setfont(unsigned char f) : f(f) { }
  unsigned char f;
};
inline std::ostream& operator<<(std::ostream& lhs, const setfont& f) {
  sendCmd(lhs, 10 + f.f, 'm');
  return lhs;
}

struct setfgc {
  setfgc(unsigned char x) : x(x) { }
  unsigned char x;
};

inline std::ostream& operator<<(std::ostream& lhs, const setfgc& c) {
  sendCmd(lhs, 38, 5, static_cast<int>(c.x), 'm');
  return lhs;
}

struct setbgc {
  setbgc(unsigned char x) : x(x) { }
  unsigned char x;
};

inline std::ostream& operator<<(std::ostream& lhs, const setbgc& c) {
  sendCmd(lhs, 48, 5, static_cast<int>(c.x), 'm');
  return lhs;
}

struct resetfmt {
  resetfmt() { }
};

inline std::ostream& operator<<(std::ostream& lhs, const resetfmt& c) {
  sendCmd(lhs, 0, 'm');
  return lhs;
}

struct clearscr {
  clearscr() { }
};

inline std::ostream& operator<<(std::ostream& lhs, const clearscr& c) {
  sendCmd(lhs, 2, 'J');
  return lhs;
}

struct setcursor {
  setcursor(int x, int y) : x(x), y(y) { }
  int x, y;
};

inline std::ostream& operator<<(std::ostream& lhs, const setcursor& sc) {
  sendCmd(lhs, sc.y, sc.x, 'H');
  return lhs;
}

struct movecursor {
  movecursor(int dx, int dy) : dx(dx), dy(dy) { }
  int dx, dy;
};
inline std::ostream& operator<<(std::ostream& lhs, const movecursor& mc) {
  if (mc.dy < 0) {
    sendCmd(lhs, -mc.dy, 'A');
  } else if (mc.dy > 0) {
    sendCmd(lhs, mc.dy, 'B');
  }

  if (mc.dx < 0) {
    sendCmd(lhs, -mc.dx, 'D');
  } else if (mc.dx > 0) {
    sendCmd(lhs, mc.dx, 'C');
  }

  return lhs;
}

struct eraseToEOL { eraseToEOL() { } };
inline std::ostream& operator<<(std::ostream& lhs, const eraseToEOL&) {
  sendCmd(lhs, 0, 'K');
  return lhs;
}

struct clearline { clearline() { } };
inline std::ostream& operator<<(std::ostream& lhs, const clearline&) {
  sendCmd(lhs, 2, 'K');
  return lhs;
}

}

#endif
