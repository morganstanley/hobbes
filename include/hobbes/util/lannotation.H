/*
 * lannotation : hold on to a reference to some section of lexical input
 */

#ifndef HOBBES_UTIL_LANNOTATION_HPP_INCLUDED
#define HOBBES_UTIL_LANNOTATION_HPP_INCLUDED

#include <hobbes/util/ptr.H>
#include <hobbes/util/str.H>
#include <string>
#include <vector>
#include <map>
#include <stdexcept>

namespace hobbes {

using BuffOrFilename = std::pair<bool, std::string>;
using BuffOrFilenamePtr = std::shared_ptr<BuffOrFilename>;
using Pos = std::pair<size_t, size_t>; // line#, offset
using LexRange = std::pair<Pos, Pos>;

struct LexicalAnnotation {
  LexicalAnnotation();

  BuffOrFilenamePtr bfptr;
  Pos               p0, p1;

  std::string filename() const;
  std::string lineDesc() const;
  str::seq lines(size_t, size_t) const;

  static LexicalAnnotation null();
  static LexicalAnnotation merge(const LexicalAnnotation&, const LexicalAnnotation&);
};

class LexicallyAnnotated {
public:
  LexicallyAnnotated() = delete;

  LexicallyAnnotated(const LexicallyAnnotated&);
  LexicallyAnnotated(const LexicalAnnotation&);
  LexicallyAnnotated(const LexRange&);
  LexicallyAnnotated(const Pos&, const Pos&);

  // the lexical annotation data (source file/buffer and line:col range)
  const LexicalAnnotation& la() const;

  // push some hidden state to apply in bulk to subsequent annotations
  static void pushFileContext(const std::string&);
  static void pushLiteralContext(const std::string&);
  static void popContext();
  static LexicalAnnotation make(const Pos&, const Pos&);
private:
  LexicalAnnotation lannotation;
};

using annmsg = std::pair<std::string, LexicalAnnotation>;
using annmsgs = std::vector<annmsg>;

class annotated_error : public std::runtime_error {
public:
  annotated_error(const annmsgs&);
  annotated_error(const LexicalAnnotation&, const std::string&);
  annotated_error(const LexicallyAnnotated&, const std::string&);

  const annmsgs& messages() const;

  static annotated_error fileError(const std::string&, const Pos&, const Pos&, const std::string&);
  static annotated_error bufferError(const std::string&, const Pos&, const Pos&, const std::string&);
private:
  annmsgs amsgs;

  static std::string plainDesc(const annmsgs&);
};

}

#endif
