
#include <hobbes/util/lannotation.H>
#include <stack>
#include <sstream>
#include <fstream>

namespace hobbes {

LexicalAnnotation::LexicalAnnotation() {
  static BuffOrFilenamePtr* n = 0;
  if (n == 0) {
    n = new BuffOrFilenamePtr(new BuffOrFilename(false, "???"));
  }
  this->bfptr = *n;
  this->p0    = Pos(0,0);
  this->p1    = Pos(0,0);
}

std::string LexicalAnnotation::filename() const {
  return this->bfptr->first ? this->bfptr->second : "stdin";
}

str::seq slurpLines(std::istream& in, size_t i, size_t f) {
  str::seq    result;
  std::string mline;
  for (size_t k = 0; k < i; ++k) {
    std::getline(in, mline);
  }
  for (size_t k = i; k < f; ++k) {
    if (std::getline(in, mline)) {
      result.push_back(mline);
    }
  }
  return result;
}

std::string LexicalAnnotation::lineDesc() const {
  if (!this->bfptr) {
    return "";
  }

  std::ostringstream ss;
  ss << filename() << ":";
  if (this->p0.first == this->p1.first) {
    ss << this->p0.first << "," << this->p0.second << "-" << this->p1.second;
  } else {
    ss << this->p0.first << "," << this->p0.second << "-" << this->p1.first << "," << this->p1.second;
  }
  return ss.str();
}

str::seq LexicalAnnotation::lines(size_t i, size_t f) const {
  if (!this->bfptr) {
    return str::seq();
  } else if (this->bfptr->first) {
    std::ifstream fd(this->bfptr->second);
    return slurpLines(fd, i, f);
  } else {
    std::istringstream sd(this->bfptr->second);
    return slurpLines(sd, i, f);
  }
}

LexicalAnnotation LexicalAnnotation::null() {
  return LexicalAnnotation();
}

LexicalAnnotation LexicalAnnotation::merge(const LexicalAnnotation& a0, const LexicalAnnotation& a1) {
  LexicalAnnotation r;
  r.bfptr = a0.bfptr;
  r.p0    = a0.p0;
  r.p1    = a1.p1;
  return r;
}

typedef std::stack<BuffOrFilenamePtr> AnnContextStack;

static AnnContextStack& annotationCtxStack() {
  static __thread AnnContextStack* actxs = 0;
  if (!actxs) {
    actxs = new AnnContextStack();
  }
  return *actxs;
}

LexicallyAnnotated::LexicallyAnnotated(const LexicallyAnnotated& rhs) : lannotation(rhs.lannotation) {
}

LexicallyAnnotated::LexicallyAnnotated(const LexicalAnnotation& la) : lannotation(la) {
}

LexicallyAnnotated::LexicallyAnnotated(const LexRange& r) : lannotation(make(r.first, r.second)) {
}

LexicallyAnnotated::LexicallyAnnotated(const Pos& p0, const Pos& p1) : lannotation(make(p0, p1)) {
}

const LexicalAnnotation& LexicallyAnnotated::la() const {
  return this->lannotation;
}

void LexicallyAnnotated::pushFileContext(const std::string& fname) {
  annotationCtxStack().push(BuffOrFilenamePtr(new BuffOrFilename(true, fname)));
}

void LexicallyAnnotated::pushLiteralContext(const std::string& txt) {
  annotationCtxStack().push(BuffOrFilenamePtr(new BuffOrFilename(false, txt)));
}

void LexicallyAnnotated::popContext() {
  annotationCtxStack().pop();
}

LexicalAnnotation LexicallyAnnotated::make(const Pos& p0, const Pos& p1) {
  auto& s = annotationCtxStack();

  LexicalAnnotation r;
  r.bfptr = s.size() > 0 ? s.top() : BuffOrFilenamePtr(new BuffOrFilename(false, "???"));
  r.p0    = p0;
  r.p1    = p1;
  return r;
}


annotated_error::annotated_error(const annmsgs& amsgs) : std::runtime_error(plainDesc(amsgs)), amsgs(amsgs) {
}

annotated_error::annotated_error(const LexicalAnnotation& la, const std::string& m) : annotated_error(list(annmsg(m, la))) {
}

annotated_error::annotated_error(const LexicallyAnnotated& la, const std::string& m) : annotated_error(list(annmsg(m, la.la()))) {
}

const annmsgs& annotated_error::messages() const {
  return this->amsgs;
}

std::string annotated_error::plainDesc(const annmsgs& amsgs) {
  std::ostringstream ss;
  for (const auto& amsg : amsgs) {
    ss << amsg.second.lineDesc() << ": " << amsg.first << "\n";
  }
  return ss.str();
}

annotated_error annotated_error::fileError(const std::string& fn, const Pos& p0, const Pos& p1, const std::string& etxt) {
  LexicalAnnotation a;
  a.bfptr = BuffOrFilenamePtr(new BuffOrFilename(true, fn));
  a.p0    = p0;
  a.p1    = p1;
  return annotated_error(list(annmsg(etxt, a)));
}

annotated_error annotated_error::bufferError(const std::string& b, const Pos& p0, const Pos& p1, const std::string& etxt) {
  LexicalAnnotation a;
  a.bfptr = BuffOrFilenamePtr(new BuffOrFilename(false, b));
  a.p0    = p0;
  a.p1    = p1;
  return annotated_error(list(annmsg(etxt, a)));
}

}

