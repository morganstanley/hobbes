
#ifndef HI_WWW_HPP_INCLUDED
#define HI_WWW_HPP_INCLUDED

#include <hobbes/hobbes.H>
#include <hobbes/events/httpd.H>
#include <unordered_map>

namespace hi {

inline bool isWebEvalRequest(const hobbes::HTTPRequest& req) {
  const hobbes::str::pair p = hobbes::str::lsplit(req.document, "?");
  return (p.first == "/" && !p.second.empty()) ||
         hobbes::str::endsWith(p.first, ".hxp");
}

class WWWServer {
public:
  // opts is applied (via translateExprWithOpts) to every query expression
  // before it is compiled, the same way the net REPL treats client
  // expressions. Safe mode and the proxy-secret gate are separate layers:
  // authentication decides who may evaluate, while Safe narrows the set of
  // primitives available after that decision (STRFR-433914/STRFR-433924).
  WWWServer(int port, hobbes::cc*, std::vector<std::string> opts = {"Safe"},
            bool allowWebEval = false,
            std::string webEvalSecret = std::string());
  ~WWWServer();
private:
  hobbes::cc* c;
  std::vector<std::string> opts;
  bool allowWebEval;
  std::string webEvalSecret;

  void printDefaultPage(int);
  void printQueryResult(int, const std::string&);
  void printFileContents(int, const std::string&);

  using PrintPageFn = void (*)(int, const hobbes::array<char> *);
  struct HxpFile {
    time_t      ftime;
    PrintPageFn f;
  };
  using HxpFiles = std::unordered_map<std::string, HxpFile>;
  HxpFiles hxpFiles;
  const HxpFile& hxpFile(const std::string& fpath);
  void evalHxpFile(const hobbes::HTTPRequest&, int fd, const std::string& fpath, const std::string& queryString);

  using MIMETypes = std::map<std::string, std::string>;
  MIMETypes mimeTypes;
  std::string mimeType(const std::string& fpath);
  std::string mimeTypeForExt(const std::string& ext);

  void eval(const hobbes::HTTPRequest& req, int fd);
  static void evalHTTPRequest(const hobbes::HTTPRequest& req, int fd, void* ud);

  // useful bindings
  using VarBindingDesc = std::pair<const hobbes::array<char> *, const hobbes::array<char> *>;
  using VarBindingDescs = hobbes::array<VarBindingDesc>;
  VarBindingDescs* getVarBindingDescs();
};

}

#endif

