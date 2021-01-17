
#ifndef HI_WWW_HPP_INCLUDED
#define HI_WWW_HPP_INCLUDED

#include <hobbes/hobbes.H>
#include <hobbes/events/httpd.H>
#include <unordered_map>

namespace hi {

class WWWServer {
public:
  WWWServer(int port, hobbes::cc*);
  ~WWWServer();
private:
  hobbes::cc* c;

  void printDefaultPage(int);
  void printQueryResult(int, const std::string&);
  void printFileContents(int, const std::string&);

  typedef void (*PrintPageFn)(int fd, const hobbes::array<char>* queryString);
  struct HxpFile {
    time_t      ftime;
    PrintPageFn f;
  };
  typedef std::unordered_map<std::string, HxpFile> HxpFiles;
  HxpFiles hxpFiles;
  const HxpFile& hxpFile(const std::string& fpath);
  void evalHxpFile(const hobbes::HTTPRequest&, int fd, const std::string& fpath, const std::string& queryString);

  typedef std::map<std::string, std::string> MIMETypes;
  MIMETypes mimeTypes;
  std::string mimeType(const std::string& fpath);
  std::string mimeTypeForExt(const std::string& ext);

  void eval(const hobbes::HTTPRequest& req, int fd);
  static void evalHTTPRequest(const hobbes::HTTPRequest& req, int fd, void* ud);

  // useful bindings
  typedef std::pair<const hobbes::array<char>*, const hobbes::array<char>*> VarBindingDesc;
  typedef hobbes::array<VarBindingDesc> VarBindingDescs;
  VarBindingDescs* getVarBindingDescs();
};

}

#endif

