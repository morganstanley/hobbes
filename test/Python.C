
#include <hobbes/hobbes.H>
#include <hobbes/db/file.H>
#include <hobbes/fregion.H>
#include <fstream>
#include "test.H"

#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#define DEF_STR(x) SDEF_STR(x)
#define SDEF_STR(x) #x

using namespace hobbes;
using namespace hobbes::fregion;

class PythonProc {
public:
  PythonProc(const std::string& py, const std::string& moddir, const std::string& script, const std::string& db) : py(py), moddir(moddir), script(script), db(db)
  {
    this->argv.push_back(this->py.c_str());
    this->argv.push_back(this->script.c_str());
    this->argv.push_back(this->db.c_str());
    this->argv.push_back(nullptr); // end
  }

  int run() {
    pid_t pid = fork();
    if (pid == -1) {
      throw std::runtime_error("error while fork: " + std::string(strerror(errno)));
    } else if (pid == 0) {
      setenv("PYTHONPATH", this->moddir.c_str(), 1);
      execv(this->argv[0], const_cast<char* const*>(this->argv.data()));

      std::cout << "error trying to exec '" << argv[0] << "' : " << strerror(errno) << std::endl;
      exit(-1);
    } else {
      int ws;
      wait(&ws);
      pid = -1;
      return WEXITSTATUS(ws);
    }
  }
private:
  std::string py;
  std::string moddir;
  std::string script;
  std::string db;
  std::vector<const char*> argv;
};

static std::string mkFName(const std::string& ext = "db") {
  return hobbes::uniqueFilename("/tmp/hdb-unittest", "." + ext);
}

typedef std::array<int, 10> IArr;
DEFINE_STRUCT(
  TestStruct,
  (int, x),
  (double, y),
  (IArr, xs)
);

DEFINE_VARIANT(
  TestVariant,
  (cars, int),
  (dogs, double),
  (chickens, IArr)
);

void makeTestData(const std::string& path) {
  hobbes::fregion::writer w(path);
  *w.define<bool>   ("f") = true;
  *w.define<char>   ("c") = 'e';
  *w.define<uint8_t>("b") = 0xff;
  *w.define<short>  ("s") = 42;
  *w.define<int>    ("i") = 42;
  *w.define<float>  ("e") = 3.14159;
  *w.define<double> ("d") = 3.14159;

  *w.define<std::pair<int, double>>("p") = std::make_pair(42, 3.14159);

  auto& xs = *w.define<std::array<int, 10>>("xs");
  for (size_t i = 0; i < xs.size(); ++i) {
    xs[i] = i;
  }

  auto& mat = *w.define<std::array<std::array<int,10>,10>>("mat");
  for (size_t i = 0; i < mat.size(); ++i) {
    for (size_t j = 0; j < mat[i].size(); ++j) {
      mat[i][j] = i*j;
    }
  }

  auto& ts = *w.define<TestStruct>("ts");
  ts.x = 42;
  ts.y = 3.14159;
  for (size_t i = 0; i < ts.xs.size(); ++i) {
    ts.xs[i] = i;
  }

  auto& tss = *w.define<std::array<TestStruct, 10>>("tss");
  for (size_t i = 0; i < tss.size(); ++i) {
    tss[i].x = 42*i;
    tss[i].y = 3.14159*i;
    for (size_t j = 0; j < tss[i].xs.size(); ++j) {
      tss[i].xs[j] = j*i;
    }
  }

  *w.define<TestVariant>("tv") = TestVariant::cars(42);

  auto& tvs = *w.define<std::array<TestVariant, 10>>("tvs");
  for (size_t i = 0; i < 10; ++i) {
    switch (i%3) {
    case 0:
      tvs[i] = TestVariant::cars(i);
      break;
    case 1:
      tvs[i] = TestVariant::dogs(i*3.14159);
      break;
    case 2:
    default: {
      IArr cs;
      for (size_t i = 0; i < cs.size(); ++i) { cs[i] = i; }
      tvs[i] = TestVariant::chickens(cs);
      break;
    }}
  }

  std::vector<TestStruct> vtss;
  for (size_t i = 0; i < 100; ++i) {
    TestStruct ts;
    ts.x = 42*i;
    ts.y = 3.14159*i;
    for (size_t j = 0; j < ts.xs.size(); ++j) {
      ts.xs[j] = i*j;
    }
    vtss.push_back(ts);
  }
  w.define("vtss", vtss);

  auto& s = w.series<TestStruct>("stss",10);
  for (size_t i = 0; i < 100; ++i) {
    TestStruct ts;
    ts.x = 42*i;
    ts.y = 3.14159*i;
    for (size_t j = 0; j < ts.xs.size(); ++j) {
      ts.xs[j] = i*j;
    }
    s(ts);
  }

  w.define<hobbes::variant<hobbes::unit, std::string>>("ms_none", hobbes::variant<hobbes::unit, std::string>(hobbes::unit()));
  w.define<hobbes::variant<hobbes::unit, std::string>>("ms_just", hobbes::variant<hobbes::unit, std::string>(std::string("chicken")));
}

void makeTestScript(const std::string& path) {
  const char* script = R"SCRIPT(
import fregion
import sys
f = fregion.FRegion(sys.argv[1])
if (f.s != 42):
  print("Expected f.s=42 but f.s="+str(f.s))
  sys.exit(-1)
if (f.ts.x != 42):
  print("Expected f.ts.x=42 but f.ts.x="+str(f.ts.x))
  sys.exit(-1)
if (f.p[0] != 42):
  print("Expected f.p[0]=42 but f.p[0]="+str(f.p[0]))
  sys.exit(-1)
if (f.tv.cn != 'cars' or f.tv.value != 42):
  print("Expected f.tv=|cars=42| but f.tv="+str(f.tv))
  sys.exit(-1)
if (sum([sum(xs) for xs in f.mat]) != 2025):
  print("Expected matrix to sum to 2025 but failed: "+str(f.mat))
  sys.exit(-1)
if (sum(map(lambda ts:ts.x,f.tss)) != 1890):
  print("Expected .x over f.tss to sum to 1890 but failed: "+str(f.tss))
  sys.exit(-1)
if (sum([sum(x.value) for x in f.tvs if x.cn=='chickens']) != 135):
  print("Expected chickens matched out of f.tvs to sum to 135 but failed: "+str(f.tvs))
  sys.exit(-1)

def smapInto(r,f,x):
  if (x == None):
    return r
  else:
    r.append(f(x[0]))
    return smapInto(r,f,x[1])
def smap(f,x):
  r=[]
  smapInto(r,f,x)
  return r

if (sum(smap(lambda vs: sum(map(lambda v:v.x, vs)),f.stss)) != 207900):
  print("Expected .x over f.stss to sum to 207900 but failed: "+str(f.stss))
  sys.exit(-1)

if (f.ms_none != None):
  print("Expected 'maybe' type with nothing as python None value: " + str(f.ms_none))
  sys.exit(-1)
if (f.ms_just != "chicken"):
  print("Expected 'maybe' type with something as just that value: " + str(f.ms_just))
  sys.exit(-1)

sys.exit(0)
  )SCRIPT";

  std::ofstream f(path.c_str());
  f << script;
}

TEST(Python, FRegion) {
#if !defined(PYTHON_EXECUTABLE) or !defined(SCRIPT_DIR)
  std::cout << "Warning: no python compatibility tests will be run" << std::endl;
#else
  auto db = mkFName("db");
  auto py = mkFName("py");
  try {
    makeTestData(db);
    makeTestScript(py);

    PythonProc p(DEF_STR(PYTHON_EXECUTABLE), DEF_STR(SCRIPT_DIR), py, db);
    EXPECT_EQ(p.run(), 0);

    unlink(py.c_str());
    unlink(db.c_str());
  } catch (...) {
    unlink(py.c_str());
    unlink(db.c_str());
    throw;
  }
#endif
}

