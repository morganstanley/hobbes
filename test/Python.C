
#include <hobbes/hobbes.H>
#include <hobbes/db/file.H>
#include <hobbes/fregion.H>
#include <hobbes/slmap.H>
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
    if(this->py.empty()) {
      this->argv.push_back("/usr/bin/env");
      this->argv.push_back("python3");
    } else {
      this->argv.push_back(this->py.c_str());
    }
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

#if defined(PYTHON_EXECUTABLE) and defined(SCRIPT_DIR)
static std::string mkFName(const std::string& ext = "db") {
  return hobbes::uniqueFilename("/tmp/hdb-unittest", "." + ext);
}
#endif

using IArr = std::array<int, 10>;
DEFINE_STRUCT(
  TestStruct,
  (int, x),
  (double, y),
  (IArr, xs)
);

DEFINE_VARIANT(
  TestPyVariant,
  (cars, int),
  (dogs, double),
  (chickens, IArr)
);

DEFINE_TYPE_ALIAS(Chickens, int);

template <typename T, size_t N>
  struct CustomBuffer {
    T buffer[N];
  };

namespace hobbes { namespace fregion {
template <typename T, size_t N>
  struct store<CustomBuffer<T,N>> {
    static const bool  can_memcpy = store<std::array<T,N>>::can_memcpy;

    static ty::desc    storeType()                                    { return ty::app(ty::prim("CustomBuffer", ty::fn("t","n", ty::array(ty::var("t"), ty::var("n")))), store<T>::storeType(), ty::nat(N)); }
    static size_t      size()                                         { return store<std::array<T,N>>::size(); }
    static size_t      alignment()                                    { return store<std::array<T,N>>::alignment(); }
    static void        write(imagefile* f, void* p,       const T& x) { store<std::array<T,N>>::write(f, p, reinterpret_cast<const std::array<T,N>&>(x)); }
    static void        read (imagefile* f, const void* p, T* x)       { store<std::array<T,N>>::read(f, p, reinterpret_cast<std::array<T,N>*>(x)); }
  };
}}

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

  *w.define<TestPyVariant>("tv") = TestPyVariant::cars(42);

  auto& tvs = *w.define<std::array<TestPyVariant, 10>>("tvs");
  for (size_t i = 0; i < 10; ++i) {
    switch (i%3) {
    case 0:
      tvs[i] = TestPyVariant::cars(i);
      break;
    case 1:
      tvs[i] = TestPyVariant::dogs(i*3.14159);
      break;
    case 2:
    default: {
      IArr cs;
      for (size_t i = 0; i < cs.size(); ++i) { cs[i] = i; }
      tvs[i] = TestPyVariant::chickens(cs);
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

  w.define<Chickens>("chickens")->value = 42;
  strcpy(w.define<CustomBuffer<char, 10>>("cbuffer")->buffer, "chickens");

  hobbes::slmap<int, int> m("slmap", w);
  m.insert(42, 31337);
  m.insert(99, 100);
}

void makeTestScript(const std::string& path) {
  const char* script = R"SCRIPT(
import fregion
import sys

class RepReader:
  def __init__(self,renv,ty):
    self.r = fregion.makeReader(renv,ty)
  def read(self,m,o):
    return self.r.read(m,o)

fregion.FRegion.addType("Chickens", lambda renv, ty, repty: RepReader(renv,repty))
fregion.FRegion.addType("CustomBuffer", lambda renv, ty, repty: RepReader(renv,repty))

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

if (sum(map(lambda v:v.x, f.stss)) != 207900):
  print("Expected .x over f.stss to sum to 207900 but failed: "+str(f.stss))
  sys.exit(-1)

if (f.ms_none != None):
  print("Expected 'maybe' type with nothing as python None value: " + str(f.ms_none))
  sys.exit(-1)
if (f.ms_just != "chicken"):
  print("Expected 'maybe' type with something as just that value: " + str(f.ms_just))
  sys.exit(-1)

if (f.chickens != 42):
  print("Expected custom 'Chickens' type to read as 42: " + str(f.chickens))
  sys.exit(-1)

def explode(s):
  r=[]
  for i in range(len(s)):
    r.append(s[i])
  return r

expectCB='chickens'
if (b''.join(f.cbuffer[0:len(expectCB)]).decode('utf-8') != expectCB):
  from pprint import pprint
  pprint(f.cbuffer[0:len(expectCB)])
  pprint(expectCB)
  print("Expected 'cbuffer' to equal 'chickens': " + str(f.cbuffer))
  sys.exit(-1)

if (f.slmap[42] != 31337):
  print("Expected slmap to have a key=42 mapping: " + str(f.slmap))
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

    std::cout << "Using Python executable: " << DEF_STR(PYTHON_EXECUTABLE) << std::endl;
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

// The page-entry chain fregion.py follows comes straight out of the file, so a
// corrupt or untrusted image can point a link out of range or back at a block
// already read. This script crafts such files and confirms the reader raises a
// clear error rather than recursing until the interpreter dies (STRFR-433962).
// It writes its crafted bytes to the path it is given as argv[1] and exits 0
// only if every case is rejected cleanly (never RecursionError, never silent).
void makeCorruptPageScript(const std::string& path) {
  static const char script[] = R"SCRIPT(
import sys, struct
import fregion

scratch = sys.argv[1]
PS = 64

def hdr(pagesize=PS, version=1):
    return struct.pack('I', 0x10A1DB0D) + struct.pack('H', pagesize) + struct.pack('H', version)

def expect_rejected(name, data):
    with open(scratch, 'wb') as fh:
        fh.write(data)
    try:
        fregion.FREnvelope(scratch)
    except RecursionError:
        print('FAIL', name, '-> RecursionError'); sys.exit(1)
    except Exception as ex:
        print('ok', name, '->', ex); return
    print('FAIL', name, '-> no exception raised'); sys.exit(1)

# a cyclic page chain: block0 -> page2 -> page3 -> page2 ...
d = bytearray(hdr() + bytes(4 * PS - 8))
struct.pack_into('H', d, 8, 0)          # block0: no entries
struct.pack_into('Q', d, PS - 8, 2)     # block0 link -> page 2
struct.pack_into('H', d, 2 * PS, 0)
struct.pack_into('Q', d, 3 * PS - 8, 3) # page2 link -> page 3
struct.pack_into('H', d, 3 * PS, 0)
struct.pack_into('Q', d, 4 * PS - 8, 2) # page3 link -> page 2 (cycle)
expect_rejected('cyclic page link', bytes(d))

# a link pointing far past the end of the file
d = bytearray(hdr() + bytes(PS - 8))
struct.pack_into('H', d, 8, 0)
struct.pack_into('Q', d, PS - 8, 1000)  # -> offset 64000 in a 64 byte file
expect_rejected('out-of-range page link', bytes(d))

# a page size that does not even fit in the file
d = bytearray(hdr() + bytes(32))        # 40 bytes, but the header claims 64
expect_rejected('truncated page block', bytes(d))

# a page size too small to hold the header and a page-table link
expect_rejected('undersized page size', hdr(4) + bytes(16))

print('all corrupt page chains rejected cleanly')
sys.exit(0)
)SCRIPT";

  std::ofstream f(path.c_str());
  f << script;
}

TEST(Python, FRegionRejectsCorruptPageChains) {
#if !defined(PYTHON_EXECUTABLE) or !defined(SCRIPT_DIR)
  std::cout << "Warning: no python compatibility tests will be run" << std::endl;
#else
  auto scratch = mkFName("db");
  auto py = mkFName("py");
  try {
    makeCorruptPageScript(py);

    PythonProc p(DEF_STR(PYTHON_EXECUTABLE), DEF_STR(SCRIPT_DIR), py, scratch);
    EXPECT_EQ(p.run(), 0);

    unlink(py.c_str());
    unlink(scratch.c_str());
  } catch (...) {
    unlink(py.c_str());
    unlink(scratch.c_str());
    throw;
  }
#endif
}

