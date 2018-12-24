#!/usr/bin/env python

########################################################
#
# fregion.py : read structured data files
#
#    to load a file from the path P into the variable f:
#      f = fregion.FRegion(P)
#
#    to read the stored field 'x' out of f:
#      f.x
#
#    to read the 'metadata' for the field 'x' (type and offset details):
#      meta(f).x
#
#    to add reading support for a custom user type:
#      fregion.FRegion.addType("MyTypeName", lambda renv, td, repty: makeMyTypeNameReader(renv,td,repty))
#    where:
#      'renv'  will be the "reader environment" (necessary for any call to makeReader)
#      'td'    will be the full type description where "MyTypeName" appears at root (e.g. for 'fileref', App(Prim('fileref', ...), [Arr(Prim('char'))]))
#      'repty' will be the determined 'representation type' (which can also be determined through 'td')
#    and:
#      the returned 'reader' must be a class with a "read" function like:
#        def read(self,m,o):
#      where
#        'm' will give access to the memory for the file being read out of
#        'o' will be the memory offset where the value to be read is placed
#      and:
#        the returned value may be whatever the application decides is sensible
#
########################################################

import os
import mmap
import struct
import math
import datetime

#######
#
# Type Descriptions
#
#######

class Prim:
  def __init__(self, name, rep):
    self.name = name
    self.rep  = rep
  def __eq__(self,x): return isinstance(x,Prim) and self.name==x.name and self.rep==x.rep
  def __repr__(self): return '()' if self.name=="unit" else self.name

class Var:
  def __init__(self, name):
    self.name = name
  def __eq__(self,x): return isinstance(x,Var) and self.name==x.name
  def __repr__(self): return self.name

class FixedArr:
  def __init__(self, ty, tlen):
    self.ty   = ty
    self.tlen = tlen
  def __eq__(self,x): return isinstance(x,FixedArr) and self.ty==x.ty and self.tlen==x.tlen
  def __repr__(self): return '[:' + str(self.ty) + '|' + str(self.tlen) + ':]'

class Arr:
  def __init__(self, ty):
    self.ty = ty
  def __eq__(self,x): return isinstance(x,Arr) and self.ty==x.ty
  def __repr__(self): return '[' + str(self.ty) + ']'

class Variant:
  def __init__(self, ctors):
    self.ctors = ctors
  def __eq__(self,x): return isinstance(x,Variant) and self.ctors==x.ctors
  def __repr__(self):
    if (len(self.ctors) == 0):
      return 'void'
    elif (self.isSum()):
      return self.showAsSum()
    else:
      return self.showAsVariant()
  def isSum(self):
    return len(self.ctors)>0 and self.ctors[0][0][0] == '.'
  def showAsSum(self):
    s = '('
    s += str(self.ctors[0][2])
    for i in range(1, len(self.ctors)):
      s += '+' + str(self.ctors[i][2])
    s += ')'
    return s
  def showAsVariant(self):
    s = '|'
    s += self.descCtor(self.ctors[0])
    for i in range(1,len(self.ctors)):
      s += ', '
      s += self.descCtor(self.ctors[i])
    s += '|'
    return s

  def descCtor(self, ctor):
    return ctor[0] + ':' + str(ctor[2])

class Struct:
  def __init__(self, fields):
    self.fields = fields
  def __eq__(self,x): return isinstance(x,Struct) and self.fields==x.fields
  def __repr__(self):
    if (len(self.fields) == 0):
      return '()'
    elif (self.isTuple()):
      return self.showAsTuple()
    else:
      return self.showAsStruct()
  def isTuple(self):
    return len(self.fields)>0 and self.fields[0][0][0] == '.'
  def showAsTuple(self):
    s = '('
    s += str(self.fields[0][2])
    for i in range(1,len(self.fields)):
      s += '*' + str(self.fields[i][2])
    s += ')'
    return s
  def showAsStruct(self):
    s = '{'
    s += self.descField(self.fields[0])
    for i in range(1,len(self.fields)):
      s += ', '
      s += self.descField(self.fields[i])
    s += '}'
    return s
  def descField(self, field):
    return field[0] + ':' + str(field[2])

class TLong:
  def __init__(self, n):
    self.n = n
  def __eq__(self,x): return isinstance(x,TLong) and self.n==x.n
  def __repr__(self): return str(self.n)

class App:
  def __init__(self,f,args):
    self.f = f
    self.args = args
  def __eq__(self,x): return isinstance(x,App) and self.f==x.f and self.args==x.args
  def __repr__(self):
    if (isinstance(self.f,Prim)):
      if (self.f.name == "fileref" and len(self.args)>0):
        return str(self.args[0])+"@?"
    return self.showGeneric()
  def showGeneric(self):
    s = str(self.f) + '('
    if (len(self.args)>0):
      s += str(self.args[0])
      for i in range(1,len(self.args)):
        s += ', ' + str(self.args[i])
    s += ')'
    return s

class Recursive:
  def __init__(self,vn,ty):
    self.vn = vn
    self.ty = ty
  def __repr__(self):
    return '^' + self.vn + '.' + str(self.ty)

class Abs:
  def __init__(self,vns,ty):
    self.vns = vns
    self.ty  = ty
  def __repr__(self):
    s = '\\'
    if (len(self.vns)>0):
      s += self.vns[0]
      for i in range(1,len(self.vns)):
        s += ', ' + self.vns[i]
    s += '.' + str(self.ty)
    return s

class TyCase:
  def __init__(self, dtors):
    self.dtors = dtors
  def apply(self,ty):
    if (isinstance(ty,Prim)):
      return self.dtors["prim"](ty)
    elif (isinstance(ty,Var)):
      return self.dtors["var"](ty)
    elif (isinstance(ty,FixedArr)):
      return self.dtors["farr"](ty)
    elif (isinstance(ty,Arr)):
      return self.dtors["arr"](ty)
    elif (isinstance(ty,Variant)):
      return self.dtors["variant"](ty)
    elif (isinstance(ty,Struct)):
      return self.dtors["struct"](ty)
    elif (isinstance(ty,TLong)):
      return self.dtors["long"](ty)
    elif (isinstance(ty,App)):
      return self.dtors["app"](ty)
    elif (isinstance(ty,Recursive)):
      return self.dtors["rec"](ty)
    elif (isinstance(ty,Abs)):
      return self.dtors["abs"](ty)
    else:
      raise Exception("Can't deconstruct unknown type description")

def fail(msg):
  raise Exception(msg)

def dictWithout(m,k):
  r=m.copy()
  r.drop(k)
  return r
def dictWithouts(m,ks):
  r=m.copy()
  for k in ks:
    r.drop(k)
  return r
def addFreeVar(m,vn):
  m[vn]=None

def freeVarsInto(m,ty):
  tyDisp = {
    "prim":    lambda p:  None,
    "var":     lambda v:  addFreeVar(m,v.name),
    "farr":    lambda fa: (freeVarsInto(m,fa.ty), freeVarsInto(m,fa.tlen)),
    "arr":     lambda a:  freeVarsInto(m,a.ty),
    "variant": lambda v:  [freeVarsInto(m,ctor[2]) for ctor in v.ctors],
    "struct":  lambda s:  [freeVarsInto(m,field[2]) for field in s.fields],
    "long":    lambda n:  None,
    "app":     lambda a:  (freeVarsInto(m,a.f), [freeVarsInto(m,arg) for arg in f.args]),
    "rec":     lambda r:  m.update(dictWithout(freeVars(r.ty),r.vn)),
    "abs":     lambda a:  m.update(dictWithouts(freeVars(a.ty),a.vns))
  }
  return TyCase(tyDisp).apply(ty)

def freeVars(ty):
  m={}
  freeVarsInto(m,ty)
  return m
def dictFreeVars(m):
  lm={}
  for n, ty in m.items():
    freeVarsInto(lm,ty)
  return lm
def freeName(m):
  vn='t0'
  n=0
  while (True):
    if (not(vn in m)):
      break
    else:
      n+=1
      vn='t'+str(n)
  return vn

def substituteInVariant(m,v):
  ctors=[]
  for ctor in v.ctors:
    ctors.append((ctor[0], ctor[1], substitute(m, ctor[2])))
  return Variant(ctors)
def substituteInStruct(m,s):
  fields=[]
  for field in s.fields:
    fields.append((field[0], field[1], substitute(m,field[2])))
  return Struct(fields)
def substituteInApp(m,a):
  args=[]
  for ty in a.args:
    args.append(substitute(m,ty))
  return App(substitute(m,a.f),args)
def substituteInRec(m,r):
  lm=dictWithout(m,r.vn)
  fvs=dictFreeVars(lm)
  if (r.vn in fvs):
    nn=freeName(fvs)
    return Recursive(nn, substitute(lm, substitute({r.vn:Var(nn)},r.ty)))
  else:
    return Recursive(r.vn, substitute(lm, r.ty))
def substituteInAbs(m,a):
  lm=dictWithouts(m,r.vns)
  fvs=dictFreeVars(lm)
  vns=[]
  for vn in a.vns:
    if (vn in fvs):
      nn=freeName(lm)
      lm[vn] = Var(nn)
      vns.append(nn)
    else:
      vns.append(vn)
  if (vns!=a.vns):
    return Abs(vns, substitute(lm,a.ty))
  else:
    return Abs(a.vns, substitute(lm,a.ty))

def substitute(m,ty):
  tyDisp = {
    "prim":    lambda p:  Prim(p.name,substitute(m,p.rep)) if (p.rep != None) else p,
    "var":     lambda v:  m[v.name] if (v.name in m.keys()) else v,
    "farr":    lambda fa: FixedArr(substitute(m,fa.ty), substitute(m,fa.tlen)),
    "arr":     lambda a:  Arr(substitute(m,a.ty)),
    "variant": lambda v:  substituteInVariant(m,v),
    "struct":  lambda s:  substituteInStruct(m,s),
    "long":    lambda n:  n,
    "app":     lambda a:  substituteInApp(m,a),
    "rec":     lambda r:  substituteInRec(m,r),
    "abs":     lambda a:  substituteInAbs(m,a)
  }
  return TyCase(tyDisp).apply(ty)

def expectFn(ty):
  if (isinstance(ty,Prim)):
    if (ty.rep == None):
      if (ty.name == "fileref"):
        return Abs(["t"], Prim("long",None))
      else:
        raise Exception("Expected function representation in place of primitive: " + ty.name)
    else:
      return expectFn(ty.rep)
  elif (isinstance(ty,Abs)):
    return ty
  else:
    raise Exception("Expected function in place of type: " + str(ty))

def evalApp(pf, args):
  f = expectFn(pf)
  if (len(args)!=len(f.vns)):
    raise Exception("Arity mismatch in application (expected " + str(len(f.vns)) + " arguments): " + str(App(pf,args)))
  m={}
  for i in range(len(f.vns)):
    m[f.vns[i]] = args[i]
  return substitute(m, f.ty)

#######
#
# determine memory layout of any type
#
#######

def align(x, b):
  if (x % b == 0):
    return x
  else:
    return b*(int(x/b)+1)

def alignOfStruct(s):
  a=1
  for field in s.fields:
    a=max(a,alignOf(field[2]))
  return a

def alignOfVariant(v):
  a=4
  for ctor in v.ctors:
    a=max(a,alignOf(ctor[2]))
  return a

def alignOfApp(a):
  return alignOf(evalApp(a.f, a.args))

def alignOf(ty):
  tyDisp = {
    "prim":    lambda p:  1 if (p.name == "unit") else alignOf(p.rep) if (p.rep != None) else sizeOfPrim(p),
    "var":     lambda v:  fail("Can't determine alignment of type variable: " + v.name),
    "farr":    lambda fa: alignOf(fa.ty),
    "arr":     lambda a:  fail("Can't determine alignment of variable-length array: " + str(a)),
    "variant": lambda v:  alignOfVariant(v),
    "struct":  lambda s:  alignOfStruct(s),
    "long":    lambda n:  fail("Can't get alignment of type-level number: " + str(n.n)),
    "app":     lambda a:  alignOfApp(a),
    "rec":     lambda r:  fail("Can't get alignment of recursive type: " + str(r)),
    "abs":     lambda a:  fail("Can't get alignment of type-level function: " + str(a))
  }
  return TyCase(tyDisp).apply(ty)

def sizeOfPrim(p):
  if (p.rep != None):
    return sizeOf(p.rep)
  else:
    if (p.name == "unit"):
      return 0
    elif (p.name == "bool"):
      return 1
    elif (p.name == "byte"):
      return 1
    elif (p.name == "char"):
      return 1
    elif (p.name == "short"):
      return 2
    elif (p.name == "int"):
      return 4
    elif (p.name == "long"):
      return 8
    elif (p.name == "float"):
      return 4
    elif (p.name == "double"):
      return 8
    else:
      raise Exception("Can't determine size of unknown primitive type: " + p.name)

def sizeOfStruct(s):
  o=0
  for f in s.fields:
    o = align(o, alignOf(f[2])) + sizeOf(f[2])
  return align(o, alignOf(s))

def sizeOfVariant(v):
  a=alignOf(v)
  maxsz=0
  for ctor in v.ctors:
    maxsz=max(maxsz,sizeOf(ctor[2]))
  return align(align(4,a)+maxsz,a)

def sizeOfApp(a):
  return sizeOf(evalApp(a.f, a.args))

def sizeOf(ty):
  tyDisp = {
    "prim":    lambda p:  sizeOfPrim(p),
    "var":     lambda v:  fail("Can't determine size of type variable: " + v.name),
    "farr":    lambda fa: sizeOf(fa.ty)*fa.tlen.n,
    "arr":     lambda a:  fail("Can't determine size of variable-length array: " + str(a)),
    "variant": lambda v:  sizeOfVariant(v),
    "struct":  lambda s:  sizeOfStruct(s),
    "long":    lambda n:  fail("Can't get size of type-level number: " + str(n.n)),
    "app":     lambda a:  sizeOfApp(a),
    "rec":     lambda r:  fail("Can't get size of recursive type: " + str(r)),
    "abs":     lambda a:  fail("Can't get size of type-level function: " + str(a))
  }
  return TyCase(tyDisp).apply(ty)

#######
#
# Type Description Decoding
#
#######

# a cheap cursor
class ReadPos:
  def __init__(self):
    self.pos = 0
  def __repr__(self): return str(self.pos)

# type descriptions
TYCTOR_PRIM      = 0
TYCTOR_TVAR      = 2
TYCTOR_FIXEDARR  = 4
TYCTOR_ARR       = 5
TYCTOR_VARIANT   = 6
TYCTOR_STRUCT    = 7
TYCTOR_SIZE      = 11
TYCTOR_TAPP      = 12
TYCTOR_RECURSIVE = 13
TYCTOR_TABS      = 15

def decodeBool(d, p):
  b = struct.unpack('B', d[p.pos:p.pos+1])[0]
  p.pos += 1
  return b != 0

def decodeInt(d, p):
  n = struct.unpack('I', d[p.pos:p.pos+4])[0]
  p.pos += 4
  return n

def decodeLong(d, p):
  n = struct.unpack('Q', d[p.pos:p.pos+8])[0]
  p.pos += 8
  return n

def decodeStr(d, p):
  n = decodeLong(d,p)
  s = str(d[p.pos:p.pos+n])
  p.pos += n
  return s

def decodeTypeDesc(d, p):
  c = decodeInt(d,p)
  if (c == TYCTOR_PRIM):
    name = decodeStr(d, p)
    if (decodeBool(d, p)):
      return Prim(name, decodeTypeDesc(d, p))
    else:
      return Prim(name, None)
  elif (c == TYCTOR_TVAR):
    name = decodeStr(d, p)
    return Var(name)
  elif (c == TYCTOR_FIXEDARR):
    ty   = decodeTypeDesc(d, p)
    tlen = decodeTypeDesc(d, p)
    return FixedArr(ty, tlen)
  elif (c == TYCTOR_ARR):
    ty = decodeTypeDesc(d, p)
    return Arr(ty)
  elif (c == TYCTOR_VARIANT):
    n = decodeLong(d,p)
    ctors = []
    for i in range(n):
      name = decodeStr(d,p)
      cid  = decodeInt(d,p)
      ty   = decodeTypeDesc(d,p)
      ctors.append((name,cid,ty))
    return Variant(ctors)
  elif (c == TYCTOR_STRUCT):
    n = decodeLong(d,p)
    fields = []
    for i in range(n):
      name = decodeStr(d,p)
      cid  = decodeInt(d,p)
      ty   = decodeTypeDesc(d,p)
      fields.append((name,cid,ty))
    return Struct(fields)
  elif (c == TYCTOR_SIZE):
    return TLong(decodeLong(d,p))
  elif (c == TYCTOR_TAPP):
    f = decodeTypeDesc(d,p)
    n = decodeLong(d,p)
    args = []
    for i in range(n):
      args.append(decodeTypeDesc(d,p))
    return App(f,args)
  elif (c == TYCTOR_RECURSIVE):
    vn = decodeStr(d,p)
    ty = decodeTypeDesc(d,p)
    return Recursive(vn,ty)
  elif (c == TYCTOR_TABS):
    n = decodeLong(d,p)
    vns = []
    for i in range(n):
      vns.append(decodeStr(d,p))
    ty = decodeTypeDesc(d,p)
    return Abs(vns,ty)
  else:
    raise Exception('Not a supported type constructor ID: ' + str(c))

#######
#
# Version updates as type transforms (where possible)
#
#######

def V1toV2Type(ty):
  tyDisp = {
    "prim":    lambda p:  p if (p.rep == None) else Prim(p.name, V1toV2Type(p.rep)),
    "var":     lambda v:  v,
    "farr":    lambda fa: FixedArr(V1toV2Type(fa.ty), V1toV2Type(ta.tlen)),
    "arr":     lambda a:  App(Prim("darray", Abs(["t"], Prim("long", None))), [V1toV2Type(a.ty)]),
    "variant": lambda v:  Variant([(ctor[0], ctor[1], V1toV2Type(ctor[2])) for ctor in v.ctors]),
    "struct":  lambda s:  Struct([(field[0], field[1], V1toV2Type(field[2])) for field in s.fields]),
    "long":    lambda n:  n,
    "app":     lambda a:  App(V1toV2Type(a.f), [V1toV2Type(arg) for arg in a.args]),
    "rec":     lambda r:  Recursive(r.vn, V1toV2Type(r.ty)),
    "abs":     lambda a:  Abs(a.vns, V1toV2Type(a.ty))
  }
  return TyCase(tyDisp).apply(ty)

#######
#
# File envelope decoding (read page data, environment data)
#
#######

# page entry decoding
def isEnvPage(p):
  return (p >> 14) == 2
def availBytes(p):
  return p & 0x3FFF

# a file variable definition
class EnvEntry:
  def __init__(self, offset, ty):
    self.offset = offset
    self.ty     = ty
  def __repr__(self):
    return str(self.ty) + "@" + str(self.offset)

# read file metadata
class FREnvelope:
  def __init__(self, fpath):
    self.p = fpath
    self.f = open(self.p, 'r+b')
    self.m = mmap.mmap(self.f.fileno(), 0, mmap.ACCESS_READ)

    # make sure that the file header is what we expect
    if (struct.unpack('I', self.m[0:4])[0] != 0x10A1DB0D):
      raise Exception('Not a valid structured data file: ' + self.p)

    self.pageSize = struct.unpack('H', self.m[4:6])[0]
    self.version  = struct.unpack('H', self.m[6:8])[0]

    if (self.pageSize != 4096):
      raise Exception('Expected 4K page size')
    if (not(self.version in [1,2])):
      raise Exception('Structured data file format version ' + str(self.version) + ' not supported')

    # read the page data in this file
    self.pages = []
    self.readPageEntries(self.pages, 8, 4096)

    # read the environment data in this file
    self.env = dict([])
    page=0
    while (page < len(self.pages)):
      if (isEnvPage(self.pages[page])):
        page += self.readEnvPage(self.env, page)
      else:
        page += 1

    # if reading the old format, we need to reinterpret recorded types
    if (self.version == 1):
      for vn, b in self.env.iteritems():
        b.ty = V1toV2Type(b.ty)

  # read page data entries into the 'pages' argument
  # if there is a link to a subsequent page to read page data from, follow it
  def readPageEntries(self, pages, i, o):
    k = i
    e = o - 8
    while (k < e):
      p = struct.unpack('H', self.m[k:k+2])[0]
      if (p == 0):
        break
      pages.append(p)
      k += 2
    n = struct.unpack('Q', self.m[e:e+8])[0]
    if (n != 0):
      self.readPageEntries(pages, n*4096, (n+1)*4096)

  # read environment data into the 'env' argument out of 'page'
  def readEnvPage(self, env, page):
    initOffset = page * 4096
    offset = initOffset
    while (True):
      offset = self.readEnvRecord(env, offset)

      pos   = offset - 1
      tpage = pos / 4096
      rpos  = (pos % 4096) + 1
      if (rpos == (4096 - availBytes(self.pages[tpage]))):
        break

    return int(math.ceil((float(offset-initOffset))/4096.0))

  def readEnvRecord(self, env, offset):
    vpos = struct.unpack('Q', self.m[offset:offset+8])[0]
    offset += 8

    vnlen = struct.unpack('Q', self.m[offset:offset+8])[0]
    offset += 8

    vn = str(self.m[offset:offset+vnlen])
    offset += vnlen

    tylen = struct.unpack('Q', self.m[offset:offset+8])[0]
    offset += 8

    if (len(vn) > 0 and vn[0] != '.' and tylen > 0):
      env[vn] = EnvEntry(vpos, decodeTypeDesc(self.m[offset:offset+tylen], ReadPos()))

    offset += tylen
    return offset

#######
#
# Read structured data
#
#######

class UnitReader:
  def read(self,m,offset): return None

class UnpackReader:
  def __init__(self,fmt,sz):
    self.fmt = fmt
    self.sz = sz
  def read(self,m,offset):
    return struct.unpack(self.fmt,m[offset:offset+self.sz])[0]

class FArrReader:
  def __init__(self, renv, ty, c):
    self.c   = c
    self.rdr = makeReader(renv, ty)
    self.esz = sizeOf(ty)
  def read(self,m,offset):
    r=[]
    o=offset;
    for i in range(self.c):
      r.append(self.rdr.read(m,o))
      o += self.esz
    return r

def tupleReaders(renv, tys):
  o  = 0
  os = []
  rs = []
  for ty in tys:
    o = align(o, alignOf(ty))
    os.append(o)
    rs.append(makeReader(renv, ty))
    o += sizeOf(ty)
  return (os,rs)

class TupleReader:
  def __init__(self, renv, tys):
    os, rs = tupleReaders(renv, tys)
    self.os = os
    self.rs = rs
  def read(self,m,offset):
    vs=[]
    for i in range(len(self.os)):
      vs.append(self.rs[i].read(m,offset+self.os[i]))
    return tuple(vs)

class StructView:
  def __init__(self, fs, foffs, vs):
    self.fs = fs
    self.foffs = foffs
    self.vs = vs
  def __repr__(self):
    r = '{'
    if (len(self.vs)>0):
      r += self.fs[0] + '=' + str(self.vs[0])
      for i in range(1,len(self.vs)):
        r += ', ' + self.fs[i] + '=' + str(self.vs[i])
    r += '}'
    return r
  def __str__(self): return self.__repr__()
  def __getattr__(self, attr):
    return self.vs[self.foffs[attr]]

class StructReader:
  def __init__(self, renv, fs, tys):
    os, rs = tupleReaders(renv, tys)
    self.fs = fs
    self.os = os
    self.rs = rs
    foffs={}
    for i in range(len(self.fs)):
      foffs[self.fs[i]] = i
    self.foffs = foffs
  def read(self,m,offset):
    vs=[]
    for i in range(len(self.os)):
      vs.append(self.rs[i].read(m,offset+self.os[i]))
    return StructView(self.fs, self.foffs, vs)

class MaybeReader:
  def __init__(self, renv, ty):
    self.poff = align(4, alignOf(ty))
    self.tr   = UnpackReader('I', 4)
    self.jr   = makeReader(renv, ty)
  def read(self,m,offset):
    t = self.tr.read(m,offset)
    if (t == 0):
      return None
    else:
      return self.jr.read(m,offset+self.poff)

class EnumView:
  def __init__(self, ns, t):
    self.ns = ns
    self.t = t
  def __repr__(self):
    return '|' + str(self.ns.get(self.t)) + '|'

class EnumReader:
  def __init__(self, ctors):
    self.tr = UnpackReader('I',4)
    ns={}
    for ctor in ctors:
      ns[ctor[1]] = ctor[0]
    self.ns = ns
  def read(self,m,offset):
    t = self.tr.read(m,offset)
    return EnumView(self.ns, t)
    
class VariantView:
  def __init__(self, cn, value):
    self.cn    = cn
    self.value = value
  def __repr__(self):
    if (len(self.cn)>0 and self.cn[0] == '.'):
      return "|" + self.cn[2:] + "=" + str(self.value) + "|"
    else:
      return "|" + self.cn + "=" + str(self.value) + "|"

class VariantReader:
  def __init__(self, renv, ctors):
    poff=4
    crs={}
    cns={}
    for ctor in ctors:
      poff = align(poff, alignOf(ctor[2]))
      crs[ctor[1]] = makeReader(renv, ctor[2])
      cns[ctor[1]] = ctor[0]

    self.tr   = UnpackReader('I', 4)
    self.poff = poff
    self.crs  = crs
    self.cns  = cns
  def read(self,m,offset):
    t = self.tr.read(m,offset)
    return VariantView(self.cns[t], self.crs[t].read(m,offset+self.poff))

class StrReader:
  def __init__(self):
    self.nr = UnpackReader('Q',8)
  def read(self,m,offset):
    n=self.nr.read(m,offset)
    return m[offset+8:offset+8+n]

class ArrReader:
  def __init__(self,renv,ty):
    self.nr   = UnpackReader('Q',8)
    self.r    = makeReader(renv,ty)
    self.vlen = sizeOf(ty)
  def read(self,m,offset):
    n=self.nr.read(m,offset)
    vs=[]
    o=offset+8
    for i in range(n):
      vs.append(self.r.read(m,o))
      o+=self.vlen
    return vs

class NYIReader:
  def read(self,m,offset):
    raise Exception("nyi")

globalTypeExts={}

def makeCustomReader(name, renv, ty, repty):
  mkR = globalTypeExts.get(name)
  if (mkR != None):
    return mkR(renv, ty, repty)
  else:
    raise Exception("I don't know how to decode this type: " + str(ty))

def makePrimReader(renv, p):
  if (p.name == "unit"):
    return UnitReader()
  elif (p.name == "bool"):
    return UnpackReader('?', 1)
  elif (p.name == "char"):
    return UnpackReader('c', 1)
  elif (p.name == "byte"):
    return UnpackReader('B', 1)
  elif (p.name == "short"):
    return UnpackReader('H', 2)
  elif (p.name == "int"):
    return UnpackReader('I', 4)
  elif (p.name == "long"):
    return UnpackReader('Q', 8)
  elif (p.name == "float"):
    return UnpackReader('f', 4)
  elif (p.name == "double"):
    return UnpackReader('d', 8)
  elif (p.rep != None):
    return makeCustomReader(p.name, renv, p, p.rep)
  else:
    raise Exception("I don't know how to decode the primitive type: " + p.name)

def makeFArrReader(renv,fa):
  return FArrReader(renv, fa.ty, fa.tlen.n)

def makeArrReader(renv,a):
  if (isinstance(a.ty,Prim) and a.ty.name == "char"):
    return StrReader()
  else:
    return ArrReader(renv,a.ty)

def makeVariantReader(renv,v):
  if (len(v.ctors)==2 and v.ctors[0][0] == ".f0" and v.ctors[0][1] == 0 and isinstance(v.ctors[0][2],Prim) and v.ctors[0][2].name == "unit"):
    return MaybeReader(renv,v.ctors[1][2])
  elif (all(map(lambda c: isinstance(c[2],Prim) and c[2].name=="unit", v.ctors))):
    return EnumReader(v.ctors)
  else:
    return VariantReader(renv,v.ctors)

def makeStructReader(renv,s):
  if (len(s.fields) == 0):
    return UnitReader()
  elif (s.fields[0][0][0] == '.'): # should we read this as a tuple?
    return TupleReader(renv, map(lambda f:f[2], s.fields))
  else:
    return StructReader(renv, map(lambda f:f[0], s.fields), map(lambda f:f[2], s.fields))

def makeAppReader(renv,app):
  if (isinstance(app.f,Prim)):
    return makeCustomReader(app.f.name, renv, app, evalApp(app.f, app.args))
  else:
    raise Exception("I don't know how to read '" + str(app) + "'")
    
class RecReader:
  def __init__(self):
    self.r = None
  def read(self,m,offset):
    return self.r.read(m,offset)

def makeRecReader(renv, rec):
  o = renv.get(rec.vn)

  r = RecReader()
  renv[rec.vn] = r
  r.r = makeReader(renv, rec.ty)

  if (o != None):
    renv[rec.vn]=o
  else:
    renv.pop(rec.vn, None)

  return r

def makeVarReader(renv, vn):
  if vn in renv:
    return renv[vn]
  else:
    raise Exception("Can't make reader with variable not in environment: " + vn)

def makeReader(renv,ty):
  readerDisp = {
    "prim":    lambda p:  makePrimReader(renv, p),
    "var":     lambda v:  makeVarReader(renv, v.name),
    "farr":    lambda fa: makeFArrReader(renv,fa),
    "arr":     lambda a:  makeArrReader(renv,a),
    "variant": lambda v:  makeVariantReader(renv,v),
    "struct":  lambda s:  makeStructReader(renv,s),
    "long":    lambda n:  fail("Can't read type-level number: " + str(n.n)),
    "app":     lambda a:  makeAppReader(renv,a),
    "rec":     lambda r:  makeRecReader(renv,r),
    "abs":     lambda a:  fail("Can't read type-level function: " + str(a))
  }
  return TyCase(readerDisp).apply(ty)

#######
#
# the user interface to structured data
#
#######

def formatRow(cns, cs, r):
  s=''
  for k in range(len(cs)-1):
    s += cs[k][r].ljust(cns[k], ' ')
  s += cs[len(cs)-1][r]
  return s

def tableFormat(cs):
  cns=[]
  rc=0
  for c in cs:
    n = 0
    rc = len(c) if rc==0 else min(rc, len(c))
    for s in c:
      n = max(n, len(s))
    cns.append(n)

  s = ''
  if (rc > 0):
    s = formatRow(cns, cs, 0)
    for r in range(1, rc):
      s += '\n' + formatRow(cns, cs, r)

  return s

class FRegion:
  def __init__(self, fpath):
    self.rep = FREnvelope(fpath)

    for vn, bind in self.rep.env.iteritems():
      bind.reader = makeReader({}, bind.ty)

  @staticmethod
  def addType(name, gen):
    globalTypeExts[name] = gen

  def __str__(self): return self.__repr__()

  def __repr__(self):
    vns = []
    hts = []
    tds = []
    for vn, bind in self.rep.env.iteritems():
      vns.append(vn)
      hts.append(' :: ')
      tds.append(str(bind.ty))
    return tableFormat([vns, hts, tds])

  def __getattr__(self, attr):
    b = self.rep.env.get(attr, None)
    if (b == None):
      raise Exception("FRegion has no field named '" + attr + "'")
    else:
      return b.reader.read(self.rep.m, b.offset)

class FRMeta:
  def __init__(self, f): self.f = f
  def __repr__(self): return repr(self.f)
  def __getattr__(self, attr):
    b = self.f.rep.env.get(attr, None)
    if (b == None):
      raise Exception("FRegion has no field named '" + attr + "'")
    else:
      return b

def meta(f): return FRMeta(f)

#######
#
# support common "application types" by default
#
#######

# date/time
class DateTimeReader:
  def __init__(self, renv, repty):
    self.nr = makeReader(renv, repty)
  def read(self,m,o):
    return datetime.datetime.fromtimestamp(self.nr.read(m,o)/1000000.0)

FRegion.addType("datetime", lambda renv, ty, repty: DateTimeReader(renv, repty))

# file refs (within-file pointer types)
class FileRefReader:
  def __init__(self,renv,ty,repty):
    self.refr = makeReader(renv,repty)
    self.r    = makeReader(renv,ty)
  def read(self,m,offset):
    return self.r.read(m,self.refr.read(m,offset))

FRegion.addType("fileref", lambda renv, ty, repty: FileRefReader(renv, ty.args[0], repty))

# carrays (variable-length arrays stored with a static capacity)
FRegion.addType("carray", lambda renv, ty, repty: makeArrReader(renv, Arr(ty.args[0])))

# darrays (old style variable-length arrays stored with capacity)
class DArrReader:
  def __init__(self,renv,ty):
    self.ar = makeArrReader(renv,Arr(ty))
  def read(self,m,offset):
    return self.ar.read(m,offset+8)

FRegion.addType("darray", lambda renv, ty, repty: DArrReader(renv, ty.args[0]))

