const types = require('./types');
const fs = require('fs');

const {
    Field,
    Prim,
    Var,
    FixedArr,
    Arr,
    Variant,
    Struct,
    App,
    Recursive,
    Abs,
    TLong,
    TYCTOR_PRIM,
    TYCTOR_VARIANT,
    TYCTOR_TVAR,
    TYCTOR_FIXEDARR,
    TYCTOR_ARR,
    TYCTOR_STRUCT,
    TYCTOR_SIZE,
    TYCTOR_TAPP,
    TYCTOR_RECURSIVE,
    TYCTOR_TABS,
    TyCase
} = types;

function fail(msg) {
    console.log(msg)
}

function str(s){
    return `${s}`;
}

function dictWithout(m, k)  {
    let r = {...m};
    delete r[k];
    return r;
}

function dictWithouts(m, ks) {
    let r = {...m};
    for(let k of ks){
        delete r[k];
    }
    return r;
}

function addFreeVar(m, vn) {
    m[vn] = undefined;
}

function updateDict(m, r) {
    return {
        ...m,
        ...r
    };
}

function freeVarsInto(m, ty) {
    const tyDisp = {
        prim:    p  =>  undefined,
        var:     v  =>  addFreeVar(m,v.name),
        farr:    fa =>  { freeVarsInto(m,fa.ty); freeVarsInto(m,fa.tlen); },
        arr:     a  =>  freeVarsInto(m,a.ty),
        variant: v  =>  { for(let ctor of v.ctors) freeVarsInto(m,ctor.ty); },
        struct:  s  =>  { for(let field of s.fields) freeVarsInto(m,field.ty); },
        long:    n  =>  undefined,
        app:     a  =>  { freeVarsInto(m,a.f); for(let arg of a.args) freeVarsInto(m,arg); },
        rec:     r  =>  updateDict(m, dictWithout(freeVars(r.ty),r.vn)),
        abs:     a  =>  updateDict(m, dictWithouts(freeVars(a.ty),a.vns))
    };
    return (new TyCase(tyDisp)).apply(ty);
}

function freeVars(ty) {
    let m = {};
    freeVarsInto(m, ty);
    return m;
}

function dictFreeVars(m) {
    let lm = {};
    for(let k in m) {
        freeVarsInto(lm, m[k])
    }
    return lm;
}

function freeName(m) {
    let vn = "t0";
    let n = 0;
    while(true){
        if(!m[vn]){
            break;
        }
        n++;
        vn = `${vn}${n}`;
    }
    return vn;
}

function substituteInVariant(m, v) {
    let ctors = [];
    v.ctors.forEach(e => {
        ctors.push(new Field(e.name, e.cid, substitute(m, e.ty)));
    });
    return new Variant(ctors);
}

function substituteInStruct(m, s) {
    let fields = [];
    s.fields.forEach(e => {
        fields.push(new Field(e.name, e.cid, substitute(m, e.ty)));
    });
    return new Struct(fields);
}

function substituteInApp(m, a) {
    let args = [];
    a.args.forEach((ty) => {
        args.push(substitute(m, ty));
    })
    return new App(substitute(m, a.f), args);
}

function substituteInRec(m, r) {
    let lm=dictWithout(m,r.vn);
    let fvs=dictFreeVars(lm);
    if(r.vn in fvs){
        let nn = freeName(fvs);
        let mm = {};
        mm[r.vn] = new Var(nn);
        return new Recursive(nn, substitute(mm, r.ty))
    }else{
        return new Recursive(r.vn, substitute(lm, r.ty));
    }
}


function arrayEquals(a, b) {
    return Array.isArray(a) &&
      Array.isArray(b) &&
      a.length === b.length &&
      a.every((val, index) => val === b[index]);
  }

function substituteInAbs(m, a) {
    let lm=dictWithouts(m,a.vns);
    let fvs=dictFreeVars(lm);
    let vns = [];
    for(let vn of a.vns){
        if(vn in fvs){
            let nn = freeName(lm);
            lm[vn] = new Var(nn);
            vns.push(nn);
        }else{
            vns.push(vn);
        }
    }

    if(arrayEquals(vns, a.vns)){
        return new Abs(vns, substitute(lm, a.ty))
    }else{
        return new Abs(a.vns, substitute(lm, a.ty));
    }
} 

function substitute(m, ty) {
    let tyDisp = {
        prim:    p  =>  p.rep ? new Prim(p.name, substitute(m, p.rep)) : p,
        var:     v  =>  m[v.name] ? m[v.name] : v,
        farr:    fa =>  new FixedArr(substitute(m, fa.ty), substitute(m, fa.tlen)),
        arr:     a  =>  new Arr(substitute(m, a.ty)),
        variant: v  =>  substituteInVariant(m, v),
        struct:  s  =>  substituteInStruct(m, s),
        long:    n  =>  n,
        app:     a  =>  substituteInApp(m, a),
        rec:     r  =>  substituteInRec(m, r),
        abs:     a  =>  substituteInAbs(m, a)
    };

    return (new TyCase(tyDisp)).apply(ty);
}

function expectFn(ty) {
    if(ty instanceof Prim) {
        if(!ty.rep){
            if(ty.name == "fileref"){
                return new Abs(["t"], new Prim("long"))
            }else{
                return fail(`Expected function representation in place of primitive: ${ty.name}`)
            }
        }else{
            return expectFn(ty.rep);
        }
    }else if(ty instanceof Abs){
        return ty;
    }else{
        fail(`Expected function in place of type: ${ty}`);
    }
}

function evalApp(pf, args){
    let f = expectFn(pf);
    if(f.vns.length != args.length){
        return fail(`Arity mismatch in application (expected ${f.vns.length} arguments: ${new App(pf, args)}`);
    }

    let m = {};
    f.vns.forEach( (e, i) => {
        m[e] = args[i]
    });
    return substitute(m, f.ty)
}

/*
    determine memory layout of any type
*/

function align(x, b) {
  if (x % b == 0)
    return x
  else
    return b*(Math.floor(x/b)+1)
}

function alignOfStruct(s) {
    let a = 1;
    s.fields.forEach(f => {
        a = Math.max(a, alignOf(f.ty))
    });
    return a;
}

function alignOfVariant(v) {
    let a = 1;
    v.ctors.forEach( ctor => {
        a = Math.max(a, alignOf(ctor.ty))
    });
    return a;
}

function alignOfApp(a){
    return alignOf(evalApp(a.f, a.args));
}

function sizeOfPrim(p) {
    if (p.rep)
        return sizeOf(p.rep);
    if (p.name == 'unit')
        return 0;
    if (p.name == 'bool')
        return 1;
    if (p.name == 'byte')
        return 1;
    if (p.name == 'char')
        return 1;
    if (p.name == 'short')
        return 2;
    if (p.name == 'int')
        return 4;
    if (p.name == 'long')
        return 8;
    if (p.name == 'float')
        return 4;
    if (p.name == 'double')
        return 8;
    fail("Can't determine size of unknown primitive type: " + p.name);
}

function alignOf(ty){
    let tyDisp = {
        prim: p => {
            if(p.name == "unit") {
                return 1;
            }
            if(p.rep) {
                return alignOf(p.rep);
            } 
            return sizeOfPrim(p);
        },
        var: v     => fail("CCan't determine alignment of type variable: " + v.name),
        farr: fa   => alignOf(fa.ty),
        arr: a     => fail(`Can't determine alignment of variable-length array: ${a}`),
        variant: v => alignOfVariant(v),
        struct: s  => alignOfStruct(s),
        long: l    => fail("Can't get alignment of type-level number ") ,
        app: a     => alignOfApp(a),
        rec: r     => fail(`Can't get alignment of recursive type: ${r}`),
        abs: a     => fail(`Can't get alignment of type-level function: ${a}`)
    };
    return (new TyCase(tyDisp)).apply(ty);
}

function sizeOfStruct(s) {
    let o = 0;
    s.fields.forEach(f => {
        o = align(o, alignOf(f.ty)) + sizeOf(f.ty)
    })
    return align(o, alignOf(s));
}

function sizeOfVariant(v) {
    let a = alignOf(v);
    let maxsz = 0;
    v.ctors.forEach( ctor => {
        maxsz=Math.max(maxsz,sizeOf(ctor.ty))
    });
    return align(align(4,a)+maxsz,a)
}

function sizeOfApp(a){
  return sizeOf(evalApp(a.f, a.args))
}

function sizeOf(ty) {
  tyDisp = {
    prim:    p =>  sizeOfPrim(p),
    var:     v =>  fail("Can't determine size of type variable: " + v.name),
    farr:    fa =>  sizeOf(fa.ty)*fa.tlen.n,
    arr:     a =>  fail("Can't determine size of variable-length array: " + str(a)),
    variant: v =>  sizeOfVariant(v),
    struct:  s =>  sizeOfStruct(s),
    long:    n =>  fail("Can't get size of type-level number: " + str(n.n)),
    app:     a =>  sizeOfApp(a),
    rec:     r =>  fail("Can't get size of recursive type: " + str(r)),
    abs:     a =>  fail("Can't get size of type-level function: " + str(a))
  }
  return (new TyCase(tyDisp)).apply(ty)
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function readUint64(buffer, offset) {
    let n = buffer.readBigUInt64LE(offset);
    let m = parseInt(n);
    if ( n!=m ){
        fail("error for int 64");
    }
    return parseInt(n);
}

class BufferDecoder {
    static decodeBool(buffer, p) {
        let b = buffer.readUInt8(p.pos)
        p.pos += 1;
        return b!= 0;
    }

    static decodeInt(buffer, p) {
        let n = buffer.readInt32LE(p.pos);
        p.pos += 4;
        return n;
    }

    static decodeLong(buffer, p) {
        let n = readUint64(buffer, p.pos);
        p.pos += 8;
        return n;
    }

    static decodeStr(buffer, p){
        let n = BufferDecoder.decodeLong(buffer, p);
        let s = buffer.toString('utf8', p.pos, p.pos + n);
        p.pos += n;
        return s;
    }
}

class UnpackReader {
    constructor(fmt, sz) {
        this.fmt = fmt;
        this.sz = sz;
    }

    read(fd, offset) {
        let buffer = Buffer.alloc(this.sz);
        let bytes = fs.readSync(fd, buffer, 0, this.sz, offset);
        if (bytes != this.sz) {
            fail(`fail to read ${bytes} != ${this.sz}`);
        }
        if(this.fmt == "?") { //bool
            return buffer.readUInt8(0) != 0;
        } else if(this.fmt == "c") { //char
            return buffer.readUInt8(0);
        } else if(this.fmt == "B") { //byte 
            return buffer.readInt8();
        } else if(this.fmt == "H") {  //short
            return buffer.readUInt16LE(0);
        } else if(this.fmt == "I") {  //int
            return buffer.readInt32LE(0);
        } else if(this.fmt == "Q") {  //long
            return readUint64(buffer, 0);
        } else if(this.fmt == "f") {  //float
            return buffer.readFloatLE(0);
        } else if(this.fmt == "d") {  //double
            return buffer.readDoubleLE(0);
        }
    }
}

class NoneT {

}

const None = new NoneT();

class ReadPos {
    constructor(pos){
        this.pos = pos;
    }
}

String.prototype.ljust = function( length, char ) {
    var fill = [];
    while ( fill.length + this.length < length ) {
      fill[fill.length] = char;
    }
    return fill.join('') + this;
}

String.prototype.rjust = function( length, char ) {
    var fill = [];
    while ( fill.length + this.length < length ) {
      fill[fill.length] = char;
    }
    return this + fill.join('');
}

function formatRow(cns, cs, r) {
    let s = '';
    cs.forEach((v, i) => {
        if (i == (cs.length - 1)) {
            s += v[r]
        } else {
            s += v[r].ljust(cns[i], ' ');
        }
    });

    return s
}

function tableFormat(cs) {
    let cns = [];
    let rc = 0;
    cs.forEach( c => {
        let n = 0;
        rc = rc == 0 ? c.length : Math.min(rc, c.length);
    });

    let s = '';
    if(rc > 0) {
        s = formatRow(cns, cs, 0);
        for(let r = 1; r < rc; i++) {
            s += `${os.EOL}${formatRow(cns, cs, r)}`;
        }
    }
    return s;
}

module.exports = {
    sizeOf,
    evalApp,
    fail,
    ReadPos,
    BufferDecoder,
    readUint64,
    UnpackReader,
    str, 
    None,
    tableFormat,
    align,
    alignOf
}
