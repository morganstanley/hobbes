const types = require('./types');
const util = require('./util')

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

const {
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
} = util;

const fs = require('fs')
const os = require('os');

const PageSize = 4096;
const all = arr => {
    return arr.every(e => !!e? true: false);
};
const map = (fn, arr) => {
    return arr.map(fn);
}

function isEnvPage(p) {
    return (p >> 14) == 2;
}

function availBytes(p) {
    return p & 0x3FFF;
}

function decodeTypeDesc(d, p) {
    let c = BufferDecoder.decodeInt(d, p);
    if (c == TYCTOR_PRIM) {
        let name = BufferDecoder.decodeStr(d, p)
        if (BufferDecoder.decodeBool(d, p)) {
            return new Prim(name, decodeTypeDesc(d, p));
        }
        else {
            return new Prim(name);
        }
    } else if (c == TYCTOR_TVAR) {
        let name = BufferDecoder.decodeStr(d, p);
        return new Var(name)
    } else if (c == TYCTOR_FIXEDARR) {
        let ty = decodeTypeDesc(d, p);
        let tlen = decodeTypeDesc(d, p);
        return new FixedArr(ty, tlen);
    } else if (c == TYCTOR_ARR) {
        let ty = decodeTypeDesc(d, p);
        return new Arr(ty);
    } else if (c == TYCTOR_VARIANT) {
        let n = BufferDecoder.decodeLong(d, p);
        let ctors = [];
        for (let i = 0; i < n; i++) {
            let name = BufferDecoder.decodeStr(d, p);
            let cid = BufferDecoder.decodeInt(d, p);
            let ty = decodeTypeDesc(d, p);
            ctors.push(new Field(name, cid, ty))
        }
        return new Variant(ctors);
    } else if (c == TYCTOR_STRUCT) {
        let n = BufferDecoder.decodeLong(d, p);
        let fields = [];
        for (let i = 0; i < n; i++) {
            let name = BufferDecoder.decodeStr(d, p);
            let cid = BufferDecoder.decodeInt(d, p);
            let ty = decodeTypeDesc(d, p);
            fields.push(new Field(name, cid, ty))
        }
        return new Struct(fields);
    } else if (c == TYCTOR_SIZE) {
        let n = BufferDecoder.decodeLong(d, p);
        return new TLong(n);
    } else if (c == TYCTOR_TAPP) {
        let f = decodeTypeDesc(d, p);
        let n = BufferDecoder.decodeLong(d, p);
        let args = [];
        for (let i = 0; i < n; i++) {
            args.push(decodeTypeDesc(d, p));
        }
        return new App(f, args);

    } else if (c == TYCTOR_RECURSIVE) {
        let vn = BufferDecoder.decodeStr(d, p);
        let ty = decodeTypeDesc(d, p);
        return new Recursive(vn, ty);
    } else if (c == TYCTOR_TABS) {
        let n = BufferDecoder.decodeLong(d, p);
        let vns = [];
        for (let i = 0; i < n; i++) {
            vns.push(BufferDecoder.decodeStr(d, p));
        }
        let ty = decodeTypeDesc(d, p);
        return new Abs(vns, ty);
    } else {
        fail(`Not a supported type constructor ID: ${c}`);
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

class Page {
    constructor(env, pageSize, index) {
        this.buffer = Buffer.alloc(pageSize);
        this.pageIndex = index;
        this.env = env;
    }

    addBuffer(buffer, offset, len) {
        buffer.copy(this.buffer, 0, offset, offset + len);
        this.parse();
        return this;
    }

    parse() {
        if (this.pageIndex == 0) {
            return this.parseHeader();
        }
        this.env.loadPage(this);
    }

    parseHeader() {
        let magic = this.buffer.readInt32LE(0);
        let pageSize = this.buffer.readUInt16LE(4);
        let version = this.buffer.readUInt16LE(6);

        console.log(`###########################${os.EOL}magic: ${magic}${os.EOL}version: ${version}${os.EOL}########################${os.EOL}`);

        this.env.version = version;
        this.env.readPageEntries(this, 8, pageSize);
    }
}

class EnvEntry {
    constructor(env, ty, offset, name) {
        this.ty = ty;
        this.offset = offset;
        this.name = name;
        this.fp = env.fp;
        this.renv = {};
        this.reader = makeReader(this.renv, this.ty);
    }

    toString() {
        return `${this.ty}`;
    }

    load() {
        if(this.reader) {
            return this.reader.read(this.fp, this.offset);
        }
    }
}

class FREnvelope {
    constructor(path) {
        this.path = path;
        this.curPage = undefined;
        this.nextPageIndex = undefined;
        this.curPageIndex = 0;
        this.pages = [];
        this.leftBuffer = undefined;
        this.curEnvPages = [];
        this.entry = {};
        this.version = 2;
        this.fp = fs.openSync(path, "r");
    }

    toString() {
        let s = "";
        for (let k in this.entry) {
            s += `${k}: ${this.entry[k]}${os.EOL}`
        }
        return s;
    }

    getStream(name) {
        return this.entry[name];
    }

    load(onSuccess, onError) {
        const reader = fs.createReadStream(this.path);
        reader.setEncoding('binary');

        reader.once('error', err => {
            fail(err);
        });

        reader.on('data', chunk => {
            let buf = Buffer.from(chunk, 'binary');
            if (this.leftBuffer) {
                buf = Buffer.concat(this.leftBuffer, buf);
                this, this.leftBuffer = undefined;
            }
            let curPos = 0;
            while (curPos < buf.length) {
                if (curPos + PageSize > buf.length) {
                    let len = buf.length - curPos;
                    this.leftBuffer = Buffer.alloc(len);
                    buf.copy(this.leftBuffer, 0, curPos);
                    break;
                }
                this.curPage = new Page(this, PageSize, this.curPageIndex++);
                this.curPage.addBuffer(buf, curPos, PageSize);
                curPos += PageSize;
            }
            return this.curPage;
        });

        reader.on('end', () => {
            if (this.leftBuffer) {
                fail('TODO: convert left buffer');
                return onError(this);
            }
            if (this.curEnvPages && this.curEnvPages.length > 0) {
                this.readEnvPage();
                this.curEnvPages = [];
                this.curPage = undefined;
                this.curPageIndex = undefined;
            }
            onSuccess(this);
        });
    }

    loadPage(page) {
        if (page.pageIndex == this.nextPageIndex) {
            let n = page.pageIndex;
            this.readPageEntries(page, 0, PageSize);
        } else if (page.pageIndex < this.pages.length) {
            if (isEnvPage(this.pages[page.pageIndex])) {
                if (this.curEnvPages.length == 0 || (page.pageIndex - this.curEnvPages[this.curEnvPages.length - 1].pageIndex == 1)) {
                    this.curEnvPages.push(page);
                } else {
                    if (this.curEnvPages.length > 0) {
                        this.readEnvPage();
                    }
                    this.curEnvPages = [page];
                }
            }
        }
    }

    readPageEntries(page, i, o) {
        let k = i;
        let e = o - 8;
        while (k < e) {
            let p = page.buffer.readUInt16LE(k);
            if (p == 0) {
                break;
            }
            this.pages.push(p);
            k += 2;
        }

        let n = readUint64(page.buffer, e);
        this.nextPageIndex = n;
    }

    readEnvPage() {
        let buffers = [];
        this.curEnvPages.forEach(p => buffers.push(p.buffer));
        let buffer = Buffer.concat(buffers);
        let initOffset = 0;
        let offset = initOffset;
        while (true) {
            offset = this.readEnvRecord(offset, buffer);
            let pos = offset - 1;
            let tpage = Math.floor(pos / PageSize);
            let rpos = (pos % PageSize) + 1;
            let pagesIndex = this.curEnvPages[tpage].pageIndex;
            if (rpos == (PageSize - availBytes(this.pages[pagesIndex]))) {
                break;
            }
        }
    }

    readEnvRecord(offset, buffer) {
        let vpos = readUint64(buffer, offset);
        offset += 8;
        let vnlen = readUint64(buffer, offset);
        offset += 8;
        let vn = buffer.toString('utf8', offset, offset + vnlen);
        offset += vnlen;
        let tylen = readUint64(buffer, offset);
        offset += 8;

        if (vn.length > 0 && vn[0] != '.' && tylen > 0) {
            let pos = new ReadPos(offset);
            this.entry[vn] = new EnvEntry(this, decodeTypeDesc(buffer, pos), vpos, vn);
        }

        offset += tylen;
        return offset;
    }
}


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
class UnitReader {
    read(fd, offset) {
        return None;
    }
}

class FArrReader {
    constructor(renv, ty, c) {
        this.c = c;
        this.esz = sizeOf(ty);
        this.rdr = makeReader(renv, ty);
    }

    read(fd, offset) {
        let r = [];
        let o = offset;
        for(let i = 0; i < this.c; i++){
            r.push(this.rdr.read(fd, o));
            o += this.esz;
        }
        return r;
    }
}

class ArrReader {
    constructor(renv, ty) {
        this.nr = new UnpackReader('Q', 8);
        this.r = makeReader(renv, ty);
        this.vlen = sizeOf(ty);
    }

    read(fd, offset) {
        let n = this.nr.read(fd, offset);
        let pos = offset + 8;
        let vs = [];
        for(let i = 0; i < n; i++) {
            let v = this.r.read(fd, pos + i * this.vlen);
            vs.push(v);
        }
        return vs;
    }
}

class StrReader {
    constructor() {
        this.nr = new UnpackReader('Q',8)
    }

    read(fd, offset) {
        let n = this.nr.read(fd, offset);
        let buffer = Buffer.alloc(n);
        fs.readSync(fd, buffer, 0, n, offset + 8);
        return buffer.toString('utf8');
    }
}

class MaybeReader {
    constructor(renv, ty){
        this.poff = align(4, alignOf(ty));
        this.tr   = new UnpackReader('I', 4);
        this.jr   = makeReader(renv, ty);
    }

    read(fd, offset) {
        let t = this.tr.read(fd, offset);
        if (t == 0) {
          return undefined;
        }
        else {
          return this.jr.read(fd, offset + this.poff);
        }
    }
}

class EnumView {
    constructor(ns, t){
        this.ns = ns;
        this.t = t;
    }

    toString() {
        return `|${this.ns[this.t]}|`;
    }
}

class EnumReader {
    constructor(ctors) {
        this.tr = new UnpackReader('I', 4);
        let ns = {};
        ctors.forEach(ctor => {
            ns[ctor.cid] = ctor.name;
        });
        this.ns = ns;
    }

    read(fd, offset) {
        let t = this.tr.read(fd, offset);
        return new EnumView(this.ns, t);
    }
}

class VariantView {
    constructor(cn, value) {
        this.cn = cn;
        this.value = value;
    }

    toString() {
        if (this.cn.length > 0 && this.cn[0] == '.') {
            return `|${this.cn.slice(2)}=${this.value}|`;
        } else {
            return `|${this.cn}=${this.value}|`;
        }
    }
}

class VariantReader {
    constructor(renv, ctors) {
        let poff=4;
        let crs={};
        let cns={};
        ctors.forEach( ctor => {
            poff = align(poff, alignOf(ctor.ty));
            crs[ctor.cid] = makeReader(renv, ctor.ty);
            cns[ctor.cid] = ctor.name;
        });

        this.tr = new UnpackReader('I', 4);
        this.poff = poff;
        this.crs = crs;
        this.cns = cns;
    }

    read(fd, offset) {
        let t = this.tr.read(fd, offset);
        return new VariantView(this.cns[t], this.crs[t].read(fd, offset + this.poff));
    }
}

function tupleReaders(renv, tys) {
    let o  = 0;
    let os = [];
    let rs = [];

    tys.forEach( ty => {
        o = align(o, alignOf(ty))
        os.push(o)
        rs.push(makeReader(renv, ty))
        o += sizeOf(ty)
    });

    return [os, rs];
}

class TupleReader {
    constructor(renv, tys) {
        let r = tupleReaders(renv, tys);
        this.os = r[0];
        this.rs = r[1];
    }

    read(fd, offset) {
        let vs = [];
        this.os.forEach( (o, i) => {
            let v = this.rs[i].read(fd, offset+o);
            vs.push(v);
        });
        return vs;
    }
}

class StructReader {
    constructor(renv, fs, tys) {
        let r = tupleReaders(renv, tys);
        this.os = r[0];
        this.rs = r[1];
        this.fs = fs;
    }

    read(fd, offset) {
        let vs = {};
        this.os.forEach( (o, i) => {
            let v = this.rs[i].read(fd, offset+o);
            vs[this.fs[i]] = v;
        });
        return vs;
    }
}

class RecReader {
    constructor() {
        this.r = undefined;
    }

    read(fd, offset) {
        if (this.r) {
            return this.r.read(fd, offset);
        }
    }
}

class FileRefReader {
    constructor(renv, ty, repty) {
        this.refr = makeReader(renv,repty);
        this.r    = makeReader(renv,ty.args[0]);
    }

    read(fd, offset) {
        let o = this.refr.read(fd, offset)
        if (o == 0)
          return undefined;
        else
          return this.r.read(fd, o)
    }
}

class FSeqReader {
    constructor(renv, ty, repty) {
        this.rr = makeReader(renv, repty);
    }

    read(fd, offset) {
        return this.rr.read(fd, offset);
    }
}

function makeCustomReader(name, renv, ty, repty) {
    let mkR = makeCustomReader.globalTypeExts[name]
    if (mkR)
        return mkR(renv, ty, repty)
    else
        fail("I don't know how to decode this type: " + str(ty))

}

makeCustomReader.globalTypeExts = {};

function addReaderType(name, fn) {
    makeCustomReader.globalTypeExts[name] = fn;
}

addReaderType("carray", (renv, ty, repty) => makeArrReader(renv, new Arr(ty.args[0])));
addReaderType("fileref", (renv, ty, repty) => new FileRefReader(renv,ty, repty));
addReaderType("fseq", (renv, ty, repty) => new FSeqReader(renv,ty, repty));

function makePrimReader(renv, p) {
    if (p.name == "unit")
        return new UnitReader()
    else if (p.name == "bool")
        return new UnpackReader('?', 1)
    else if (p.name == "char")
        return new UnpackReader('c', 1)
    else if (p.name == "byte")
        return new UnpackReader('B', 1)
    else if (p.name == "short")
        return new UnpackReader('H', 2)
    else if (p.name == "int")
        return new UnpackReader('I', 4)
    else if (p.name == "long")
        return new UnpackReader('Q', 8)
    else if (p.name == "float")
        return new UnpackReader('f', 4)
    else if (p.name == "double")
        return new UnpackReader('d', 8)
    else if (p.rep != None)
        return makeCustomReader(p.name, renv, p, p.rep)
    else
        fail("I don't know how to decode the primitive type: " + p.name)
}

function makeVarReader(renv, vn) {
    if(vn in renv){
        return renv[vn];
    }
    else {
        fail("Can't make reader with variable not in environment: " + vn);
    }
}

function makeFArrReader(renv, fa) {
    return new FArrReader(renv, fa.ty, fa.tlen.n) 
}

function makeArrReader(renv, a) {
    if (a.ty instanceof Prim && a.ty.name == "char") {
        return new StrReader()
    } else {
        return new ArrReader(renv, a.ty);
    }
}

function makeVariantReader(renv, v) {
    if (v.ctors.length == 2 && v.ctors[0].name == ".f0" && v.ctors[0].cid == 0 && (v.ctors[0].ty instanceof Prim) && v.ctors[0].ty.name == "unit") {
        return new MaybeReader(renv, v.ctors[1].ty);
    } else if(all(map(c => {
        return (c.ty instanceof Prim) && (c.ty.name == "unit");
    }, v.ctors))) {
        return new EnumReader(v.ctors)
    } else {
        return new VariantReader(renv, v.ctors);
    }
}

function makeStructReader(renv, s) {
    if (s.fields.length == 0) {
        return new UnitReader();
    } else if (s.fields[0].name[0] == '.') {
        return new TupleReader(renv, map( f => f.ty, s.fields))
    } else {
        return new StructReader(renv, map(f => f.name, s.fields), map(f => f.ty, s.fields))
    }
}

function makeAppReader(renv, app) {
    if (app.f instanceof Prim) {
        return makeCustomReader(app.f.name, renv, app, evalApp(app.f, app.args))
    }
    else {
        fail("I don't know how to read '" + str(app) + "'");
    }
}

function makeRecReader(renv, rec) {
    let o = renv[rec.vn];
    let r = new RecReader();
    renv[rec.vn] = r;
    r.r = makeReader(renv, rec.ty);

    if(!o) {
        renv[rec.vn] = o;
    } else {
        delete renv[rec.vn];
    }

    return r;
}

function makeReader(renv, ty) {
    readerDisp = {
        prim: p => makePrimReader(renv, p),
        var: v => makeVarReader(renv, v.name),
        farr: fa => makeFArrReader(renv, fa),
        arr: a => makeArrReader(renv, a),
        variant: v => makeVariantReader(renv, v),
        struct: s => makeStructReader(renv, s),
        long: n => fail("Can't read type-level number: " + str(n.n)),
        app: a => makeAppReader(renv, a),
        rec: r => makeRecReader(renv, r),
        abs: a => fail("Can't read type-level function: " + str(a))
    };
    return (new TyCase(readerDisp)).apply(ty);
}

module.exports = {
    addReaderType,
    FREnvelope
}

