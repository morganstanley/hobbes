//type descriptions
const TYCTOR_PRIM      = 0
const TYCTOR_TVAR      = 2
const TYCTOR_FIXEDARR  = 4
const TYCTOR_ARR       = 5
const TYCTOR_VARIANT   = 6
const TYCTOR_STRUCT    = 7
const TYCTOR_SIZE      = 11
const TYCTOR_TAPP      = 12
const TYCTOR_RECURSIVE = 13
const TYCTOR_TABS      = 15

class Field {
    constructor(name, cid, ty) {
        this.name = name;
        this.cid = cid;
        this.ty = ty;
    }
}

class Prim {
    constructor(name, rep) {
        this.name = name;
        this.rep = rep;
    }

    toString() {
        return this.name == "unit" ? "()" : this.name;
    }

    size() {

    }
}

class Var {
    constructor(name) {
        this.name = name;
    }

    toString() {
        return this.name;
    }

    size() {
        
    }
}

class FixedArr {
    constructor(ty, tlen) {
        this.ty = ty;
        this.tlen = tlen;
    }

    toString() {
        return `[:${this.ty}|${this.tlen}:]`
    }

    size() {
        
    }
}

class Arr {
    constructor(ty) {
        this.ty = ty;
    }

    toString() {
        return `[${this.ty}]`
    }

    size() {
        
    }
}

class Variant {
    constructor(ctors) {
        this.ctors = ctors;
    }

    toString() {
        if(this.ctors.length == 0) {
            return 'void';
        }
        if(this.isSum()) {
            return this.showAsSum();
        }
        return this.showAsVariant();
    }

    size() {
        
    }

    isSum() {
        return this.ctors.length > 0 && this.ctors[0].name[0] == ".";
    }

    showAsSum() {
        let s = "(";
        this.ctors.forEach((e, i) => {
            if(i == 0) {
                s += `${e.ty}`
            }else{
                s += `+${e.ty}`;
            }
        });
        s += ")";
        return s;
    }

    showAsVariant() {
        let s = "|";
        this.ctors.forEach((e, i) => {
            if(i == 0) {
                s += `${this.descCtor(e)}`
            }else{
                s += `, ${this.descCtor(e)}`;
            }
        });
        s += "|";
        return s;
    }

    descCtor(e) {
        return `${e.name}:${e.ty}`
    }
}

class Struct {
    constructor(fields) {
        this.fields = fields;
    }

    toString() {
        if(this.fields.length == 0) {
            return "()";
        };
        if(this.isTuple()){
            return this.showAsTuple();
        }
        return this.showAsStruct();
    }

    size() {
        
    }

    isTuple(){
        return this.fields.length > 0 && this.fields[0].name[0] == '.';
    }

    showAsTuple(){
        let s = '(';
        this.fields.forEach((field, i) => {
            if(i == 0){
                s += `${field.ty}`;
            }else{
                s += `*${field.ty}`;
            }
        });
        s += ')';
        return s;
    }

    showAsStruct(){
        let s = '{';
        this.fields.forEach((field, i) => {
            if(i == 0){
                s += this.descField(field);
            }else{
                s += `, ${this.descField(field)}`;
            }
        });
        s += '}';
        return s;
    }

    descField(field) {
        return `${field.name}:${field.ty}` ;
    }
}


class TLong {
    constructor(n) {
        this.n = n;
    }

    toString() {
        return `${this.n}`;
    }

    size() {
        
    }
}


class App {
    constructor(f, args) {
        this.f = f;
        this.args = args;
    }

    toString() {
        if(this.f instanceof Prim){
            if(this.f.name == "fileref" && this.args.length > 0){
                return `${this.args[0]}@?`;
            }
            return this.showGeneric();
        }
    }

    size() {
        
    }

    showGeneric() {
        let s =  `${this.f}(`; 
        this.args.forEach((e, i) => {
            if(i == 0) {
                s += `${e}`;
            }else{
                s += `, ${e}`;
            }
        });
        s += ")";
        return s;       
    }
}


class Recursive {
    constructor(vn, ty) {
        this.vn = vn;
        this.ty = ty;
    }

    toString() {
        return `^${this.vn}.${this.ty}`;
    }

    size() {
        
    }
}


class Abs {
    constructor(vns, ty) {
        this.vns = vns;
        this.ty = ty;
    }

    toString() {
        let s = '\\';
        this.vns.forEach((e, i) => {
            if(i == 0) {
                s += e
            }else{
                s += `, ${e}`;
            }
        });
        s += `.${this.ty}`;
        return s;
    }

    size() {
        
    }
}

class TyCase {
    constructor(dtors) {
        this.dtors = dtors;
    }

    apply(ty, ...args) {
        if(ty instanceof Prim) {
            return this.dtors['prim'](ty, ...args);
        }

        if(ty instanceof Var) {
            return this.dtors['var'](ty, ...args);
        }

        if(ty instanceof FixedArr) {
            return this.dtors['farr'](ty, ...args);
        }

        if(ty instanceof Arr) {
            return this.dtors['arr'](ty, ...args);
        }

        if(ty instanceof Variant) {
            return this.dtors['variant'](ty, ...args);
        }

        if(ty instanceof Struct) {
            return this.dtors['struct'](ty, ...args);
        }

        if(ty instanceof TLong) {
            return this.dtors['long'](ty, ...args);
        }

        if(ty instanceof App) {
            return this.dtors['app'](ty, ...args);
        }

        if(ty instanceof Recursive) {
            return this.dtors['rec'](ty, ...args);
        }

        if(ty instanceof Abs) {
            return this.dtors['abs'](ty, ...args);
        }
    }
}

module.exports = {
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
}