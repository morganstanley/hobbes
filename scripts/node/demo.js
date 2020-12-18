const fregion = require('./fregion')
const path = require('path')

function test() {
    let env = new fregion.FREnvelope(path.join(__dirname, "..", "demo.log"));
    env.load(e => {
        let vs = e.getStream("vs");
        let r = vs.load();
        console.log(`${r}`);

        let vss = e.getStream("vss");
        r = vss.load();
        console.log(r);
        //console.log(`${vss.ty}`);
        //let temp = evalApp(vss.ty.f, vss.ty.args);
        //console.log(`${temp}`);
    }, e => {
        console.log("Failed");
    })
}

test();