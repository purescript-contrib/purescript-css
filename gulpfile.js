var mandragora = require("mandragora-bucket");

mandragora.define({
    paths: {
        bower: [
            "bower_components/purescript-*/src/**/*.purs",
            "bower_components/purescript-*/purescript-*/src/**/*.purs"
        ]
    }
});
