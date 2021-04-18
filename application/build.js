mkdir("build");
mkdir("build/classes");

var name = "scas.application";
dotc("../" + name.replace(".", "/") + "/src", "build/classes");

mkdir("dist");
var name_rev = name + "_3.0.1-RC1";
jar("dist/" + name_rev + ".jar", "build/classes", ".*", "manifest.mf");

publish("dist")
