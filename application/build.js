mkdir("build");
mkdir("build/classes");

var name = "scas.application";
dotc("../" + name.replace(".", "/") + "/src", "build/classes", ["-language:implicitConversions"]);

mkdir("dist");
var name_rev = name + "_0.24";
jar("dist/" + name_rev + ".jar", "build/classes", ".*", "manifest.mf");

publish("dist")
