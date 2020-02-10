mkdir("build");
mkdir("build/classes");

var name = "scas";
dotc(name + "/src", "build/classes");
copy(name + "/resources", "build/classes");

mkdir("dist");
jar("dist/" + name + ".jar", "build/classes", ".*", "manifest.mf");

publish("dist")
