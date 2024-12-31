mkdir("build");
mkdir("build/classes");
mkdir("build/javadoc");

var name = "scas.application";
dotc("../" + name.replace(".", "/") + "/src", "build/classes");
copy("../" + name.replace(".", "/") + "/resources", "build/classes");
dottydoc("build/classes", "build/javadoc");

mkdir("dist");
var name_rev = name + "_3";
jar("dist/" + name_rev + ".jar", "build/classes", ".*", "manifest.mf");
jar("dist/" + name_rev + "-javadoc.jar", "build/javadoc");

publish("dist")
