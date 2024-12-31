mkdir("build");
mkdir("build/classes");
mkdir("build/sources");
mkdir("build/javadoc");

var name = "scas.application";
dotc("../" + name.replace(".", "/") + "/src", "build/classes");
copy("../" + name.replace(".", "/") + "/resources", "build/classes");
copy("../" + name.replace(".", "/") + "/src", "build/sources");
copy("../" + name.replace(".", "/") + "/resources", "build/sources");
dottydoc("build/classes", "build/javadoc");

mkdir("dist");
var name_rev = name + "_3";
jar("dist/" + name_rev + ".jar", "build/classes", ".*", "manifest.mf");
jar("dist/" + name_rev + "-source.jar", "build/sources");
jar("dist/" + name_rev + "-javadoc.jar", "build/javadoc");
cp("pom.xml", "dist/" + name_rev + ".pom")

publish("dist")
