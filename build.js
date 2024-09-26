mkdir("build");
mkdir("build/classes");
mkdir("build/sources");
mkdir("build/javadoc");

var name = "scas";
dotc(name + "/src", "build/classes");
copy(name + "/resources", "build/classes");
copy(name + "/src", "build/sources");
copy(name + "/resources", "build/sources");
dottydoc("build/classes", "build/javadoc");

mkdir("dist");
var name_rev = name + "_3";
jar("dist/" + name_rev + ".jar", "build/classes", ".*", "manifest.mf");
jar("dist/" + name_rev + "-source.jar", "build/sources");
jar("dist/" + name_rev + "-javadoc.jar", "build/javadoc");
cp("pom.xml", "dist/" + name_rev + ".pom")

publish("dist")
