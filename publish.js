mkdir("build/sources");

var name = "scas";
copy(name + "/src", "build/sources");
copy(name + "/application/src", "build/sources");
copy(name + "/application/resources", "build/sources");

mkdir("dist");
var name_rev = name + "_0.23";
jar("dist/" + name_rev + ".jar", "build/classes", ".*", "manifest.mf");
jar("dist/" + name_rev + "-source.jar", "build/sources");
cp("pom.xml", "dist/" + name_rev + ".pom")

publish("dist")
