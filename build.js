mkdir("build");
mkdir("build/classes");
mkdir("build/sources");

var name = "scas";
dotc(name + "/src", "build/classes", ["-language:implicitConversions"]);
copy(name + "/resources", "build/classes");
copy(name + "/src", "build/sources");
copy(name + "/resources", "build/sources");

mkdir("dist");
var name_rev = name + "_0.25";
jar("dist/" + name_rev + ".jar", "build/classes", ".*", "manifest.mf");
jar("dist/" + name_rev + "-source.jar", "build/sources");
cp("pom.xml", "dist/" + name_rev + ".pom")

publish("dist")
