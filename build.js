mkdir("build");
mkdir("build/classes");

var name = "scas";
dotc(name + "/src", "build/classes", ["-language:strictEquality,implicitConversions"]);
dotc(name + "/application/src", "build/classes", ["-language:strictEquality,implicitConversions"]);
copy(name + "/application/resources", "build/classes");
