mkdir("build");
mkdir("build/classes");

var name = "scas";
dotc(name + "/src", "build/classes", ["-language:strictEquality"]);
dotc(name + "/application/src", "build/classes", ["-language:strictEquality"]);
copy(name + "/application/resources", "build/classes");
