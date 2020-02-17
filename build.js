mkdir("build");
mkdir("build/classes");

var name = "scas";
dotc(name + "/src", "build/classes", ["-language:implicitConversions"]);
dotc(name + "/application/src", "build/classes", ["-language:implicitConversions"]);
copy(name + "/application/resources", "build/classes");

println("done.")
