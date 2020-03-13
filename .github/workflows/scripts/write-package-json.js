const fs = require("fs");
const path = require("path");

const lib = require("../../../lib/package.json");

const packageJson = JSON.stringify(
  {
    name: lib.name,
    version: lib.version,
    description: lib.description,
    author: lib.author,
    license: lib.license,
    repository: lib.repository,
    files: lib.files,
    dependencies: lib.dependencies,
    keywords: lib.keywords,
    scripts: {
      postinstall: "node ./postinstall.js"
    }
  },
  null,
  2
);

fs.writeFileSync(
  path.join(__dirname, "..", "..", "..", "_release", "package.json"),
  packageJson,
  { encoding: "utf8" }
);
