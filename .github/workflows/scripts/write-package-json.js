const fs = require("fs");
const path = require("path");

const { name, version, description, author, license, repository, keywords } = require("../../../package.json");
const { dependencies } = require("../../../lib/package.json");

const packageJson = JSON.stringify(
    {
        name,
        version,
        description,
        author,
        license,
        repository,
        keywords,
        dependencies,
        files: ["src", "bin", "rescript.json", "postinstall.js"],
        scripts: {
            postinstall: "node ./postinstall.js",
        },
    },
    null,
    2,
);

fs.writeFileSync(path.join(__dirname, "..", "..", "..", "_release", "package.json"), packageJson, { encoding: "utf8" });
