#!/usr/bin/env node

const fs = require("fs");

const PPX = "re-formality-ppx";

let arch = process.arch;
let platform = process.platform;

if (arch === "ia32") {
    arch = "x86";
}

if (platform === "win32") {
    platform = "win";
}

const filename = `bin/${PPX}-${platform}-${arch}.exe`;

const supported = fs.existsSync(filename);

if (!supported) {
    console.error(`${PPX} does not support this platform :(`);
    console.error("");
    console.error(`${PPX} comes prepacked as built binaries to avoid large`);
    console.error("dependencies at build-time.");
    console.error("");
    console.error(`If you want ${PPX} to support this platform natively,`);
    console.error("please open an issue at our repository, linked above. Please");
    console.error(`specify that you are on the ${platform} platform,`);
    console.error(`on the ${arch} architecture.`);
}

if (!fs.existsSync("ppx.exe")) {
    copyFileSync(filename, "ppx.exe");
    fs.chmodSync("ppx.exe", 0755);
}

if (!fs.existsSync("ppx")) {
    copyFileSync(filename, "ppx");
    fs.chmodSync("ppx", 0755);
}

function copyFileSync(source, dest) {
    if (typeof fs.copyFileSync === "function") {
        fs.copyFileSync(source, dest);
    } else {
        fs.writeFileSync(dest, fs.readFileSync(source));
    }
}
