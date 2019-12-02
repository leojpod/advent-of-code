#!/usr/bin/env node
const program = require("./elm.js").Elm.Main.init({
  flags: { argv: process.argv, versionMessage: "0.1" }
});

program.ports.print.subscribe(message => console.log(message));
program.ports.printAndExitFailure.subscribe(message => {
  console.log(message);
  process.exit(1);
});
program.ports.printAndExitSuccess.subscribe(message => {
  console.log(message);
  process.exit(0);
});
