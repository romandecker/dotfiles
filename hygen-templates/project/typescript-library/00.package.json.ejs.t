---
message: "Generated package.json and added devDependencies"
to: <%= projectName %>/package.json
sh: >
      cd <%= cwd %>/<%= projectName %> &&
      yarn add -D typescript \
                  prettier \
                  jest \
                  ts-jest \
                  ts-node \
                  tslint \
                  tslint-config-prettier \
                  npm-run-all \
                  rimraf
---
{
  "name": "<%= projectName %>",
  "version": "1.0.0",
  "description": "<%= locals.description %>",
  "main": "index.js",
  <%_ if( locals.repository ) { _%>
  "repository": "<%= locals.repository %>",
  <%_ } _%>
  "scripts": {
    "compile": "tsc -p tsconfig.json",
    "build": "run-s clean compile",
    "clean": "rimraf lib/",
    "test": "echo \"Error: no test specified\" && exit 1"
  },
  "author": "<%= locals.author %>",
  "license": "<%= locals.license %>"
}