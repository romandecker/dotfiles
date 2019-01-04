---
to: <%= projectName %>/package.json
sh: "cd <%= cwd %>/<%= projectName %> && yarn add -D typescript prettier jest ts-jest ts-node tslint tslint-config-prettier"
---
{
  "name": "<%= projectName %>",
  "version": "1.0.0",
  "description": "<%= locals.description %>",
  "main": "index.js",
  "scripts": {
    "test": "echo \"Error: no test specified\" && exit 1"
  },
  "author": "<%= locals.author %>",
  "license": "<%= locals.license %>"
}