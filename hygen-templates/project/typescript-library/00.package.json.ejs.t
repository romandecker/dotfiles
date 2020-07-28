---
message: "Generated package.json and added devDependencies"
to: <%= projectName %>/package.json
sh: >
      cd <%= cwd %>/<%= projectName %> &&
      npm install --save-dev typescript \
                             prettier \
                             jest \
                             ts-jest \
                             ts-node \
                             eslint \
                             @typescript-eslint/parser \
                             @typescript-eslint/eslint-plugin \
                             eslint-config-prettier \
                             eslint-plugin-prettier \
                             eslint-plugin-jest \
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
    "clean": "rimraf dist/",
    "test": "echo \"Error: no test specified\" && exit 1",
    "lint": "eslint src/**/*.{ts,tsx,js,jsx}"
  },
  "author": "<%= locals.author %>",
  "license": "<%= locals.license %>"
}
