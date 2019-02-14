---
message: "Generated typescript project configuration"
to: <%= projectName %>/tsconfig.json
---
{
  "compilerOptions": {
    "module": "commonjs",
    "declaration": true,
    "strict": true,
    "removeComments": false,
    "noLib": false,
    "emitDecoratorMetadata": true,
    "experimentalDecorators": true,
    "target": "es6",
    "sourceMap": true,
    "outDir": "./lib",
    "rootDir": "./src",
    "skipLibCheck": true,
    "lib": [
      "es7",
      <%_ if( locals.targets.includes('browser') ) { _%>
      "dom",
      <%_ } _%>
  ]
  },
  "include": ["src/**/*"],
  "exclude": ["node_modules", "**/*.spec.ts"]
}