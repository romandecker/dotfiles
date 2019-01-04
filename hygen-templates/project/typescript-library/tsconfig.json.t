---
to: <%= projectName %>/tsconfig.json
---
{
  "compilerOptions": {
    "module": "commonjs",
    "declaration": true,
    "noImplicitAny": true,
    "removeComments": false,
    "noLib": false,
    "emitDecoratorMetadata": true,
    "experimentalDecorators": true,
    "target": "es6",
    "sourceMap": true,
    "outDir": "./lib",
    "rootDir": "./src",
    "skipLibCheck": true,
    "lib": ["es7"]
  },
  "include": ["src/**/*"],
  "exclude": ["node_modules", "**/*.spec.ts"]
}