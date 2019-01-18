---
inject: true
to: <%= projectName %>/package.json
after: '"scripts": {'
skip_if: '"export": "spectacle-renderer",'
---
    "export": "spectacle-renderer",