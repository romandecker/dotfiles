---
to: <%= projectName %>/.prettierrc.json
---
{
    "printWidth": 100,
    "tabWidth": <%= locals.tabWidth || 4 %>,
    "singleQuote": true,
    "trailingComma": "all",
    "arrowParens": "always"
}
