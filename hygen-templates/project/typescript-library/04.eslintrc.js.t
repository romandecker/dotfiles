---
to: <%= projectName %>/.eslintrc.js
---
module.exports = {
    root: true,
    env: {
        es6: true,
    },
    extends: [
        'plugin:@typescript-eslint/eslint-recommended',
        'prettier/@typescript-eslint',
        'plugin:prettier/recommended',
    ],
    parser: '@typescript-eslint/parser',
    parserOptions: {
        project: 'tsconfig.json',
        ecmaVersion: 6,
        sourceType: 'module',
    },

    plugins: ['@typescript-eslint', 'jest'],
    rules: {
    },
};
