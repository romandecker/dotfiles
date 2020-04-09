---
to: <%= cwd %>/<%= projectName %>/.dir-locals.el
---
<%
const rootDir = `${cwd}/${projectName}`;
const tabWidth = locals.tabWidth || 2;
%>
;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")
((nil
  (fill-column . 100)
  (create-lockfiles nil)
  (prettier-command . "<%= rootDir %>/node_modules/.bin/prettier")
  (prettier-args "--config" "<%= rootDir %>/.prettierrc.json")
  (evil-shift-width . <%= tabWidth %>)
  (js-indent-level . <%= tabWidth %>)
  (js2-basic-offset . <%= tabWidth %>)
  (tab-width . <%= tabWidth %>)
  (eval puthash
        'prettier
        "<%= rootDir %>/node_modules/.bin/prettier"
        format-all--executable-table))
 (typescript-mode
  (flycheck-typescript-tslint-executable . "<%= rootDir %>/node_modules/.bin/tslint")
  (flycheck-javascript-eslint-executable . "<%= rootDir %>/node_modules/.bin/eslint.js")
  (flycheck-tslint-args . ("--project" "<%= rootDir %>/tsconfig.json"))
  (typescript-indent-level . <%= tabWidth %>)))
