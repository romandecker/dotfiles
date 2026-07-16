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
  (evil-shift-width . <%= tabWidth %>)
  (js-indent-level . <%= tabWidth %>)
  (js2-basic-offset . <%= tabWidth %>)
  (tab-width . <%= tabWidth %>)
  (eval puthash
        'prettier
        "<%= rootDir %>/node_modules/.bin/prettier"
        format-all--executable-table))
 (typescript-mode
  (flycheck-javascript-eslint-executable . "<%= rootDir %>/node_modules/.bin/eslint.js")
  (typescript-indent-level . <%= tabWidth %>)))
