---
to: <%= cwd %>/<%= projectName %>/.dir-locals.el %>
---
<% const rootDir = `${cwd}/${projectName}`; %>
;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")
((nil
     (fill-column . 100)
     (create-lockfiles nil)
     (prettier-args "--config" "<%= rootDir %>/.prettierrc.json"))
    (typescript-mode
     (flycheck-typescript-tslint-executable . "<%= rootDir %>/node_modules/.bin/tslint")
     (flycheck-tslint-args . ("--project" "<%= rootDir %>/tsconfig.json"))))
