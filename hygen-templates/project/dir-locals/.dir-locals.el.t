---
to: .dir-locals.el
---
<%
const which = locals.customHelpers.which(cwd);
const findFile = locals.customHelpers.findUp(cwd);

const prettierExecutable = which('prettier');
const prettierConfig = findFile([".prettierrc.json", ".prettierrc"]);

const tslintExecutable = which('tslint');
const tsConfig = findFile('tsconfig.json');
%>
((nil
  (fill-column . 100)
  <%_ if( features.includes('jsx') ) { _%>
  (eval add-to-list 'auto-mode-alist
        '("\\.jsx\\'" . rjsx-mode))
  <%_ } _%>
  <%_ if( features.includes('js') ) { _%>
  (eval add-to-list 'auto-mode-alist
        '("\\.js\\'" . rjsx-mode))
  <%_ } _%>
  <%_ if(prettierConfig) { _%>
  (prettier-args "--config" "<%= prettierConfig %>")
  <%_ } _%>
  <%_ if(prettierExecutable) { _%>
  (prettier-executable "<%= prettierExecutable %>")
  <%_ } _%>
  <%_ if(tslintExecutable) { _%>
  (prettier-executable "<%= tslintExecutable %>")
  <%_ } _%>
  <%_ if(tsConfig) { _%>
  (flycheck-typescript-tslint-executable . ("--project" "<%= tslintExecutable %>"))
  (flycheck-tslint-args . ("--project" "<%= tsConfig %>"))
  <%_ } _%>
  ))