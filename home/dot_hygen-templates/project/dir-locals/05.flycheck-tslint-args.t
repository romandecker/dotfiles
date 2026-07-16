---
inject: true
to: .dir-locals.el
skip_if: \(flycheck-tslint-args.*?\)
after: \(\(nil
---
<%_
const findFile = locals.customHelpers.findUp(cwd);
const tsConfig = findFile('tsconfig.json');
_%>
<%_ if(tsConfig) { _%>
  (flycheck-tslint-args . ("--project" "<%= tsConfig %>"))
<%_ } _%>