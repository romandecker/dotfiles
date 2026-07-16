---
inject: true
to: .dir-locals.el
skip_if: \(flycheck-typescript-tslint-executable.*?\)
after: \(\(nil
---
<%_
const which = locals.customHelpers.which(cwd);
const tslintExecutable = which('tslint');
_%>
<%_ if(tslintExecutable) { _%>
  (flycheck-typescript-tslint-executable . ("--project" "<%= tslintExecutable %>"))
<%_ } _%>