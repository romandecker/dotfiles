---
inject: true
to: .dir-locals.el
skip_if: \(prettier-executable.*?\)
after: \(\(nil
---
<%_
const which = locals.customHelpers.which(cwd);
const prettierExecutable = which('prettier');
_%>
<%_ if(prettierExecutable) { _%>
  (prettier-executable "<%= prettierExecutable %>")
<%_ } _%>