---
inject: true
to: .dir-locals.el
skip_if: \(jest-executable.*?\)
after: \(\(nil
---
<%_
const which = locals.customHelpers.which(cwd);
const jestExecutable = which('jest');
_%>
<%_ if(jestExecutable) { _%>
  (jest-executable "<%= jestExecutable %>")
<%_ } _%>