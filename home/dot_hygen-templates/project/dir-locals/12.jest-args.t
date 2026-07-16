---
inject: true
to: .dir-locals.el
skip_if: \(jest-args.*?\)
after: \(\(nil
---
<%_
const findFile = locals.customHelpers.findUp(cwd);
const jestConfig = findFile(["jest.config.json", "jest.config.js"]);
_%>
<%_ if(jestConfig) { _%>
  (jest-args "--config" "<%= jestConfig %>")
<%_ } _%>