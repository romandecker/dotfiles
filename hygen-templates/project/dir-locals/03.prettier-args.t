---
inject: true
to: .dir-locals.el
skip_if: \(prettier-args.*?\)
after: \(\(nil
---
<%_
const findFile = locals.customHelpers.findUp(cwd);
const prettierConfig = findFile([".prettierrc.json", ".prettierrc"]);
_%>
<%_ if(prettierConfig) { _%>
  (prettier-args "--config" "<%= prettierConfig %>")
<%_ } _%>