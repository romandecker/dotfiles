---
to: <%= cwd %>/<%= projectName %>/.editorconfig
---
<%
const tabWidth = locals.tabWidth || 2;
%>
[*]
end_of_line = lf
charset = utf-8
indent_size = <%= tabWidth %> 
indent_style = space
insert_final_newline = true
trim_trailing_whitespace = true

[*.md]
trim_trailing_whitespace = false