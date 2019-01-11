---
inject: true
to: .dir-locals.el
skip_if: \(js-indent-level\s*.\s*\d+\)
after: \(\(nil
---
  (js-indent-level . <%= locals.tabWidth %>)