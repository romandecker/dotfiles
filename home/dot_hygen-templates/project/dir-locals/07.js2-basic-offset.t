---
inject: true
to: .dir-locals.el
skip_if: \(js2-basic-offset\s*.\s*\d+\)
after: \(\(nil
---
  (js2-basic-offset . <%= locals.tabWidth %>)