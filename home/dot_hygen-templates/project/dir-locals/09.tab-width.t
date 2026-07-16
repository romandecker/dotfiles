---
inject: true
to: .dir-locals.el
skip_if: \(tab-width\s*.\s*\d+\)
after: \(\(nil
---
  (tab-width . <%= locals.tabWidth %>)