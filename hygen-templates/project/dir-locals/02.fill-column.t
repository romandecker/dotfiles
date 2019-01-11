---
inject: true
to: .dir-locals.el
skip_if: \(fill-column\s*.\s*\d+\)
after: \(\(nil
---
  (fill-column . 100)