---
inject: true
to: .dir-locals.el
skip_if: \(evil-shift-width\s*.\s*\d+\)
after: \(\(nil
---
  (evil-shift-width . <%= locals.tabWidth %>)