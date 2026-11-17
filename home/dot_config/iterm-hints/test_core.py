import os
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(__file__))
from hints_core import find_targets, load_config, make_hints  # noqa: E402


class HintTests(unittest.TestCase):
    def test_prefix_free_and_unique(self):
        for n in (1, 2, 9, 10, 30, 81, 82, 200):
            h = make_hints(n, "ASDFGHJKL")
            self.assertEqual(len(h), n)
            self.assertEqual(len(set(h)), n)
            for a in h:
                for b in h:
                    if a != b:
                        self.assertFalse(b.startswith(a), (n, a, b))

    def test_short(self):
        self.assertEqual(max(map(len, make_hints(9, "ASDFGHJKL"))), 1)
        self.assertEqual(max(map(len, make_hints(10, "ASDFGHJKL"))), 2)


class MatchTests(unittest.TestCase):
    def setUp(self):
        self.cfg = load_config(os.path.join(tempfile.gettempdir(), "nonexistent.toml"))
        self.dir = tempfile.mkdtemp()
        os.makedirs(os.path.join(self.dir, "src"))
        open(os.path.join(self.dir, "src", "foo.ts"), "w").close()

    def find(self, text):
        return find_targets(text, self.cfg, self.dir)

    def test_url(self):
        (t,) = self.find("see https://example.com/a/b?x=1, ok")
        self.assertEqual(t.url, "https://example.com/a/b?x=1")

    def test_url_not_split_into_file(self):
        self.assertEqual(len(self.find("https://example.com/a/b.ts")), 1)

    def test_jira(self):
        (t,) = self.find("fix FP-1234 now")
        self.assertEqual(t.url, "https://zooplus.atlassian.net/browse/FP-1234")

    def test_file_variants(self):
        for s, line, col in [("src/foo.ts", None, None), ("./src/foo.ts", None, None),
                             ("src/foo.ts:123", "123", None), ("src/foo.ts:123:45", "123", "45")]:
            (t,) = self.find(f"at {s} here")
            self.assertEqual((t.kind, t.line, t.col), ("file", line, col), s)
            self.assertEqual(t.path, os.path.join(self.dir, "src", "foo.ts"))

    def test_missing_file_ignored(self):
        self.assertEqual(self.find("and/or nope/missing.ts"), [])


if __name__ == "__main__":
    unittest.main()
