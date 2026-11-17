"""Pure logic for iterm-hints: config, matchers, hint labels. No iTerm2 or AppKit imports."""
from __future__ import annotations

import os
import re
import tomllib
from dataclasses import dataclass, field
from pathlib import Path

CONFIG_PATH = Path(os.environ.get("ITERM_HINTS_CONFIG", "~/.config/iterm-hints/config.toml")).expanduser()

DEFAULT_CONFIG = {
    "activation": "ctrl+opt+f",
    "alphabet": "asdfghjkl",
    "jira_base_url": "https://zooplus.atlassian.net/browse/",
    # Command for file targets. {path}, {line}, {col} are filled in. No shell is used.
    "open_file_command": "open {path}",
    "url_command": "open {url}",
    # Pixel margins between pane edge and the text grid. Tune if badges look shifted.
    "margin_x": 5.0,
    "margin_y": 2.0,
    "matchers": [
        {
            "name": "url",
            "regex": r"https?://[^\s<>\"'`)\]}]+[^\s<>\"'`)\]}.,;:!?]",
            "type": "url",
            "action": "{0}",
        },
        {
            "name": "jira",
            "regex": r"\b([A-Z][A-Z0-9]+-\d+)\b",
            "type": "url",
            "action": "{jira_base_url}{1}",
        },
        {
            "name": "file",
            "regex": (
                r"(?<![\w/.~:@-])"
                r"((?:~/|\.{1,2}/|/)?(?:[\w.@+-]+/)+[\w.@+-]+|\.{1,2}/[\w.@+-]+)"
                r"(?::(\d+))?(?::(\d+))?"
            ),
            "type": "file",
            "path_group": 1,
            "line_group": 2,
            "col_group": 3,
            "must_exist": True,
        },
    ],
}


@dataclass
class Matcher:
    name: str
    regex: re.Pattern
    type: str
    action: str = "{0}"
    path_group: int = 1
    line_group: int = 2
    col_group: int = 3
    must_exist: bool = False


@dataclass
class Target:
    kind: str  # "url" or "file"
    start: int  # offset in logical text
    end: int
    text: str
    url: str | None = None
    path: str | None = None
    line: str | None = None
    col: str | None = None
    matcher: str = ""
    label: str = ""


@dataclass
class Config:
    activation: str
    alphabet: str
    jira_base_url: str
    open_file_command: str
    url_command: str
    margin_x: float
    margin_y: float
    matchers: list[Matcher] = field(default_factory=list)


def load_config(path: Path | str = CONFIG_PATH) -> Config:
    path = Path(path)
    raw = dict(DEFAULT_CONFIG)
    if path.exists():
        with path.open("rb") as f:
            raw.update(tomllib.load(f))
    matchers = [
        Matcher(
            name=m.get("name", m["regex"]),
            regex=re.compile(m["regex"]),
            type=m.get("type", "url"),
            action=m.get("action", "{0}"),
            path_group=m.get("path_group", 1),
            line_group=m.get("line_group", 2),
            col_group=m.get("col_group", 3),
            must_exist=m.get("must_exist", False),
        )
        for m in raw["matchers"]
    ]
    return Config(
        activation=raw["activation"],
        alphabet=raw["alphabet"].upper(),
        jira_base_url=raw["jira_base_url"],
        open_file_command=raw["open_file_command"],
        url_command=raw["url_command"],
        margin_x=float(raw["margin_x"]),
        margin_y=float(raw["margin_y"]),
        matchers=matchers,
    )


def resolve_path(path: str, cwd: str | None) -> str:
    p = os.path.expanduser(path)
    if not os.path.isabs(p):
        p = os.path.join(cwd or os.getcwd(), p)
    return os.path.normpath(p)


def find_targets(text: str, cfg: Config, cwd: str | None = None) -> list[Target]:
    """Find targets in text. Earlier matchers win on overlap."""
    taken: list[tuple[int, int]] = []
    found: list[Target] = []
    for m in cfg.matchers:
        for hit in m.regex.finditer(text):
            span = hit.span()
            if any(span[0] < e and s < span[1] for s, e in taken):
                continue
            t = _build_target(m, hit, cfg, cwd)
            if t is None:
                continue
            taken.append(span)
            found.append(t)
    found.sort(key=lambda t: t.start)
    return found


def _group(hit: re.Match, n: int) -> str | None:
    try:
        return hit.group(n)
    except IndexError:
        return None


def _build_target(m: Matcher, hit: re.Match, cfg: Config, cwd: str | None) -> Target | None:
    t = Target(kind=m.type, start=hit.start(), end=hit.end(), text=hit.group(0), matcher=m.name)
    if m.type == "file":
        raw = _group(hit, m.path_group)
        if not raw:
            return None
        path = resolve_path(raw, cwd)
        if m.must_exist and not os.path.exists(path):
            return None
        t.path, t.line, t.col = path, _group(hit, m.line_group), _group(hit, m.col_group)
    else:
        groups = [hit.group(i) or "" for i in range(hit.re.groups + 1)]
        t.url = m.action.format(*groups, jira_base_url=cfg.jira_base_url)
    return t


def make_hints(count: int, alphabet: str) -> list[str]:
    """Shortest prefix-free labels (Vimium style): no label is a prefix of another."""
    if count <= 0:
        return []
    labels = [""]
    while len(labels) < count or labels == [""]:
        head = labels.pop(0)
        labels.extend(head + c for c in alphabet)
    return labels[:count]
