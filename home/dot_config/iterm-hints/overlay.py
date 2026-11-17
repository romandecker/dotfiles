# /// script
# requires-python = ">=3.11"
# dependencies = ["pyobjc-framework-Cocoa"]
# ///
"""Transparent click-through overlay that draws hint badges. Controlled over stdin.

Protocol: one JSON object per line.
  {"frame": [x, y, w, h], "badges": [{"x", "y", "w", "h", "label", "typed"}]}
Coordinates are Cocoa screen points (origin bottom left of the main screen).
Closing stdin or sending {"quit": true} exits.
"""
import json
import sys
import threading

import AppKit
import objc
from Foundation import NSObject

YELLOW = AppKit.NSColor.colorWithCalibratedRed_green_blue_alpha_(1.0, 0.85, 0.2, 0.97)
BORDER = AppKit.NSColor.colorWithCalibratedRed_green_blue_alpha_(0.55, 0.4, 0.0, 1.0)
HILITE = AppKit.NSColor.colorWithCalibratedRed_green_blue_alpha_(1.0, 0.85, 0.2, 0.28)
TEXT = AppKit.NSColor.blackColor()
TYPED = AppKit.NSColor.colorWithCalibratedRed_green_blue_alpha_(0.6, 0.45, 0.1, 1.0)


class HintView(AppKit.NSView):
    badges = objc.ivar()
    origin = objc.ivar()

    def isFlipped(self):
        return False

    def drawRect_(self, _rect):
        font = AppKit.NSFont.boldSystemFontOfSize_(12)
        for b in self.badges or []:
            x, y = b["x"] - self.origin[0], b["y"] - self.origin[1]
            HILITE.setFill()
            AppKit.NSBezierPath.fillRect_(((x, y), (b["w"], b["h"])))
            label, typed = b["label"], b["typed"]
            size = AppKit.NSString.stringWithString_(label).sizeWithAttributes_({AppKit.NSFontAttributeName: font})
            bw, bh = size.width + 6, size.height + 1
            rect = ((x, y + b["h"] - bh + 1), (bw, bh))
            path = AppKit.NSBezierPath.bezierPathWithRoundedRect_xRadius_yRadius_(rect, 3, 3)
            YELLOW.setFill()
            path.fill()
            BORDER.setStroke()
            path.stroke()
            attrs = {AppKit.NSFontAttributeName: font, AppKit.NSForegroundColorAttributeName: TEXT}
            dim = {AppKit.NSFontAttributeName: font, AppKit.NSForegroundColorAttributeName: TYPED}
            pre = AppKit.NSString.stringWithString_(label[: len(typed)])
            rest = AppKit.NSString.stringWithString_(label[len(typed):])
            pre.drawAtPoint_withAttributes_((rect[0][0] + 3, rect[0][1]), dim)
            pw = pre.sizeWithAttributes_(dim).width
            rest.drawAtPoint_withAttributes_((rect[0][0] + 3 + pw, rect[0][1]), attrs)


class Controller(NSObject):
    def initWithWindow_view_(self, window, view):
        self = objc.super(Controller, self).init()
        self.window, self.view = window, view
        return self

    def apply_(self, msg):
        if msg.get("quit"):
            AppKit.NSApp.terminate_(None)
            return
        if "frame" in msg:
            x, y, w, h = msg["frame"]
            self.window.setFrame_display_(((x, y), (w, h)), True)
            self.view.setFrame_(((0, 0), (w, h)))
            self.view.origin = (x, y)
        self.view.badges = msg["badges"]
        self.view.setNeedsDisplay_(True)
        self.window.orderFrontRegardless()
        if not getattr(self, "announced", False):
            self.announced = True
            print("ready", flush=True)  # daemon then hands focus back to iTerm2


def main():
    app = AppKit.NSApplication.sharedApplication()
    app.setActivationPolicy_(AppKit.NSApplicationActivationPolicyAccessory)
    window = AppKit.NSWindow.alloc().initWithContentRect_styleMask_backing_defer_(
        ((0, 0), (10, 10)), AppKit.NSWindowStyleMaskBorderless, AppKit.NSBackingStoreBuffered, False
    )
    window.setOpaque_(False)
    window.setBackgroundColor_(AppKit.NSColor.clearColor())
    window.setIgnoresMouseEvents_(True)
    window.setHasShadow_(False)
    window.setLevel_(AppKit.NSScreenSaverWindowLevel)
    window.setCollectionBehavior_(
        AppKit.NSWindowCollectionBehaviorCanJoinAllSpaces | AppKit.NSWindowCollectionBehaviorFullScreenAuxiliary
    )
    view = HintView.alloc().initWithFrame_(((0, 0), (10, 10)))
    view.badges, view.origin = [], (0, 0)
    window.setContentView_(view)
    ctl = Controller.alloc().initWithWindow_view_(window, view)

    def reader():
        for line in sys.stdin:
            line = line.strip()
            if line:
                ctl.performSelectorOnMainThread_withObject_waitUntilDone_("apply:", json.loads(line), False)
        ctl.performSelectorOnMainThread_withObject_waitUntilDone_("apply:", {"quit": True}, False)

    threading.Thread(target=reader, daemon=True).start()
    app.run()


if __name__ == "__main__":
    main()
