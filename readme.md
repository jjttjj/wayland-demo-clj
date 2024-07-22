# A minimial clojure Wayland client demo

A demo of a minimal Wayland client in clojure.

I try to minimize C interop and implement the [wire format](https://wayland.freedesktop.org/docs/html/ch04.html) in clojure. Some interop is unavoidable because the Wayland protocol requires that we manage our own memory for what it displays.

# Demo

If you are running wayland (`echo $XDG_SESSION_TYPE` should show "wayland"), running the demo will open a new window that will just show a white box that cycles from zero to full opacity. It will probably be broken in some ways (e.g. not resize properly). You can type "q" in this window to quit, or just kill the clojure process.


```
git clone https://github.com/jjttjj/wayland-demo-clj
cd wayland-demo-clj
clojure -X:demo
```

# Credits

I found this walkthough of writing a minimal client in C very useful
https://github.com/willth7/wayland-client-example
https://www.youtube.com/watch?v=iIVIu7YRdY0

This project seems like another similar demo:
https://github.com/telent/psadan [related blog post](https://ww.telent.net/2013/3/7/still_some_way_land_to_go)
