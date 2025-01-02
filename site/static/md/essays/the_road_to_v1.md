# The Road to V1

TL;DR Stop over complicating your life. Use Datastar.

It's been a long time since I wrote an essay on the site. Part of the reason is that the project overall was done in my head, and was "good enough" for my needs. A while back, Carson Gross prompted me to give a talk at [Utah.js](https://www.youtube.com/watch?v=0K71AyAF6E4&t=8s&pp=ygUPdXRhaC5qcyBkZWxhbmV5), and I later appeared as a guest on the [HX-Podcast](https://www.youtube.com/watch?v=HbTFlUqELVc&t=36s&pp=ygUcaHgtcG9kIGRlbGFuZXkgaHRteCBnaWxsaWxhbg%3D%3D) to talk about why everyone has been doing web development wrong – especially me. I put out a call for help before calling it V1, and man, the response was overwhelming.

That was all before the PHP nation attacked.

There have been people that come in with their own itch and want changes to Datastar, which makes perfect sense. But all of a sudden, I'm getting PHP devs really interested, including an early contributor to HTMX. I don't think of PHP as a real-time language, so if Datastar works for their needs as well as my real-time focused project, I must be doing something right.

It's been a maelstrom of activity the last month or so. The entire codebase has been rewritten twice. That might seem silly, but it's all about what assumptions you're making. It's no surprise that I dislike TypeScript and loathe JavaScript, but they are the only game in town (WASM doesn't count yet). Through the rewrites, I've gone from a prisoner of the constraints to trying to thrive in them. It's a very weird feeling, but I'm actually proud of the codebase now. Looking at it, it seems obvious in a way that only hindsight can provide.

Some of the highlights of the changes since the last essay:

- The core `engine.ts` is now 266 lines, including more checks and features than before. It's not built to be code golf but to be readable and maintainable.
- The ES6 module was 17.14KiB, and it's currently 12KiB. That's over 30% smaller, with more features and more checks. Some of this is due to not using giant strings for errors, but a lot is down to better type checking and handling. My original thesis was that TypeScript would make the codebase smaller and more maintainable. I was right – it's made a huge difference.
- We've removed all the magic. Prior iterations had `$` and `$$` functions that would do magic things. Now it's all explicit. It's a little more verbose, but it's a lot more maintainable. Maybe it's because I'm a Go developer, but I'll take some verbosity for long-term readability and maintainability.
- There are extensions available to make it less typing while still being explicit. I'll probably do a whole essay on just function constructors and the meta-magic you have to do to make them work.
- Official SDKs. One of the flaws in my initial getting started guide was, I wanted to show there is no magic. You started from raw HTTP response and built up to using Datastar. While valuable, it's intimidating. Now there are SDKs for the major languages that will get you up and running in minutes. You learn the "Datastar way" and can transfer to other languages easily.
- Consistency is a whole lot better. Converting my older Datastar projects to the new version was a breeze. In general, it's a lot easier to reason about the codebase.
- Better documentation. Ben Croker has been a huge help in getting the documentation up to snuff. It's still not perfect, but it's a lot better than it was.
- Built-in bundler. All the plugins in Datastar are to scratch my own itch. If you don't need the same tools you can make Datastar even smaller. It's using esbuild as a Go library and is built right into the site. Make your choice and get a custom build of Datastar.

The crazy part to me at this point is trying to understand why this hasn't already been done in other projects. I'm not a JS wizard like Evan You, Ryan Carniato, or Rich Harris. I'm just a guy that felt like the way we make front end was ***WAY*** overcomplicated. The more I work with this codebase, the more I think we all threw the baby out with the bathwater when it comes to hypermedia. Put state in the right place, and it's a lot easier to reason about.

Datastar originally was my attempt to combine the parts of HTMX and Alpine I liked theoretically, but not in practice. For me, it was a library that allowed you to make your own framework. At this point, I think it's a lot more than that. It's changed the way I think about front-end development as a whole. I'm very confident that it can do ***ANYTHING*** that React, Vue, or Svelte can do, faster and with less code. I'll take on anyone that disagrees; it's the model that makes the browser do what it's best at. It's fast, robust, and easy. There is still performance work that can be done, but the overall approach is solid.

Now that the rewrite is done, I can't wait to get back to boring. In my experience as the person with the most Datastar code out there, it's pretty awesome that actual Datastar work is <5% of any project. You just hook up a few bindings and events and get back to the real work. I'm excited to get back to making things that matter instead of fighting with the tools.
