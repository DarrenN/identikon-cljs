# identikon-cljs

Quick and dirty port of a subset of my Racket [Identikon](https://github.com/DarrenN/identikon) to ClojureScript for use in the browser.

**Very much in an initial proof of concept phase**

## Setup

To get an interactive development environment run:

    lein figwheel

and open your browser at [localhost:3449](http://localhost:3449/).
This will auto compile and send all changes to the browser without the
need to reload. After the compilation process is complete, you will
get a Browser Connected REPL. An easy way to try it is:

    (require '[identikon-cljs.core :as identikon] :reload)
    (identikon/make-identikon "#idk0" 200 200 "identikon")

and you should see an identikon appear in the browser window:

![identikon](https://dl.dropbox.com/s/i9rk5vzxio44fnh/Screenshot%202016-02-21%2017.56.51.png)

From JavaScript you can import `/resources/public/js/compiled/identikon_cljs.js` and then use it like so:

```javascript
    identikon_cljs.core.make_identikon("#idk0", 300, 300, "identikon");
    identikon_cljs.core.make_identikon("p.idkp", 60, 60, "small identikons");
```

To clean all compiled files:

    lein clean

To create a production build run:

    lein do clean, cljsbuild once min

And open your browser in `resources/public/index.html`. You will not
get live reloading, nor a REPL.

## License

Copyright © 2016 Darren Newton

Distributed under the Eclipse Public License either version 1.0.
