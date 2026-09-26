# okite

*Okite* (掟) is Japanese for the rules a group lives by. This library holds a
Common Lisp program to two kinds:

- **Layers.** Which files of a
  [package-inferred system](https://asdf.common-lisp.dev/asdf/The-package_002dinferred_002dsystem-extension.html)
  may depend on which: each file in a layer, each layer depending only on the
  layers it is allowed to, each library used only where it belongs.
- **Interfaces.** That every generic function a set of packages declares has a
  method, once the code that implements them is loaded.

## Layers

In a package-inferred system every file is a system, and what it depends on is
what its `defpackage` uses and imports from. ASDF reads that from the first form
of the file, so okite checks the layers from ASDF alone: nothing is compiled or
loaded.

```lisp
(okite:define-layers my-app
  (:layers (:domain   "domain/")
           (:usecases "usecases/")
           (:infra    "infra/")
           (:web      "web/")
           (:main     "main"))
  (:allow (:usecases :domain)
          (:infra    :domain :usecases)
          (:web      :domain :usecases)
          (:main     :domain :usecases :infra :web))
  (:libraries ("dbi" :infra)
              ("clack" :web :main))
  (:forbid "my-app-client"))
```

The name, `my-app`, is what the layers are asked for by, and downcased it is
the system whose files are checked. `(:system "name")` names the system when it
is not the same: two definitions for one system, or a name of your own choosing.

| Clause | |
|---|---|
| `(:system "name")` | The system whose files are checked, when it is not the name downcased. |
| `(:layers (layer pattern ...) ...)` | Where each file belongs. A pattern ending in `/` covers every file under that directory, any other pattern the one file; both are relative to the system's `:pathname`, without `.lisp`. The longest pattern that matches a file wins, so `"usecases/ports/"` can be a layer of its own inside `"usecases/"`. A file no pattern matches is a violation. |
| `(:allow (layer layer ...) ...)` | What the first layer may depend on besides itself. Anything else is a violation. |
| `(:libraries ("name" layer ...) ...)` | A system outside this one and the layers that may use it. The name covers its subsystems and extensions: `"lack"` covers `lack/request` and `lack-middleware-session`. When more than one name covers a dependency, the longest wins. A library not listed may be used anywhere. |
| `(:forbid "name" ...)` | Systems no file may use, with their subsystems. |
| `(:ignore pattern ...)` | Files under the system's `:pathname` that are not part of it — scripts, fixtures — as patterns like those of `:layers`. Directories whose name starts with a dot (`.qlot`, `.git`) are never looked in. |

A dependency on the system itself (a file that imports `my-app`) counts as one
on its `main` file, which package-inferred systems conventionally nickname after
the system. Packages ASDF provides without a system — `common-lisp`, `uiop`,
`asdf` — are not dependencies as ASDF sees them, so no layer can be kept from
them.

```lisp
(okite:layer-violations 'my-app)
;; => (#<OKITE/LAYERS:VIOLATION my-app/infra/db (infra) uses my-app/web/page (web)>)

(okite:ensure-layers 'my-app)   ; signals okite:violation-error listing every violation
```

A violation prints as a sentence with `princ`, and its kind (`:unplaced`,
`:layer`, `:library` or `:forbidden`), file, layer, dependency and the
dependency's layer are readable with `violation-kind` and the other readers. In a
test, with [rove](https://github.com/fukamachi/rove):

```lisp
(deftest layers
  (let ((violations (okite:layer-violations 'my-app)))
    (ok (null violations) (format nil "~{~a~^~%~}" violations))))
```

## Interfaces

Ports and adapters in Common Lisp: a package of generic functions is the
interface, and the methods somewhere else are the implementation. A generic
function nothing implements is found only when it is first called; okite finds
it when the implementation is loaded.

```lisp
(okite:unimplemented-generics '(#:my-app/ports/store #:my-app/ports/mail))
;; => (MY-APP/PORTS/MAIL:SEND-MAIL)

(okite:ensure-implemented '(#:my-app/ports/store #:my-app/ports/mail))
;; signals okite:unimplemented-error
```

Only exported generic functions are looked at. A method defined with the
generic function, a default, counts as an implementation.

## License

MIT
