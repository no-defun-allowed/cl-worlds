# cl-worlds

This is an implementation of the "worlds" model for scoping side effects, as
described in <http://www.vpri.org/pdf/tr2011001_final_worlds.pdf>. It only
depends on `closer-mop` to allow us to mess with the meta-object protocol, and
`trivial-garbage` to create weak hash-tables (for tracking object changes 
without memory leaks), and `bordeaux-threads` ~~cause we can~~ since we imagine
this could be very useful in multi-threaded programs.

Worlds act similar to "transactions" in databases, where no changes to the 
outside world are made until the world is committed. Using the metaclass
`WORLDS:WORLD-CLASS`, classes can have changes to their instances' slots
managed by the world system. Worlds can be created using the 
`WORLDS:SPROUT` function (which "sprouts" a world from the current world), can
be entered using the `WORLDS:WITH-WORLD` macro and have their changes committed
to their parents using the `WORLDS:COMMIT` function.

## An example

```lisp
(defclass f ()
  ((bar :initarg :bar)
   (baz :initarg :baz))
  (:metaclass worlds:world-class))

(let ((world (worlds:sprout))
      (f (make-instance 'foo :bar "BAR")))
  (worlds:with-world (world)
     ;; changes in this body will only be seen within WORLD
     (setf (slot-value f 'bar) "QUUX")
     (print (slot-value f 'bar)))
  ;; the change in the slot BAR will not be seen here...
  (print (slot-value f 'bar))
  ;; ...until it is committed
  (worlds:commit world)
  ;; then the change will be seen
  (print (slot-value f 'bar)))
#|
prints
  "QUUX" 
  "BAR" 
  "QUUX" 
|#
```

## License

This is the "BSD 0-clause license", which cl-worlds is licensed under.

**Copyright (C) 2020 by Hayley Patton <theemacsshibe at gmail.com>**

Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
