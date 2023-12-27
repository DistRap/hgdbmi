# Version [0.3.0.0](https://github.com/distrap/hgdbmi/compare/0.2.0.0...main) (2023-MM-DD)

* Additions
  * Support for `ExtendedRemoteTarget`
  * Support for `async` mode
  * `ConfigTCP` for connecting to GDB over network
  * `Gdb` module, `Gdb.Monad` module wrapping MI interface
    into a monadic one

* Changes
  * `frameFile` and `frameLine` fields of `Frame` are now
    optional (`Maybe`) to allow debugging embedded targets
    where these might not be available.

# Version 0.2.0.0 (2013-01-14)

* No history available

# Version 0.1.0.0 (2008-03-30)

* Initial release

---

`hgdbmi` uses [PVP Versioning][1].

[1]: https://pvp.haskell.org

