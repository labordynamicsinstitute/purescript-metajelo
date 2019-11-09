These files are generated using `XsdToHaskell`.

Installed like:

```
cabal new-install HaXml
```

Run like:

```
 ~/.cabal/bin/XsdToHaskell < ~/workspace/metajelo/schema/xsd/reproMetadata0.7.xsd > ~/workspace/metajelo-web/attic/xsd_to_haskell/reproMetadata0.7.xsd.hs
```

Note: using XsdToHaskell, or more likely, some other schema parser that can
be used more easily as a library, in conjunction with a templating system like
[heterocephalus](http://hackage.haskell.org/package/heterocephalus), might be worth
a shot for propagating certain classes of schema updates directly into the source code.
