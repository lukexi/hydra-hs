hydra-hs
========

Haskell binding to the Sixense SDK for the Razer Hydra

How-To
------

1. Get the SDK from [Sixense](http://sixense.com/developers)

2. Install it. If you're on 32 bit OS X, you might have to manually copy e.g `libsixense.dylib` to `/usr/local/lib`. Also ensure that `sixense.h` can be found, e.g by copying it to `/usr/local/include`.

3. Install hydra-hs, using cabal-install