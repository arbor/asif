#include <stdio.h>
#include <stdlib.h>

// Include Haskell FFI file, which we will use to initialize a Haskell runtime
#include "HsFFI.h"

/* #ifdef __GLASGOW_HASKELL__ */
#include ".stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/Example_stub.h"
/* #endif */

int
main( int argc, char *argv[] )
{
  // Initialize Haskell Runtime _before_ any calls to the Haskell code
  hs_init (&argc, &argv);

  // Make a call to Haskell code
  entrypoint();
}
