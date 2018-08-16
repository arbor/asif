#include <stdio.h>
#include <stdlib.h>

// Include Haskell FFI file, which we will use to initialize a Haskell runtime
#include "HsFFI.h"

#include "src/Example_stub.h"

int
main( int argc, char *argv[] )
{
  // Initialize Haskell Runtime _before_ any calls to the Haskell code
  hs_init (&argc, &argv);

  // Make a call to Haskell code
  entrypoint();
}
