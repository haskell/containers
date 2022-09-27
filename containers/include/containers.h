/*
 * Common macros for containers
 */

#ifndef HASKELL_CONTAINERS_H
#define HASKELL_CONTAINERS_H

/*
 * On GHC, include MachDeps.h to get WORD_SIZE_IN_BITS macro.
 */
#ifdef __GLASGOW_HASKELL__
#include "MachDeps.h"
#endif

#ifdef __GLASGOW_HASKELL__
#define DEFINE_PATTERN_SYNONYMS 1
#endif

#ifdef __GLASGOW_HASKELL__
# define USE_ST_MONAD 1
#ifndef WORDS_BIGENDIAN
/*
 * Unboxed arrays are broken on big-endian architectures.
 * See https://gitlab.haskell.org/ghc/ghc/-/issues/16998
 */
# define USE_UNBOXED_ARRAYS 1
#endif
#endif

#endif
