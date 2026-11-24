/*
 * Common macros for containers
 */

#ifndef HASKELL_CONTAINERS_H
#define HASKELL_CONTAINERS_H

/*
 * On GHC and MicroHs, include MachDeps.h to get WORD_SIZE_IN_BITS macro.
 */
#if defined(__GLASGOW_HASKELL__) || defined(__MHS__)
#include "MachDeps.h"
#endif

#if defined(__GLASGOW_HASKELL__) || defined(__MHS__)
#define DEFINE_PATTERN_SYNONYMS 1
#endif

#ifdef __GLASGOW_HASKELL__
# define USE_ST_MONAD 1
#if !defined(WORDS_BIGENDIAN) || MIN_VERSION_array(0,5,6)
/*
 * Before array-0.5.6.0, unboxed arrays of Bool are broken on big-endian
 * architectures: newArray does not initialize all the bits that it reads.
 * See https://gitlab.haskell.org/ghc/ghc/-/issues/16998
 */
# define USE_UNBOXED_ARRAYS 1
#endif
#endif

#endif
