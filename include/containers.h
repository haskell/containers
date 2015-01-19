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

/*
 * Define INSTANCE_TYPEABLE[0-2]
 */
#if __GLASGOW_HASKELL__ >= 707
#define INSTANCE_TYPEABLE0(tycon,tcname,str) deriving instance Typeable tycon
#define INSTANCE_TYPEABLE1(tycon,tcname,str) deriving instance Typeable tycon
#define INSTANCE_TYPEABLE2(tycon,tcname,str) deriving instance Typeable tycon
#elif defined(__GLASGOW_HASKELL__)
#define INSTANCE_TYPEABLE0(tycon,tcname,str) deriving instance Typeable tycon
#define INSTANCE_TYPEABLE1(tycon,tcname,str) deriving instance Typeable1 tycon
#define INSTANCE_TYPEABLE2(tycon,tcname,str) deriving instance Typeable2 tycon
#else
#define INSTANCE_TYPEABLE0(tycon,tcname,str) tcname :: TyCon; tcname = mkTyCon str; \
  instance Typeable tycon where { typeOf _ = mkTyConApp tcname [] }
#define INSTANCE_TYPEABLE1(tycon,tcname,str) tcname :: TyCon; tcname = mkTyCon str; \
  instance Typeable1 tycon where { typeOf1 _ = mkTyConApp tcname [] }; \
  instance Typeable a => Typeable (tycon a) where { typeOf = typeOfDefault }
#define INSTANCE_TYPEABLE2(tycon,tcname,str) tcname :: TyCon; tcname = mkTyCon str; \
  instance Typeable2 tycon where { typeOf2 _ = mkTyConApp tcname [] }; \
  instance Typeable a => Typeable1 (tycon a) where { typeOf1 = typeOf1Default }; \
  instance (Typeable a, Typeable b) => Typeable (tycon a b) where { typeOf = typeOfDefault }
#endif

/*
 * Use macros to define strictness of functions.
 * STRICT_x_OF_y denotes an y-ary function strict in the x-th parameter.
 * We do not use BangPatterns, because they are not in any standard and we
 * want the compilers to be compiled by as many compilers as possible.
 */
#define STRICT_1_OF_2(fn) fn arg _ | arg `seq` False = undefined
#define STRICT_2_OF_2(fn) fn _ arg | arg `seq` False = undefined
#define STRICT_1_OF_3(fn) fn arg _ _ | arg `seq` False = undefined
#define STRICT_2_OF_3(fn) fn _ arg _ | arg `seq` False = undefined
#define STRICT_1_OF_4(fn) fn arg _ _ _ | arg `seq` False = undefined
#define STRICT_2_OF_4(fn) fn _ arg _ _ | arg `seq` False = undefined

/*
 * We use cabal-generated MIN_VERSION_base to adapt to changes of base.
 * Nevertheless, as a convenience, we also allow compiling without cabal by
 * defining an approximate MIN_VERSION_base if needed. The alternative version
 * guesses the version of base using the version of GHC. This is usually
 * sufficiently accurate. However, it completely ignores minor version numbers,
 * and it makes the assumption that a pre-release version of GHC will ship with
 * base libraries with the same version numbers as the final release. This
 * assumption is violated in certain stages of GHC development, but in practice
 * this should very rarely matter, and will not affect any released version.
 */
#ifndef MIN_VERSION_base
#if __GLASGOW_HASKELL__ >= 709
#define MIN_VERSION_base(major1,major2,minor) (((major1)<4)||(((major1) == 4)&&((major2)<=8)))
#elif __GLASGOW_HASKELL__ >= 707
#define MIN_VERSION_base(major1,major2,minor) (((major1)<4)||(((major1) == 4)&&((major2)<=7)))
#elif __GLASGOW_HASKELL__ >= 705
#define MIN_VERSION_base(major1,major2,minor) (((major1)<4)||(((major1) == 4)&&((major2)<=6)))
#elif __GLASGOW_HASKELL__ >= 703
#define MIN_VERSION_base(major1,major2,minor) (((major1)<4)||(((major1) == 4)&&((major2)<=5)))
#elif __GLASGOW_HASKELL__ >= 701
#define MIN_VERSION_base(major1,major2,minor) (((major1)<4)||(((major1) == 4)&&((major2)<=4)))
#elif __GLASGOW_HASKELL__ >= 700
#define MIN_VERSION_base(major1,major2,minor) (((major1)<4)||(((major1) == 4)&&((major2)<=3)))
#else
#define MIN_VERSION_base(major1,major2,minor) (0)
#endif
#endif // MIN_VERSION_base was not defined

#endif // This file was already included
