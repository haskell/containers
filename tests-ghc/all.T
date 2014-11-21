# This is a test script for use with GHC's testsuite framework, see
# http://darcs.haskell.org/testsuite

test('datamap001', normal, compile_and_run, ['-package containers'])
test('datamap002', normal, compile_and_run, ['-package containers'])
test('dataintset001', normal, compile_and_run, ['-package containers'])
test('sequence001', normal, compile_and_run, ['-package containers'])
test('mapcoerceseq', when(compiler_ge('ghc','7.9')), compile_and_run, ['-package containers'])
test('mapcoercemap', when(compiler_ge('ghc','7.9')), compile_and_run, ['-package containers'])
test('mapcoercesmap', when(compiler_ge('ghc','7.9')), compile_and_run, ['-package containers'])
test('mapcoerceintmap', when(compiler_ge('ghc','7.9')), compile_and_run, ['-package containers'])
test('mapcoerceintmapstrict', when(compiler_ge('ghc','7.9')), compile_and_run, ['-package containers'])
