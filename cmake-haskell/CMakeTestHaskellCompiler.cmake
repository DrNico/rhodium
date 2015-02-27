
# CMakeTestHaskellCompiler.cmake
# Verifies that the selected Haskell compiler can produce a working program.

include (CMakeTestCompilerCommon)

if (NOT CMAKE_Haskell_COMPILER_WORKS)
  PrintTestCompilerStatus ("Haskell" "")
  file (WRITE ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/testHaskellCompiler.hs "main = return ()")
  try_compile (CMAKE_Haskell_COMPILER_WORKS ${CMAKE_BINARY_DIR}
    ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/testHaskellCompiler.hs
    OUTPUT_VARIABLE __CMAKE_Haskell_COMPILER_OUTPUT
  )
endif ()

if (NOT CMAKE_Haskell_COMPILER_WORKS)
  PrintTestCompilerStatus ("Haskell" " -- broken")
  file (APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
    "Determining if the Haskell compiler works failed with "
    "the following output:\n${__CMAKE_Haskell_COMPILER_OUTPUT}\n\n")
  message (FATAL_ERROR "The Haskell compiler \"${CMAKE_Haskell_COMPILER}\" "
    "is not able to compile a simple test program.\nIt fails "
    "with the following output:\n ${__CMAKE_Haskell_COMPILER_OUTPUT}\n\n"
    "CMake will not be able to correctly generate this project.")
else ()
  PrintTestCompilerStatus ("Haskell" " -- works")
  file (APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeOutput.log
    "Determining if the Haskell compiler works passed with "
    "the following output:\n${__CMAKE_Haskell_COMPILER_OUTPUT}\n\n")
endif ()

