
# CMakeDetermineHaskellCompiler.cmake
# Determines which compiler to use for Haskell files.

set (CMAKE_Haskell_COMPILER_ENV_VAR "HASKELL_COMPILER")

if (NOT CMAKE_Haskell_COMPILER)
  # Prefer the environment variable
  if ($ENV{${CMAKE_Haskell_COMPILER_ENV_VAR}} MATCHES ".+")
    get_filename_component (CMAKE_Haskell_COMPILER_LIST $ENV{${CMAKE_Haskell_COMPILER_ENV_VAR}} PROGRAM PROGRAM_ARGS CMAKE_Haskell_FLAGS)
    if (NOT EXISTS ${CMAKE_Haskell_COMPILER_LIST})
      message (FATAL_ERROR "Could not find compiler set in environment variable ${CMAKE_Haskell_COMPILER_ENV_VAR}")
    endif ()
  else ()
    set (CMAKE_Haskell_COMPILER_LIST ghc)
  endif ()

  # See if any of the compilers are reachable
  # (The user may specify a custom PATH in _CMAKE_USER_Haskell_COMPILER_PATH)
  if (_CMAKE_USER_Haskell_COMPILER_PATH)
    find_program (CMAKE_Haskell_COMPILER NAMES ${CMAKE_Haskell_COMPILER_LIST} PATHS ${_CMAKE_USER_Haskell_COMPILER_PATH} NO_DEFAULT_PATH)
  else ()
    find_program (CMAKE_Haskell_COMPILER NAMES ${CMAKE_Haskell_COMPILER_LIST})
  endif ()

  set (CMAKE_Haskell_COMPILER "${CMAKE_Haskell_COMPILER}" CACHE FILEPATH "Haskell Compiler")
endif ()

include (CMakeFindBinUtils)

# Configure the variables in this file for faster reloads
configure_file (${CMAKE_CURRENT_LIST_DIR}/CMakeHaskellCompiler.cmake.in
  ${CMAKE_PLATFORM_INFO_DIR}/CMakeHaskellCompiler.cmake
  @ONLY IMMEDIATE # IMMEDIATE must be here for compatibility mode <= 2.0
)

