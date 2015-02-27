
# CMakeHaskellInformation.cmake
# Provides information to CMake about how to build Haskell files.

set (CMAKE_Haskell_OUTPUT_EXTENSION .o)

include (CMakeCommonLanguageInclude)

# Haskell shared library
if (NOT CMAKE_Haskell_CREATE_SHARED_LIBRARY)
  set (CMAKE_Haskell_CREATE_SHARED_LIBRARY "<CMAKE_Haskell_COMPILER> <CMAKE_SHARED_LIBRARY_Haskell_FLAGS> <LANGUAGE_COMPILE_FLAGS> <LINK_FLAGS> <CMAKE_SHARED_LIBRARY_CREATE_Haskell_FLAGS> <CMAKE_SHARED_LIBRARY_SONAME_Haskell_FLAG><TARGET_SONAME> -o <TARGET> <OBJECTS> <LINK_LIBRARIES>")
endif ()

# Haskell shared module
if (NOT CMAKE_Haskell_CREATE_SHARED_MODULE)
  set (CMAKE_Haskell_CREATE_SHARED_MODULE ${CMAKE_Haskell_CREATE_SHARED_LIBRARY})
endif ()

# Haskell incremental static archive
if (NOT DEFINED CMAKE_Haskell_ARCHIVE_CREATE)
  set (CMAKE_Haskell_ARCHIVE_CREATE "<CMAKE_AR> cr <TARGET> <LINK_FLAGS> <OBJECTS>")
endif ()

if (NOT DEFINED CMAKE_Haskell_ARCHIVE_APPEND)
  set (CMAKE_Haskell_ARCHIVE_APPEND "<CMAKE_AR> r <TARGET> <LINK_FLAGS> <OBJECTS>")
endif ()

if (NOT DEFINED CMAKE_Haskell_ARCHIVE_FINISH)
  set (CMAKE_Haskell_ARCHIVE_FINISH "<CMAKE_RANLIB> <TARGET>")
endif ()

# Haskell compile to object files
if (NOT CMAKE_Haskell_COMPILE_OBJECT)
  set (CMAKE_Haskell_COMPILE_OBJECT
    "<CMAKE_Haskell_COMPILER> <DEFINES> <FLAGS> -hidir <CMAKE_CURRENT_BINARY_DIR> -o <OBJECT> -c <SOURCE>")
endif ()

if (NOT CMAKE_Haskell_LINK_EXECUTABLE)
  set (CMAKE_Haskell_LINK_EXECUTABLE
    "<CMAKE_Haskell_COMPILER> <FLAGS> <CMAKE_Haskell_LINK_FLAGS> <LINK_FLAGS> <OBJECTS> -o <TARGET> <LINK_LIBRARIES>")
endif ()

mark_as_advanced (
  CMAKE_BUILD_TOOL
  CMAKE_VERBOSE_MAKEFILE 
  CMAKE_Haskell_FLAGS
  CMAKE_Haskell_FLAGS_RELEASE
  CMAKE_Haskell_FLAGS_RELWITHDEBINFO
  CMAKE_Haskell_FLAGS_MINSIZEREL
  CMAKE_Haskell_FLAGS_DEBUG
)

set (CMAKE_Haskell_INFORMATION_LOADED 1)

