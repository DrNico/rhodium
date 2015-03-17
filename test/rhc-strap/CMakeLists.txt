cmake_minimum_required(VERSION 2.8.8)

project (TestStrap Haskell)

set (STRAP_SRC "../../tools/rhc-strap/Rhodium")

add_executable (TestStrap Main.hs TestRhodium/Context.hs ${STRAP_SRC}/Context.hs)

add_file_dependencies (Test.hs TestRhodium/Context.hs)
add_file_dependencies (TestRhodium/Context.hs ${STRAP_SRC}/Context.hs)
