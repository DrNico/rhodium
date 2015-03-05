cmake_minimum_required(VERSION 2.8.8)

project (TestStrap Haskell)

# TODO: figure out a graceful way to have the tests built
add_executable (TestStrap TestRhodium/Context.hs Test.hs)

AddFileDependencies (Test.hs TestRhodium/Context.hs)
AddFileDependencies (TestRhodium/Context.hs ../../tools/rhc-strap/Rhodium/Context.hs)
# set_source_files_properties (
#     TestRhodium/Context.hs
#     PROPERTIES COMPILE_FLAGS -i../../tools/rhc-strap )