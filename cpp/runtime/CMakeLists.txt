cmake_minimum_required(VERSION 3.0)

project(ADL_RUNTIME C CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)


add_subdirectory(3rd-party)

include_directories(
  ./include
  ./3rd-party/libb64-1.2/include
  ./3rd-party
  ./src-generated
)

add_library(adl_runtime
  src/adl.cpp
  src/json.cpp
  src/jsonimpl.cpp
  src/types.cpp
  src-generated/sys.adlast.cpp
  src-generated/sys.types.cpp
)

target_link_libraries(adl_runtime
  b64
)

add_executable(adl_tests
  tests/main.cpp
  tests/tests-serialisation.cpp
  src-generated/unittests.cpp
)

target_link_libraries(adl_tests
  adl_runtime
) 


include(GNUInstallDirs)
install(TARGETS adl_runtime)
install(DIRECTORY include/adl
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
  FILES_MATCHING PATTERN "*.h"
)
  
