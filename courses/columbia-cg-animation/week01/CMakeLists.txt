cmake_minimum_required (VERSION 2.6)

project (FOSSSim)
set (FOSSSIM_VERSION "Fall 2010 v.A")
add_definitions (-DFOSSSIM_VERSION="${FOSSSIM_VERSION}")

set(CMAKE_INSTALL_PREFIX "${CMAKE_SOURCE_DIR}/install")

# Initialize the build type (Release, Debug, etc)
if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE Debug CACHE STRING
    "Choose the type of build, options are: Debug Release."
    FORCE)
endif (NOT CMAKE_BUILD_TYPE)

add_definitions (-DCMAKE_BUILD_TYPE="${CMAKE_BUILD_TYPE}")

# Add warnings to the compiler flags
set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -Wno-unused")

if (CMAKE_BUILD_TYPE MATCHES Debug)
  add_definitions (-DDEBUG -DEIGEN_DONT_ALIGN -DEIGEN_DONT_VECTORIZE)
endif (CMAKE_BUILD_TYPE MATCHES Debug)

if (CMAKE_BUILD_TYPE MATCHES Release)
  add_definitions (-DNDEBUG)
  set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DEIGEN_DONT_ALIGN_STATICALLY -msse2")
endif (CMAKE_BUILD_TYPE MATCHES Release)

# Add directory with macros
set (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${PROJECT_SOURCE_DIR}/cmake)

# Add cmake file with useful functions
include (FOSSSimCommon)

# Eigen3 library is required
find_package (Eigen3 REQUIRED)
if (EIGEN3_FOUND)
  include_directories (${EIGEN3_INCLUDE_DIR})
else (EIGEN3_FOUND)
  message (SEND_ERROR "Unable to locate Eigen3")
endif (EIGEN3_FOUND)

include_directories (${CMAKE_CURRENT_SOURCE_DIR})
add_subdirectory (FOSSSim)
execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink ${CMAKE_SOURCE_DIR}/assets ${CMAKE_CURRENT_BINARY_DIR}/FOSSSim/assets )

