cmake_minimum_required(VERSION 3.16)
project(ChessEngine LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
# Required to link chess_lib into the Python .so on Linux (Fly). macOS is more lenient.
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

option(CHESS_BUILD_PYTHON "Build the pybind11 Python module" ON)
option(CHESS_BUILD_TESTS "Build unit tests" ON)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()

add_library(chess_lib STATIC
    src/attacks.cpp
    src/board.cpp
    src/perft.cpp
    src/search.cpp
    src/zobrist.cpp
)
target_include_directories(chess_lib PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/src)
target_compile_features(chess_lib PUBLIC cxx_std_20)

if(MSVC)
  target_compile_options(chess_lib PRIVATE /W4)
else()
  target_compile_options(chess_lib PRIVATE -Wall -Wextra -Wpedantic)
endif()

add_executable(chess_cli src/main.cpp)
set_target_properties(chess_cli PROPERTIES OUTPUT_NAME chess)
target_link_libraries(chess_cli PRIVATE chess_lib)

if(CHESS_BUILD_TESTS)
  include(FetchContent)
  FetchContent_Declare(
      googletest
      GIT_REPOSITORY https://github.com/google/googletest.git
      GIT_TAG v1.15.2
  )
  set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
  set(BUILD_GMOCK OFF CACHE BOOL "" FORCE)
  FetchContent_MakeAvailable(googletest)

  add_executable(chess_tests
      tests/test_board.cpp
      tests/test_perft.cpp
  )
  target_link_libraries(chess_tests PRIVATE chess_lib GTest::gtest_main)
  enable_testing()
  add_test(NAME chess_tests COMMAND chess_tests)
endif()

if(CHESS_BUILD_PYTHON)
  set(PYBIND11_FINDPYTHON ON)
  include(FetchContent)
  FetchContent_Declare(
      pybind11
      GIT_REPOSITORY https://github.com/pybind/pybind11.git
      GIT_TAG v2.13.6
  )
  FetchContent_MakeAvailable(pybind11)
  pybind11_add_module(chessengine src/python_module.cpp)
  target_link_libraries(chessengine PRIVATE chess_lib)
  # pybind11 enables LTO in Release; mixing that with a static lib breaks GNU ld on Linux.
  set_target_properties(chessengine PROPERTIES INTERPROCEDURAL_OPTIMIZATION OFF)
endif()
