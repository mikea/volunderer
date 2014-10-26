cmake_minimum_required (VERSION 2.6)

project (volunderer)

add_executable(volunderer volunderer.cpp)
target_link_libraries(volunderer -lglfw3 -lGLU -lGL -lX11 -lXrandr -lXxf86vm -lpthread -lGLEW -lXi)

SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -std=c++11")

set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} ${CMAKE_CXX_FLAGS} -g -O3")
set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} ${CMAKE_CXX_FLAGS} -O3")
set(CMAKE_CXX_FLAGS_PROFILE "${CMAKE_CXX_FLAGS_DEBUG} -pg")
