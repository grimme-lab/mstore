set(srcs
  main.f90
)
add_executable(fortranize ${srcs})
target_link_libraries(fortranize mstore-lib)
