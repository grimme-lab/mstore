set(srcs
  main.f90
)
add_executable(info ${srcs})
target_link_libraries(info mstore-lib)
