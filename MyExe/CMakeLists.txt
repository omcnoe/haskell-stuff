cmake_minimum_required(VERSION 3.28)
project(MyExe)

set(GHC_INCLUDE_DIR   C:/ghcup/ghc/9.6.3/include)
set(MYLIB_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../dist-newstyle/build/x86_64-windows/ghc-9.6.3/haskell-stuff-0.1.0.0/f/myLib/build/myLib/myLib-tmp)
set(MYLIB_LIB         ${CMAKE_CURRENT_SOURCE_DIR}/../dist-newstyle/build/x86_64-windows/ghc-9.6.3/haskell-stuff-0.1.0.0/f/myLib/build/myLib/myLib.dll.a)

add_executable(MyExe MyExe.c)
target_include_directories(MyExe
    PRIVATE
    ${GHC_INCLUDE_DIR}
    ${MYLIB_INCLUDE_DIR}
)
target_link_libraries(MyExe PRIVATE ${MYLIB_LIB})
