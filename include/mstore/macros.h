/* This file is part of mstore.
 * SPDX-Identifier: Apache-2.0
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
**/

/**
 * @file mstore/macros.h
 * @brief
 * General macro definitions for the mstore C API bindings.
 */

#pragma once

/** @def MSTORE_API_ENTRY
 * Defines an external function exported by the Fortran library.
 */

/** @def MSTORE_API_CALL
 * Macro to define calling convention.
 */

/** @def MSTORE_CFFI
 * Guard macro for CFFI preprocessing of the header files.
 *
 * Use this macro to conditionally enable or disable code snippets for the pass
 * of the CFFI generation step to obtain the Python extension module.
 * Notably, header includes should be removed if the macro is defined to avoid
 * creating bindings for the system library. Furthermore, callbacks need a
 * special extern "Python" declaration which should conditionally included.
 */

#ifdef __cplusplus
#define MSTORE_API_ENTRY extern "C"
#ifndef MSTORE_CFFI
#include <cstdint>
#endif
#else
#define MSTORE_API_ENTRY extern
#ifndef MSTORE_CFFI
#include <stdbool.h>
#include <stdint.h>
#endif
#endif

#ifndef MSTORE_API_CALL
#define MSTORE_API_CALL
#endif
