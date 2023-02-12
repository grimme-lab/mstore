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
 * @dir include/mstore
 * @brief
 * Collection of public API bindings for the mstore library.
 */

/**
 * @file mstore.h
 * @brief
 * Provides convenience functionality for working with the C API bindings for
 * mstore by including all relevant headers and defining general type generic
 * macros for working with the object handles.
 */

#pragma once

#include "mstore/error.h"
#include "mstore/structure.h"
#include "mstore/version.h"

/// Generic macro to free an object handle.
///
/// @param ptr: Opaque object handle
#define mstore_delete(ptr) _Generic((ptr), \
                       mstore_error: mstore_delete_error, \
                   mstore_structure: mstore_delete_structure \
                                   )(&ptr)