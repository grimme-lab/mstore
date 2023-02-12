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
 * @file mstore/error.h
 * @brief
 * Provides a light-weight error handler for communicating with the library.
 *
 * The library provides two different kinds handlers, a light error handle type
 * #mstore_error is defined here.
 *
 * The error handle is used in the context of simple tasks and requires
 * only small overhead to construct and use.
 */

#pragma once

#include "mstore/macros.h"

/// Error instance
typedef struct _mstore_error* mstore_error;

/// Create new error handle object
MSTORE_API_ENTRY mstore_error MSTORE_API_CALL
mstore_new_error(void);

/// Delete an error handle object
MSTORE_API_ENTRY void MSTORE_API_CALL
mstore_delete_error(mstore_error* /* error */);

/// Check error handle status
MSTORE_API_ENTRY int MSTORE_API_CALL
mstore_check_error(mstore_error /* error */);

/// Clear error handle status
MSTORE_API_ENTRY void MSTORE_API_CALL
mstore_clear_error(mstore_error /* error */);

/// Get error message from error handle
MSTORE_API_ENTRY void MSTORE_API_CALL
mstore_get_error(mstore_error /* error */,
                 char* /* buffer */,
                 const int* /* buffersize */);
