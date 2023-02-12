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
 * @file mstore/version.h
 * @brief
 * Provides access to the version, compatibility and features exported by
 * this API.
 */

#pragma once

#include "mstore/macros.h"

/// Retrieve version of library used
///
/// @return Compact version number in the format 10000 * major + 100 * minor + patch
MSTORE_API_ENTRY int MSTORE_API_CALL
mstore_get_version(void);
