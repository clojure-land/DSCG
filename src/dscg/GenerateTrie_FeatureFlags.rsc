/**
 * Copyright (c) 2015 CWI
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *
 *   * Michael Steindorfer - Michael.Steindorfer@cwi.nl - CWI  
 */
module dscg::GenerateTrie_FeatureFlags

import List;
import dscg::Common;
import dscg::ArrayUtils;
import util::Math;

//data UnsafeFeatureFlags
//	= supportsNodeUFF()
//	| supportsValuesUFF();

str generateFeatureFlagsClassString(TrieSpecifics ts, bool isLegacy = true) =
	"static final class FeatureFlags {
	'	public static final long SUPPORTS_NODES =   1 \<\< 0;
	'	public static final long SUPPORTS_PAYLOAD = 1 \<\< 1;
	'}";
