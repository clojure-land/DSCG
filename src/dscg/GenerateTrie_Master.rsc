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
module dscg::GenerateTrie_Master

extend dscg::Common;

extend dscg::GenerateImmutableMap;

extend dscg::GenerateTrie_Optional;
extend dscg::GenerateTrie_Result;

extend dscg::GenerateTrie_FeatureFlags;
extend dscg::GenerateTrie_AbstractAnyNode;
extend dscg::GenerateTrie_AbstractNode;
extend dscg::GenerateTrie_CompactNode;
extend dscg::GenerateTrie_CompactHeterogeneousNode;
extend dscg::GenerateTrie_BitmapIndexedNode;
extend dscg::GenerateTrie_SpecializedBitmapIndexedNode;
extend dscg::GenerateTrie_HashCollisionNode;
extend dscg::GenerateTrie_LeafNode;
extend dscg::GenerateTrie_Iterator;
extend dscg::GenerateTrie_EasyIterator;
extend dscg::GenerateTrie_NodeIterator;
extend dscg::GenerateTrie_Core_Common;
extend dscg::GenerateTrie_Core;
extend dscg::GenerateTrie_CoreTransient;
extend dscg::GenerateTrie_ImmutableInterface;