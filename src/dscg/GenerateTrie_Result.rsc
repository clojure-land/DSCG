/**
 * Copyright (c) 2014 CWI
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *
 *   * Michael Steindorfer - Michael.Steindorfer@cwi.nl - CWI  
 */
module dscg::GenerateTrie_Result

import dscg::GenerateTrie;

str generateResultClassString() {

	className = "Result";

	return
	"static final class <className>\<T1, T2, N extends AbstractNode\<T1, T2\>\> {
		private final N result;
		private final T2 replacedValue;
		private final boolean isModified;

		// update: inserted/removed single element, element count changed
		public static \<T1, T2, N extends AbstractNode\<T1, T2\>\> Result\<T1, T2, N\> modified(N node) {
			return new Result\<\>(node, null, true);
		}

		// update: replaced single mapping, but element count unchanged
		public static \<T1, T2, N extends AbstractNode\<T1, T2\>\> Result\<T1, T2, N\> updated(N node,
						T2 replacedValue) {
			return new Result\<\>(node, replacedValue, true);
		}

		// update: neither element, nor element count changed
		public static \<T1, T2, N extends AbstractNode\<T1, T2\>\> Result\<T1, T2, N\> unchanged(N node) {
			return new Result\<\>(node, null, false);
		}

		private Result(N node, T2 replacedValue, boolean isMutated) {
			this.result = node;
			this.replacedValue = replacedValue;
			this.isModified = isMutated;
		}

		public N getNode() {
			return result;
		}

		public boolean isModified() {
			return isModified;
		}

		public boolean hasReplacedValue() {
			return replacedValue != null;
		}

		public T2 getReplacedValue() {
			return replacedValue;
		}
	}";
}
