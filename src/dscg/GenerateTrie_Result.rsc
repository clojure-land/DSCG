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

import dscg::Common;
import dscg::GenerateTrie;

str generateResultClassString(TrieSpecifics ts, rel[Option,bool] setup) {

	className = "<ts.ResultStr>";

	return
	"static final class <className><GenericsStr(ts.tupleTypes)> {
		private <typeToString(dsAtFunction__range_type_of_tuple(ts.ds, ts.tupleTypes))> replacedValue;
		private boolean isModified;
		private boolean isReplaced;

		// update: inserted/removed single element, element count changed
		public void modified() {
			this.isModified = true;
		}

		public void updated(<typeToString(dsAtFunction__range_type_of_tuple(ts.ds, ts.tupleTypes))> replacedValue) {
			this.replacedValue = replacedValue;
			this.isModified = true;
			this.isReplaced = true;
		}

		// update: neither element, nor element count changed
		public static <GenericsStr(ts.tupleTypes)> <className><GenericsStr(ts.tupleTypes)> unchanged() {
			return new <className><InferredGenerics(ts.ds, ts.tupleTypes)>();
		}

		private <className>() {
		}

		public boolean isModified() {
			return isModified;
		}

		public boolean hasReplacedValue() {
			return isReplaced;
		}

		public <typeToString(dsAtFunction__range_type_of_tuple(ts.ds, ts.tupleTypes))> getReplacedValue() {
			return replacedValue;
		}
	}";
}
