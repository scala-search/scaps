/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.nucleus.indexing

import scaps.nucleus.ValueDef
import scaps.nucleus.Document
import scaps.nucleus.ValueDoc

private[nucleus] object ValueIndex {
  def docToValue(d: Document): ValueDef =
    upickle.default.read[ValueDef](new String(d.data))

  def valueToDoc(v: ValueDef): Document =
    ValueDoc(
      v.name,
      Fingerprint(v).map(_.key),
      upickle.default.write(v).getBytes,
      v.source)
}
