package scaps.webapi

import utest._

object EntityNameSpecs extends TestSuite {
  val tests = TestSuite {
    import EntityName._

    'nameEncoding{
      'createNames{
        * - {
          val name = appendStaticMember("pkg", "Obj")
          assert(name == "pkg.Obj")
        }
        * - {
          val name = appendClassMember("pkg.Obj", "member")
          assert(name == "pkg.Obj#member")
        }
        * - {
          val name = appendStaticMember("pkg", "##")
          assert(name == "pkg.'#'#")
        }
        * - {
          val name = appendClassMember("pkg.Obj", "##")
          assert(name == "pkg.Obj#'#'#")
        }
      }
    }
    'nameDecoding{
      'splitNames{
        * - {
          val splitted = splitName("pkg.Obj#member")
          assert(splitted == List("pkg", "Obj", "member"))
        }
        * - {
          val splitted = splitName("'#.'#'##'#'#'#")
          assert(splitted == List("#", "##", "###"))
        }
      }
      'fullName{
        * - {
          val decoded = decodeFullName("pkg.Outer#Inner")
          assert(decoded == "pkg.Outer#Inner")
        }
        * - {
          val decoded = decodeFullName("pkg.'#'#'##'#'#")
          assert(decoded == "pkg.######")
        }
      }
    }
    'bijectivity{
      * - {
        val parts = List("pkg", "#", "##")
        val name = parts.foldLeft("")(appendStaticMember)
        val splitted = splitName(name)
        assert(splitted == parts)
      }
      * - {
        val parts = List("pkg", "#", "##")
        val name = parts.foldLeft("")(appendClassMember)
        val splitted = splitName(name)
        assert(splitted == parts)
      }
    }
  }
}
