# Optimizations

* Type Class Instance Substitution
  1. Unify Parameter Substitution Algorithms
* Create Views from Explicit Conversions
  e.g. Iterator[A].toList: List[A] creates +Iterator %> +List and -List %> -Iterator
  1. Use parameters to optimize distance of implicit and explicit views

# Refactor to Language Agnostic Architecture with Remote Extractors

1. View must become an extraction result
  1. ViewDef(from: TypeRef, to: TypeRef, distance: Float) :> Definition
  1. Extractor creates views for various variances according to the language rules
    +Int %> +Nothing
    -Int %> -Any
    /_ %> /<unknown>
    ...
  1. Change ViewIndex accordingly
1. Move Extractors to separate project
  1. 2.10/2.11 cross builds
  1. Unit tests depend on 2.11
1. Use Extractor in SBT Plugin
1. Change API
  1. index(m: Module, defs: List[Definition])
1. ???
1. Profit!