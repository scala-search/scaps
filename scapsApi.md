# Scaps API

This document describes the targeted API of the core library for version 1.0. The following goals guide the design of this API:

* _Do one thing and do it well_ - The Scaps core library will be stripped down to its essential functionality: Managing and querying the index. All other tasks like query parsing, name resolution etc. will be removed from the core.
* _Reduce complexity for clients_ - All preprocessing will be moved to the core, clients will no longer have to worry about variance annotations and other internals of the index.
* _Increase flexibility for clients_ - The formats of fully qualified names (FQN) and file system paths can be freely chosen by clients.

## Initialize Scaps

* `Scaps$.apply(language: LanguageModel, settings: Settings): Scaps`

    Initializes the search engine with the `language` and settings provided.

## Update Index

* `Scaps.startIndexBatch(): Batch`
    
    Returns a handle that allows modifications of the index. It is possible to start multiple concurrent batches, but the changes will only be committed after the last batch has been completed. Executing index modifications in batches is necessary to avoid redundant recalculations of type relations and statistics.

* `Batch.addDefinitions(definitions: List[Definition]): Future[Unit]`
    
    Adds `definitions` to the index.

* `Batch.removeDefinitions(files: List[Path]): Future[Unit]`
    
    Removes all definitions from the index whose `file` value is contained in `files`. By removing a 

* `Batch.complete(): Future[List[NameNotFound]]`
    
    Commits all modifications to the index and updates statistics if necessary. This operation may take up to several minutes.

    `complete` may return a list of FQN referring to types for which no `TypeDef` has been supplied but have been referenced in a value or type definition. If this list is non-empty, the index is inconsistent and wont continue finalization. In this case, clients must create another batch and provide the missing `TypeDef`s.

## Search

* `Scaps.search(query: Query): Future[QueryError \/ ResultSet]`
    
    Returns a result set with FQNs of values matching the query.

## Data

```
type FQN = String                   // Fully qualified name of indexed definitions, format can be chosen by clients
type Path = String                  // Identifies files that contain a certain definition, format can be chosen by clients
type Documentation = String         // Text content used to support keyword search without markup, recommended format: `<identifier>\n<fullyQualifiedIdentifier>\n<scalaDoc>`

LanguageModel(
  topType: FQN = "scala.Any",
  bottomType: FQN = "scala.Nothing",

  // to what type repeated arguments get expanded to
  repeatedType: FQN = "scala.collection.Seq",
  
  functionTypePattern: String = """scala\.Function(\d*)""",
  tupleTypePattern: String = """scala\.Tuple(\d*)"""
)

Settings(
  // I/O
  dbUrl: String,
  luceneIndexDir: String,

  // Index
  typeFrequencySampleSize: Int,

  // Querying
  explainScores: Boolean,           // If true, `ResultSet` and `Result` will include information about the scoring process, useful for debugging/optimizing
  maxClauseCount: Int,
  maxResultSetSize: Int,
  penaltyWeight: Double,
  distanceBoostWeight: Double,
  typeFrequencyWeight: Double,
  docBoost: Double,
  fingerprintFrequencyCutoff: Double
)

Query(
  keywords: String,
  typeParameters: List[TypeParam],
  tpe: Type
)

ResultSet(
  results: List[Result],
  queryExplanation: Option[String]
)

Result(
  name: FQN,
  score: Float,
  explanation: Option[String]
)

Definition :
  ValueDef(
    name: FQN,
    doc: Documentation,
    typeParameters: List[TypeParam],
    tpe: Type,
    isImplicit: Boolean,
    file: Path
  ) |
  TypeDef(
    name: FQN,
    typeParameters: List[TypeParam],
    extends: List[Type],
    file: Path
  )

TypeParam(
  name: String,
  variance: Option[Variance],
  lowerBound: Option[Type],
  upperBound: Option[Type]
)

Variance:
  Covariant |
  Contravariant |
  Invariant

Type: 
  TypeRef(
    name: String,                   // FQN or a type parameter name
    args: List[Type]
  ) |
  MemberAccess(
    owner: Type,
    member: Type
  ) |
  MethodInvocation(
    args: List[Type],
    returns: Type,
    isImplicit: Boolean
  ) |
  Repeated(
    tpe: Type
  )

QueryError:
  IndexBusy |
  NameNotFound(name: FQN, noTypeParameters: Int)
```

## Examples

### Objects

An object definition may or may not produce an according type definition. If the object extends only one type T, it is sufficient to produce a ValueDef with `tpe = T`. If there are more than one supertypes, it is recommended to produce both a ValueDef and a TypeDef. Refinement types should be avoided.

```
package scala.math

object Numeric {
  implicit object CharIsIntegral extends CharIsIntegral with CharOrdering
}
```

Expected definitions:

```
ValueDef(
  name = "scala.math.Numeric$",
  tpe = TypeRef("scala.Any"))

ValueDef(
  name = "scala.math.Numeric$.CharIsIntegral",
  tpe = TypeRef("scala.math.Numeric$.CharIsIntegral$"),
  isImplicit = true)

TypeDef(
  name = "scala.math.Numeric$.CharIsIntegral$",
  extends = List(
    TypeRef("scala.math.Numeric$.CharIsIntegral$"),
    TypeRef("scala.math.Ordering$.CharOrdering")))
```

### Values and Methods

```
val a = 1

def f(i: Int)(implicit c: Char): String = ???
```

Expected definitions:

```
ValueDef(
  name = "a",
  tpe = TypeRef("scala.Int"))

ValueDef(
  name = "f(Int)(Char)",
  tpe = MethodInvocation(
    TypeRef("scala.Int"),
    MethodInvocation(
      TypeRef("scala.Char"),
      TypeRef("java.lang.String"),
      isImplicit = true)))
```

### Classes and Class Members

...
