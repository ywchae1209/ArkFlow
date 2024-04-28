## Json Trasnformer 

[ Features ]
1. Very high-spped multi-pattern matching
2. Validate a json with given validation-rule
3. Extract and convert a json to other json format with give convert-rul

[ Based on ]   
Scala 2.1x  ( ove)
fast-parser ( parser-combinator library.)
json4s (json library)

Business를 지원하는 거의 모든 프로젝트에는 구조화된 데이터 처리가 포함되어 있고 
그 처리과정에는 데이터의 검증(구조/형식/내용)과 변환이 필수적으로 수반된다.

Overview 
--------------------

구조화된 데이터 처리에서 요구되는 검증/변환 기능은 Domain에 따라 매우 다양한 요구를 가지고 있으므로
일반화된 기능을 제공하는데에는 많은 제한이 존재한다.
실무에 도움이 되도록 하기 위해 최대한 단순하고, 사용하기 용이한 형태의 DSL을 제공하여 검증/변환의 요구를 지원하고자 한다.

본 프로젝트에는 크게 3가지 기능을 포함하고 있다.
1. 고속분류 기능
2. 검증 기능
3. 변환 기능

---------------------
## classify JSON

이 모듈은 Json 데이터에 존재하는 `증거`를 찾아서 그 Json이 어떤 유형에 해당하는 지를 판정하는 기능을 제공한다.

이를 위해서서는 
`무엇을 증거로 볼것인가`와 `어떻게 증거를 찾을 것인가`의 두 가지를 정해야 한다.

현 버전은 
`어떻게 증거를 찾을 것인가`를 위해 문자열에 대한 고속의 다중 패턴매칭기능을 지원하며, 이를 사용하기 위한 특수한 DSL 문법을 제공한다.
(특수한 용도의 RegularExpression 이라고 생각해도 됨.)

제한
----------

`무엇을 증거로 볼것인가`는 공통적으로 존재하는 key를 찾거나, key하위의 값(특히 문자열)이 어떤 패턴을 가지는 지
조사하는 것을 생각해 볼 수 있는데, 이 기능에 대한 일반 Spec 정의는 완료되지 못했다. 

[생각] 

활용되는 영역마다 `무엇을 증거로 볼것인가`에 대한 생각과 요구가 다른데, 
이것을 정의하는 것은 어쩌면 본 과제의 범위 밖 인 듯 함.

`어떻게 증거를 찾을 것인가`에 해당하는 기능을 제공하고, 사용자(개발자)가 알아서 사용하도록 해도 되지 않을까?
지금까지 만난 개발자들은 `무엇을 증거로 볼것인가`라는 질문 자체를 이해하지 못했고,
그런 아이디어 자체를 이해시키는 것도 상당한 시간이 걸리는 지라, 
base기능(`어떻게 증거를 찾을 것인가`)을 제공하고 상황에 맞춰 사용하도록 하기로 했음.

### Mulitpattern matching

이하에서는 `어떻게 증거를 찾을 것인가`로서 MultiPattern Matching 기능의 사용법 중심으로 설명하겠음.

* import
```scala
import transform.jsonClassify.patternMatching.MultiPattern
```

* initialize pattern matching rule

```scala
val mp = MultiPattern( 
  21 -> "string-rule-of-patten-matching1",
  22 -> "string-rule-of-patten-matching2",
  23 -> "string-rule-of-patten-matching3",
  24 -> "string-rule-of-patten-matching4",
  25 -> "string-rule-of-patten-matching4" )
```

21~25번까지의 번호를 부여한 pattern-matching 규칙들을 가진 mp를 생성


* initialize pattern matching rule
 
```scala
val target_string = "What is functional programming? that is a programming style having focus on the functions"
val result = mp.search(target_string)
```

`target_string`내에 mp에서 지정한 pattern이 발견되는 지 조사.

이렇게 하나의 대상에서 여러 패턴을 한꺼번에 찾기 때문에, `Multi-pattern matching`으로 명명

### Syntax of string-rule-of-pattern-matching

```scala
val string-rule-of-pattern-matching = "`this` ~ `is` ~ `very` ~ `suspicious` ~ `case`"
```

searching 대상 단어는 `'` 로 감싸서 정의한다.

`this` ~ `is` ~ `very` ~ `suspicious` ~ `case` 
단어가 (모두 존재하고) 순서대로 등장하는 지 ( 중간에 어떤 문자열이 있던지 간에.)

```scala
val string-rule-of-pattern-matching = "^`must-start-with`"
```

`must-start-with` 라는 문자열로 시작하는 지 (`^`는 시작점의 의미)

```scala
val string-rule-of-pattern-matching = "`must-end-with`$"
```

`must-end-with` 라는 문자열로 끝나는 지 (`$`는 끝점 뜻)


```scala
val string-rule-of-pattern-matching1 = "^`must-start-with` | `must-end-with`$"
```

`must-start-with` 라는 문자열로 시작하거나, 
`must-end-with` 라는 문자열로 끝나는 지 

```scala
val string-rule-of-pattern-matching1 = "^`must-start-with` & `must-end-with`$"
```

`must-start-with` 라는 문자열로 시작하고,
`must-end-with` 라는 문자열로 끝나는 지


```scala
val string-rule-of-pattern-matching1 = "!(^`must-start-with` & `must-end-with`$)"
```

`!` 은 not의 뜻.

```scala
val string-rule-of-pattern-matching1 = "!('a' & !( 'b' | !'c')) | ((!(  !( 'a' & 'b' ) & 'c') | 'd' ) & 'e') "
```

이런 식으로 괄호`()`와 `|`, `&`를 조합할 수 있음.

```scala
val string-rule-of-pattern-matching1 = "*"
```

`*` 모든 경우 참
( 이런 조건이 필요할까 싶지만, 실무경험상 필요한 경우가 많이 있었기에 넣은 기능)


### Resturn Value

```scala
val result: Seq[(Int, String, Seq[Occurrence])] = mp.search(sampleMessage)
```

찾아진 결과는 여러개 일 수 있으므로, `Seq[...]`로 반환된다.   
`Int`는 대응한 pattern의 id(생성시에 부여한 고유번호)  
`String`은 초기에 입력한 pattern-string   
`Seq[Occurrence]`는 해당 패턴을 구성하는 하위 문자열들이 발견된 위치들  

```scala
case class Occurrence( keyword: String, loc: Seq[Int])
```

### Performance consideration

본 기능을 별도로 만든 이유는, 
대상문자열에 대응시킬 패턴이 수백~수만이 될 경우, 패턴수가 증가할 때 선형적으로 성능감소가 있어서 이를 회피하기 위해 만든 것.
( 실무에서는 약 10,000개의 패턴을 matching하는 용도로 사용했음. Thread당 1,000~ 10,000 EPS 정도의 성능.)

따라서, 1개의 대상문자열에 많은 수의 패턴을 적용해야하는 경우에 사용하기 바람.

내부에서 사용되는 알고리즘은 변형 Trie-tree 알고리즘으로 알려진 알고리즘 중에서는 가장 빠른 알고리즘.
( Trie에 기반한 성능이 조금더 좋은 최적 알고리즘이 하나 존재하기는 한다. )


-----------------
## verify JSON

이 모듈은 Json 데이터가 `원하는 구조와 값형식, 값들간의 관계`을 가지는지 판정하는 기능을 제공한다.

[ Verify ]
1. 구조에 대한 verify
2. 값에 대한 verify
3. 값들간 관계에 대한 verify

`verify-rule` 또는 `validation-rule`은 위의 내용을 포함한 Json형식의 데이터이다.


### Concept

#### 구조에 대한 verify

구조를 검증한다는 것은 Json데이터가 지정한 Tree구조를 가지는 지 점검하는 것이다.

Json은 트리 구조를 가지는 데, 각 트리의 Branch는  
 * `Object`(`키 -> 값`의 반복으로 구성된 것)   
 * `Array`(`값`의 반복으로 구성된 것)

으로 구성된다.

[ 참고 ]
`validation-rule`도 Json형식이므로, 구조를 정하는 *다른 문법* 대신,   
`validation-rule`구조 자체가 `검증대상인 json이 가져야하는 구조`로 정했다.

Branch 가 어떤 필드를 _반드시_ 가져야 되는지, 어떤 필드는 _임의_(없어도 되는) 필드인지도 
구조검증의 내용중 하나 이다.

[참고: Branch node in Json4s]
```scala
case class JObject(obj: Seq[(String, JValue)]) 
case class JArray(arr: Seq[JValue]) 
```

[ 구조검증 Rule class]
```scala
case FormatArray(format: FormatJson, syntaxArray: Option[SyntaxArray]) 
case FormatObject(fields: Map[String, FormatJson], syntax: Option[(SyntaxObject, Boolean)]) 
case FormatOptional(format: FormatJson)

```

#### 값에 대한 verify

값을 검증한다는 것은 Json데이터(Tree 구조)의 끝단(Edge)의 값들이 어떤 형식과 내용을 가지는 지 점검하는 것이다.

[참고: Edge node in Json4s]
```scala
case class JNull
case class JString(s: String)
case class JDouble(num: Double)
case class JDecimal(num: BigDecimal)
case class JLong(num: Long)
case class JInt(num: BigInt)
case class JBool(value: Boolean) 
```

[ 참고 ]
Edge값이 가져야할 형식을 정의한는 별도의 문법과 함수들이 존재한다.
( 검증문법 설명문서 참고)

[ Edge값이 지켜야할 Rule class]
```scala
case class FormatValue(syntax: Seq[SyntaxValue]) 
```

#### 값들간의 관계에 대한 verify?

값들간의 관계검증한다는 것은 끝단(Edge)의 값들간의 관계를 점검하는 것이다.

[ 참고 ]
이를 위해서는 `비교대상값`을 지정하는 방법과 `비교대상간의 관계`연산방법이 필요하다.

`비교대상값`을 지정하는 방법과 `비교대상간의 관계`연산방법을 위한 문법과 방법은
별도 문서 참고.

[ 값들간의 관계검증은 FormatObject내에 존재: SyntaxObjct]

```scala
case class FormatObject(fields: Map[String, FormatJson], syntax: Option[(SyntaxObject, Boolean)])
```


-----------------------
## Convert JSON

별도 문서 참고.

To-be-updated..