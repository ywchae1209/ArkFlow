## Json Trasnformer 

[ Features ]
1. Very high-spped multi-pattern matching
2. Validate a json with given validation-rule
3. Extract and convert a json to other json format with give convert-rul

[ Based on ]   
* Scala 2.1x ( language )
* fast-parser ( parser-combinator library.)
* json4s (json library)
* no other dependency

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
대상문자열에 대응시킬 패턴이 수백,수만이 될 경우, 패턴수가 증가할 때 선형적으로 성능감소가 있어서 이를 회피하기 위해 만든 것.

실무에서는 약 10,000개의 패턴을 수 KB의 문자열에 대해 matching하는 용도로 사용해서, 
Thread당 1,000 EPS부터 10,000EPS 정도의 처리성능을 보였음.

따라서, 1개의 대상문자열에 많은 수의 패턴을 적용해야하는 경우에 적극 사용하는 것이 유리함.

내부에서 사용되는 알고리즘은 변형 Trie-tree 알고리즘으로 알려진 알고리즘 중에서는 가장 빠른 알고리즘.
( Trie에 기반한 성능이 조금더 좋은 최적 알고리즘이 하나 존재하기는 한다. )


### 생각해볼 꺼리

구조화된 데이터를 분류하기 위해서는 구조를 따라가면서 어디에 어떤것이 있는지를 함께 조사하는 것이 타당할 것이다.
그것을 포함하면, `어디의 무엇을 증거로 볼것인가`와 `어떻게 증거를 찾을 것인가`의 두 가지를 정해야 한다.

현 버전은
`어떻게 증거를 찾을 것인가`를 위해 문자열에 대한 고속의 다중 패턴매칭기능을 지원하며, 이를 사용하기 위한 특수한 DSL 문법을 제공한다.
(특수한 용도의 RegularExpression 이라고 생각해도 됨.)

Full-text multi-pattern matching이라고 할 수 있는데, 한단계를 더 생각한다면
`어디의 무엇을 증거로 볼것인가`에 대한 기능이 필요해 질 수 있다.

지금까지 만난 개발자들은 `어디의 무엇을 증거로 볼것인가`라는 질문 자체를 이해하지 못했고,
그런 아이디어 자체를 이해시키는 것도 상당한 시간이 걸리는 지라,
base기능(`어떻게 증거를 찾을 것인가`)을 제공하고 `어디의 무엇을 증거로 볼것인가`는 개발자가 상황에 맞춰 개발하도록 하였다.

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
case class FormatArray(format: FormatJson, syntaxArray: Option[SyntaxArray]) 
case class FormatObject(fields: Map[String, FormatJson], syntax: Option[(SyntaxObject, Boolean)]) 
case class FormatOptional(format: FormatJson)

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

이 모듈은 Json데이터를 원하는 다른 Json구조로 mapping하는 기능을 제공한다.

[ `Convert Rule`의 내용]
1. 만드려고 하는 Json의 구조와 필드의 정의 (Target-Structure)
2. 원천 Json에서 원하는 데이터를 추출하는 것에 대한 정의( Extract-Source)


( `Convert Rule`도 앞서와 마찬가지로 Json형식으로 작성한다. Root는 JObject형식)

### Target-Structure

Target-Structure는 `Convert Rule`에서 구조를 표현하는 JObject형식의 반복으로 정의한다.

주요 키워드는 아래와 같다. 

| Rule-Json의 예약 Key | desc                                                                       |
|:-----------------:|----------------------------------------------------------------------------|
|     for Root      |                                                                            |
|       {$$}        | 원본 Json을 하나의 큰 Json-Object로 변환하자.(Target에서 추출할 내용은 `문자열값`으로 지정             |
|       {$>}        | Json-Object의 내용을 만드는 방법을 지시한다.                                             |
|       [$$]        | Target-Json을 Json-Array 변환하자.(Target에서 추출할 내용은 `문자열값`으로 지정                 |
|       [$>]        | 반복되는 Array의 값(Json-Object)을 만드는 방법을 지시한다.                                  |
|  for sub-branch   |                                                                            |
|       {::}        | Json노드에서 `문자열값`으로 지정된 Child를 추출하여 Json-Object를 만들자.                        |
|       {=>}        | Json-Object의 내용을 만드는 방법을 지시한다.                                             |
|       [::]        | Json노드에서 `문자열값`으로 지정된 Child를 추출하여 Json-Array를 만들자.                         |
|       [=>]        | Child를 Json-Object의 내용을 만드는 방법을 지시한다. (만들어진 Json-Object는 Array의 항목이 된다.) |

상세한 내용은 `별첨문서(convert.docx)`를 참고하도록 한다.

### Extract-Source

추출대상의 구조를 미리 가정할 수 없으므로, 최대한 flexible한 추출대상지정방식이 필요하다.

이를 위해, json-path문법을 빌려 추출 대상을 지정하는 방식을 지원하도록 하였다.

* Note : 바퀴의 재발명?   
  다음의 이유로 자체 구현.  
  1. Convert 기능이 추가로 확장될 필요가 있을 것(json-path문법자체를 확장해야 할 수도)
  2. 구현라이브러리마다 지원기능의 편차존재( 미묘하게 다른 점 찾는 것도 일.)

상세한 내용은 `별첨문서(jsonpath.docx)`를 참고하도록 한다.

### Manipluate-Extracted-Value
( 추가된 기능 )

Source-Json에서 추출된 값(추출방법은 `Extract-Source` 참고)을 다음의 조작할 수 있다.


[추출 + 조작 규칙 사례]
```json

{
  "[$$]": "$[?(.TRT_INFO && .ISR_INFO && .RCPT_HEADER && .FEE_DETAIL && .PRS_INFO)]",
  "[$>]": {
    "{::}": "$.TRT_INFO",
    "{=>}": {
      "addItem4": "@.CUSTOM4::toString().replaceAllIn('[0-9]', '_').replace('_', '1').toLong()",
      "subInsuranceCd": null,
      "addItem5": "@.CUSTOM5",
      "certificateDocId": null,
      "treatCls": "O"
    }
  }
}
```

[ 설명 ]

```
@.CUSTOM4::toString().replaceAllIn('[0-9]', '_').replace('_', '1').toLong()
```

* `@.CUSTOM4` : source의 CUSTOM4 필드의 값을 추출하라.
* `::toString().replaceAllIn('[0-9]', '_').replace('_', '1').toLong()`  


|             keyword             | desc                      |
|:-------------------------------:|---------------------------|
| `::toString()`          | 추출된 값을 문자열로 변환하라.         |
| `.replaceAllIn('[0-9]', '_')`  | 문자열내의 숫자(regex)를 _ 로 치환하라 |
| `.replace('_', '1')`       | 문자열내의 _를 1 로 치환하라         |
| `.toLong()`                     | 문자열을 정수로 변환하라             |


* 조작함수는 `::` 문자열로 시작한다.
* 각 함수는 `.function_name(arg1, arg2)` 형식으로 연결지어 작성한다.
* 인자에 문자열을 사용할 경우 `'문자열'`의 방식으로 작성한다.

* 조작함수의 목록은 다음과 같다.

|                                                                             function                                                                              | desc                      |
|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------:|---------------------------|
|                                                                          trim(), _trim()                                                                          | 문자열의 앞뒤 공백을 제거함 |
|                                                                       toLower() _toLower()                                                                        | 문자열을 lowercase 로 변환함|
|                                                                       toUpper(), _toUpper()                                                                       | 문자열을 uppercase로 변환함|
|                                                                        toLong(), _toLong()                                                                        | 문자열을 Long-integer 로 변환함|
|                                                                      toDouble(), _toDouble()                                                                      | 문자열을 Doube-float 로 변환함| 
|                                                                   toLongOr(123), _toLongOr(123)                                                                   | 문자열을 Long-integer 로 변환시도하고, 실패하면 지정한 값을 반환함 ( 주의 : 입력값은 0 또는 자연수여야 함)|
|                                                                 toDoubleOr(123), _toDoubleOr(123)                                                                 | 문자열을 Double-floa로 변환시도하고, 실패하면 지정한 값을 반환함 ( 주의 : 입력값은 0 또는 자연수여야 함)|
|                                                                  toLongSep(','), _toLongSep(',')                                                                  | 문자열에서 인자로 준 character (예의 경우는 comma)로 3-digit씩 끊어진 값을 추출하여 정수로 변환함. ( "-12,345" ==> -12345 ) |
|                                                                toDoubleSep(','), _toDoubleSep(',')                                                                | 문자열에서 인자로 준 character (예의 경우는 comma)로 3-digit씩 끊어진 값을 추출하여 정수로 변환함. ( "-12,345.456" ==> -12345.456 )|
|                                                             toLongSepOr(',', 0), _toLongSepOr(',', 0)                                                             | 문자열에서 인자로 준 character (예의 경우는 comma)로 3-digit씩 끊어진 값을 추출하여 정수로 변환시도하고, 만약 실패할 경우 주어진 default값(예의 경우 0)을 반환함. ( "-12,345.456" ==> -12345.456 ) ( 주의 : default 값은 0 또는 자연수여야 함)|
|                                                           toDoubleSepOr(',', 0), _toDoubleSepOr(',', 0)                                                           | 문자열에서 인자로 준 character (예의 경우는 comma)로 3-digit씩 끊어진 값을 추출하여 Double-float로 변환시도하고, 만약 실패할 경우 주어진 default값(예의 경우 0)을 반환함. ( "-12,345.456" ==> -12345.456 ) ( 주의 : default 값은 0 또는 자연수여야 함)|
|                                                               subString( 1, 10), _subString( 1, 10)                                                               | 문자열에서 주어진 범위의 부분문자열을 추출함 ( index는 0부터 시작, 예의 경우는 두번째부터 열번째까지의 부분문자열을 추출함)|
|                                                               replace('0', '_'), _replace('0', '_')                                                               |문자열에서 첫번째 인자로 주어진 부분문자열을 찾아서, 두번째 인자로 주어진 부분문자열로 치환한 문자열을 반환 (예의 경우, 0을 _ 로 바꿈 : "20240101" => "2_24_1_1" )|
| regex( '[0-9]', '_'), _regex( '[0-9]', '_'), regexReplace( '[0-9]', '_'), _regexReplace( '[0-9]', '_'), replaceAllIn( '[0-9]', '_'), _replaceAllIn( '[0-9]', '_') | 문자열에서 첫번째 인자로 주어진 정규표현식에 대응하는 값을 찾아서, 두번째 인자로 주어진 치환한 규칙에 따라 치환한 문자열을 반환. (예의 경우, 0~9의 문자들을 _ 로 바꿈 : "2024-01-01" => "____-__-__" )** 치환규칙(두번째 인자)는 matching-group을 지정할 수 있음. ($ 키워드) : replaceAllIn 함수 사용법 참고바람.|
|                                                                               abs()                                                                               | 숫자형의 값을 읽어서 절대값을 반환|
|                                                                           toString(), asString()                                                                  | 문자열 형식으로 변환| 


[ Developer Note ]
* 발생가능한 exception은 룰 작성시의 syntax-error와 run-time시의 runtime-exception으로 구분
* 조작함수의 실행결과는 `Either`의 형식으로 처리. ( Success or Fail)


-----------------------
## for Java Developer

자바개발자를 위한 facade 클래스들과 매우 간단한 sample.

```scala
import transform.java
```

```java

// classify 

import transform.java.ClassifyResult;
import transform.java.JsonClassifier;

Map<Integer, String> mps = Map(1, "'this'~'is'~'test'");

JsonClassifier jc = JsonClassifier.apply(mps);

var a0 = jc.isSuccess();
var a1 = jc.isFail();
var a2 = jc.getFailReason();
var a3 = jc.show();

String someString = "target-string to find which pattern is matched to";
ClassifyResult ret = jc.search(someString);

var b0 = ret.isSuccess();
var b1 = ret.isFail();
var b2 = ret.getFailReason();
var b3 = ret.show();

```

```java
// Validate

import transform.java.JsonValidator;
import transform.java.ValidateResult;

JsonValidator jv = JsonValidator.apply("my-validate-rule-in-json-format");

var a0 = jv.isSuccess();
var a1 = jv.isFail();
var a2 = jv.getFailReason();
var a3 = jv.show();

ValidateResult ret = jv.validate("target-json");

var b0 = ret.isSuccess();
var b1 = ret.isFail();
var b2 = ret.getFailReason();
var b3 = ret.show();

```

```java

import transform.java.JsonConverter;
import transform.java.ConvertResult;

JsonConverter jc = JsonConverter.apply("my-convert-rule-in-json-format");

var a0 = jc.isSuccess();
var a1 = jc.isFail();
var a2 = jc.getFailReason();
var a3 = jc.show();

ConvertResult ret = jc.convert("target-json");

var b0 = ret.isSuccess();
var b1 = ret.isFail();
var b2 = ret.getFailReason();
var b3 = ret.show();

```
