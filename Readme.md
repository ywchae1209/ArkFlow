# 설명
## JsonValidator
JSON 데이터가 특정한 규칙을 지키는 지 여부를 점검하는 기능

### 특징
1. Json의 데이터 구조( N-depth Tree)를 그대로 규칙에 반영할 수 있음.
2. Json의 구성요소( Object, Array, Value)별로 규칙점검이 이루어짐.
3. 점검오류/실패는 점검규칙 생성시, 규칙오류( Rule-Syntax-Error)와 점검시 실패( Evaluation-Fail)로 구분됨.
4. 점검오류/실패는 점검규칙과 마찬가지로 Tree 구조를 가지며, 그 내역을 Json으로 제공함.

------------------

### 점검 규칙 설명

#### Value Validation Rule

##### Value Validation 사례
- "_longerThan(8) && _shorterThan(20)"  
   8~20길이의 문자열. 논리 연산자(&&, ||를 사용할 수 있음)   
   _(underscore)로 시작하는 함수들은 문자열값에 적용하는 함수.


- "gt(100) && lt(200) || gt(200) && lt(300)"   
   100~200 또는 200~300인 숫자값  
   &&연산자가 ||연산자보다 우선순위가 높음.


- "_oneOf(this, is, good) || ( _gt(100) && _lt(200) || lt(200))"
  



    

