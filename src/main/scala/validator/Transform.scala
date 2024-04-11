package validator

import org.json4s.{DefaultFormats, JValue}
import org.json4s.JsonAST.JArray

import scala.collection.immutable.HashMap
import scala.io.Source.fromResource

object Transform {

}

////////////////////////////////////////////////////////////////////////////////
// Target Mapping
////////////////////////////////////////////////////////////////////////////////
sealed trait TMap {
  def take(j: JValue): Either[String, JValue]
}

////
case class TMapField(from: String) extends TMap {

  val l : List[String] = ???

  // direct path from where ?
  //
  // \a.b.c.d
  // \[3].b.c
  //
  // composition direct path
  // \[3].b + \[3].d
  //
  // [ array ]
  // mapping
  // reduce
  // reduce of reduce ..??? later

  override def take(j: JValue)  // parent node
  : Either[String, JValue] = Right(j)
}

case class TMapArray(obj: TMap ) extends TMap {

  // from JArray : mapping
  override def take(j: JValue)
  : Either[String, JValue] = {

    val ret = j.getArrayValues().flatMap { l =>
      val (lefts, rights) = l.map(obj.take).partitionMap(identity)

      if(lefts.nonEmpty)
        Left( lefts.mkString(","))
      else
        Right( JArray( rights))
    }
    ret
  }
}

case class TMapObject(fields: Map[String, TMap]) extends TMap {

  // from JObject : mapping
  override def take(j: JValue)
  : Either[String, JValue] = {
    Right(j)
  }
}

object TMapField {
  implicit def stringToMappingField(s: String) = {
    TMapField(s)
  }
}

object TMapObject {
  def apply(elems: (String, TMap)*): TMapObject = {
    new TMapObject( HashMap( elems:_*))
  }
}

////////////////////////////////////////////////////////////////////////////////
// Lemon
////////////////////////////////////////////////////////////////////////////////
object Lemon {

  import TMapField.stringToMappingField

  /////////////////////////
  // Target Mapping Rules
  /////////////////////////
  val TDiags = TMapArray(
    TMapObject(
      "deptCd"          -> "deptCd",
      "deptNm"          -> "deptNm",
      "diagnosisCd"     -> "diagnosisCd",
      "diagnosisDt"     -> "diagnosisDt",
      "diagnosisNm"     -> "diagnosisNm",
      "doctorLicense"   -> "doctorLicense",
      "doctorNm"        -> "doctorNm",
      "insuranceClaimCd"-> "insuranceClaimCd",
      "mainDiagnoYn"    -> "mainDiagnoYn",
      "medicalOpinion"  -> "medicalOpinion",
      "sequenceNo"      -> "sequenceNo",
      "surgeryYn"       -> "surgeryYn",
      "rgstHspKeyInfo"  -> "rgstHspKeyInfo",
      "hospitalCd"      -> "hospitalCd",
      "patientNo"       -> "patientNo",
      "hspClncInsText"  -> "hspClncInsText",
      "medSvcfeeApplCd" -> "medSvcfeeApplCd",
      "receiptNo"       -> "receiptNo",
      "ioFlag"          -> "ioFlag",
    )
  )

  val TPrescriptions = TMapArray(
    TMapObject(
      "age"				            -> "age"								  ,	// "72",
      "deptCd"			          -> "deptCd"								,	// "ABBCAA",
      "deptNm"			          -> "deptNm"								,	// "심장 내과",
      "doctorLicense"		      -> "doctorLicense"				,	// "70173",
      "doctorNm"			        -> "doctorNm"							,	// "김원호",
      "dosageFormCd"		      -> "dosageFormCd"					,	// "",
      "doseDt"			          -> "doseDt"								,	// "20210907",
      "doseNum"			          -> "doseNum"							,	// "263",
      "doseQtyPer1Day"		    -> "doseQtyPer1Day"				,	// "1",
      "doseQtyPer1Tim"		    -> "doseQtyPer1Tim"				,	// "1.00",
      "doseRouteCls"		      -> "doseRouteCls"					,	// "20210907^ABBCAA^20090270^M^263",
      "doseTotalQty"		      -> "doseTotalQty"					,	// "180.00",
      "doseUnit"			        -> "doseUnit"							,	// "",
      "drugDetail"			      -> "drugDetail"						,	// "아침 식후 30분에 복용하세요 ※ 심장내과 처방약 한봉지에 포장",
      "drugMethCd"			      -> "drugMethCd"						,	// "BFPC",
      "ediCd"			            -> "ediCd"								,	// "647802630",
      "emergencyYn"			      -> "emergencyYn"					,	// "",
      "enforceDt"			        -> "enforceDt"						,	// "",
      "expensivePriceYn"		  -> "expensivePriceYn"			,	// "",
      "firstTreatYn"		      -> "firstTreatYn"					,	// "N",
      "gender"			          -> "gender"								,	// "M",
      "identificationNo"		  -> "identificationNo"			,	// "5108281345312",
      "insuranceCd"			      -> "insuranceCd"					,	// "H",
      "insuranceClaimCd"		  -> "insuranceClaimCd"			,	// "20210907^ABBCAA^20090270^M^263",
      "insurant"			        -> "insurant"							,	// "",
      "insurantIdentityNo"		-> "insurantIdentityNo"		,	// "",
      "insureCardNo"		      -> "insureCardNo"					,	// "10580348901",
      "insureRelCls"		      -> "insureRelCls"					,	// "5",
      "insurer"			          -> "insurer"							,	// "",
      "itemNm"			          -> "itemNm"								,	// "플래리스정 75mg(SJ)",
      "medicalChrg"			      -> "medicalChrg"					,	// "647802630",
      "orderDt"			          -> "orderDt"							,	// "20210907",
      "orderOut"			        -> "orderOut"							,	// "2",
      "orderPrintCnt"		      -> "orderPrintCnt"				,	// "",
      "orderStatus"			      -> "orderStatus"					,	// "",
      "patientId"			        -> "patientId"						,	// "01168847",
      "patientNm"			        -> "patientNm"						,	// "김재학",
      "paymentPerCd"		      -> "paymentPerCd"					,	// "  ",
      "powderYn"			        -> "powderYn"							,	// "",
      "prcpMixNo"			        -> "prcpMixNo"						,	// "",
      "prescriptionAmt"		    -> "prescriptionAmt"			,	// "",
      "prescriptionNo"		    -> "prescriptionNo"				,	// "",
      "prescriptionPageNum"		-> "prescriptionPageNum"	,	// "",
      "prescriptionPeriod"		-> "prescriptionPeriod"		,	// "",
      "prescriptionValidDt"		-> "prescriptionValidDt"	,	// "3",
      "pretreatOfDocOfficeYn"	-> "pretreatOfDocOfficeYn",	// "",
      "printDt"			          -> "printDt"							,	// "",
      "receiptNo"			        -> "receiptNo"						,	// "3",
      "representativeTel"		  -> "representativeTel"		,	// "1899-0001",
      "selfPaymentCd"		      -> "selfPaymentCd"				,	// "",
      "specialCaseAppCalYn"		-> "specialCaseAppCalYn"	,	// "",
      "specialCd"			        -> "specialCd"						,	// "",
      "specialDosageCd"		    -> "specialDosageCd"			,	// "1",
      "specialNm"			        -> "specialNm"						,	// "",
      "subInsuranceCd"		    -> "subInsuranceCd"				,	// "-",
      "treatCls"			        -> "treatCls"							,	// "O",
      "visitDt"			          -> "visitDt"							,	// "20210907",
      "zipCd"			            -> "zipCd"								,	// "34188",
      "zipCdTxt"			        -> "zipCdTxt"							,	// "대전광역시 유성구 도안대로 560 110-1408(봉명동, 도안마을1단지)",
      "rgstHspKeyInfo"		    -> "rgstHspKeyInfo"				,	// "0116884720210907ABBCAAMA3O",
      "hospitalCd"			      -> "hospitalCd"						,	// "34100041",
      "patientNo"			        -> "patientNo"						,	// "01168847",
      "hspClncInsText"		    -> "hspClncInsText"				,	// "건강보험",
      "medSvcfeeApplCd"		    -> "medSvcfeeApplCd"			,	// "H",
      "ioFlag"			          -> "ioFlag"								,	// "O"
    )
  )

  val TReceipts = TMapArray(
    TMapObject(
      "billNo"                 -> "billNo",
      "deptCd"                 -> "deptCd",
      "deptNm"                 -> "deptNm",
      "discountAmt"            -> "discountAmt",
      "discountRmk"            -> "discountRmk",
      "diseaseGrpNo"           -> "diseaseGrpNo",
      "endDt"                  -> "endDt",
      "exceptSelectMedicalAmt" -> "exceptSelectMedicalAmt",
      "fullPatientAmt"         -> "fullPatientAmt",
      "insuranceClaimCd"       -> "insuranceClaimCd",
      "insureAmt"              -> "insureAmt",
      "insureKindNm"           -> "insureKindNm",
      "nightHolidayYn"         -> "nightHolidayYn",
      "patientAmt"             -> "patientAmt",
      "patientTotalAmt"        -> "patientTotalAmt",
      "paymentTargetAmt"       -> "paymentTargetAmt",
      "preAmt"                 -> "preAmt",
      "realAmt"                -> "realAmt",
      "receiptNo"              -> "receiptNo",
      "receiptTyNm"            -> "receiptTyNm",
      "receivableAmt"          -> "receivableAmt",
      "receivableRmk"          -> "receivableRmk",
      "roomCls"                -> "roomCls",
      "roomNm"                 -> "roomNm",
      "selectMedicalAmt"       -> "selectMedicalAmt",
      "startDt"                -> "startDt",
      "totalAmt"               -> "totalAmt",
      "upperLmtExcdAmt"        -> "upperLmtExcdAmt",
      "rgstHspKeyInfo"         -> "rgstHspKeyInfo",
      "hospitalCd"             -> "hospitalCd",
      "patientNo"              -> "patientNo",
      "hspClncInsText"         -> "hspClncInsText",
      "medSvcfeeApplCd"        -> "medSvcfeeApplCd",
      "ioFlag"                 -> "ioFlag",
      "visitDt"                -> "visitDt",
      "certificateDocId"       -> "certificateDocId",
    )
  )

  val TReceiptDetails = TMapArray(
    TMapObject(
      "accumulatedLclsCd"      -> "accumulatedLclsCd",
      "accumulatedLclsNm"      -> "accumulatedLclsNm",
      "doseQtyPer1Day"         -> "doseQtyPer1Day",
      "doseQtyPer1Tim"         -> "doseQtyPer1Tim",
      "ediCd"                  -> "ediCd",
      "enforceDt"              -> "enforceDt",
      "exceptSelectMedicalAmt" -> "exceptSelectMedicalAmt",
      "fullPatientAmt"         -> "fullPatientAmt",
      "insuranceClaimCd"       -> "insuranceClaimCd",
      "insuranceTargetCd"      -> "insuranceTargetCd",
      "insureAmt"              -> "insureAmt",
      "itemNm"                 -> "itemNm",
      "medicalChrg"            -> "medicalChrg",
      "memo"                   -> "memo",
      "patientAmt"             -> "patientAmt",
      "patientTotalAmt"        -> "patientTotalAmt",
      "selectMedicalAmt"       -> "selectMedicalAmt",
      "sequenceNo"             -> "sequenceNo",
      "totalAmt"               -> "totalAmt",
      "unitCost"               -> "unitCost",
      "visitDt"                -> "visitDt",
      "rgstHspKeyInfo"         -> "rgstHspKeyInfo",
      "hospitalCd"             -> "hospitalCd",
      "patientNo"              -> "patientNo",
      "deptCd"                 -> "deptCd",
      "hspClncInsText"         -> "hspClncInsText",
      "medSvcfeeApplCd"        -> "medSvcfeeApplCd",
      "receiptNo"              -> "receiptNo",
      "ioFlag"                 -> "ioFlag",
      "startDt"                -> "startDt",
      "certificateDocId"       -> "certificateDocId",
    )
  )

  val TReceiptItems = TMapArray(
    TMapObject(
      "accumulatedLclsCd"      -> "accumulatedLclsCd",
      "accumulatedLclsNm"      -> "accumulatedLclsNm",
      "billNo"                 -> "billNo",
      "exceptSelectMedicalAmt" -> "exceptSelectMedicalAmt",
      "fullPatientAmt"         -> "fullPatientAmt",
      "insuranceClaimCd"       -> "insuranceClaimCd",
      "insureAmt"              -> "insureAmt",
      "patientAmt"             -> "patientAmt",
      "patientTotalAmt"        -> "patientTotalAmt",
      "selectMedicalAmt"       -> "selectMedicalAmt",
      "sequenceNo"             -> "sequenceNo",
      "totalAmt"               -> "totalAmt",
      "rgstHspKeyInfo"         -> "rgstHspKeyInfo",
      "hospitalCd"             -> "hospitalCd",
      "patientNo"              -> "patientNo",
      "deptCd"                 -> "deptCd",
      "hspClncInsText"         -> "hspClncInsText",
      "medSvcfeeApplCd"        -> "medSvcfeeApplCd",
      "receiptNo"              -> "receiptNo",
      "ioFlag"                 -> "ioFlag",
      "startDt"                -> "startDt",
      "visitDt"                -> "visitDt",
      "certificateDocId"       -> "certificateDocId",
    )
  )

  val THospital = TMapObject(
    "diagList"         -> TDiags,
    "prescriptionList" -> TPrescriptions,
    "receipt"          -> TReceipts,
    "receiptDetail"    -> TReceiptDetails,
    "receiptItem"      -> TReceiptItems,
  )

  val TBillInfo = TMapObject(
    "billNo"             -> "billNo",
    "deptCd"             -> "deptCd",
    "deptNm"             -> "deptNm",
    "insuranceCd"        -> "insuranceCd",
    "insuranceClaimCd"   -> "insuranceClaimCd",
    "insuranceNm"        -> "insuranceNm",
    "patientTotalAmt"    -> "patientTotalAmt",
    "receiptNo"          -> "receiptNo",
    "totalAmt"           -> "totalAmt",
    "treatCls"           -> "treatCls",
    "visitDt"            -> "visitDt",
    "visitTm"            -> "visitTm",
    "hospitalCd"         -> "hospitalCd",
    "hospitalClsNo"      -> "hospitalClsNo",
    "hospitalNm"         -> "hospitalNm",
    "patientNo"          -> "patientNo",
    "addItem1"           -> "addItem1",
    "addItem2"           -> "addItem2",
    "addItem3"           -> "addItem3",
    "addItem4"           -> "addItem4",
    "addItem5"           -> "addItem5",
    "dgnCd"              -> "dgnCd",
    "dgnNm"              -> "dgnNm",
    "doctorId"           -> "doctorId",
    "subTreatListId"     -> "subTreatListId",
    "insuClaimTreatsId"  -> "insuClaimTreatsId",
    "certificateDocId"   -> "certificateDocId",
    "startDt"            -> "startDt",
    "endDt"              -> "endDt",
    "hospialData"        -> THospital
  )

  val TBillInfos = TMapArray(
    TBillInfo
  )

}

object SpecLemon extends App {

  val jsonString = fromResource("lemon\\to.json").mkString

  implicit val formats: DefaultFormats.type = DefaultFormats

  val root = jsonString.toJValue()

  val one = root.getArrayValues().map( _.head)

  def get(j: JValue, route: String)
  : JValue = {
    println(route.split('.').mkString(" -> "))
    j.getJValue0(route.split('.'))
  }

  def showMap(prefix: String= "\t")
  : Map[String,JValue] => Map[String,JValue] = m => {
    println(s"$prefix----------------------------------------")
    m.foreach{ case (k, v) => println( s"$prefix$k : $v")}
    m
  }


  ////////////////////////////////////////////////////////////////////////////////
  //  one.map( _.takeJFields0(TBillInfo)).map(showMap())

  ////////////////////////////////////////////////////////////////////////////////
  //  one.flatMap( j => get(j, KDiags).getJArray())
  //    .map( _.map(  _.takeJFields0(TDiags)).map(showMap()) )
  //
  //  println()
  //  one.flatMap( j => get(j, KPrescriptions).getJArray() )
  //    .map( _.map(  _.takeJFields0(TPrescription)).map(showMap()) )
  //
  //  println()
  //  one.flatMap( j => get(j, KReceipts).getJArray() )
  //    .map( _.map(  _.takeJFields0(TReceipt)).map(showMap()) )
  //
  //  println()
  //  one.flatMap( j => get(j, KReceiptDetails).getJArray() )
  //    .map( _.map(  _.takeJFields0(TReceiptDetail)).map(showMap()) )
  //
  //  println()
  //  one.flatMap( j => get(j, KReceiptItems).getJArray() )
  //    .map( _.map(  _.takeJFields0(TReceiptItem)).map(showMap()) )
}


