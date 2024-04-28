package transform.java

trait ShowStatus {

  def isFail(): Boolean
  def isSuccess(): Boolean
  def getFailReason(): String
  def show(): String

}
