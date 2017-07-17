package com.knoldus.kip.service

import com.knoldus.kip.RamDatabase
import com.knoldus.kip.models.{Marks, Student}

trait ListAssignment {

  //Assignment -1
  def failedStudents(subjectId: Long, percentage: Double, passOrFail: String): Int = {

    val passCount = RamDatabase.marksList.count(marks => marks.subjectId == subjectId && marks.marksObtained >= percentage)
    val failCount = RamDatabase.marksList.count(marks => marks.subjectId == subjectId && marks.marksObtained < percentage)

    if(passOrFail.toLowerCase.equals("pass")){

      passCount

    }

    else {

      failCount

    }
  }


  def topBottomStudents(subjectId: Long, count: Int, topOrBottom: String): List[Student] = {

    val sortedMarksList:List[Marks]  = RamDatabase.marksList.filter(_.subjectId == subjectId).sortBy(_.marksObtained)

    def listStudents(marks: List[Marks]) : List[Student] = {

      for {mark <- marks
           student <- RamDatabase.studentList
           if student.id == mark.studentId
      } yield student

    }

    topOrBottom.toLowerCase match {

      case "top" => listStudents(sortedMarksList.reverse.take(count))
      case "bottom" => listStudents(sortedMarksList.take(count))

    }
  }


  def topAndLeastScorers(topOrBottom: String, count: Int): List[Student] = {

  val totalMarks : List[Float] = {

    for(student <- RamDatabase.studentList)

      yield RamDatabase.marksList.takeWhile(_.studentId == student.id).map(_.marksObtained).sum

  }

   val studentNameWithMarks = RamDatabase.studentList.zip(totalMarks).sortBy(_._2)

    topOrBottom.toLowerCase match{

      case "top" => studentNameWithMarks.reverse.take(count).map(_._1)
      case "bottom" => studentNameWithMarks.take(count).map(_._1)

    }

  }


def getScholarshipGroups(percentage: Float, goodScholarship: Int, normalScholarship: Int): (List[(Student, Int)], List[(Student, Int)]) = {

  val inputPercentage = percentage*5

  val studentIdWithMarks = RamDatabase.studentList.map(_.id).zip(RamDatabase.studentList.map(x => RamDatabase.marksList.
    filter(x.id == _.studentId).map(_.marksObtained).sum))

  val (studentIdWithGoodScholarship,studentIdWithNormalScholarship) = studentIdWithMarks.partition(_._2>=inputPercentage)


  val studentWithGoodScholarship = RamDatabase.studentList.filter(studentIdWithGoodScholarship.map(_._1) contains _.id)

  val studentWithNormalScholarship = RamDatabase.studentList.filter(studentIdWithNormalScholarship.map(_._1) contains _.id)

  val listOfGoodScholarship = List.fill(studentWithGoodScholarship.length)(goodScholarship)

  val listOfNormalScholarship = List.fill(studentWithNormalScholarship.length)(normalScholarship)

  val zippedListGoodScholarship = studentWithGoodScholarship zip listOfGoodScholarship

  val zippedListNormalScholarship = studentWithNormalScholarship zip listOfNormalScholarship
  (zippedListGoodScholarship, zippedListNormalScholarship)
}


  def passedOrFailed(passOrFail: String, percentage: Float): List[Student] = {

    val totalMarks:List[Float]={

      for(student<- RamDatabase.studentList)
        yield RamDatabase.marksList.takeWhile(_.studentId==student.id).map(_.marksObtained).sum
    }

    val percentages=totalMarks.map(_/5)

    passOrFail.toLowerCase match {

      case "pass"=>val studentNameMorePercentage=RamDatabase.studentList.zip(percentages).filter(_._2>=percentage)
        studentNameMorePercentage.unzip._1

      case "fail"=>
        val studentNameLessPercentage=RamDatabase.studentList.zip(percentages).filter(_._2<percentage)

        studentNameLessPercentage.unzip._1

    }

  }


  def studentsWithMoreThan95: List[Student] = {

    val totalMarks:List[Float]={

      for(student<- RamDatabase.studentList)
        yield RamDatabase.marksList.takeWhile(_.studentId==student.id).map(_.marksObtained).sum
    }

    val percentage=totalMarks.map(_/5)

    val studentNameWithMarks=RamDatabase.studentList.zip(percentage).filter(_._2>95)

    studentNameWithMarks.unzip._1

  }

  def generateReport: List[(String, List[Int])] = {  //Must use the groupBy() Method of the List

    val marks: List[List[Int]] = RamDatabase.studentList.map(x => RamDatabase.marksList.groupBy(_.studentId == x.id)(true)
      .map(_.marksObtained.toInt))

    val student_name = RamDatabase.studentList.map(_.name)

    student_name.zip(marks)

  }






  //Assignment - 2
  def getLastElementWithIndex(list: List[String]): (String, Int) = {

    def lengthOfList[A](l:List[A]):Int = l match {
      case Nil => 0
      case h::tail => 1 + lengthOfList(tail)
    }

    val l = lengthOfList(list)
    (list(l-1), lengthOfList(list))

  }

  def printTable(list: List[Long]): List[Long] = {

   val index : List[Int] = (for(counter <- 1 to 10)
     yield  counter).toList

    index.flatMap(item => list.map(_*item))

  }


  def aggregateLists(list1: List[String], list2: List[Long]): List[List[(String, Long)]] = {

    list1.zip(list2).map(item => List(item))

  }

  def getSumOfList(list: List[Long]): Long = list match{

    case head :: tail => head + getSumOfList(tail)
    case Nil => 0

  }

  def getMultiplicationOfList(list: List[Long]) : Long = list match{

    case head :: tail => head * getSumOfList(tail)
    case head :: Nil => 1
    case Nil => 0

  }

  def quickSortList(list: List[Long]): List[Long] = list match {

    case head :: tail => quickSortList(tail.filter(_< head)) ::: head :: quickSortList(tail.filter(_>=head))
    case Nil => Nil

  }

  def mergeSortList(list: List[Long]):List[Long] = {

    val mid = list.length/2
    if (mid == 0){
      list
    }
    else {
      val (left, right) = list.splitAt(mid)
      merge(mergeSortList(left), mergeSortList(right))
    }
  }

  def merge(left: List[Long], right: List[Long]) : List[Long] =(left, right) match{

    case (left,Nil) => left
    case (Nil,right) => right
    case (leftHead :: leftTail, righthead :: rightTail) => if(leftHead  < righthead){
      leftHead :: merge(leftTail, right)
    }
    else {
      righthead :: merge(left, rightTail)
    }

  }

}
