package com.knoldus.kip.service

import com.knoldus.kip.RamDatabase
import com.knoldus.kip.models.Gender._
import com.knoldus.kip.models.{Marks, ScoreCard, Student}

trait CollectionAssignment {

  def computeScorecard(id: Long): ScoreCard = {
    val list: List[Marks] = RamDatabase.marksList.filter(id == _.studentId)

    val subjectList : List[Long] = list.map(_.subjectId.toLong)
    val subjectsCount: Int = subjectList.length

    val marksList: List[Float] = list.map(_.marksObtained)

    val percentage: Float = marksList.sum / subjectsCount

    val map: Map[Long, Float] = subjectList zip marksList toMap

    ScoreCard(id, map, percentage)
  }


  //Collection Based - Assignment 1
  def generateScorecards: Map[String, AnyRef] = {

    val names: List[String] = RamDatabase.studentList.map(_.name)

    val scorecards: List[ScoreCard] = for{ s <- RamDatabase.studentList}
      yield computeScorecard(s.id)

    names zip scorecards toMap
  }

  def getScorecardsByName(name: String): List[ScoreCard] = {

    val students: List[Student] = RamDatabase.studentList.filter(_.name == name).sortBy(_.id)
    val scorecardList: List[ScoreCard] = for{s <- students
                                              }yield computeScorecard(s.id)

    if(scorecardList.isEmpty) {

      throw new Exception("No Data Found")
    }
    else {
      scorecardList
    }

  }

  //Collection Based - Assignment 2
  def getScoreCardByGender: (List[ScoreCard], List[ScoreCard]) = {

    val maleStudents: List[Student] = RamDatabase.studentList.filter(_.gender == MALE).sortBy(_.id)
    val femaleStudents: List[Student] = RamDatabase.studentList.filter(_.gender == FEMALE).sortBy(_.id)

    val maleScorecard: List[ScoreCard] = for{s <- maleStudents
    }yield computeScorecard(s.id)

    val femaleScorecard: List[ScoreCard] = for{s <- femaleStudents
    }yield computeScorecard(s.id)

    (maleScorecard, femaleScorecard)
  }

  def getScoreCardsWithHigherPercentage: (List[ScoreCard], List[ScoreCard]) = {
    val t:(List[ScoreCard], List[ScoreCard]) = getScoreCardByGender

    val maleScorecard: List[ScoreCard] = t._1.filter(_.percentage >= 50)
    val femaleScorecard: List[ScoreCard] = t._2.filter(_.percentage >= 50)

    (maleScorecard, femaleScorecard)

  }

  def getSimilarPercentageBwGroups: List[((String, ScoreCard), (String, ScoreCard))] = {

    def getStudentName(st_Id : Long):String = RamDatabase.studentList.find(_.id==st_Id).get.name

    val (mScoreCard,fScoreCard)  =  getScoreCardByGender

    mScoreCard.flatMap(x => fScoreCard.filter(_.percentage==x.percentage).map(y=>
      ((getStudentName(x.studentId),x),(getStudentName(y.studentId),y))))
  }

  def femalePercentageNotInMales: List[(String, ScoreCard)] = {
    val t:(List[ScoreCard], List[ScoreCard]) = getScoreCardByGender

    val maleScorecard: List[ScoreCard] = t._1
    val femaleScorecard: List[ScoreCard] = t._2

    val selectedFemales: List[ScoreCard] =for{f <- femaleScorecard
                                              m <- maleScorecard
                                             if f.percentage != m.percentage
    }yield f

    val names: List[String] = selectedFemales.flatMap(x => RamDatabase.studentList.filter(x.studentId == _.id).map(_.name))

    names.zip(selectedFemales)
  }

}
