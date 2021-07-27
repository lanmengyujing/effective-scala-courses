import scala.util.Failure
import scala.util.Success
import java.time.{LocalDate, Period}

import scala.util.Try

type Errors = Seq[String]

type Validated[A] = Either[Errors, A]

def parseDateV2(str: String): Validated[LocalDate] =
  Try(LocalDate.parse(str)).match {
    case Failure(exception) => Left(Seq(exception.toString()))
    case Success(date) => Right(date)
  }

def parseDate(str: String): Validated[LocalDate] =
  Try(LocalDate.parse(str)).toEither
    .left.map(error => Seq(error.getMessage))

def validatePeriod(str1: String, str2: String): Validated[Period] =
  parseDate(str1).flatMap { date1 =>
    parseDate(str2).map { date2 =>
      Period.between(date1, date2)
    }
  }

validatePeriod("not a date", "not a date either")


def validateBoth[A, B](
  validatedA: Validated[A],
  validatedB: Validated[B]
): Validated[(A, B)] =
(validatedA, validatedB) match {
  case (Right(a), Right(b))=> Right((a,b))
  case (Left(e), Right(b)) => Left(e)
  case (Right(a), Left(e)) => Left(e)
  case (Left(a), Left(b)) => Left(a++b)
}


def parseDates(dates: Seq[String]): Validated[Seq[LocalDate]] = 
  dates.foldLeft[Validated[List[LocalDate]]](Right(Nil)) {
   (validatedDates, str) =>
      val validatedDate: Validated[LocalDate] = parseDate(str)
      validateBoth(validatedDate, validatedDates)
      .map((date, dateResults)=> dateResults :+ date)
  }

def validateEach[A,B](items: Seq[A])(f: A =>Validated[B]): Validated[Seq[B]] = 
  items.foldLeft[Validated[List[B]]](Right(Nil)) {
   (validatedItems, item) =>
      val validatedData: Validated[B] = f(item)
      validateBoth(validatedData, validatedItems)
      .map((data, results)=> results :+ data)
  }  

validateEach(List("2020-01-04", "2020-08-09"))(parseDate)
parseDates(List("2020-01-04", "2020-08-09"))
parseDates(List("not a date", "2020-09-13", "not a date either"))
