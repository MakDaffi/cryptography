import java.io.Console
import scala.util.control.Breaks._
import scala.collection.mutable.HashMap
import scala.math.sqrt

def simple_check(num: Int): Boolean = {
    val end: Double = sqrt(num.toDouble)
    for (i <- 2 to end.toInt) {
        if (num % i == 0) {
            return false
        }
    }
    return true
}

object Main extends App {
    try {
        val l: Int = System.console().readLine("Введите  количество элементов простого конечного поля: ").toInt
        if (simple_check(l) && l > 1) {
            var table = HashMap[Int, Int]()
            for (i <- 1 until l) {
                var cur_elem: Int = 1
                breakable {
                    for (j <- 1 until l) {
                        cur_elem = cur_elem * i % l
                        if (cur_elem == 1) {
                            table(i) = j
                            break
                        }
                    }
                }
            }
            println("Генераторы поля:")
            for (i <- table) {
                if (i._2 == l - 1) {
                    println(i._1)
                }
            }
            val num_to_check: Int = System.console().readLine("Введите число, которое вы хотите проверить: ").toInt
            if (table(num_to_check) == l - 1) {
                println("Введеное число является примитивным корнем!")
            } else {
                println("Введеное число не является примитивным корнем!")
            }
        } else {
            println("Похоже ваше поле не является простым!")
        }
    } catch {
        case e: Exception => println("Вы ввели не число!")
    }
}