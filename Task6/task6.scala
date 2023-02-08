import java.io.Console
import scala.util.control.Breaks._
import scala.collection.mutable.{HashMap, Set}
import scala.math.sqrt

def brute_force(n : Int, q : Int): Unit = {
    // var table = HashMap[Int, Set[Int]]()
    var ans: Vector[Int] = Vector()
    for (i <- 1 until n) {
        var a: Int = 0
        var b: Int = i
        var s: Set[Int] = Set()
        for (j <- 1 to n) {
            a = (a + b) % n
            s += a
        }
        if (s.size == q) {
            ans :+= i
            println("С помощью элемента группы " + i.toString + ", использованного в качестве генератора получается следующая подгруппа порядка " + q.toString +":")
            println(s)
        }
        // table(i) = s
    }
    println("Геннераторы подгруппы порядка " + q.toString + " группы порядка " + n.toString + ":")
    println(ans)
    // println(table)
}

def order_table(n: Int, q: Int): Unit = {
    var table = HashMap[Int, Int]()
    for (i <- 1 until n) {
        var cur_elem: Int = 0
        breakable {
            for (j <- 1 to n) {
                cur_elem = (cur_elem + i) % n
                if (cur_elem == 0) {
                    table(i) = j
                    break
                }
            }
        }
    }
    // println(table)
    var ans: Vector[Int] = Vector()
    for (i <- table) {
        if (i._2 == q) {
            ans :+= i._1
        }
    }
    for (i <- ans) {
        var s: Set[Int] = Set()
        var a: Int = 0
        for (j <- 1 to q) {
            a = (a + i) % n
            s += a
        }
        println("С помощью элемента группы " + i.toString + ", использованного в качестве генератора получается следующая подгруппа порядка " + q.toString +":")
        println(s)
    }
    println("Геннераторы подгруппы порядка " + q.toString + " группы порядка " + n.toString + ":")
    println(ans)
}

object Main extends App {
    try {
        val n: Int = System.console().readLine("Введите  порядок группы n: ").toInt
        val q: Int = System.console().readLine("Введите  порядок подгруппы q, который является делителем порядка группы n: ").toInt
        if (n % q != 0) {
            println("Число q, введенное вами, не является делителем n!")
        } else {
            val str: String = "Введите способ генерации подгруппы порядка " + q.toString + " для группы порядка " + n.toString + " (0 - брут форс, 1 - таблица порядков): "
            val f: Int = System.console().readLine(str).toInt
            f match {
                case 0 => {
                    brute_force(n, q)
                }
                case 1 => {
                    order_table(n, q)
                }
                case _ => {
                println("Введеного способа не существует!")
            }
            }
        }
    } catch {
        case e: Exception => println("Вы ввели не число!")
    }
}