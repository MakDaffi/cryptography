import java.io.Console
import scala.util.control.Breaks._
import scala.math.BigInt
import scala.collection.mutable.Stack
import scala.util.Random

def gcd(a : BigInt, b : BigInt) : BigInt = {
    if (b == 0) {
        return a
    } else {
        return gcd(b, a % b)
    }
}

def sqrt(n: BigInt) : BigInt = {
  val d = BigDecimal(n)
  var a = BigDecimal(1.0)
  var b = d
  while(b - a >= 0) {
    val mid = (a + b) / 2
    if (mid * mid - d > 0) {
        b = mid - 0.0001
    }
    else {             
        a = mid + 0.0001
    }
  }
  return b.toBigInt
}

def cout_answer(answers: Vector[BigInt], alg: String, a: BigInt) : Unit = {
    println("Вы использовали " + alg + " для разложения числа " + a.toString + " на простые множители")
    if (answers.length == 1) {
        println("Ваше число является простым, поэтому его нельзя разложить на простые множители!")
    }
    else {
        print(a.toString + " = ")
        for (i <- 0 until answers.length - 1) {
            print(answers(i).toString + " * ")
        }
        print(answers(answers.length - 1))
    }
}

def naive_method(a: BigInt): Vector[BigInt] = {
    var answers: Vector[BigInt] = Vector()
    var n: BigInt = a
    var i: BigInt = 2
    while (n > 1) {
        while (n % i == 0) {
            answers :+= i
            n = n / i
        }
        i += 1
    }
    return answers
}

def fermat_method(a: BigInt): Tuple2[BigInt, BigInt] = {
    var x: BigInt = sqrt(a)
    var y: BigInt = 0
    breakable {
        while (true) {
            y = x * x - a
            var b: BigInt = sqrt(y)
            if (b * b == y | (b + 1) * (b + 1) == y) {
                break
            }
            x += 1
        }
    }
    var b: BigInt = sqrt(y)
    if ((b + 1) * (b + 1) == y) {
        b = b + 1
    }
    return (x + b, x - b)
}

def rho_pollard(a: BigInt) : BigInt = {
    if (a > 2) {
        var ans: BigInt = 1
        breakable {
            for (j <- 0 to 10) {
                val start = 1
                val end = (a - 2).min(1000000000)
                val r = new scala.util.Random
                var x: BigInt = start + r.nextLong( (end.toLong - start) + 1 )
                var y: BigInt = 1 
                var i: BigInt = 0
                var stage: BigInt = 2
                while (gcd(a, (x - y).abs) == 1)
                {
                    if (i == stage){
                        y = x
                        stage = stage * 2;
                    }
                    x = (x * x + 1) % a
                    i = i + 1
                }
                var b: BigInt = gcd(a, (x-y).abs)
                if (b != a | j == 10) {
                    ans = b
                    break
                }
            }
        }
        if (a == 4) {
            return 2
        }
        else return ans
    }
    return 1
}

object Main extends App {
    try {
        var a: BigInt = BigInt(System.console().readLine("Введите число, которое хотите разложить на множители: "))
        val f: Int = System.console().readLine("Введите каким методом вы хотите разложить число: наивный, Ферма или Ро-Полларда (0, 1, 2) (Не рекомендуется использовать первые два метода для больших чисел): ").toInt
        f match {
            case 0 => {
                val answers: Vector[BigInt] = naive_method(a)
                cout_answer(answers, "наивный подход", a)
            }
            case 1 => {
                var answers: Vector[BigInt] = Vector()
                val n: BigInt = a
                while (a % 2 == 0) {
                    a = a / 2
                    answers :+= 2
                }
                var s = Stack[BigInt]()
                if (a != 1) {
                    s.push(a)
                }
                while (!(s.isEmpty)) {
                    val cur: BigInt = s.pop
                    val p = fermat_method(cur)
                    if (p._2 == 1) {
                       answers :+= p._1 
                    }
                    else {
                        s.push(p._1)
                        s.push(p._2)
                    }
                }
                cout_answer(answers, "метод Ферма", n)
            }
            case 2 => {
                var answers: Vector[BigInt] = Vector()
                var s = Stack[BigInt]()
                s.push(a)
                while (!(s.isEmpty)) {
                    val cur: BigInt = s.pop
                    val p: BigInt = rho_pollard(cur)
                    if (p == cur | p == 1) {
                       answers :+= cur
                    }
                    else {
                        s.push(p)
                        s.push(cur / p)
                    }
                }
                cout_answer(answers, "ро-алгоритм Полларда", a)
            }
            case _ => {
                println("Вы ввели число метода не корректно!")
            }
        }
    } catch {
        case e: Exception => println("Вы ввели не число!")
    }
}