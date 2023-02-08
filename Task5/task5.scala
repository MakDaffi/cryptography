import java.io.Console
import java.lang.Math.sqrt
import scala.collection.immutable.ArraySeq
import scala.math.BigInt
import scala.util.control.Breaks._
import scala.util.Random

def prime_check(num: Long): Boolean = {
    val end: Double = sqrt(num.toDouble)
    for (i <- 2 to end.toInt) {
        if (num % i == 0) {
            return false
        }
    }
    return true
}

class El_Gamal() {

    private val keys = gen_new_keys()
    val y: Long = keys._1
    val g: Long = keys._2
    val p: Long = keys._3
    private val x: Long = keys._4

    private def gen_new_keys() : Tuple4[Long, Long, Long, Long] = {
        var start: Long = System.console().readLine("Введите начало интервала для генерации простого числа p : ").toLong
        var end: Long = System.console().readLine("Введите конец интервала для генерации простого числа p : ").toLong
        val rnd: Random = new Random
        var p: Long = 0
        breakable {
            while(true) {
                p = start + rnd.nextLong( (end - start) + 1 )
                if (prime_check(p)) {
                    break
                }
            }
        }
        var g: Long = 2
        breakable {
            while(true) {
                g = 2 + rnd.nextLong( (p - 3) + 1 )
                if (gen_check(g, p - 1)) {
                    break
                }
            }
        }
        start = 1
        end = p - 1
        var x: Long = start + rnd.nextLong( (end - start) + 1 )
        var y: Long = pow(g, x, p)
        return (y, g, p, x)
    }

    def encode(msg: Long): Tuple2[Long, Long] = {
        var h: Long = phi(msg)
        breakable {
            while (gcd(h, p - 1)._1 != 1) {
                h -= 1
            }
        }
        //var z: Long = find_rev_el(h, p - 1) * x % (p - 1)
        //var a: Long = pow(g, z, p)
        var h_1 = gcd(h, p - 1)._2 % (p - 1)
        if (h_1 < 0) {
            h_1 = p + h_1 - 1
        }
        return (pow(y, h_1, p), h)
    }

    def decoder(code: Tuple2[Long, Long]): Long = {
        val msg: Long = pow(code._1, code._2, p)
        return msg
    }
}

def phi (n : Long) : Long = {
    var n1: Long = n
    var result: Long = n
    var i: Long = 2
    while (i * i <= n1) {
        if (n1 % i == 0) {
            while (n1 % i == 0)
                n1 = n1 / i;
            result -= result / i;
        }
        i += 1
    }
    if (n1 > 1)
        result -= result / n1;
    return result
}

def pow(num : Long, deg : Long, mod : Long) : Long = {
    var ans: Long = 1
    var i: Long = 1
    while (i <= deg) {
        ans = ans * num % mod
        i += 1
    }
    return ans
}

def gcd (a: Long, b: Long): Tuple3[Long, Long, Long] = {
    if (a == 0) {
        return (b, 0, 1)
    }
    val p = gcd (b % a, a)  
    val d = p._1
    val x1 = p._2
    val y1 = p._3
    val x = y1 - (b / a) * x1
    val y = x1
    return (d, x, y)
}

def gen_check(g: Long, n: Long): Boolean = {
    var ans: Long = 1
    var i: Long = 1
    while (i <= n) {
        ans = ans * g % (n + 1)
        if (ans == 1) {
            break
        }
        i += 1
    }
    if (i == n) {
        return true
    } else {
        return false
    }
}

object Main extends App {
    try {
        var coder: El_Gamal = new El_Gamal()
        println("Ваш открытый ключ:")
        println("(" + coder.y.toString + ", " + coder.g.toString + ", " + coder.p.toString + ")")
        var messeg: Array[Long] = System.console().readLine("Введите сообщение: ").split(' ').map(_.toLong)
        var code: Array[Tuple2[Long, Long]] = Array()
        breakable { 
            for (msg <- messeg) {
                val c: Tuple2[Long, Long] = coder.encode(msg)
                if (c._1 == -1) {
                    break
                }
                code +:= c
            }
        }
        if (messeg.length == code.length) {
            println("Сгенерированная подпись: ")
            for (i <- 0 until code.length) {
                print(code(i).toString + " ")
            }
            println()
            var decode: Array[Long] = Array()
            for (msg <- code) {
                decode +:= coder.decoder(msg)
            }
            var f: Boolean = true
            println("Проверка подтверждения : ")
            for (i <- 0 until decode.length) {
                print(decode(i).toString + " ")
                if (coder.y != decode(i)) {
                    f = false
                }
            }
            println()
            if (f) {
                println("Подпись принята!")
            }
        }
        else {
            println("В сообщение присутствует чило большее p - 1")
        }
    } catch {
        case e: Exception => println("Ошибка ввода, похоже вы ввели не число!")
    }
}