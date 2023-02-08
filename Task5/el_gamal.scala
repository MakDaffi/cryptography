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
        var ph: Long = phi(p)
        var g: Long = 2
        breakable {
            while(true) {
                if (pow(g, ph, p) == 1) {
                    break
                }
                g += 1
            }
        }
        start = 1
        end = p - 1
        var x: Long = start + rnd.nextLong( (end - start) + 1 )
        var y: Long = pow(g, x, p)
        return (y, g, p, x)
    }

    def encode(msg: Long): Tuple2[Long, Long] = {
        var start: Long = 1
        var end: Long = p - 1
        val rnd: Random = new Random
        var k: Long = 0
        breakable {
            while(true) {
                k = start + rnd.nextLong( (end - start) + 1 )
                if (gcd(k, p - 1) == 1) {
                    break
                }
            }
        }
        if (msg >= p - 1) {
            return (-1, -1)
        }
        var a: Long = pow(g, k, p)
        var b: Long = pow(y, k, p) * msg % p
        return (a, b)
    }

    def decoder(code: Tuple2[Long, Long]): Long = {
        val msg = pow(code._1, p - 1 - x, p) * code._2 % p
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

def gcd(a : Long, b : Long) : Long = {
    if (b == 0) {
        return a
    } else {
        return gcd(b, a % b)
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
            println("Зашифрованное сообщение")
            for (i <- 0 until code.length) {
                print(code(i).toString + " ")
            }
            println()
            var decode: Array[Long] = Array()
            for (msg <- code) {
                decode +:= coder.decoder(msg)
            }
            println("Расшифрованное сообщение : ")
            for (i <- 0 until decode.length) {
                print(decode(i).toString + " ")
            }
            println()
        }
        else {
            println("В сообщение присутствует чило большее p - 1")
        }
    } catch {
        case e: Exception => println("Ошибка ввода, похоже вы ввели не число!")
    }
}