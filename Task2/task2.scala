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

def pow(num : Long, deg : Long, mod : Long) : Long = {
    var ans: Long = 1
    var i: Long = 1
    while (i <= deg) {
        ans = ans * num % mod
        i += 1
    }
    return ans
}

class RSA() {

    private val keys: Tuple3[Long, Long, Long] = gen_new_keys()
    val e: Long = keys._1
    val n: Long = keys._3
    private val d: Long = keys._2


    private def gen_new_keys(): Tuple3[Long, Long, Long] = {
        var start: Long = System.console().readLine("Введите начало интервала для генерации двух простых чисел, дающих модуль : ").toLong
        var end: Long = System.console().readLine("Введите конец интервала для генерации двух простых чисел, дающих модуль: ").toLong
        var p: Long = 0
        var q: Long = 0
        val rnd: Random = new Random
        breakable {
            while(true) {
                p = start + rnd.nextLong( (end - start) + 1 )
                q = start + rnd.nextLong( (end - start) + 1 )
                if (prime_check(p) & prime_check(q)) {
                    break
                }
            }
        }
        var n: Long = p * q
        var phi: Long = (p - 1) * (q -1)
        start = 2
        end = phi - 1
        var e: Long = 0
        var cur_typle: Tuple3[Long, Long, Long] = (0, 0, 0)
        breakable {
            while(true) {
                e = start + rnd.nextLong( (end - start) + 1 )
                cur_typle = gcd(e, phi)
                if (cur_typle._1 == 1) {
                    break
                }
            }
        }
        var d = cur_typle._2 % phi
        if (d < 0) {
            d = phi + d
        }
        return (e, d, n)
    }

    private def check_messeg(msg: Array[Long]): Boolean = {
        for (m <- msg) {
            if (gcd(m, n)._1 != 1 | m > n - 1 | m < 0) {
                return false
            } 
        }
        return true
    }

    def gen_new_message(): Array[Long] = {
        val len: Int = System.console().readLine("Введите длину сообщения, которое хотите сгенерировать : ").toInt
        var msg: Array[Long] = Array()
        var start: Long = 0
        var end: Long = n - 1
        val rnd: Random = new Random
        while(msg.length != len) {
            var c = start + rnd.nextLong( (end - start) + 1 )
            if (gcd(c, n)._1 == 1) {
                msg +:= c
            }
        }
        println("Сгенерированное сообщение: ")
        for (i <- 0 until len) {
            print(msg(i).toString + " ")
        }
        println()
        return msg
    }

    def encode(msg: Array[Long]) : Array[Long] = {
        if (check_messeg(msg)) {
            var ans: Array[Long] = new Array[Long](msg.length)
            for (i <- 0 until msg.length) {
                ans(i) = pow(msg(i), e, n)
            }
            println("Зашифрованное сообщение: ")
            for (i <- 0 until ans.length) {
                print(ans(i).toString + " ")
            }
            println()
            return ans
        }
        else {
            println("В сообщение присутствуют числа не являеющиеся взаимно простыми с n")
            return Array(-1)
        }
    }

    def decode(msg: Array[Long]) : Array[Long] = {
        var ans: Array[Long] = new Array[Long](msg.length)
        for (i <- 0 until msg.length) {
            ans(i) = pow(msg(i), d, n)
        }
        println("Расшифрованное сообщение: ")
        for (i <- 0 until ans.length) {
            print(ans(i).toString + " ")
        }
        println()
        return ans   
    }
}

object Main extends App {
    try {
        val coder: RSA = new RSA()
        println("Ваш открытый ключ:")
        println("(" + coder.e.toString + ", " + coder.n.toString + ")")
        var flag: Int = System.console().readLine("Вы хотите сгенерировать сообщение или ввести его сами? (0, 1): ").toInt
        var messeg: Array[Long] = Array()
        if (flag == 0) {
            messeg = coder.gen_new_message()
        }
        else {
            messeg = System.console().readLine("Введите сообщение: ").split(' ').map(_.toLong)
        }
        flag = System.console().readLine("Вы хотите зашифровать данное сообщение? (0 - да, 1 - нет): ").toInt
        if (flag == 0) {
            val code: Array[Long] = coder.encode(messeg)
            if (code(0) != -1) {
                flag = System.console().readLine("Вы хотите расшифровать данное сообщение? (0 - да, 1 - нет): ").toInt
                if (flag == 0) {
                    val decode: Array[Long] = coder.decode(code)
                }
            }
        }
    } catch {
        case e: Exception => println("Ошибка ввода, похоже вы ввели не число!")
    }
}