import java.io.Console
import scala.util.control.Breaks._
import scala.collection.mutable.HashMap
import scala.collection.immutable.ArraySeq
import scala.math.sqrt

def prime_check(num: Int): Boolean = {
    val end: Double = sqrt(num.toDouble)
    for (i <- 2 to end.toInt) {
        if (num % i == 0) {
            return false
        }
    }
    return true
}

def coefficients_check(p: Int, k: Array[Int]) : Boolean = {
    for (i <- 0 until k.length) {
        if (k(i) < 0 | k(i) > p - 1) {
            return true
        }
    }
    return false
}

def pow(num : Int, deg : Int, mod : Int) : Int = {
    var ans: Int = 1
    for (i <- 1 to deg) {
        ans = ans * num % mod
    }
    return ans
}

def reducible_polynomial(p: Int, k: Array[Int]) : Unit = {
    if (!prime_check(p)) {
        println("Введеное вами p, не является простым числом")
        return
    }
    if (coefficients_check(p, k)) {
        println("Введеные вами коэфициенты многочлена не являются элементами поля")
        return
    }
    if (k.length > 4) {
        println("Предупреждение: для многочленов степени большей 3 способ может работать не корретно, так как наличие корней у многочлена таких степеней в заданном поле является достаточным условием, но не необходимым")
    }
    var ans_arr: Array[Int] =  Array[Int]()
    for (i <- 0 until p) {
        var ans: Int = 0
        for (j <- 0 until k.length) {
            ans = (ans + pow(i, j, p) * k(j)) % p
        }
        if (ans == 0) {
            ans_arr :+= i
        }
    }
    if (ans_arr.length == 0) {
        println("Похоже, что ваш многочлен не приводим в заданном поле")
    } else {
        println("Ваш многочлен является приводимым в заданном поле")
        println("Корни многочлена:")
        for (i <- ans_arr) {
            print(i.toString + " ")
        }
        println()
    }
    if (ans_arr.length == k.length - 1) {
        for (i <- 0 until k.length - 1) {
            print(k(i).toString + "x^" + i.toString + " + ")
        }
        print(k(k.length - 1).toString + "x^" + (k.length - 1).toString + " = ")
        for (i <- ans_arr) {
            print("(x + " + i.toString + ") * ")
        }
        println(1)
        return
    }
}

object Main extends App {
    try {
        val p = System.console().readLine("Введите p, количество элементов в вашем поле: ").toInt
        val k = System.console().readLine("Введите коэфициенты вашего многочлена, начиная с коэфициента при x^0: ").split(' ').map(_.toInt)
        println("--------------------------------------------------------------------------------------------")
        reducible_polynomial(p, k)
    } catch {
        case e: Exception => println("Вы ввели не целое число!")
    }
}