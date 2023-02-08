import scala.io.Source
import scala.util.control.Breaks._
import java.io._

class Coder() {

    def code(input_filename: String, filename_right_page: String, filename_left_page: String): Unit = {
        val input: String = Source.fromFile(input_filename).mkString
        var str: Vector[String] = Vector()
        var ps: String = ""
        for (i <- 0 until input.length) {
            if (input(i) == '\n') {
                ps += "\n"
                str :+= ps
                ps = ""
            } else {
                ps += input(i)
            }
        }
        str :+= ps
        var first_strings: String = ""
        var second_strings: String = ""
        var third_strings: String = ""
        var fourth_strings: String = ""
        var j: Int = 0
        for (i <- 0 until str.length) {
            j match {
                case 0 => {
                    first_strings += str(i)
                    j += 1
                }
                case 1 => {
                    second_strings += str(i)
                    j += 1
                }
                case 2 => {
                    third_strings += str(i)
                    j += 1
                }
                case 3 => {
                    fourth_strings += str(i)
                    j = 0
                }
            }
        }
        val writer1 = new PrintWriter(new File(filename_right_page))
        val writer2 = new PrintWriter(new File(filename_left_page))
        writer1.write(first_strings)
        writer1.write(second_strings)
        writer1.close()
        writer2.write(third_strings)
        writer2.write(fourth_strings)
        writer2.close()
    }

    def decode(filename_right_page: String, filename_left_page: String, output_filename: String): Unit = {
        val rp: String = Source.fromFile(filename_right_page).mkString
        val lp: String = Source.fromFile(filename_left_page).mkString
        var right_page: Vector[String] = Vector()
        var left_page: Vector[String] = Vector()
        var ps: String = ""
        for (i <- 0 until rp.length) {
            if (rp(i) == '\n') {
                right_page :+= ps
                ps = ""
            } else {
                ps += rp(i)
            }
        }
        right_page :+= ps
        ps = ""
        for (i <- 0 until lp.length) {
            if (lp(i) == '\n') {
                left_page :+= ps
                ps = ""
            } else {
                ps += lp(i)
            }
        }
        left_page :+= ps
        var first_strings: Vector[String] = Vector()
        var second_strings: Vector[String] = Vector()
        var third_strings: Vector[String] = Vector()
        var fourth_strings: Vector[String] = Vector()
        for (i <- 0 until right_page.length) {
            if (i < right_page.length / 2) {
                first_strings :+= right_page(i)
                third_strings :+= left_page(i)
            } else {
                second_strings :+= right_page(i)
                fourth_strings :+= left_page(i)
            }
        }
        ps = ""
        for (i <- 0 until first_strings.length) {
            ps = ps + first_strings(i) + "\n" + second_strings(i) + "\n" + third_strings(i) + "\n" + fourth_strings(i) + "\n\n"
        }
        val writer = new PrintWriter(new File(output_filename))
        writer.write(ps)
        writer.close()
    }
}

object Main extends App {
    try {
        val f: Int = System.console().readLine("Вы хотите закодировать или раскодировать стих? (0, 1): ").toInt
        val c: Coder = new Coder()
        f match {
            case 0 => {
                val in_name: String = System.console().readLine("Введите название файла со стихотворением: ")
                val rp_name: String = System.console().readLine("Введите название файла, куда будет записана првая страница: ")
                val lp_name: String = System.console().readLine("Введите название файла, куда будет записана левая страница: ")
                c.code(in_name, rp_name, lp_name)
            }
            case 1 => {
                val rp_name: String = System.console().readLine("Введите название файла, в котором записана првая страница: ")
                val lp_name: String = System.console().readLine("Введите название файла, в котором записана левая страница: ")
                val out_name: String = System.console().readLine("Введите название файла, куда будет записано расшифрованное стихотворение: ")
                c.decode(rp_name, lp_name, out_name)
            }
            case _ => {
                println("Введеного режима не существует!")
            }
        }
    } catch {
        case e: Exception => println("Введеного файла(ов) не существует или ваш стих не отвечает требованиям кодирования или вы неправильно ввели номер режима работы программы!")
    }
}