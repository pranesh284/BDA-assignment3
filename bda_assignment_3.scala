import scala.io.Source
import scala.collection.immutable.List
import scala.collection.immutable.Set
import java.io.File
import scala.collection.mutable.Map
import scala.util.control.Breaks._


object Runner {
  val helpmsg: String =
    "Invalid Args!"

  def main(args: Array[String]) = {
    if (args.size != 3) {
      println(helpmsg)
      System.exit(1)
    }
    val alg = new AprioriAlgorithm(new File(args(0)))
    alg.runFirstApriori(args(1))
    alg.runSecondApriori(args(2))
  }
}
class AprioriAlgorithm(inputFile: File) {
  var categories: List[Set[String]] = List()
  var itemSet: Set[String] = Set()
  for (line < -Source.fromFile(inputFile).getLines()) {
    val elementSet = line.trim.split(';').toSet
    if (elementSet.size > 0) {
      categories = categories: +elementSet
      itemSet = itemSet++elementSet
    }
  }
  var toRetItems: Map[Set[String], java.lang.Double] = Map()

  def getSupport(itemComb: Set[String]): Integer = {
    val count = categories.filter(category => itemComb.subsetOf(category)).size
    return count
  }

  def runFirstApriori(outputPath: String, minSupport: Integer = 771) = {
    var currentCSet: Set[Set[String]] = itemSet.map(word => Set(word))

    val currentItemCombs: Set[(Set[String], Integer)] = currentCSet.map(wordSet => (wordSet, getSupport(wordSet)))
      .filter(wordSetSupportPair => (wordSetSupportPair._2 > minSupport))
    val currentLSet = currentItemCombs.map(wordSetSupportPair => (wordSetSupportPair._2, wordSetSupportPair._1.head)).toSet

    printToFile(new File(outputPath)) {
      print =>
        currentLSet.map(listElement => listElement._1.toString + ":" + listElement._2).foreach(print.println)
    }

  }

def runSecondApriori(outputPath: String, minSupport: Integer = 771) = {
    var currentCSet: Set[Set[String]] = itemSet.map(word => Set(word))

    var currentItemCombs: Set[(Set[String], Integer)] = currentCSet.map(wordSet => (wordSet, getSupport(wordSet)))
      .filter(wordSetSupportPair => (wordSetSupportPair._2 > minSupport))
    var currentLSet = currentItemCombs.map(wordSetSupportPair => wordSetSupportPair._1).toSet
    var currentLSet1 = currentItemCombs.map(wordSetSupportPair => (wordSetSupportPair._2, wordSetSupportPair._1.head)).toSet

    printToFile(new File(outputPath)) {
      print =>
        currentLSet1.map(listElement => listElement._1.toString + ":" + listElement._2).foreach(print.println)
    }

    if (currentLSet.isEmpty) break

    currentCSet = currentLSet.map(wordSet => currentLSet.map(wordSet1 => wordSet | wordSet1))
    .reduceRight((set1, set2) => set1 | set2)
    .filter(wordSet => (wordSet.size == 2))

    currentItemCombs = currentCSet.map(wordSet => (wordSet, getSupport(wordSet)))
    .filter(wordSetSupportPair => (wordSetSupportPair._2 > minSupport))
    currentLSet = currentItemCombs.map(wordSetSupportPair => wordSetSupportPair._1).toSet
    var currentLSet2 = currentItemCombs.map(wordSetSupportPair => (wordSetSupportPair._2, wordSetSupportPair._1.head, wordSetSupportPair._1.last)).toSet

    printToFile(new File(outputPath)) {
      print =>
        currentLSet2.map(listElement => listElement._1.toString + ":" + listElement._2 + "," + listElement._3).foreach(print.println)
    }
    if (currentLSet.isEmpty) break

    currentCSet = currentLSet.map(wordSet => currentLSet.map(wordSet1 => wordSet | wordSet1))
    .reduceRight((set1, set2) => set1 | set2)
    .filter(wordSet => (wordSet.size == 3))

    currentItemCombs = currentCSet.map(wordSet => (wordSet, getSupport(wordSet)))
    .filter(wordSetSupportPair => (wordSetSupportPair._2 > minSupport))
    var currentLSet3 = currentItemCombs.map(wordSetSupportPair => (wordSetSupportPair._2, wordSetSupportPair._1)).toSet

    printToFile(new File(outputPath)) {
      print =>
        currentLSet3.map(listElement => listElement._1.toString + ":" + fn(listElement._2)).foreach(print.println)
    }

    println("Write completed!")
  }

  def fn(x: Set[String]): String = {
    var res = ""
    for (xx < -x) {
      res = res + xx + ","
    }
    return res
  }

  def printToFile(outputFile: java.io.File)(out: java.io.PrintWriter => Unit): Unit = {
    val print = new java.io.PrintWriter(new java.io.FileOutputStream(new File(outputFile.getPath()), true))
    try {
      out(print)
    } finally {
      print.close()
    }
  }

}
