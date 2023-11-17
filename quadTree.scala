import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import scala.util.control.Breaks._

object ImageProcessing {

    def convertToGrayscale(filePath: String): Array[Array[Double]] = {

        val image = ImageIO.read(new File(filePath))

        val width = image.getWidth
        val height = image.getHeight

        val grayscaleMatrixDouble = Array.ofDim[Double](height, width)

        for (y <- 0 until height) {
            for (x <- 0 until width) {
                val color = image.getRGB(x, y)
                val red = (color >> 16) & 0xFF
                val green = (color >> 8) & 0xFF
                val blue = color & 0xFF

                // Convert to grayscale using the luminosity method
                val grayscaleValue = (0.21 * red + 0.72 * green + 0.07 * blue).toInt

                grayscaleMatrixDouble(y)(x) = grayscaleValue.toDouble
            }
        }

        grayscaleMatrixDouble
    }


    def matrixToImage(mat: Array[Array[Double]], outputPath: String) = {
        
        // Tamaño de la matriz
        val height = mat.length
        val width = mat(0).length

        // Crea una imagen en escala de grises
        val image = new BufferedImage(width, height, BufferedImage.TYPE_BYTE_GRAY)

        // Llena la imagen con los valores de la matriz
        for (y <- 0 until height; x <- 0 until width) {
            val value = (mat(y)(x)).toInt // Convierte el valor en un nivel de gris entre 0 y 255
            image.getRaster().setSample(x, y, 0, value)
        }

        // Guarda la imagen en un archivo
        val outputImageFile = new File(outputPath + ".png")
        ImageIO.write(image, "png", outputImageFile)

        println("Imagen en escala de grises creada y guardada en '" + outputPath + ".png'")
    }

}



sealed trait Quadtree
case class Empty() extends Quadtree
case class Leaf(w: Int, h: Int, value: Double) extends Quadtree
case class Node(quads: Array[Quadtree]) extends Quadtree

object Quadtree {

    def createEmpty(): Quadtree = Empty()

    def sonTodosElementosIguales(matriz: Array[Array[Double]]): Boolean = {
        val primerElemento = matriz.head.head
        matriz.flatten.forall(_ == primerElemento)
    }

    def dividirMatrizEnCuatroPartes(matriz: Array[Array[Double]]): Array[Array[Array[Double]]] = {

        val filas = matriz.length
        val columnas = matriz(0).length

        val mitadFilas = filas / 2
        val mitadColumnas = columnas / 2

        val parte1 = matriz.slice(0, mitadFilas).map(row => row.slice(0, mitadColumnas))
        val parte2 = matriz.slice(0, mitadFilas).map(row => row.slice(mitadColumnas, columnas))
        val parte3 = matriz.slice(mitadFilas, filas).map(row => row.slice(0, mitadColumnas))
        val parte4 = matriz.slice(mitadFilas, filas).map(row => row.slice(mitadColumnas, columnas))

        Array(parte1, parte2, parte3, parte4)
    }

    def joinMats(mat0: Array[Array[Double]], mat1: Array[Array[Double]], mat2: Array[Array[Double]], mat3: Array[Array[Double]]) 
        : Array[Array[Double]] = {
        
        val finalMat = Array.ofDim[Double](mat0.length + mat2.length, mat0(0).length + mat1(0).length)

        for (i <- 0 until mat0.length){
            for (j <- 0 until mat0(0).length) finalMat(i)(j) = mat0(i)(j)
        }

        for (i <- 0 until mat1.length){
            for (j <- 0 until mat1(0).length) finalMat(i)(mat0(0).length + j) = mat1(i)(j)
        }

        for (i <- 0 until mat2.length){
            for (j <- 0 until mat2(0).length) finalMat(mat0.length + i)(j) = mat2(i)(j)
        }

        for (i <- 0 until mat3.length){
            for (j <- 0 until mat3(0).length) finalMat(mat0.length + i)(mat0(0).length + j) = mat3(i)(j)
        }

        finalMat

    }


    def build(mat: Array[Array[Double]]): Quadtree = {

        sonTodosElementosIguales(mat) match {
            case true => 

                val width  : Int = mat(0).length
                val height : Int = mat.length
                Leaf(width, height, mat(0)(0))

            case false =>

                val width  : Int = mat(0).length
                val height : Int = mat.length

                if (width == 1 || height == 1) { // Forzar promedio
                    
                    var average : Double = 0.0

                    for (i <- 0 until mat.length){
                        for (j <- 0 until mat(0).length) average = average + mat(i)(j)
                    }
                    average = average / (mat.length * mat(0).length)

                    Leaf(width, height, average)

                }
                else{

                    val arrQuads = Array.ofDim[Quadtree](4)
                    val splitMat = dividirMatrizEnCuatroPartes(mat)

                    for (i <- 0 until 4){
                        arrQuads(i) = build(splitMat(i))
                    }

                    Node(arrQuads)
                }

                

        }
    }

    def getAverage(tree: Quadtree) : Double = {
        tree match {
            case Empty() => 0.0
            case Leaf(w, h, value) => value
            case Node(quads) =>
                var average : Double = 0.0
                for (i <- 0 until 4) {
                    average = average + getAverage(quads(i))
                }
                average = average / 4
                average
        }
    }

    def query(tree: Quadtree, maxDeep: Int, width: Int, height: Int, actualDeep: Int = 0) : Array[Array[Double]] = {

        tree match {
            case Empty() => 
                val mat = Array.ofDim[Double](1, 1)
                mat(0)(0) = 0.0
                mat

            case Leaf(w, h, value) =>
                val mat = Array.ofDim[Double](h,w)
                for (i <- 0 until h) {
                    for (j <- 0 until w) {
                        mat(i)(j) = value
                    }
                }

                mat

            case Node(quads) => 

                // println(s"\nActual Deep: $actualDeep\nWidth: $width\nHeight: $height")

                if (actualDeep == maxDeep){ // Forzar Promedio

                    val average = getAverage(tree)
                    
                    val mat = Array.ofDim[Double](height, width)
                    for (i <- 0 until height) {
                        for (j <- 0 until width) {
                            mat(i)(j) = average
                        }
                    }

                    mat

                }
                else{

                    var queryMat0 = query(quads(0), maxDeep, (width/2), (height/2), actualDeep +1)
                    var queryMat1 = query(quads(1), maxDeep, (width/2) + width%2, (height/2), actualDeep +1)
                    var queryMat2 = query(quads(2), maxDeep, (width/2), (height/2) + height%2, actualDeep +1)
                    var queryMat3 = query(quads(3), maxDeep, (width/2) + width%2, (height/2) + height%2, actualDeep +1)

                    val finalMat = joinMats(queryMat0, queryMat1, queryMat2, queryMat3)
                    finalMat

                }
        }

    }

    def printQuadTree(tree: Quadtree) : Unit = {

        tree match{
            case Empty() => print("-\n")
            case Leaf(w, h, value) => println(s"Leaf: Width = $w, height = $h, value = $value")
            case Node(quads) =>
                for (i <- 0 until 4){
                    printQuadTree(quads(i))
                }
        }
    }


}

// Ejemplo de uso
object QuadtreeExample {
    def main(args: Array[String]): Unit = {        

        val imagePath = "images/halloween.png" // Replace with the actual path to your image file
        val grayscaleMatrix = ImageProcessing.convertToGrayscale(imagePath)

        print("\n\n")


        ImageProcessing.matrixToImage(grayscaleMatrix, "images/grayScaleOutput")

        print("\nStart BUILD\n")
        var tree: Quadtree = Quadtree.build(grayscaleMatrix)
        print("\nFINISH BUILD\n")

        // Quadtree.printQuadTree(tree)
        // print("\n\n")

        var deep = 0
        var resultMat = Array.ofDim[Double](1,1);

        while (deep != -1){

            print("Enter an Deep Quey: ")
            deep = scala.io.StdIn.readInt()

            if (deep == -1)  break()

            resultMat = Quadtree.query(tree, deep, grayscaleMatrix(0).length, grayscaleMatrix.length)

            ImageProcessing.matrixToImage(resultMat, "images/output")


            print("\n\n\n")
        }
        
    }
}