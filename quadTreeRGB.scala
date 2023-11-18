import java.awt.image.{BufferedImage, DataBufferInt}
// import java.io.File
// import javax.imageio.ImageIO
import scala.util.control.Breaks._

import java.io.{File, FileOutputStream}
import javax.imageio.{IIOImage, ImageWriteParam, ImageWriter, ImageIO}
import javax.imageio.plugins.jpeg.JPEGImageWriteParam
import java.nio.ByteBuffer


object ImageProcessing {

    def loadGrayScaleImage(filePath: String): Array[Array[Double]] = {

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

    def grayScaleMatrixToImage(mat: Array[Array[Double]], outputPath: String) = {
        
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


    def rgbMatrixToImage(rgbValues: Array[Array[Array[Int]]], outputPath: String): Unit = {
        // Obtiene las dimensiones de la matriz
        val height: Int = rgbValues.length
        val width: Int = rgbValues(0).length

        // Crea una nueva imagen BufferedImage
        val bufferedImage: BufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

        // Itera sobre cada píxel de la matriz y establece los valores RGB en la imagen
        for (y <- 0 until height) {
            for (x <- 0 until width) {
                val rgb = (rgbValues(y)(x)(0) << 16) | (rgbValues(y)(x)(1) << 8) | rgbValues(y)(x)(2)
                bufferedImage.setRGB(x, y, rgb)
            }
        }

        // Guarda la imagen en el archivo de salida
        ImageIO.write(bufferedImage, "png", new File(outputPath + ".png" ))
    }
    
    

    def loadRgbImage(filePath: String): Array[Array[Array[Int]]] = {
        val image = ImageIO.read(new File(filePath))
        val width = image.getWidth
        val height = image.getHeight

        val rgbMatrix = Array.ofDim[Int](height, width, 3)

        for (y <- 0 until height) {
            for (x <- 0 until width) {
                val color = image.getRGB(x, y)
                rgbMatrix(y)(x)(0) = (color >> 16) & 0xFF // Red
                rgbMatrix(y)(x)(1) = (color >> 8) & 0xFF  // Green
                rgbMatrix(y)(x)(2) = color & 0xFF         // Blue
            }
        }

        rgbMatrix
    }

}



sealed trait Quadtree
case class Empty() extends Quadtree
case class Leaf(w: Int, h: Int, rgb: Array[Int]) extends Quadtree
case class Node(quads: Array[Quadtree]) extends Quadtree

object Quadtree {

    def createEmpty(): Quadtree = Empty()

    def areAllRGBValuesEqual(matrix: Array[Array[Array[Int]]]): Boolean = {
        val firstPixel = matrix.head.head
        val red = firstPixel(0)
        val green = firstPixel(1)
        val blue = firstPixel(2)

        matrix.flatten.forall(pixel => pixel(0) == red && pixel(1) == green && pixel(2) == blue)
    }

    def splitRGBMatrix(matrix: Array[Array[Array[Int]]]): Array[Array[Array[Array[Int]]]] = {
        val height = matrix.length
        val width = matrix(0).length

        val halfHeight = height / 2
        val halfWidth = width / 2

        val part1 = matrix.slice(0, halfHeight).map(row => row.slice(0, halfWidth))
        val part2 = matrix.slice(0, halfHeight).map(row => row.slice(halfWidth, width))
        val part3 = matrix.slice(halfHeight, height).map(row => row.slice(0, halfWidth))
        val part4 = matrix.slice(halfHeight, height).map(row => row.slice(halfWidth, width))

        Array(part1, part2, part3, part4)
    }

    def joinMats(mat0: Array[Array[Array[Int]]], 
                 mat1: Array[Array[Array[Int]]], 
                 mat2: Array[Array[Array[Int]]], 
                 mat3: Array[Array[Array[Int]]])  : Array[Array[Array[Int]]] = {
        
        val finalMat = Array.ofDim[Int](mat0.length + mat2.length, mat0(0).length + mat1(0).length, 3)

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


    def build(mat: Array[Array[Array[Int]]]): Quadtree = {

        areAllRGBValuesEqual(mat) match {
            case true => 

                val width  : Int = mat(0).length
                val height : Int = mat.length
                Leaf(width, height, mat(0)(0))

            case false =>

                val width  : Int = mat(0).length
                val height : Int = mat.length
                val arrQuads = Array.ofDim[Quadtree](4)

                if (width == 1){

                    val parte1 = mat.slice(0, mat.length/2);
                    val parte2 = mat.slice(mat.length/2, mat.length);

                    arrQuads(0) = build(parte1)
                    arrQuads(1) = build(parte2)
                    arrQuads(2) = Empty()
                    arrQuads(3) = Empty()

                    Node(arrQuads)

                }
                else if (height == 1){

                    var parte1 = Array( mat(0).slice(0, mat(0).length/2) )
                    var parte2 = Array( mat(0).slice(mat(0).length/2, mat(0).length) )

                    arrQuads(0) = build(parte1)
                    arrQuads(1) = build(parte2)
                    arrQuads(2) = Empty()
                    arrQuads(3) = Empty()

                    Node(arrQuads)

                }
                else{

                    val splitMat = splitRGBMatrix(mat)

                    for (i <- 0 until 4){
                        arrQuads(i) = build(splitMat(i))
                    }

                    Node(arrQuads)
                }

                

        }
    }

    def getAverage(tree: Quadtree, width: Int, height: Int) : Array[Double] = {
        tree match {
            case Empty() => Array(0.0, 0.0, 0.0)
            case Leaf(w, h, value) => Array(value(0).toDouble, value(1).toDouble, value(2).toDouble)
            case Node(quads) =>
                var average = Array(0.0, 0.0, 0.0)
                var averageChildrens = Array.ofDim[Double   ](4, 3)

                if (width == 1){
                    
                    averageChildrens(0) = getAverage(quads(0), 1, height/2)
                    averageChildrens(1) = getAverage(quads(1), 1, height/2 + height%2)

                    for (i <- 0 until 2){ // Childrens
                        for (j <- 0 until 3){ // RGB
                            average(j) = average(j) + averageChildrens(i)(j)
                        }
                    }

                    for (j <- 0 until 3) average(j) = average(j) / 2

                    average

                }
                else if (height == 1){
                    
                    averageChildrens(0) = getAverage(quads(0), width/2, 1)
                    averageChildrens(1) = getAverage(quads(1), width/2 + width%2 , 1)

                    for (i <- 0 until 2){ // Childrens
                        for (j <- 0 until 3){ // RGB
                            average(j) = average(j) + averageChildrens(i)(j)
                        }
                    }

                    for (j <- 0 until 3) average(j) = average(j) / 2

                    average

                }
                else{

                    averageChildrens(0) = getAverage(quads(0), width/2, height/2)
                    averageChildrens(1) = getAverage(quads(1), width/2 + width%2 , height/2)
                    averageChildrens(2) = getAverage(quads(2), width/2, height/2 + height%2)
                    averageChildrens(3) = getAverage(quads(3), width/2 + width%2 , height/2 + height%2)

                    for (i <- 0 until 4){ // Childrens
                        for (j <- 0 until 3){ // RGB
                            average(j) = average(j) + averageChildrens(i)(j)
                        }
                    }

                    for (j <- 0 until 3) average(j) = average(j) / 4

                    average
                }
        }
    }

    def query(tree: Quadtree, maxDeep: Int, width: Int, height: Int, actualDeep: Int = 0) : Array[Array[Array[Int]]] = {

        tree match {
            case Empty() => 
                val mat = Array.ofDim[Int](1, 1, 3)
                mat(0)(0) = Array(0, 0, 0)
                mat

            case Leaf(w, h, value) =>
                val mat = Array.ofDim[Int](h, w, 3)
                for (i <- 0 until h) {
                    for (j <- 0 until w) {
                        mat(i)(j) = value
                    }
                }

                mat

            case Node(quads) => 

                // println(s"\nActual Deep: $actualDeep\nWidth: $width\nHeight: $height")

                if (actualDeep == maxDeep){ // Forzar Promedio

                    val averageDouble = getAverage(tree, width, height)
                    val average = Array(averageDouble(0).toInt, averageDouble(1).toInt, averageDouble(2).toInt)
                    
                    val mat = Array.ofDim[Int](height, width, 3)
                    for (i <- 0 until height) {
                        for (j <- 0 until width) {
                            mat(i)(j) = average
                        }
                    }

                    mat

                }
                else{
                    
                    if (width == 1){ // Vertical Mat
                        var queryMat0 = query(quads(0), maxDeep, (width/2), (height/2), actualDeep +1)
                        var queryMat1 = query(quads(1), maxDeep, (width/2) + width%2, (height/2), actualDeep +1)

                        var finalMat = Array.ofDim[Int](queryMat0.length + queryMat1.length, 1, 3);
                        
                        for (i <- 0 until queryMat0.length) finalMat(i)(0) = queryMat0(i)(0)
                        for (i <- 0 until queryMat1.length) finalMat(queryMat0.length + i)(0) = queryMat1(i)(0)
                        
                        finalMat
                    }
                    else if (height == 1){ // Horizontal Mat
                        var queryMat0 = query(quads(0), maxDeep, (width/2), (height/2), actualDeep +1)
                        var queryMat1 = query(quads(1), maxDeep, (width/2) + width%2, (height/2), actualDeep +1)

                        var finalMat = Array.ofDim[Int](1, queryMat0(0).length + queryMat1(0).length, 3);
                        
                        for (i <- 0 until queryMat0(0).length) finalMat(0)(i) = queryMat0(0)(i)
                        for (i <- 0 until queryMat1(0).length) finalMat(0)(queryMat0(0).length + i) = queryMat1(0)(i)

                        finalMat
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

    }

    def maxDeep(tree: Quadtree, actualDeep: Int = 0) : Int = {
        tree match{
            case Empty() => 0
            case Leaf(w, h, value) => 0
            case Node(quads) =>
                var maxDeepResult : Int = 0
                for (i <- 0 until 4){
                    maxDeepResult = maxDeepResult.max(maxDeep(quads(i)))
                }
                maxDeepResult = maxDeepResult + 1
                maxDeepResult
        }
    }

    def printQuadTree(tree: Quadtree) : Unit = {

        tree match{
            case Empty() => print("-\n")
            case Leaf(w, h, value) => 
                val red = value(0)
                val green = value(1)
                val blue = value(2)
                println(s"Leaf: Width = $w, height = $h, value = ($red ,$green, $blue)")
            case Node(quads) =>
                for (i <- 0 until 4) printQuadTree(quads(i))
        }
    }


}

// Ejemplo de uso
object QuadtreeExample {
    def main(args: Array[String]): Unit = {        
        
        // Original Image
        var imagePath = "images/halloween.png" // Replace with the actual path to your image file

        // Load matrix
        var rgbMatrix = ImageProcessing.loadRgbImage(imagePath)

        // Save the image that will be compress
        ImageProcessing.rgbMatrixToImage(rgbMatrix, "images/imageRgbToCompress")

        // Matrix to compress
        rgbMatrix = ImageProcessing.loadRgbImage("images/imageRgbToCompress.png")



        println("\nImage Loaded!\n")

        print("START BUILD\n")
        var tree: Quadtree = Quadtree.build(rgbMatrix)
        print("FINISH BUILD\n")

        // Quadtree.printQuadTree(tree)
        // print("\n\n")

        print("Max Deep Query: " + Quadtree.maxDeep(tree) + "\n\n")

        var deep = 0
        var resultMat = Array.ofDim[Int](1,1,3);

        while (deep != -1){

            print("Enter an Deep Query: ")
            deep = scala.io.StdIn.readInt()

            if (deep == -1)  break()

            resultMat = Quadtree.query(tree, deep, rgbMatrix(0).length, rgbMatrix.length)

            ImageProcessing.rgbMatrixToImage(resultMat, "images/output")


            print("\n\n\n")
        }
        
    }
}