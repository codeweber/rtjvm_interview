package com.rockthejvm.numbers

object ApproxPi:


    def approxPi(nTotal: Long): Double = 

        import scala.util.Random
        val rand = Random(System.currentTimeMillis)
        
        def getPoint(): (Double, Double) = 
            val x = rand.between(-1.0, 1.0)
            val y = rand.between(-1.0, 1.0)
            (x,y)

        def isInCircle(x: Double, y: Double): Boolean =
            (x * x + y * y) < 1.0 

        def loop(n: Long, nInCircle: Long): Long =
            if n >= nTotal then 
                nInCircle
            else
                val (x,y) = getPoint() 
                val isPointInCircle = isInCircle(x,y)

                if isPointInCircle then
                    loop(n+1, nInCircle+1)
                else 
                    loop(n+1, nInCircle)

        val nInCircle = loop(0,0)

        4.0 * nInCircle.toDouble / nTotal.toDouble


    @main def TryApproxPi = 

        val nPoints = (1 to 20).map {
            n => 
            val nT = math.pow(2.0, n.toDouble).toLong
            nT -> approxPi(nT)
        }.toMap

        println(nPoints)


        
        
