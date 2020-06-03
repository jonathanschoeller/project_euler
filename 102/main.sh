#!/bin/sh
exec scala -deprecation "$0" "$@"
!#

import scala.io.Source

val filename = "p102_triangles.txt"

case class Point(x: Integer, y: Integer)
case class Triangle(a: Point, b: Point, c: Point)

def TriangleContains(t: Triangle, p: Point): Boolean = {
    val s = t.a.y * t.c.x - t.a.x * t.c.y + (t.c.y - t.a.y) * p.x + (t.a.x - t.c.x) * p.y
    val tt = t.a.x * t.b.y - t.a.y * t.b.x + (t.a.y - t.b.y) * p.x + (t.b.x - t.a.x) * p.y

    if ((s < 0) != (tt < 0)) {
        return false;
    }

    val a = (-t.b.y) * t.c.x + t.a.y * (t.c.x - t.b.x) + t.a.x * (t.b.y - t.c.y) + t.b.x * t.c.y
    return if (a < 0) 
        (s <= 0 && s + tt >= a) else (s >= 0 && s + tt <= a)
}

def ReadTriangle(line: String): Triangle = {
    line.split(",").map(_.toInt) match {
        case Array(a, b, c, d, e, f) => Triangle(Point(a, b), Point(c, d), Point(e, f))
    }
}

println {
    Source.fromFile(filename).getLines.map {
        ReadTriangle
    } count {
        TriangleContains(_, Point(0,0))
    }
}
