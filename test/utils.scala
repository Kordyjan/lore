//> using lib "org.scalameta::munit:0.7.29"

package test
package utils

trait A:
  def a: Int

object A:
  def apply(_a: Int): A = new:
    val a = _a

def obtainA(using A) = summon[A].a

trait B:
  def b: String

object B:
  def apply(_b: String): B = new:
    val b = _b

def obtainB(using B) = summon[B].b

trait C

object C:
  def apply(): C = new {}

def obtainC(using C): Unit = summon[C]
