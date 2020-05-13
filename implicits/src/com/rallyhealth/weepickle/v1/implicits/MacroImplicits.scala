package com.rallyhealth.weepickle.v1.implicits

import scala.language.experimental.macros

/**
  * Stupid hacks to work around scalac not forwarding macro type params properly
  */
object MacroImplicits {

  def dieIfNothing[T: c.WeakTypeTag](c: scala.reflect.macros.blackbox.Context)(name: String) = {
    if (c.weakTypeOf[T] =:= c.weakTypeOf[Nothing]) {
      c.abort(
        c.enclosingPosition,
        s"weepickle is trying to infer a $name[Nothing]. That probably means you messed up"
      )
    }
  }

  def applyTo[T](c: scala.reflect.macros.blackbox.Context)(implicit e: c.WeakTypeTag[T]): c.Expr[T] = {
    import c.universe._
    dieIfNothing[T](c)("To")
    println("***********************************")
    println("applyTo is doing stuff")
    println("***********************************")
    c.Expr[T](q"${c.prefix}.macroTo0[$e, ${c.prefix}.To]")
  }

  def applyToIfNotExists[T, To](c: scala.reflect.macros.blackbox.Context)(implicit e: c.WeakTypeTag[T], t: c.WeakTypeTag[To]): c.Expr[T] = {
    import c.universe._
    dieIfNothing[T](c)("To")
    println("***********************************")
    println("applyToIfNotExists is doing stuff")
    val blah = c.inferImplicitValue(weakTypeOf[To])
    println(blah)
    println("***********************************")
    
    c.inferImplicitValue(weakTypeOf[To]) match {
      case EmptyTree =>
        println(s"applyToIfNotExists[$e] is using polymorphic implementation")
        c.Expr[T](q"${c.prefix}.macroTo0[$e, ${c.prefix}.To]")
      case existingImplicit =>
        println(s"applyToIfNotExists[$e] LETS USE existingImplicit")
        c.Expr(existingImplicit)
    }
  }

  def applyFrom[T](c: scala.reflect.macros.blackbox.Context)(implicit e: c.WeakTypeTag[T]): c.Expr[T] = {
    import c.universe._
    dieIfNothing[T](c)("From")
    c.Expr[T](q"${c.prefix}.macroFrom0[$e, ${c.prefix}.From]")
  }


  def applyFromTo[T](c: scala.reflect.macros.blackbox.Context)(implicit e: c.WeakTypeTag[T]): c.Expr[T] = {
    import c.universe._
    dieIfNothing[T](c)("From")
    println("applyFromTo")
    c.Expr[T](q"${c.prefix}.FromTo.join(${c.prefix}.macroTo, ${c.prefix}.macroFrom)")
  }

  def applyFromToIfNotExistz[X, FromTo](c: scala.reflect.macros.whitebox.Context)(implicit e: c.WeakTypeTag[X], ft: c.WeakTypeTag[FromTo]): c.Expr[X] = {
    import c.universe._
    if (c.weakTypeOf[X] =:= c.weakTypeOf[Nothing]) {
      c.abort(
        c.enclosingPosition,
        s"whitebox weepickle is trying to infer a From[Nothing]. That probably means you messed up"
      )
    }
    val mactree = c.macroApplication
    val mactpe = mactree.tpe
    val tref: Option[TypeRef] = mactpe match {
      case tr:TypeRef => Some(tr)
      case _ => None
    }
    c.enclosingImplicits.collect { 
      case i @  c.ImplicitCandidate(_, _, TypeRef(itpe, isym, ilst), itree) =>
        println("HAPPY PATH")
        mactpe match {
          case TypeRef(mtpe, msym, mlst) =>
            if(mtpe == itpe && mlst == ilst) {
              println("MATCH")
              c.Expr[X](i)
              // c.abort(
              //   c.enclosingPosition,
              //   s"implicit collision"
              // )
            } else {
              println("NO MATCH")
              None
            }
          case _ => None
        }
    }
    println("OMG")
      c.Expr[X](q"${c.prefix}.FromTo.join(${c.prefix}.macroTo, ${c.prefix}.macroFrom)")
  }

  def applyFromToIfNotExistzdebug[X, FromTo](c: scala.reflect.macros.whitebox.Context)(implicit e: c.WeakTypeTag[X], ft: c.WeakTypeTag[FromTo]): c.Expr[X] = {
    // import scala.reflect.macros.whitebox._
    import c.universe._
    val mat = c.macroApplication.tpe
    val mlt = ft.tpe
    println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    println(s"z;macro app type mat: ${mat}")
    println(scala.reflect.runtime.universe.showRaw(mat))
    println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    println(s"z;macro local type mlt: ${mlt}")
    if (c.weakTypeOf[X] =:= c.weakTypeOf[Nothing]) {
      c.abort(
        c.enclosingPosition,
        s"whitebox weepickle is trying to infer a From[Nothing]. That probably means you messed up"
      )
    }
    // dieIfNothing[X](c)("From")
    // println(s"e: ${e} tpe: ${e.tpe}")
    // println(s"ft: ${ft}")
    // c.enclosingImplicits.map { 
    //   i => println(s"enclosing: ${i.tree}")
    // }
    // c.openImplicits.map {
    //   i => 
    //     println(s"open: pre: ${i.pre}")
    //     println(s"open: sym: ${i.sym}")
    //     println(s"open: pt: ${i.pt}")
    //     println(s"open: tree: ${i.tree}")
    // }
    //
    // val blah  = q"com.rallyhealth.weepickle.v1.WeePickle.FromTo[${e.tpe}]"
    // println(s"blah: ${blah}")
    // val ftq = q"com.rallyhealth.weepickle.v1.WeePickle.FromTo[${e.tpe}]"
    // val tq = q"com.rallyhealth.weepickle.v1.WeePickle.To[${e.tpe}]"
    // val fq = q"com.rallyhealth.weepickle.v1.WeePickle.From[${e.tpe}]"
    // val qq = q"scala.Predef.implicitly[com.rallyhealth.weepickle.v1.WeePickle.To[com.rallyhealth.weepickle.v1.Fruit.Peach.type]]"

    // open: tree: scala.Predef.implicitly[com.rallyhealth.weepickle.v1.WeePickle.To[com.rallyhealth.weepickle.v1.Fruit.Peach.type]]
// ======================================
// TypeApply(Select(Select(Ident(scala), Predef), implicitly), List(TypeTree()))
// ======================================
// TypeApply(Select(Select(Ident(scala), Predef), implicitly), List(AppliedTypeTree(Select(Select(Select(Select(Select(Ident(com), rallyhealth), weepickle), v1), WeePickle), To), List(SingletonTypeTree(Select(Select(Select(Select(Select(Ident(com), r
// allyhealth), weepickle), v1), Fruit), Peach))))))
// ======================================
    c.enclosingImplicits.collect { 
// TypeRef(
//   SingleType(SingleType(SingleType(SingleType(SingleType(
//         ThisType(package <root>), package com), package rallyhealth
//       ), package weepickle
//     ), package v1), object WeePickle
//   ), 
//   trait To, 
//   List(SingleType(SingleType(ThisType(package v1), object Fruit), object Peach))
// )
      // case i @  c.ImplicitCandidate(TypeRef(stp,trt,list), sym, pt, tt) => 
      case i @  c.ImplicitCandidate(pre, sym, pt, tt) => 
        println("======================================")
        pt match {
          case TypeRef(tpve,bol,lst) => 
            mat match {
              case TypeRef(mtpe, msym, mlst) =>
                println("MATCHES")
                println(tpve.equals(mtpe))
                if (mtpe == tpve) {
                  if(mlst == lst) {
                    println("THEY EQUAL DOG")
                  } else {
                    println("THEY NOT DOG")
                    if(mlst.head == lst.head) {
                      println("THEY HEAD EQUAL")
                    }
                  }
                } else {
                  println(scala.reflect.runtime.universe.showRaw(mtpe))
                  println(scala.reflect.runtime.universe.showRaw(tpve))
                }
            }
            // println(scala.reflect.runtime.universe.showRaw(tpve))
            // println(scala.reflect.runtime.universe.showRaw(bol))
            // println(scala.reflect.runtime.universe.showRaw(lst))
          case x => 
            println("NO MATCH")
        }
        println("--------------------------------------")
        println(scala.reflect.runtime.universe.showRaw(pre))
        println(scala.reflect.runtime.universe.showRaw(sym))
        println(scala.reflect.runtime.universe.showRaw(pt))
        println(scala.reflect.runtime.universe.showRaw(tt))
        println("======================================")
    }

    val out = c.openImplicits.collectFirst {
      case i @ c.ImplicitCandidate(pre, sym, pt, tree) if pt == q"com.rallyhealth.weepickle.v1.WeePickle.FromTo[${e.tpe}]" || pt == q"FromTo[${e.tpe}]" =>
        println("correct")
        c.Expr[X](i.tree)
    }.getOrElse {
      println("or else")
      c.Expr[X](q"${c.prefix}.FromTo.join(${c.prefix}.macroTo, ${c.prefix}.macroFrom)")
    }
    println(s"out: ${out}")

    val infer = c.inferImplicitValue(c.weakTypeOf[FromTo])
    println(s"infer: ${infer}")
    // val infer2 = c.inferImplicitValue(mat)
    // println(s"infer: ${infer2}")
    out



    // openImplicits
    // mat match {
    //   case EmptyTree =>
    //     // :(
    //     // applyFromToIfNotExists() didn't find implicits for WeakTypeTag[com.rallyhealth.weepickle.v1.Fruit.Peach.type]
    //     //
    //     // applyFromToIfNotExists() didn't find implicits for typeTag: WeakTypeTag[com.rallyhealth.weepickle.v1.Fruit.Peach.type]
    //     // applyFromToIfNotExists() didn't find implicits for ft: WeakTypeTag[Types.this.FromTo[T]]
    //
    //     println(s"applyFromToIfNotExists() didn't find implicits for typeTag: ${e}")
    //     println(s"applyFromToIfNotExists() didn't find implicits for ft: ${ft}")
    //     c.Expr[X](q"${c.prefix}.FromTo.join(${c.prefix}.macroTo, ${c.prefix}.macroFrom)")
    //   case existingImplicit =>
    //     // :)
    //     println(s"applyFromToIfNotExists[${e}] found existing implicit! $existingImplicit")
    //     c.Expr(existingImplicit)
    // }
  }

  def applyFromToIfNotExists[X, FromTo](c: scala.reflect.macros.blackbox.Context)(implicit e: c.WeakTypeTag[X], ft: c.WeakTypeTag[FromTo]): c.Expr[X] = {
    import c.universe._
    dieIfNothing[X](c)("From")

// macro app type mat: com.rallyhealth.weepickle.v1.WeePickle.FromTo[com.rallyhealth.weepickle.v1.Fruit.Peach.type]
// macro local type mlt: Types.this.FromTo[T]
// pre: Types.this.type
// symm: trait FromTo
// args: T
// foo: Types.this.FromTo[com.rallyhealth.weepickle.v1.Fruit.Peach.type]
    val mat = c.macroApplication.tpe
    val mlt = ft.tpe
    println(s"macro app type mat: ${mat}")
    println(s"macro local type mlt: ${mlt}")
    val foo = mlt match {
      case TypeRef(pre, sym, args) =>
        import compat._
        println(s"pre: ${pre}")
        println(s"sym: ${sym}")
        println(s"args: ${args.mkString(",")}")
        internal.typeRef(pre, sym, List(e.tpe))
    }
    println(s"foo: ${foo}")
    val blah = c.inferImplicitValue(foo)
    val boo = c.inferImplicitValue(mat)
    println(s"blah: ${blah}")
    println(s"boo: ${boo}")

// from type: com.rallyhealth.weepickle.v1.WeePickle.FromTo[com.rallyhealth.weepickle.v1.Fruit.Peach.type]
// infer from type: com.rallyhealth.weepickle.v1.WeePickle.macroSingletonFromTo[com.rallyhealth.weepickle.v1.Fruit.Peach.type]
// applyFromToIfNotExists() didn't find implicits for typeTag: WeakTypeTag[com.rallyhealth.weepickle.v1.Fruit.Peach.type]
// applyFromToIfNotExists() didn't find implicits for ft: WeakTypeTag[Types.this.FromTo[T]]
    // val fromType = c.macroApplication.tpe 
    // println(s"from type: ${fromType}")
    // val ift = c.inferImplicitValue(fromType)
    // val tft = q"${ift}"
    // println(s"infer from type: ${ift}")
    // val stuff = ift.collect {
    //   case EmptyTree =>
    //     // :(
    //     // applyFromToIfNotExists() didn't find implicits for WeakTypeTag[com.rallyhealth.weepickle.v1.Fruit.Peach.type]
    //     //
    //     // applyFromToIfNotExists() didn't find implicits for typeTag: WeakTypeTag[com.rallyhealth.weepickle.v1.Fruit.Peach.type]
    //     // applyFromToIfNotExists() didn't find implicits for ft: WeakTypeTag[Types.this.FromTo[T]]
    //
    //     println(s"applyFromToIfNotExists() didn't find implicits for typeTag: ${e}")
    //     println(s"applyFromToIfNotExists() didn't find implicits for ft: ${ft}")
    //     c.Expr[X](q"${c.prefix}.FromTo.join(${c.prefix}.macroTo, ${c.prefix}.macroFrom)")
    //   case existingImplicit =>
    //     // :)
    //     println(s"applyFromToIfNotExists[${e}] found existing implicit! $existingImplicit")
    //     println("make it so")
    //     c.Expr(tft)
    // }
    // stuff.head
    
    // Original
    c.inferImplicitValue(foo) match {
      case EmptyTree =>
        // :(
        // applyFromToIfNotExists() didn't find implicits for WeakTypeTag[com.rallyhealth.weepickle.v1.Fruit.Peach.type]
        //
        // applyFromToIfNotExists() didn't find implicits for typeTag: WeakTypeTag[com.rallyhealth.weepickle.v1.Fruit.Peach.type]
        // applyFromToIfNotExists() didn't find implicits for ft: WeakTypeTag[Types.this.FromTo[T]]

        println(s"applyFromToIfNotExists() didn't find implicits for typeTag: ${e}")
        println(s"applyFromToIfNotExists() didn't find implicits for ft: ${ft}")
        c.Expr[X](q"${c.prefix}.FromTo.join(${c.prefix}.macroTo, ${c.prefix}.macroFrom)")
      case existingImplicit =>
        // :)
        println(s"applyFromToIfNotExists[${e}] found existing implicit! $existingImplicit")
        c.Expr(existingImplicit)
    }
  }
}

trait MacroImplicits {
  this: com.rallyhealth.weepickle.v1.core.Types =>
  implicit def macroSingletonTo[T <: Singleton]: To[T] = macro MacroImplicits.applyToIfNotExists[T, To[T]]
  implicit def macroSingletonFrom[F <: Singleton]: From[F] = macro MacroImplicits.applyFrom[F]
  // implicit def macroSingletonFromTo[X <: Singleton]: FromTo[X] = macro MacroImplicits.applyFromToIfNotExists[X, FromTo[X]]
  implicit def macroSingletonFromTo[X <: Singleton]: FromTo[X] = macro MacroImplicits.applyFromToIfNotExistz[X, FromTo[X]]
  def macroFrom[F]: From[F] = macro MacroImplicits.applyFrom[F]
  def macroTo[T]: To[T] = macro MacroImplicits.applyTo[T]
  def macroFromTo[X]: FromTo[X] = macro MacroImplicits.applyFromTo[FromTo[X]]

  def macroTo0[T, M[_]]: To[T] = macro internal.Macros.macroRImpl[T, M]
  def macroFrom0[T, M[_]]: From[T] = macro internal.Macros.macroTImpl[T, M]
}
