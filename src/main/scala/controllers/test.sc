val i: Int = 1
val s: String = "hello"
val b: Boolean = true
val li:List[Int] = List(1,2,3,4,5)
val ls:List[String] = List("qbc","def", "hij")
val lb:List[Boolean] = List(true, true, false)
val oi: Option[Int] = Some(1)
val os: Option[String] = Some("hello")
val ob: Option[Boolean] = Some(true)
val n: Option[Int] = None
val m:Map[String, Int]= Map("id" -> 1, "age" -> 21)
val t: (String, String) = ("","")

/*  map
*
*   The map method takes a predicate function and applies it to every element in the collection. It creates a new
*   collection with the result of the predicate function applied to each and every element of the collection.
*
*   def map[B](f: (A) ⇒ B): Traversable[B]
*
* */
li.map(i => i + 1)
ls.map(_ + "!")
lb.map {
  case true => "correct"
  case false => "incorrect"
  case _ => "No bool"
}

//flatMap



//filter


//filterNot


//fold


//foldLeft


//foldRight


//for


//for yield


//higher order functions


//partially applied functions


//currying


//pattern matching


//generics



