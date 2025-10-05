package part1intro

object TCVariance {

  import cats.Eq
  import cats.instances.int._
  import cats.instances.option._ // construct a Eq[Options[Int]] TC instance
  import cats.syntax.eq._

  val aComparison = Option(3) === Option(2)
  //  val anInvalidComparison = Some(2) === None // Eq[Some[Int]] not found

  // variance
  class Animal

  class Cat extends Animal

  // covariant type: subtyping is propagated to the generic type
  class Cage[+T]

  val cage: Cage[Animal] = new Cage[Cat] // Cat <: Animal, so Cage[Cat] <: Cage[Animal]

  // contravariant type: subtyping is propagated backwards
  class Vet[-T]

  val vet: Vet[Cat] = new Vet[Animal] // Cat <: Animal, then Vet[Animal <: Vet[Cat]

  // rule of thumb: if a generic type "Has a T" = covariant, "ACTS on T" = contravariant
  // variance affect how TC instances are being fetched

  trait SoundMaker[-T]

  implicit object AnimalSoundMaker extends SoundMaker[Animal]

  //  implicit object CatSoundMaker extends SoundMaker[Cat]

  def makeSound[T](implicit soundMaker: SoundMaker[T]): Unit = println("Wow")

  makeSound[Cat] // ok - TC instance defined above
  makeSound[Animal] // ok - TC instance for Animal is also application for Cats
  // rule 1 : contravariant TCs can use the superclass instances if nothing is available strictly for that type



  implicit object OptionSoundMaker extends SoundMaker[Option[Int]]

  implicit object SomeSoundMaker extends SoundMaker[Some[Int]]

  makeSound[Option[Int]]
  //  makeSound(Some[Int])

  // covariant TC
  trait AnimalShow[+T] {
    def show: String
  }

  implicit object GeneralAnimalShow extends AnimalShow[Animal] {
    override def show: String = "animals everywhere "
  }

  implicit object CatsShow extends AnimalShow[Cat] {
    override def show: String = "so many cats!"
  }

  def organizeShow[T](implicit event: AnimalShow[T]): String = event.show

  println(organizeShow[Cat]) // ok - the compiler will inject CatsShow as implicit
  // rule 2: covariant TC will always use the more specific TC instance for that type
  // but may confuse the compiler if the general TC is also present
  // println(organizeShow[Animal]) - will not compile because of the presence of GeneralAnimalShow

  // rules 3: you can't have both covariant and contravariant
  // Cats uses INVARIANT TCs

  Option(2) === Option.empty[Int]


  def main(args: Array[String]): Unit = {

  }

}
