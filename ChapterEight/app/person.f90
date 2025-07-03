program callPerson
    use mod_person, only: Person
    implicit none
    
    !!type(Person):: somePerson = Person("Jill")
    type(Person) :: somePerson
    type(Person) :: anotherPerson
    type(Person) :: oneLastPerson

    somePerson = Person("Bob", 10, "Builder")
    anotherPerson  = Person(name="Alice", age=20)
    oneLastPerson = Person("Charlie", 20, "Pirate")

    call somePerson % greet()
    call anotherPerson % greet()

    print*, trim(oneLastPerson%name)//" says: "//trim(oneLastPerson%greetingMessage)

end program callPerson