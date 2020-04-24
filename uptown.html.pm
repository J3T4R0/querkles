#lang pollen
◊(define my-inset "30%")
<body style="margin: ◊my-inset; border:1px solid black">
◊my-inset is the inset.
</body>

 
◊(define metal "Plutonium")
 
Pollen + ◊metal
---------------
 
+ You **wanted** ◊metal — you _got_ it.
 
+ [search for ◊metal](https://google.com/search?q=◊metal)

◊headline{Pollen markup}
 
◊items{
  ◊item{You ◊strong{wanted} it — you ◊em{got} it.}
  ◊item{◊link["https://google.com/search?q=racket"]{search for Racket}}
}