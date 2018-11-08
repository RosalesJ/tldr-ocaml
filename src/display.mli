module Parser : sig
  type example =
    | Command of string
    | Argument of string
  
  type expression =
    | Title of string
    | Description of string
    | Example of string * (example list)

  val parse : String.t -> expression list
end

val color_example : Parser.example -> string
  
val color_expression : Parser.expression -> string
  
val display : string -> Base.unit

                                        
