module Program

open System

type Shape =
    | Square of sideLength: float
    | Rectangle of height:float * width:float
    | Circle of radius:float
    | Composite of shapes:Shape list

let rec area (shape: Shape) =
    match shape with
    | Square(side) -> side * side
    | Circle(radius) -> Math.PI * radius * radius
    | Rectangle(height, width) -> height * width
    | Composite shapes ->
        shapes
        |> List.map (fun shape -> area shape)
        |> List.sum

let private readInt character =
    match character with
    | '0' -> Some 0
    | '1' -> Some 1
    | '2' -> Some 2
    | '3' -> Some 3
    | '4' -> Some 4
    | '5' -> Some 5
    | '6' -> Some 6
    | '7' -> Some 7
    | '8' -> Some 8
    | '9' -> Some 9
    | _   -> None

let private power x n =
    List.replicate n x
    |> List.fold (fun acc elem -> acc * elem) 1

let parseInt (input: string) =
    let containsNonDigits =
        input
        |> Seq.exists (fun character -> not (Char.IsDigit character))

    if containsNonDigits then
        Error "Contains non digits"
    elif String.IsNullOrWhiteSpace input then
        Error "Input was empty"
    else
        input
        |> Seq.choose (fun character -> readInt character)
        |> Seq.rev
        |> Seq.mapi (fun index number -> number * (power 10 index))
        |> Seq.sum
        |> Ok

[<EntryPoint>]
let main argv = 0