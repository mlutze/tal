// The main entry point.
def main(): Unit \ IO = println(toText(99))

enum Digit {
    case D0
    case D1
    case D2
    case D3
    case D4
    case D5
    case D6
    case D7
    case D8
    case D9
}

def toText(i: Int32): String = {
    i |> toDigits |> digitsToText
}

def toDigits(i: Int32): List[Digit] = {
    i |> Int32.toString |> String.toList |> List.map(charToDigit) |> List.reverse
}

def charToDigit(c: Char): Digit = match c {
    case '0' => Digit.D0
    case '1' => Digit.D1
    case '2' => Digit.D2
    case '3' => Digit.D3
    case '4' => Digit.D4
    case '5' => Digit.D5
    case '6' => Digit.D6
    case '7' => Digit.D7
    case '8' => Digit.D8
    case '9' => Digit.D9
    case _ => ???
}

def digitsToText(digits: List[Digit]): String = match digits {
    case d1 :: Nil => ones(d1)
    case d1 :: Digit.D0 :: Nil => ones(d1)
    case d1 :: Digit.D0 :: Digit.D0 :: Nil => ones(d1)

    case d1 :: Digit.D1 :: Nil => teens(d1)
    case d1 :: Digit.D1 :: Digit.D0 :: Nil => teens(d1)

    case Digit.D0 :: d10 :: Nil => tens(d10)
    case Digit.D0 :: d10 :: Digit.D0 :: Nil => tens(d10)

    case d1 :: d10 :: Nil => ones(d1) ++ "og" ++ tens(d10)
    case d1 :: d10 :: Digit.D0 :: Nil => ones(d1) ++ "og" ++ tens(d10)

    case _ => ???
}

// def onesAndTensAndHundreds(d1_: Digit, d10_: Digit, d100_: Digit): String = ()

def onesAndTens(d1_: Digit, d10_: Digit): String = match (d1_, d10_) {
    case (d1, Digit.D0) => ones(d1)
    case (d1, Digit.D1) => teens(d1)
    case (Digit.D0, d10) => tens(d10)
    case (d1, d10) => ones(d1) ++ "og" ++ tens(d10)
}

def ones(d: Digit): String = match d {
    case Digit.D0 => "nul"
    case Digit.D1 => "en"
    case Digit.D2 => "to"
    case Digit.D3 => "tre"
    case Digit.D4 => "fire"
    case Digit.D5 => "fem"
    case Digit.D6 => "seks"
    case Digit.D7 => "syv"
    case Digit.D8 => "otte"
    case Digit.D9 => "ni"
}

def teens(d: Digit): String = match d {
    case Digit.D0 => "ti"
    case Digit.D1 => "elleve"
    case Digit.D2 => "tolv"
    case Digit.D3 => "tretten"
    case Digit.D4 => "fjorten"
    case Digit.D5 => "femten"
    case Digit.D6 => "seksten"
    case Digit.D7 => "sytten"
    case Digit.D8 => "atten"
    case Digit.D9 => "nitten"
}

def tens(d: Digit): String = match d {
    case Digit.D0 => "nul"
    case Digit.D1 => "ti"
    case Digit.D2 => "tyve"
    case Digit.D3 => "tredje"
    case Digit.D4 => "fyrre"
    case Digit.D5 => "halvtreds"
    case Digit.D6 => "tres"
    case Digit.D7 => "halvfjerds"
    case Digit.D8 => "firs"
    case Digit.D9 => "halvfems"
}