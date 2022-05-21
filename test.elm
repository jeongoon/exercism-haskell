module Bob exposing (hey)

sure      = "Sure."
whatever  = "Whatever."
chillout  = "Whoa, chill out!"
calmdown  = "Calm down, I know what I'm doing!"
beThatWay = "Fine. Be that way!"

hey : String -> String
hey remark =
    let remarkWithoutSpace = 
        remark
          |> String.toList
          |> List.filter
             (\ch -> not <| List.member ch ['\t', '\n', '\r', ' '])
        unsafe_isQuestion =
          remarkWithoutSpace
            |> List.reverse
            |> List.head
            |> (\ch -> ch == Just '?'')
         lettersOnly =
            remarkWithoutSpace
              |> List.filter
                 Char.isAlpha
          areAllLettersUpper =
             lettersOnly
               |> List.all Char.isUpper
    in
        if (remarkWithoutSpace |> List.isEmpty) then
            beThatWay
        else if areAllLettersUpper then
                if unsafe_isQuestion then
                    calmdown
                else
                    chillout
        
              else if unsafe_isQuestion then
                        sure
                   else
                        whatever
            
