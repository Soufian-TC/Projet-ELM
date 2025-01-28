module Drawing exposing (display, Program, Instruction(..))

import Svg exposing (Svg, line, svg)
import Svg.Attributes exposing (width, height, viewBox, stroke, x1, y1, x2, y2)

-- Types
type Instruction = Forward Float | Left Float | Right Float

type alias Program = List Instruction

type alias Turtle = { x : Float, y : Float, angle : Float}

-- degrees en radians
degrees_Radians degrees = degrees * pi / 180

--
display instructions =
    let centre = { x = 250, y = 250, angle = 0 }
        step instruction turtle = case instruction of
                Forward distance ->
                    let radianAngle = degrees_Radians turtle.angle
                        newX = turtle.x + (distance * cos radianAngle)
                        newY = turtle.y - (distance * sin radianAngle)
                        lineSvg = line
                                [ x1 (String.fromFloat turtle.x)
                                , y1 (String.fromFloat turtle.y)
                                , x2 (String.fromFloat newX)
                                , y2 (String.fromFloat newY)
                                , stroke "black"]
                                []
                    in ( { turtle | x = newX, y = newY }, lineSvg )

                Left angle -> ( { turtle | angle = turtle.angle - angle },svg [] [])

                Right angle -> ( { turtle | angle = turtle.angle + angle },svg [] [])

        -- Parcourt les instructions et accumule les lignes SVG
        draw instruction turtle = case instruction of
            [] -> [] 
            instr::q -> let (newTurtle, lineSvg) = step instr turtle in lineSvg :: draw q newTurtle
    in svg [ width "500", height "500", viewBox "0 0 500 500"]
        (draw instructions centre)
    
        
        