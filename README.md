Idris to Ruby back end
----------------------

As Edwin Brady said:

Please, don't ever use this.

This repo is based on Edwin's PHP back end for Idris. It targets the ANF intermediate representation and hasn't been optimized at all!

Example (taken from Edwin)
--------------------------

    beaker:edwin$ cat pythag.idr 
    module Main

    pythag : Int -> List (Int, Int, Int)
    pythag max = [(x, y, z) | z <- [1..max], y <- [1..z], x <- [1..y],
                              x * x + y *y == z * z]

    main : IO ()
    main = print (pythag 50)

    beaker:edwin$ idris pythag.idr --codegen ruby -o pythag.rb
    beaker:edwin$ rb pythag.rb 
    [(3, (4, 5)), (6, (8, 10)), (5, (12, 13)), (9, (12, 15)), (8, (15, 17)), 
     (12, (16, 20)), (15, (20, 25)), (7, (24, 25)), (10, (24, 26)), (20, (21, 29)), 
     (18, (24, 30)), (16, (30, 34)), (21, (28, 35)), (12, (35, 37)), (15, (36, 39)), 
     (24, (32, 40)), (9, (40, 41)), (27, (36, 45)), (30, (40, 50)), (14, (48, 50))]

