#lang aful
(require aful/reader)
(use-aful-readtable)
(aful-read (open-input-string "#λ(+ % %2)"))
(use-aful-readtable #:arg-str "_")
(map #λ(+ _ _2) '(1 2 3) '(1 2 3))
;;'(2 4 6)

;;create a list of expressions of the cost function based and then supply it to the open-input-string of the readtable
;;https://www.khanacademy.org/math/ap-calculus-ab/ab-diff-analytical-applications-new/ab-5-11/v/minimizing-the-cost-of-a-storage-container
;; Determine the order of placing circles so that they cover each other up correctly
;;Very important!!! Add one more thing
;;So, what if you have a red square with a blue circle in the dead center of it.
;; You create the red circle first, then the blue circle.
;; Create a list of circles id's from which one should be place first, to which one should be placed last.
;;; Determine through the proximity. The ideal level should be determined by the amount of iterations it must make to satisfy the lowest range
;;; The amount of circles it created before the limit of proximity was reached.  If it took 5 circles with radius size 1 before the proximity was unsatisfied, then that circle will be placed underneath the other circle.
;; Which means, it must find the ellipses of the circle. This is pretty simple, but can be hard to track.
;; There can be multiple circles getting into each circle, and circles that overlap other cirlcles in a circle create weird shapes.
;; Define what those shapes look like for matching hard shapes like a square

;; Solve for proximity, or how close one circle's border is to the center of another cirle. This parameter is more to help in the assisting of the solver
;; It will look to see the radius to the border of that circle, and determine whether its worth it.
;;variance is which is greatest change of values along the two circles' circumfrence.
;; In the case it is, it will try to extend the radius to other circles's centers by finding the distance between the pixels, and then find the midline, where it will solve the ideal radius between the circles, determining the range between the two circles's, assuming the variances are less than 51
;; This allows for multiple circles to be in one/or more bigger circle/s 
;;not, it will 

;Write the equation of the horizontal line that is tangent to the curve and is above the x-axis
;dy/dx = -2(x + 3)/ 4y^3

;y^4 = 16  y = 2 y = -2

;when x = -3 dy/dx = 0

;visualize it as a box of functions: λ * λ * λ
;Cost λ = λ * λ * λ + (cost of sides or surface area) or (2 * 6xh + 2 * 6 * 2x * h)
;λ * 2λ * h = Q
;2λ^2 * h = 10
;h = Q/2x^2
;h = 5/x^2

;So...
;20x^2 + 12xh + 24xh
;20x^2 + 36 xh
;20x^2 + 36x(5 / x^2)

;C(x) = 20x^2 + 180x^-1
;C'(x) = 40x - 180x^-2
;x = (9/2)^1/3 ~ 1.65

;C''(x) = 40 + 360/x^3
;C''(1.65) > 0
;concave upwards

;40x - 180x^-2 = 0
;40x = 180/x^2
;40x^3 = 180
;x^3 = 180/40 = 9/2
;C(1.65) ~ 163.54

;Find the smallest cost representation based on the functions. The functions themselves will rely off of others, and will need to be evaluated as well through synthax rosette. Pass them into the aful, and combine it to solve, finding the output.
;From there, create an AST tree, that saves the values put in as variables for the solver replication. These will be the inputs for the image
;For example, circle 1: radius ??, x: ??, y: ??.
;Radius: upper: ??, lower: ??, variance: ??, range: ??
;X/Y: color: ??, monochrome-level: ??, proximity: ??

;https://mathworld.wolfram.com/Circle-CircleIntersection.html
;https://mathworld.wolfram.com/RadicalLine.html
;https://en.wikipedia.org/wiki/Radical_axis
;http://www.awarenessofnothing.com/sacred-geometry-ndashthe-building-blocks-of-the-universe.html
;The generation of a Fourier transform can be used to reveal regularity and periodicity within an image
;https://brettcvz.github.io/epicycles/
;https://docs.racket-lang.org/math/array_other.html