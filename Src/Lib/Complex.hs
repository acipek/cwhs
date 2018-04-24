--Implementing complex numbers
module Src.Lib.Complex where
data Complex a = Complex a a
  deriving (Eq, Show)

instance RealFloat a => Num (Complex a) where
  (+) (Complex x y) (Complex z t) = Complex (x+z) (y+t)
  (-) (Complex x y) (Complex z t) = Complex (x-z) (y-t)
  (*) (Complex x y) (Complex z t) = Complex (x*z - y*t) (y*z + x*t)
  negate (Complex x y) = Complex (-1 * x) (-1 * y) 
  abs (Complex x y) = Complex (sqrt ((x ^ 2) + (y ^ 2))) 0
  signum (Complex 0 0) = Complex 0 0
  signum a@(Complex x y) = Complex (x/r) (y/r) where r = magnitude a
  fromInteger n = Complex (fromInteger n) 0

instance RealFloat a => Fractional (Complex a) where
  (/) (Complex x y) (Complex z t) = Complex ((x*z + y*t)/((z^2) + (t^2))) ((y*z - x*t) / ((z^2) + (t^2)))
  fromRational n = Complex (fromRational n) 0 

real, imaginary, magnitude, phase :: RealFloat a => Complex a -> a
real (Complex x _) = x
imaginary (Complex _ y) = y
magnitude (Complex x y) = sqrt ((x ^ 2) + (y ^ 2))
phase (Complex 0 0) = 0
phase (Complex x y) = atan2 y x

cartesian, polar :: RealFloat a => Complex a -> (a,a)
cartesian (Complex x y) = (x, y) 
polar c = (magnitude c, phase c) 

conjugate :: RealFloat a => Complex a -> Complex a
conjugate (Complex x y)  = Complex x (-y) 
