You are tasked with implementing complex numbers and functions to operate on them.

Your implementation should be generic in the data type used to support the numeric computations. A minimal expectation is that it should support floats and doubles (in languages that distinguish these types).

The required operations are:

    Add, subtract, multiply and divide complex numbers
    Negate (w -> -w) and invert (w -> 1/w) a complex number
    Conjugate a complex number
    Extract the real and imaginary parts of a complex number
    Convert a complex number to cartesian coordinates
    Extract the magnitude and phase (in the range [-pi,pi]) of a complex number
    Convert a complex number to polar coordinates
    Convert a real number represented in the underlying type to a complex number
    Compute the complex absolute value and signum of a complex number as defined below

The complex absolute value of a number w has the same magnitude but phase 0. The complex signum of anumber w has the same phase but magnitude 1. The absolute value and signum of 0 = 0 + 0i are both also 0.

These two operations implicitly require that complex numbers with nonintegral components can be represented. This makes it in general impossible to use complex numbers based on ints, longs, Integers and similar types. You should aim to support as many underlying types as posible (including floats and doubles, as mentioned above), but your solution's behavior will not be tested with types that would lead to incorrect implementations.

