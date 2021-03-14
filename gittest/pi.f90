MODULE multimal_arithmetic

  IMPLICIT NONE
  INTEGER :: mal = 10 ** 5, prec

  ! .plus.  = +
  INTERFACE OPERATOR(.plus.)
    MODULE PROCEDURE add
  END INTERFACE

  ! .minus. = -
  INTERFACE OPERATOR(.minus.)
    MODULE PROCEDURE subtract
  END INTERFACE

  ! .times. = *
  INTERFACE OPERATOR(.times.)
    MODULE PROCEDURE multiply
  END INTERFACE

  ! .div.   = /
  INTERFACE OPERATOR(.div.)
    MODULE PROCEDURE divide
  END INTERFACE

CONTAINS

  !
  ! z = x + y
  !
  FUNCTION add( x, y ) RESULT( z )

    IMPLICIT NONE
    INTEGER, INTENT(IN) :: x(0:prec), y(0:prec)
    INTEGER :: z(0:prec), i, zi, r

    r = 0
    DO i = prec, 0, - 1
      zi = x(i) + y(i) + r
      r = zi / mal                       ! 繰り上がり
      z(i) = zi - r * mal
    END DO

  END FUNCTION add

  !
  ! z = x - y
  !
  FUNCTION subtract( x, y ) RESULT( z )

    IMPLICIT NONE
    INTEGER, INTENT(IN) :: x(0:prec), y(0:prec)
    INTEGER :: z(0:prec), i, zi, r

    r = 1
    DO i = prec, 0, - 1
      zi = x(i) + ( mal - 1 - y(i) ) + r
      r = zi / mal                       ! 繰り上がり
      z(i) = zi - r * mal
    END DO

  END FUNCTION subtract

  !
  ! z = x * s    ( s = scalar integer )
  !
  FUNCTION multiply( x, s ) RESULT( z )

    IMPLICIT NONE
    INTEGER, INTENT(IN) :: x(0:prec), s
    INTEGER :: z(0:prec), i, r, zi

    r = 0
    DO i = prec, 0, - 1
      zi   = x(i) * s + r
      r    = zi / mal                    ! 繰り上がり
      z(i) = zi - r * mal
    END DO

  END FUNCTION multiply

  !
  ! z = x / s    ( s = scalar integer )
  !
  FUNCTION divide( x, s ) RESULT( z )

    IMPLICIT NONE
    INTEGER, INTENT(IN) :: x(0:prec), s
    INTEGER :: z(0:prec), i, r, zi

    r = 0
    DO i = 0, prec
      zi   = x(i) + r * mal
      z(i) = zi / s
      r    = zi - s * z(i)               ! 剰り
    END DO

  END FUNCTION divide

END MODULE multimal_arithmetic
PROGRAM pi_main

  USE multimal_arithmetic
  IMPLICIT NONE
  INTEGER :: decimal
  INTEGER, ALLOCATABLE :: pi(:), pi1(:), pi2(:), pi3(:)

  PRINT '(1X, A)', 'How many decimals ? ( 0 < decimal < 100,000 ): '
  READ *, decimal
  prec  =  CEILING( decimal / 5.0E0 ) + 1

  ALLOCATE( pi(0:prec), pi1(0:prec), pi2(0:prec), pi3(0:prec) )

  pi1 = arctan( 10 )
  pi1 = pi1 .times. 32
  pi2 = arctan( 239 )
  pi2 = pi2 .times.  4 
  pi3 = arctan( 515 )
  pi3 = pi3 .times. 16
  pi  = pi1 .minus. pi2 
  pi  = pi  .minus. pi3

  PRINT '(20X,"*****  Pi ( ", I6, " decimals ) *****"/)', decimal
  PRINT '(1X, "Pi = 3.", I5.5, 9I6.5/ 19(7X, 10I6.5/)/ 20(7x, 10I6.5/)/ )', &
         pi(1:prec-1)

CONTAINS
  !
  ! Arctan(1/k)
  !
  FUNCTION arctan( k ) RESULT( a )

    IMPLICIT NONE
    INTEGER, INTENT(IN) :: k
    INTEGER :: a(0:prec), a0(0:prec), unity(0:prec), n, nmax

    nmax = INT( 0.5E0 * decimal / LOG10( REAL( k ) ) ) + 1

    unity(0) = 1                                ! unity = 1.00000 00000...
    unity(1:prec) = 0

    a = 0
    DO n = nmax, 1, -1
      a0 = unity .div. (2*n + 1)
      a = a0 .minus. a
      a = a .div. k
      a = a .div. k
    END DO
      a0 = unity             ! for n = 0
      a = a0 .minus. a 
      a = a .div. k

  END FUNCTION arctan

END PROGRAM pi_main