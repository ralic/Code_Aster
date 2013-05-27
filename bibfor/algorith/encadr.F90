subroutine encadr(f, x1, x2, f1, f2,&
                  niter, xmult, iret)
    implicit none
!
    interface
    function f(x)
        real(kind=8) :: f, x
    end function f
    end interface
    real(kind=8) :: x1, x2, f1, f2, xmult
    integer :: niter, iret
!
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     DETERMINATION D'UN ENCADREMENT DU ZERO D'UNE FONCTION.
!
! IN      F       : FONCTION F
! IN      PREC    : PRECISION ABSOLUE :
!                   LA SOLUTION EST TELLE QUE F(X)<PREC
! IN      NITER   : NOMBRE D'ITERATIONS MAXIMUM
! IN/OUT  X1      : BORNE A GAUCHE TROUVEE
! IN/OUT  X2      : BORNE A DROITE TROUVEE
! OUT     F1      : VALEUR DE F EN X1
! OUT     F2      : VALEUR DE F EN X2
! OUT     IRET    : CODE RETOUR : IRET = 0 : OK
!                                 IRET = 1 : PB
!
! ----------------------------------------------------------------------
!
    integer :: i
!
    iret = 1
!
    if (x1 .eq. x2) goto 9999
!
    f1 = f(x1)
    f2 = f(x2)
    do 10 i = 1, niter
        if (f1*f2 .lt. 0.d0) then
            iret = 0
            goto 9999
        endif
        if (abs(f1) .lt. abs(f2)) then
            x1 = x1 + xmult*(x1-x2)
            f1 = f(x1)
        else
            x2 = x2 + xmult*(x2-x1)
            f2 = f(x2)
        endif
10  end do
!
! ----------------------------------------------------------------------
!
9999  continue
end subroutine
