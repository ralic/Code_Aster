subroutine di2epx(a, d)
!
    implicit none
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!======================================================================
!
!     DIAGONALISATION D'UNE MATRICE SYMETRIQUE 2x2
!
! IN  A : MATRICE SYM 2x2 SOUS FORME VECTORIEL 3x1 (A11,A22,A12)**t
!
! OUT D : MATRICE DIAG 2x2 SOUS FORME VECTORIEL 2x1 (D1,D2)**t
!
    real(kind=8) :: a(3), d(2), zero
    real(kind=8) :: aux, delta, rdelta, b, norme
!
    zero = 1.d-10
!
!     CALCUL DU (DELTA SUR QUATRE) DU POLYNOME CARACT DE LA MATRICE
!     POLY CARC = det(X*I-A) = X**2-X*(A(1)+A(2))+A(1)*A(2)-A(3)**2
!     DELTA / 4 = (A(1)-A(2))**2 / 4 + A(3)**2
!
    aux = (a(1)-a(2))*(a(1)-a(2))
    aux = 0.25d0 * aux
    delta = aux + a(3)*a(3)
!
    norme=abs(a(1))+abs(a(2))+abs(a(3))
!
!     CALCUL DE RACINE(DELTA/4)
    if (delta .ge. zero * norme**2) then
        rdelta = sqrt(delta)
    else
        rdelta=0.d0
    endif
!
!     CALCUL DES TERMES DE LA MATRICE DIAGONALISEE
    b = 0.5d0 * (a(1)+a(2))
    d(1) = b + rdelta
    d(2) = b - rdelta
!
end subroutine
