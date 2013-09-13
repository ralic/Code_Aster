subroutine caltol(np3, nbnl, typch, nbseg, rc,&
                  theta, tol, tolc, toln, tolv)
    implicit none
!-----------------------------------------------------------------------
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
! ======================================================================
!-----------------------------------------------------------------------
! DESCRIPTION : CALCUL DES TOLERANCES
! -----------
!               APPELANT : MDITM2
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
#include "asterc/r8pi.h"
#include "asterfort/utmess.h"
    integer :: np3, nbnl, typch(*), nbseg(*)
    real(kind=8) :: rc(np3, *), theta(np3, *), tol, tolc, toln, tolv
!
! VARIABLES LOCALES
! -----------------
    integer :: i, ic, nbs, typobs
    real(kind=8) :: tolx, pi, jeu, a1, b1, r, r1, r2, t
!
! FONCTIONS INTRINSEQUES
! ----------------------
!     INTRINSIC  ABS, COS, MIN, SIN, SQRT
!
! FONCTIONS EXTERNES
! ------------------
!     EXTERNAL   R8PI
!
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    tolx = 1.0d-03
    tol = 1.0d+10
    pi = r8pi()
!
    do 10 ic = 1, nbnl
        typobs = typch(ic)
        nbs = nbseg(ic)
        if ((typobs.eq.0) .or. (typobs.eq.1) .or. (typobs.eq.2)) then
            jeu = rc(1,ic)
        else
            jeu = rc(1,ic)
            do 20 i = 2, nbs
                r1 = rc(i-1,ic)
                r2 = rc(i ,ic)
                t = theta(i,ic) - theta(i-1,ic)
                if (t .lt. pi) then
                    a1 = (r2-r1*cos(t))
                    b1 = r1*sin(t)
                    r = abs(r2*b1/sqrt(a1*a1+b1*b1))
                    if (r .lt. jeu) jeu = r
                else
                    call utmess('F', 'ALGORITH_73')
                endif
20          continue
        endif
        tol = min(tol,tolx*jeu)
10  end do
!
    tolc = tol * 10.0d0
    toln = tol * 1.0d-04
    tolv = tol * 200.0d0
!
! --- FIN DE CALTOL.
end subroutine
