subroutine zerof2(func, x0, xap, epsi, nitmax,&
                  solu, iret, n)
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
    implicit none
! aslint: disable=W0307
#include "asterfort/utmess.h"
!
!     ARGUMENTS:
!     ----------
    interface
        function func(x)
            real(kind=8) :: func, x
        end function func
    end interface
    real(kind=8) :: x0, xap, epsi, solu
    integer :: nitmax, iret
! ----------------------------------------------------------------------
!     BUT:
!         TROUVER UNE RACINE DE L'EQUATION func(X)=0
!         ON SUPPOSE QUE LA FONCTION func EST CROISSANTE ET QUE func(X0)<0
!         ON EMPLOIE LA METHODE DE SECANTE UTILISEE DANS ZEROFO AVEC
!          EN PLUS UN "COUP" DE DICHOTOMIE TOUS LES 3 ITERATIONS
!          POUR FACILITER LA CONVERGENCE SI func EST TRES NON-LINEAIRE
!
!     IN:
!         func  : FONCTION DONT ON CHERCHE LE "ZERO"
!         X0 : POINT 0
!         XAP: APPROXIMATION DE LA SOLUTION.
!        EPSI: TOLERANCE ABSOLU SUR LE ZERO CHERCHE : ABS(func(SOLU))<EPSI
!      NITMAX: NOMBRE MAXI D'ITERATIONS AUTORISEES.
!
!     OUT:
!         SOLU: VALEUR DE LA RACINE CHERCHEE.
!         IRET: CODE RETOUR DE LA RECHERCHE DE ZERO DE func(X)=0
!                   IRET=0 => PAS DE PROBLEME
!                   IRET=1 => ECHEC
!     N       : NOMBRE D'ITERATIONS REALISEES
! ----------------------------------------------------------------------
    real(kind=8) :: fy, fz, x, y, z, a, b, fa, fb, fdbg(20), xdbg(20), ecresd
    real(kind=8) :: fx
    real(kind=8) :: valr(44)
    integer :: n, k, nd
    integer :: vali
! DEB-------------------------------------------------------------------
!
!     INITIALISATIONS
!
!
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    n = 1
    x = x0
    fx = func(x0)
    y = xap
    fy = func(y)
!
    if (abs(fy) .lt. epsi) then
        z = y
        goto 90
    endif
!
    if (abs(x-y) .le. 1d-15) then
        goto 100
    endif
!
!     DEBUT DES ITERATIONS
!
10  continue
    if (fy .gt. 0.d0) then
        a = x
        b = y
        fa = fx
        fb = fy
!       ND = INT(SQRT(DBLE(NITMAX)))
        nd = 3
20      continue
        if ((n-(n/nd)*nd) .eq. 0) then
            z = (a+b)*0.5d0
        else
            z = (a*fb-b*fa)/(fb-fa)
        endif
!
        n = n + 1
        fz = func(z)
!
        if (abs(fz) .lt. epsi) goto 90
        ecresd = abs(b-a)
! SOLUTION PROVISOIRE PERMETTANT DE PASSER LES CAS
! DIFFICILES CF AL98-193 AL98-197
! IL FAUDRAIT FAIRE MIEUX....
        if (ecresd .le. (epsi*b)) goto 90
        if (n .gt. nitmax) goto 98
        if (fz .lt. 0.d0) then
            a = z
            fa = fz
        else
            b = z
            fb = fz
        endif
        goto 20
    else
!
        if (fy .lt. fx) goto 99
!
        if (fy .eq. fx) then
            goto 100
        endif
!
        z = (x*fy-y*fx)/(fy-fx)
!
!
        if (abs(z-y) .le. 1d-15) then
            goto 100
        endif
!
        n = n + 1
        x = y
        fx = fy
        y = z
        fy = func(z)
!
!
        if (abs(fy) .lt. epsi) goto 90
        if (n .gt. nitmax) goto 98
    endif
    goto 10
!
90  continue
    solu=z
    goto 9999
!
98  continue
    iret = 1
    goto 9999
!
99  continue
    do 21 k = 1, 20
        xdbg(k) = xap/(21-k)
        fdbg(k) = func((xap)/(21-k))
21  end do
    vali = n
    valr (1) = x
    valr (2) = fx
    valr (3) = y
    valr (4) = fy
    do 30 k = 1, 20
        valr (4+k) = xdbg(k)
        valr (24+k) = fdbg(k)
30  end do
!
    call utmess('F', 'ELEMENTS5_39', si=vali, nr=44, valr=valr)
!
100  continue
    do 22 k = 1, 20
        xdbg(k) = xap/(21-k)
        fdbg(k) = func((xap)/(21-k))
22  end do
    vali = n
    valr (1) = x
    valr (2) = fx
    valr (3) = y
    valr (4) = fy
    do 31 k = 1, 20
        valr (4+k) = xdbg(k)
        valr (24+k) = fdbg(k)
31  end do
    call utmess('F', 'ELEMENTS5_40', si=vali, nr=44, valr=valr)
!
9999  continue
end subroutine
