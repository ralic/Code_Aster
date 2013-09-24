subroutine zerofo(func, x0, xap, epsi, nitmax,&
                  solu, iret, n)
! aslint: disable=
    implicit none
!
    interface
        function func(x)
            real(kind=8) :: x
            real(kind=8) :: func
        end function
    end interface
    real(kind=8) :: x0, xap, epsi, solu
    integer :: nitmax, iret, n
!
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
! ----------------------------------------------------------------------
!     BUT:
!         TROUVER UNE RACINE DE L'EQUATION func(X)=0
!         ON SUPPOSE QUE LA FONCTION func EST CROISSANTE ET QUE func(X0)<0
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
!     IRET    : CODE RETOUR DE LA RECHERCHE DE ZERO
!               IRET=0 => PAS DE PROBLEME
!               IRET=1 => ECHEC DANS LA RECHERCHE DE ZERO
!     N       : NOMBRE D'ITERATIONS REALISEES
!
! ----------------------------------------------------------------------
    real(kind=8) :: fx, fy, fz, x, y, z, a, b
! DEB-------------------------------------------------------------------
!
!     INITIALISATIONS
!
    iret = 1
    n = 1
    x = x0
    fx = func(x0)
    if (abs(fx) .lt. epsi) then
        z=0.d0
        goto 800
    endif
    y = xap
    fy = func(y)
!
!     DEBUT DES ITERATIONS
!
10  continue
    if (fy .gt. 0.d0) then
        a = x
        b = y
20      continue
        if (fx .eq. fy) goto 999
        z = y - (y-x)*fy/(fy-fx)
        if (((z-a)*(z-b)) .gt. 0.d0) then
            z = (a+b)/2.d0
        endif
!
        n = n + 1
        fz = func(z)
        if (abs(fz) .lt. epsi) goto 800
        if (n .gt. nitmax) goto 999
        if (fz .lt. 0.d0) then
            a = z
        else
            b = z
        endif
        x = y
        fx = fy
        y = z
        fy = fz
        goto 20
    else
        if (fy .lt. fx) goto 999
        if (fx .eq. fy) goto 999
        z = y - (y-x)*fy/(fy-fx)
        n = n + 1
        x = y
        fx = fy
        y = z
        fy = func(z)
!
        if (abs(fy) .lt. epsi) goto 800
        if (n .gt. nitmax) goto 999
    endif
    goto 10
!
!     SUCCES
800  continue
    solu=z
    iret=0
!
!     SORTIE
999  continue
end subroutine
