function spect3(x, a, b, func, tol,&
                coeff, xlc, vitn, defm, rhoe,&
                nbp, im, jm)
!-----------------------------------------------------------------------
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
! **********************************************************************
    implicit none
!
!                                 A
! DESCRIPTION : CALCULE DINTG2 = S func(X,Y) DY   POUR DINTEG.
! -----------                     B
!
!               (S EST LE SYMBOLE DE L'INTEGRALE).
!
!               TOL DONNE LE SEUIL DE CONVERGENCE RELATIVE.
!
!               func EST LA FONCTION A INTEGRER.
!               ELLE DOIT ETRE DECLAREE EXTERNAL DANS L'APPELANT.
!               SA SPECIFICATION EST :
!                        DOUBLE PRECISION FUNCTION func ( X, Y )
!                        DOUBLE PRECISION X, Y
!
!               A ET B DONNENT LES BORNES DE L'INTEGRALE.
!               COEFF EST LE TABLEAU DES COEFFICIENTS FOURNI PAR DINTEG.
!
! *****************   DECLARATIONS DES VARIABLES   *********************
!
!
! ARGUMENTS
! ---------
! aslint: disable=W0307
#include "jeveux.h"
    integer :: nbp
    real(kind=8) :: x
    real(kind=8) :: a
    real(kind=8) :: b
    real(kind=8) :: tol
    real(kind=8) :: coeff(*)
    real(kind=8) :: xlc
    real(kind=8) :: vitn(nbp, *)
    real(kind=8) :: defm(nbp, *)
    real(kind=8) :: rhoe(nbp, *)
    integer :: im
    integer :: jm
    real(kind=8) :: spect3
    interface
        function func(xx, y, xlc, vitn, rhoe,&
                      defm, nbp, im, jm)
            integer :: nbp
            real(kind=8) :: xx
            real(kind=8) :: y
            real(kind=8) :: xlc
            real(kind=8) :: vitn(nbp, *)
            real(kind=8) :: rhoe(nbp, *)
            real(kind=8) :: defm(nbp, *)
            integer :: im
            integer :: jm
            real(kind=8) :: func
        end function func
    end interface
!
! VARIABLES LOCALES
! -----------------
    integer :: index, n1, n2, i, arret
    real(kind=8) :: res, ym, dy, y0, r1, som, y
    real(kind=8) :: w(127)
!
! *****************    DEBUT DU CODE EXECUTABLE    *********************
!
    res = 0.0d0
!
    if (abs(a-b) .lt. 1.0d-30) then
        spect3 = res
        goto 9999
    endif
!
    ym = ( a + b ) / 2.0d0
    dy = ( b - a ) / 2.0d0
    y0 = func(x, ym ,xlc, vitn, rhoe, defm, nbp, im, jm)
    r1 = (y0+y0) * dy
    index = 0
    n1 = 0
    n2 = 1
    som = 0.0d0
!
! --- REPETER ...
!
10  continue
    n1 = n1 + n2
    do 20 i = n2, n1
        index = index + 1
        y = coeff(index) * dy
        w(i) = func(x,ym+y,xlc,vitn,rhoe,defm,nbp,im,jm) + &
               func(x,ym-y,xlc, vitn,rhoe,defm,nbp,im,jm)
        index = index + 1
        som = som + coeff(index)*w(i)
20  end do
    n2 = n1 + 1
    index = index + 1
    res = ( som + coeff(index)*y0 ) * dy
!
! --- TEST DE CONVERGENCE.
!
    if (abs(res-r1) .le. abs(r1*tol)) then
        arret = 1
    else
        if (n1 .ge. 127) then
            arret = 1
        else
            arret = 0
            r1 = res
            som = 0.0d0
            do 22 i = 1, n1
                index = index + 1
                som = som + coeff(index)*w(i)
22          continue
        endif
    endif
!
! --- JUSQUE ARRET = 1.
!
    if (arret .eq. 0) goto 10
!
    spect3 = res
!
9999  continue
end function
