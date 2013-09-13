subroutine fgpic2(method, rtrv, point, npoint, pic,&
                  npic)
!       ----------------------------------------------------------------
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
!       ----------------------------------------------------------------
!       REARRANGEMENT ET EXTRACTION DES PICS
!       ----------------------------------------------------------------
!       IN  POINT VECTEUR DES POINTS
!           NPOINT NOMBRE DE POINTS
!           RTRV VECTEUR DE TRAVAIL REEL
!           METHOD METHODE DE DEXTRACTION DES PICS EMPLOYEE
!       OUT PIC VECTEUR DES PICS
!           NPIC NOMBRE DE PICS (NPIC = NPOINT AU MAXIMUM)
!       ----------------------------------------------------------------
!
    implicit none
#include "asterfort/utmess.h"
    integer :: i, npoint, npic, nmax, ntrv
    real(kind=8) :: point(*), pic(*), rtrv(*), pmax, pinter
    real(kind=8) :: dp1, dp2, epsi
    character(len=*) :: method
    character(len=16) :: k16b
!
    epsi = 1.0d-6
!
!     ----------------------------------------------------------------
! -   EXTRACTION DES PICS POUR RAINFLOW=PIC LE PLUS GRAND EN DEBUT
!     ----------------------------------------------------------------
    if (( method.eq.'RAINFLOW' ) .or. ( method.eq.'RFLO_MAX' )) then
!
! -     RECHERCHE DU POINT LE PLUS GRAND EN VALEUR ABSOLUE
!
        pmax = abs(point(1))
        nmax = 1
        do 10 i = 2, npoint
            if (abs(point(i)) .gt. pmax*(1.0d0+epsi)) then
                pmax = abs(point(i))
                nmax = i
            endif
10      continue
        pmax = point(nmax)
!
! -     REARANGEMENT AVEC POINT LE PLUS GRAND AU DEBUT ET A LA FIN
!
        do 20 i = nmax, npoint
            rtrv(i-nmax+1) = point(i)
20      continue
        do 30 i = 1, nmax-1
            rtrv(npoint-nmax+1+i) = point(i)
30      continue
        ntrv = npoint
!
! -     EXTRACTION DES PICS SUR LE VECTEUR REARANGE
!
! -      LE PREMIER POINT EST UN PIC
        npic = 1
        pic(1) = rtrv(1)
        pinter = rtrv(2)
!
! -     ON RECHERCHE TOUS LES PICS
        do 40 i = 3, ntrv
            dp1 = pinter - pic(npic)
            dp2 = rtrv(i) - pinter
!
! -         ON CONSERVE LE POINT INTERMEDIAIRE COMME UN PIC
            if (dp2*dp1 .lt. 0.d0) then
                npic = npic+1
                pic(npic) = pinter
            endif
!
! -         LE DERNIER POINT DEVIENT POINT INTERMEDIAIRE
            pinter = rtrv(i)
40      continue
!
! -      LE DERNIER POINT EST UN PIC
        npic = npic+1
        pic(npic) = rtrv(ntrv)
    else
        k16b = method(1:16)
        call utmess('F', 'PREPOST_4', sk=k16b)
    endif
!
end subroutine
