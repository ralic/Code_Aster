subroutine ustuen(dimtub, tubuse, rcray, nomt19, ns,&
                  parusu, typusu)
    implicit   none
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/tbajli.h"
#include "asterfort/usvect.h"
    integer :: dimtub, ns, typusu(*)
    real(kind=8) :: tubuse(*), rcray, parusu(20, *)
    character(len=19) :: nomt19
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
!     GUIDAGE ENCO_1, ENCO_2
! ----------------------------------------------------------------------
    integer :: i, l, ifm, niv
    real(kind=8) :: tabr(4), deltan
    real(kind=8) :: alphad, alpham, alphaf, prof, ad, am, af
    complex(kind=8) :: c16b
    character(len=4) :: t2
    character(len=8) :: tabk(2)
    character(len=16) :: nopara(7)
!
    data nopara / 'LIEU'    , 'SECTEUR' , 'TYPE'    , 'ANGL_DEBUT',&
     &              'ANGL_FIN', 'ANGL_MAX', 'PROF_MAX' /
!-----------------------------------------------------------------------
!
    call infniv(ifm, niv)
!
    tabk(1) = 'TUBE'
    tabk(2) = 'TYPE'
    if (niv .ge. 2) write(ifm,1000)
    do 140 i = 1, ns
        if (typusu(i) .eq. 0) then
            goto 140
        else if (typusu(i) .eq. 1) then
            t2 = '   L'
        else if (typusu(i) .eq. 2) then
            t2 = 'LV_1'
        else if (typusu(i) .eq. 3) then
            t2 = ' V_1'
        else if (typusu(i) .eq. 4) then
            t2 = 'LV_2'
        else if (typusu(i) .eq. 5) then
            t2 = ' V_2'
        else
            t2 = '????'
        endif
        deltan = parusu(i,3) - parusu(i,1)
        tabk(2) = t2
        tabr(1) = parusu(i,1)
        tabr(2) = parusu(i,3)
        tabr(3) = parusu(i,2)
        tabr(4) = parusu(i,4)
        call tbajli(nomt19, 7, nopara, i, tabr,&
                    c16b, tabk, 0)
        if (niv .ge. 2) write(ifm, 1010) i, t2, parusu(i, 1), parusu( i, 3), parusu(i, 2),&
                        deltan, parusu(i, 4)
140  end do
!
!     TRACE DES USURES :
!     ------------------
!
    dimtub = 720
    do 30 l = 1, dimtub
        tubuse(2*l-1) = ( l - 1 ) * 0.5d0
        tubuse(2*l ) = rcray
30  end do
!
    do 150 i = 1, ns
!
        if (typusu(i) .eq. 0) goto 150
!
        alphad = parusu(i,1)
        alpham = parusu(i,2)
        alphaf = parusu(i,3)
        prof = parusu(i,4)
!
! ------ ON VERIFIE QUE ALPHAD < ALPHAM < ALPHAF
!
        ad = alphad
        if (alphad .lt. 0.d0) ad = alphad + 360.d0
!
        am = alpham
        if (ad .gt. alpham) am = alpham + 360.d0
!
        af = alphaf
        if (am .gt. alphaf) af = alphaf + 360.d0
!
        call assert(ad.lt.am .and. am.lt.af)
        call usvect(-1.d0, ad, am, af, prof,&
                    dimtub, tubuse)
!
150  end do
!
    1000 format('==> IMPRESSION DE PARAMETRES "TUBE" PAR SECTEUR USE:',/,&
     &       ' SECTEUR   TYPE     ANGL_DEBUT      ANGL_FIN',&
     &       '      ANGLE_MAX      DELTA_ANGL     PROFONDEUR')
    1010 format(1p,4x,i2,5x,a4,5(3x,e12.5))
!
end subroutine
