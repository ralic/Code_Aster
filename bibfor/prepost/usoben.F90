subroutine usoben(guidag, dimobs, obsuse, nco, rayo,&
                  thet, nbsect, parusu, typusu, nomt19,&
                  arete, arete2, rcarte, denc)
    implicit none
#include "asterc/r8dgrd.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/tbajli.h"
#include "asterfort/usenco.h"
#include "asterfort/usvect.h"
#include "asterfort/utmess.h"
    integer :: dimobs, nco, nbsect, typusu(*)
    real(kind=8) :: obsuse(*), parusu(20, *), rayo(*), thet(*), arete, arete2
    real(kind=8) :: rcarte, denc
    character(len=8) :: guidag
    character(len=19) :: nomt19
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     CALCULE LES TRACES USES D'OBSTACLE (GUIDAGE ENCO_1, ENCO_2)
!
!     PAR SECTEUR I
!        PARUSU(I,1) = ANGLE DEBUT
!        PARUSU(I,2) = ANGLE OU LA PROFONDEUR EST MAXI
!        PARUSU(I,3) = ANGLE FIN
!        PARUSU(I,4) = PROFONDEUR MAX
!
! ----------------------------------------------------------------------
    integer :: i, l, ifm, niv
    integer :: vali
    real(kind=8) :: theta, deltan, ai1, bi1, r, y
    real(kind=8) :: rad, tabr(4), aret1f, aret2f
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
    rad = r8dgrd( )
    aret1f = 360.d0 - arete
    aret2f = 180.d0 + arete2
!
    tabk(1) = 'OBST'
    tabk(2) = 'TYPE'
    if (niv .ge. 2) write(ifm,1000)
    do 10 i = 1, nbsect
        if (typusu(i) .eq. 0) then
            goto 10
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
10  end do
!
!     TRACE DES USURES :
!     ------------------
!
    dimobs = nco
    do 20 l = 1, dimobs
        obsuse(2*l-1) = thet(l) / rad
        obsuse(2*l ) = rcarte
20  end do
!
    do 100 i = 1, nbsect
!
        if (typusu(i) .eq. 0) goto 100
!
        alphad = parusu(i,1)
        alpham = parusu(i,2)
        alphaf = parusu(i,3)
        prof = parusu(i,4)
!
        if (abs(alpham-alphaf) .le. r8prem()) then
            vali = i
            call utmess('A', 'PREPOST6_4', si=vali)
            goto 100
        endif
!
        if (abs(alpham-alphad) .le. r8prem()) then
            vali = i
            call utmess('A', 'PREPOST6_5', si=vali)
            goto 100
        endif
!
! ------ VE + LUNULE SUR LA PREMIERE ENCOCHE
        if (typusu(i) .eq. 2) then
            if (i .eq. 2) then
                ai1 = prof / ( alpham - alphaf )
                bi1 = ai1*alphaf
                call usenco(ai1, bi1, 20.d0, alphaf, dimobs,&
                            obsuse)
            else
                ai1 = prof / ( alpham - alphad )
                bi1 = ai1*alphad
                call usenco(ai1, bi1, alphad, 340.d0, dimobs,&
                            obsuse)
            endif
!
! ------ VE SUR LA PREMIERE ENCOCHE
        else if (typusu(i) .eq. 3) then
            if (i .eq. 1) then
                ai1 = prof / ( alpham - alphaf )
                bi1 = ai1*alphaf
            else
                ai1 = prof / ( alpham - alphad )
                bi1 = ai1*alphad
            endif
            call usenco(ai1, bi1, alphad, alphaf, dimobs,&
                        obsuse)
!
! ------ VE + LUNULE SUR LA DEUXIEME ENCOCHE
        else if (typusu(i) .eq. 4) then
            if (nbsect .eq. 12 .and. i .eq. 8) then
                ai1 = prof / ( alpham - alphaf )
                bi1 = ai1*alphaf
            else if (nbsect.eq.10 .and. i.eq.7) then
                ai1 = prof / ( alpham - alphaf )
                bi1 = ai1*alphaf
            else
                ai1 = prof / ( alpham - alphad )
                bi1 = ai1*alphad
            endif
            call usenco(ai1, bi1, alphad, alphaf, dimobs,&
                        obsuse)
!
! ------ VE SUR LA DEUXIEME ENCOCHE
        else if (typusu(i) .eq. 5) then
            if (nbsect .eq. 12 .and. i .eq. 7) then
                ai1 = prof / ( alpham - alphaf )
                bi1 = ai1*alphaf
            else if (nbsect.eq.10 .and. i.eq.6) then
                ai1 = prof / ( alpham - alphaf )
                bi1 = ai1*alphaf
            else
                ai1 = prof / ( alpham - alphad )
                bi1 = ai1*alphad
            endif
            call usenco(ai1, bi1, alphad, alphaf, dimobs,&
                        obsuse)
!
! ------ LUNULE
        else
!
            ad = alphad
            if (alphad .lt. 0.d0) ad = alphad + 360.d0
            am = alpham
            if (ad .gt. alpham) am = alpham + 360.d0
            af = alphaf
            if (am .gt. alphaf) af = alphaf + 360.d0
            if (ad .lt. am .and. am .lt. af) then
                call usvect(1.d0, ad, am, af, prof,&
                            dimobs, obsuse)
            else
                ASSERT(.false.)
            endif
        endif
100  end do
!
    if (guidag .eq. 'ENCO_1') then
        do 200 l = 1, dimobs
            theta = obsuse(2*l-1)
            if (theta .lt. arete) then
                r = obsuse(2*l)
                y = r * sin(theta*rad)
                if (y .lt. denc) obsuse(2*l) = rayo(l)
            else if (theta .gt. aret1f) then
                r = obsuse(2*l)
                y = abs( r * sin( theta*rad ) )
                if (y .lt. denc) obsuse(2*l) = rayo(l)
            endif
200      continue
    else if (guidag .eq. 'ENCO_2') then
        do 210 l = 1, dimobs
            theta = obsuse(2*l-1)
            if (theta .lt. arete) then
                r = obsuse(2*l)
                y = r * sin(theta*rad)
                if (y .lt. denc) obsuse(2*l) = rayo(l)
            else if (theta.gt.arete2 .and. theta.lt.180.d0) then
                r = obsuse(2*l)
                y = r * sin( theta*rad )
                if (y .lt. denc) obsuse(2*l) = rayo(l)
            else if (theta.gt.180.d0 .and. theta.lt.aret2f) then
                r = obsuse(2*l)
                y = abs( r * sin( theta*rad ) )
                if (y .lt. denc) obsuse(2*l) = rayo(l)
            else if (theta .gt. aret1f) then
                r = obsuse(2*l)
                y = abs( r * sin( theta*rad ) )
                if (y .lt. denc) obsuse(2*l) = rayo(l)
            endif
210      continue
    endif
!
    1000 format('==> IMPRESSION DE PARAMETRES "OBST" PAR SECTEUR USE:',/,&
     &       ' SECTEUR   TYPE     ANGL_DEBUT      ANGL_FIN',&
     &       '      ANGLE_MAX      DELTA_ANGL     PROFONDEUR')
    1010 format(1p,4x,i2,5x,a4,5(3x,e12.5))
!
end subroutine
