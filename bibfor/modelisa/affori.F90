subroutine affori(typ, nomt, cara, val, jad,&
                  jdno, jdco, ivr, nutyma, ntseg,&
                  carori, nco, ier)
    implicit none
#include "jeveux.h"
#include "asterc/r8dgrd.h"
#include "asterc/r8miem.h"
#include "asterc/r8prem.h"
#include "asterfort/angvxy.h"
#include "asterfort/utmess.h"
#include "asterfort/vdiff.h"
    integer :: nco, ivr(*), nutyma, ntseg, jad, jdno, jdco, ier
    character(len=*) :: typ, nomt, cara, carori(nco)
    real(kind=8) :: val(6)
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
! --------------------------------------------------------------------------------------------------
!
!   AFFECTATION DES ORIENTATIONS AUX POI1 ET SEG2 POSSIBLES DANS LE VECTEUR TAMPON TMPORI
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: car, nom
    character(len=24) :: valk(2)
    integer :: ibid, no1, no2, lg, i, locvr(2)
    real(kind=8) :: x1(3), x2(3), x3(3), angl(3), valr(2)
    real(kind=8) :: alpha, beta, gamma
    real(kind=8) :: dgrd
    real(kind=8) :: tst
!
! --------------------------------------------------------------------------------------------------
!
    dgrd = r8dgrd()
    tst = r8miem()
    car = cara
    nom = nomt
!
! --------------------------------------------------------------------------------------------------
! Vérifications preliminaires
!   ivr(1) = maille     0 = pas verif, 1 = verif
!   ivr(2) = noeud      0 = pas verif, 1 = verif
! En cas d'erreur ier+=1 ==> <F> en sortie dans la subroutine appelante
!
! Si pas de  vérification systématique :
    locvr(1:2) = ivr(1:2)
! Si vérification systématique :
    locvr(1) = 1
    locvr(2) = 1
! --------------------------------------------------------------------------------------------------
! calcul de la longueur du segment
    if (typ(1:6) .eq. 'MAILLE') then
        if (nutyma .eq. ntseg) then
            no1 = zi(jdno)
            no2 = zi(jdno+1)
            do i = 1, 3
                x1(i) = zr(jdco+(no1-1)*3+i-1)
                x2(i) = zr(jdco+(no2-1)*3+i-1)
            enddo
            call vdiff(3, x2, x1, x3)
            if (abs(x3(1)) .gt. tst .or. abs(x3(2)) .gt. tst .or. abs(x3(3)) .gt. tst) then
                lg = 1
            else
                lg = 0
            endif
        else
            lg = 0
        endif
    endif
!
! ---------------------------------------------------------------------- cara = "ANGL_VRIL"
    if (car .eq. carori(4)) then
        gamma = dgrd * val(1)
!   maille
        if (typ(1:6) .eq. 'MAILLE') then
!       si la maille n'est pas un seg2 > return
            if (nutyma .ne. ntseg) then
                if (locvr(1) .eq. 1) then
                    valk(1) = car
                    valk(2) = nom
                    call utmess('A', 'MODELISA_87', nk=2, valk=valk)
                    ier = ier + 1
                endif
                goto 999
            endif
!       si la maille (seg2) est de longueur nulle > return
            if (lg .eq. 0) then
                if (locvr(1) .eq. 1) then
                    valk(1) = car
                    valk(2) = nom
                    call utmess('A', 'MODELISA_88', nk=2, valk=valk)
                    ier = ier + 1
                endif
                goto 999
            endif
!       règles de surcharge
            if (abs(zr(jad+2)) .gt. r8prem()) then
                valk(1) = car
                valk(2) = nom
                valr(1) = zr(jad+2)
                valr(2) = gamma
                call utmess('A', 'MODELISA2_7', nk=2, valk=valk, nr=2,&
                            valr=valr)
            endif
!       si maille=seg2, longueur<>0 : affectation de gamma seul
            zr(jad+2) = gamma
            goto 999
        else
!       noeud : pas d affectation sur un noeud poi1 > return
            if (locvr(2) .eq. 1) then
                valk(1) = car
                valk(2) = nom
                call utmess('A', 'MODELISA_89', nk=2, valk=valk)
                ier = ier + 1
            endif
            goto 999
        endif
! ---------------------------------------------------------------------- cara = "ANGL_NAUT"
    else if (car .eq. carori(3)) then
        alpha = dgrd * val(1)
        beta = dgrd * val(2)
        gamma = dgrd * val(3)
!   maille
        if (typ(1:6) .eq. 'MAILLE') then
!       si la maille (seg2) est de longueur non nulle > return
            if (lg .eq. 1) then
                if (locvr(1) .eq. 1) then
                    valk(1) = car
                    valk(2) = nom
                    call utmess('A', 'MODELISA_90', nk=2, valk=valk)
                    ier = ier + 1
                endif
                goto 999
            else
!           règles de surcharge
                if (abs(zr(jad)) .gt. r8prem()) then
                    valk(1) = car
                    valk(2) = nom
                    valr(1) = zr(jad)
                    valr(2) = alpha
                    call utmess('A', 'MODELISA2_7', nk=2, valk=valk, nr=2,&
                                valr=valr)
                endif
                if (abs(zr(jad+1)) .gt. r8prem()) then
                    valk(1) = car
                    valk(2) = nom
                    valr(1) = zr(jad+1)
                    valr(2) = beta
                    call utmess('A', 'MODELISA2_7', nk=2, valk=valk, nr=2,&
                                valr=valr)
                endif
                if (abs(zr(jad+2)) .gt. r8prem()) then
                    valk(1) = car
                    valk(2) = nom
                    valr(1) = zr(jad+2)
                    valr(2) = gamma
                    call utmess('A', 'MODELISA2_7', nk=2, valk=valk, nr=2,&
                                valr=valr)
                endif
!           si maille seg2 (longueur=0) ou poi1 : 3 angles
                zr(jad) = alpha
                zr(jad+1) = beta
                zr(jad+2) = gamma
                goto 999
            endif
        else
!       noeud : règles de surcharge
            if (abs(zr(jad)) .gt. r8prem()) then
                valk(1) = car
                valk(2) = nom
                valr(1) = zr(jad)
                valr(2) = alpha
                call utmess('A', 'MODELISA2_7', nk=2, valk=valk, nr=2,&
                            valr=valr)
            endif
            if (abs(zr(jad+1)) .gt. r8prem()) then
                valk(1) = car
                valk(2) = nom
                valr(1) = zr(jad+1)
                valr(2) = beta
                call utmess('A', 'MODELISA2_7', nk=2, valk=valk, nr=2,&
                            valr=valr)
            endif
            if (abs(zr(jad+2)) .gt. r8prem()) then
                valk(1) = car
                valk(2) = nom
                valr(1) = zr(jad+2)
                valr(2) = gamma
                call utmess('A', 'MODELISA2_7', nk=2, valk=valk, nr=2,&
                            valr=valr)
            endif
            zr(jad) = alpha
            zr(jad+1) = beta
            zr(jad+2) = gamma
            goto 999
        endif
!
! ---------------------------------------------------------------------- cara = "VECT_X_Y"
    else if (car .eq. carori(2)) then
!   maille
        if (typ(1:6) .eq. 'MAILLE') then
!       si la maille (seg2) est de longueur non nulle > return
            if (lg .eq. 1) then
                if (locvr(1) .eq. 1) then
                    valk(1) = car
                    valk(2) = nom
                    call utmess('A', 'MODELISA_90', nk=2, valk=valk)
                    ier = ier + 1
                endif
                goto 999
            else
!           si maille seg2 (longueur=0) ou poi1 : 3 angles
                call angvxy(val(1), val(4), angl)
                alpha = angl(1)
                beta = angl(2)
                gamma = angl(3)
!           regles de surcharge
                if (abs(zr(jad)) .gt. r8prem()) then
                    valk(1) = car
                    valk(2) = nom
                    valr(1) = zr(jad)
                    valr(2) = alpha
                    call utmess('A', 'MODELISA2_7', nk=2, valk=valk, nr=2,&
                                valr=valr)
                endif
                if (abs(zr(jad+1)) .gt. r8prem()) then
                    valk(1) = car
                    valk(2) = nom
                    valr(1) = zr(jad+1)
                    valr(2) = beta
                    call utmess('A', 'MODELISA2_7', nk=2, valk=valk, nr=2,&
                                valr=valr)
                endif
                if (abs(zr(jad+2)) .gt. r8prem()) then
                    valk(1) = car
                    valk(2) = nom
                    valr(1) = zr(jad+2)
                    valr(2) = gamma
                    call utmess('A', 'MODELISA2_7', nk=2, valk=valk, nr=2,&
                                valr=valr)
                endif
                zr(jad) = alpha
                zr(jad+1) = beta
                zr(jad+2) = gamma
                goto 999
            endif
        else
!       noeud : si noeud (POI1) > affectation des 3 angles
            call angvxy(val(1), val(4), angl)
            alpha = angl(1)
            beta = angl(2)
            gamma = angl(3)
!       regles de surcharge
            if (abs(zr(jad)) .gt. r8prem()) then
                valk(1) = car
                valk(2) = nom
                valr(1) = zr(jad)
                valr(2) = alpha
                call utmess('A', 'MODELISA2_7', nk=2, valk=valk, nr=2,&
                            valr=valr)
            endif
            if (abs(zr(jad+1)) .gt. r8prem()) then
                valk(1) = car
                valk(2) = nom
                valr(1) = zr(jad+1)
                valr(2) = beta
                call utmess('A', 'MODELISA2_7', nk=2, valk=valk, nr=2,&
                            valr=valr)
            endif
            if (abs(zr(jad+2)) .gt. r8prem()) then
                valk(1) = car
                valk(2) = nom
                valr(1) = zr(jad+2)
                valr(2) = gamma
                call utmess('A', 'MODELISA2_7', nk=2, valk=valk, nr=2,&
                            valr=valr)
            endif
            zr(jad) = alpha
            zr(jad+1) = beta
            zr(jad+2) = gamma
            goto 999
        endif
! ---------------------------------------------------------------------- cara = "VECT_Y"
    else if (car .eq. carori(1)) then
!   maille
        if (typ(1:6) .eq. 'MAILLE') then
!       si la maille n est pas un seg2 > return
            if (nutyma .ne. ntseg) then
                if (locvr(1) .eq. 1) then
                    valk(1) = car
                    valk(2) = nom
                    call utmess('A', 'MODELISA_91', nk=2, valk=valk)
                    ier = ier + 1
                endif
                goto 999
            endif
!       si la maille (seg2) est de longueur nulle > goto 999
            if (lg .eq. 0) then
                if (locvr(1) .eq. 1) then
                    valk(1) = car
                    valk(2) = nom
                    call utmess('A', 'MODELISA_88', nk=2, valk=valk)
                    ier = ier + 1
                endif
                goto 999
            endif
!       si maille = seg2 longueur<>0 : affectation de gamma seul
            call angvxy(x3, val(1), angl)
            alpha = angl(1)
            beta = angl(2)
            gamma = angl(3)
!       regles de surcharge
            if (abs(zr(jad+2)) .gt. r8prem()) then
                valk(1) = car
                valk(2) = nom
                valr(1) = zr(jad+2)
                valr(2) = gamma
                call utmess('A', 'MODELISA2_7', nk=2, valk=valk, nr=2,&
                            valr=valr)
            endif
            zr(jad+2) = gamma
            goto 999
        else
!       noeud : pas d'affectation sur un noeud poi1 > return
            if (locvr(2) .eq. 1) then
                valk(1) = car
                valk(2) = nom
                call utmess('A', 'MODELISA_89', nk=2, valk=valk)
                ier = ier + 1
            endif
            goto 999
        endif
    endif
!
999  continue
end subroutine
