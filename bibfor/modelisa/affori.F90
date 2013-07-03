subroutine affori(typ, nomt, cara, val, jad,&
                  jdno, jdco, ivr, nutyma, ntseg,&
                  carori, nco, ier)
    implicit        none
#include "jeveux.h"
#include "asterc/r8dgrd.h"
#include "asterc/r8miem.h"
#include "asterc/r8prem.h"
#include "asterfort/angvxy.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
#include "asterfort/vdiff.h"
    integer :: nco, ivr(*), nutyma, ntseg, jad, jdno, jdco, ier
    character(len=*) :: typ, nomt, cara, carori(nco)
    real(kind=8) :: val(6)
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
!       AFFECTATION DES ORIENTATIONS AUX POI1 ET SEG2 POSSIBLES
!       DANS LE VECTEUR TAMPON TMPORI
!       ----------------------------------------------------------------
!
    character(len=16) :: cmd, car, nom
    character(len=24) :: valk(2)
    integer :: ibid, no1, no2, lg, i
    real(kind=8) :: x1(3), x2(3), x3(3), angl(3), valr(2)
    real(kind=8) :: alpha, beta, gamma
    real(kind=8) :: dgrd
    real(kind=8) :: tst
    common          /opcara/ cmd
!       ----------------------------------------------------------------
!
    dgrd = r8dgrd()
    tst = r8miem()
    car = cara
    nom = nomt
! --- ------------------------------------------------------------------
! ---   VERIFS PRELIMINAIRES
!       0 = PAS VERIF, 1 = VERIF, (1) = MAILLE, (2) = NOEUD, (3) = IMPR
!
! --- ------------------------------------------------------------------
! --- CALCUL DE LA LONGUEUR DU SEGMENT
    if (typ(1:6) .eq. 'MAILLE') then
        if (nutyma .eq. ntseg) then
            no1 = zi(jdno)
            no2 = zi(jdno+1)
            do 10 i = 1, 3
                x1(i) = zr(jdco+(no1-1)*3+i-1)
                x2(i) = zr(jdco+(no2-1)*3+i-1)
10          continue
            call vdiff(3, x2, x1, x3)
            if (abs(x3(1)) .gt. tst .or. abs(x3(2)) .gt. tst .or. abs(x3( 3)) .gt. tst) then
                lg = 1
            else
                lg = 0
            endif
        else
            lg = 0
        endif
    endif
!
! --- ------------------------------------------------------------------
! ---   CARA = "ANGL_VRIL"
    if (car .eq. carori(4)) then
        gamma = dgrd * val(1)
! --- ------------------------------------------------------------------
! ---    MAILLE
        if (typ(1:6) .eq. 'MAILLE') then
! ---       SI LA MAILLE N EST PAS UN SEG2 > RETURN
            if (nutyma .ne. ntseg) then
                if (ivr(1) .eq. 1) then
                    valk(1) = car
                    valk(2) = nom
                    call u2mesk('A', 'MODELISA_87', 2, valk)
                    ier = ier + 1
                endif
                goto 9999
            endif
! ---       SI LA MAILLE (SEG2) EST DE LONGUEUR NULLE > RETURN
            if (lg .eq. 0) then
                if (ivr(1) .eq. 1) then
                    valk(1) = car
                    valk(2) = nom
                    call u2mesk('A', 'MODELISA_88', 2, valk)
                    ier = ier + 1
                endif
                goto 9999
            endif
! --- ------------------------------------------------------------------
! ---    REGLES DE SURCHARGE
            if (abs(zr(jad+2)) .gt. r8prem()) then
                valk(1) = car
                valk(2) = nom
                valr(1) = zr(jad+2)
                valr(2) = gamma
                call u2mesg('A', 'MODELISA2_7', 2, valk, 0,&
                            ibid, 2, valr)
            endif
! ---       SI MAILLE=SEG2, LONGUEUR<>0 : AFFECTATION DE GAMMA SEUL
            zr(jad+2) = gamma
            goto 9999
        else
! --- ------------------------------------------------------------------
! ---    NOEUD
! --        PAS D AFFECTATION SUR UN NOEUD POI1 > RETURN
            if (ivr(2) .eq. 1) then
                valk(1) = car
                valk(2) = nom
                call u2mesk('A', 'MODELISA_89', 2, valk)
                ier = ier + 1
            endif
            goto 9999
        endif
    endif
!
! --- ------------------------------------------------------------------
! --- CARA = "ANGL_NAUT"
    if (car .eq. carori(3)) then
        alpha = dgrd * val(1)
        beta = dgrd * val(2)
        gamma = dgrd * val(3)
! --- ------------------------------------------------------------------
! ----   MAILLE
        if (typ(1:6) .eq. 'MAILLE') then
! ---       SI LA MAILLE (SEG2) EST DE LONGUEUR NON NULLE > RETURN
            if (lg .eq. 1) then
                if (ivr(1) .eq. 1) then
                    valk(1) = car
                    valk(2) = nom
                    call u2mesk('A', 'MODELISA_90', 2, valk)
                    ier = ier + 1
                endif
                goto 9999
            else
! ---          ---------------------------------------------------------
! ---          REGLES DE SURCHARGE
                if (abs(zr(jad)) .gt. r8prem()) then
                    valk(1) = car
                    valk(2) = nom
                    valr(1) = zr(jad)
                    valr(2) = alpha
                    call u2mesg('A', 'MODELISA2_7', 2, valk, 0,&
                                ibid, 2, valr)
                endif
                if (abs(zr(jad+1)) .gt. r8prem()) then
                    valk(1) = car
                    valk(2) = nom
                    valr(1) = zr(jad+1)
                    valr(2) = beta
                    call u2mesg('A', 'MODELISA2_7', 2, valk, 0,&
                                ibid, 2, valr)
                endif
                if (abs(zr(jad+2)) .gt. r8prem()) then
                    valk(1) = car
                    valk(2) = nom
                    valr(1) = zr(jad+2)
                    valr(2) = gamma
                    call u2mesg('A', 'MODELISA2_7', 2, valk, 0,&
                                ibid, 2, valr)
                endif
! ---          ---------------------------------------------------------
! ---          SI MAILLE SEG2 (LONGUEUR=0) OU POI1 : 3 ANGLES
                zr(jad) = alpha
                zr(jad+1) = beta
                zr(jad+2) = gamma
                goto 9999
            endif
        else
! --- ------------------------------------------------------------------
! ---    NOEUD
! ---       ------------------------------------------------------------
! ---       REGLES DE SURCHARGE
            if (abs(zr(jad)) .gt. r8prem()) then
                valk(1) = car
                valk(2) = nom
                valr(1) = zr(jad)
                valr(2) = alpha
                call u2mesg('A', 'MODELISA2_7', 2, valk, 0,&
                            ibid, 2, valr)
            endif
            if (abs(zr(jad+1)) .gt. r8prem()) then
                valk(1) = car
                valk(2) = nom
                valr(1) = zr(jad+1)
                valr(2) = beta
                call u2mesg('A', 'MODELISA2_7', 2, valk, 0,&
                            ibid, 2, valr)
            endif
            if (abs(zr(jad+2)) .gt. r8prem()) then
                valk(1) = car
                valk(2) = nom
                valr(1) = zr(jad+2)
                valr(2) = gamma
                call u2mesg('A', 'MODELISA2_7', 2, valk, 0,&
                            ibid, 2, valr)
            endif
! ---       ------------------------------------------------------------
! --        SI NOEUD (POI1) > AFFECTATION DES 3 ANGLES
            zr(jad) = alpha
            zr(jad+1) = beta
            zr(jad+2) = gamma
            goto 9999
        endif
    endif
!
! --- ------------------------------------------------------------------
! --- CARA = "VECT_X_Y"
    if (car .eq. carori(2)) then
! --- ------------------------------------------------------------------
! ---    MAILLE
        if (typ(1:6) .eq. 'MAILLE') then
! ---       SI LA MAILLE (SEG2) EST DE LONGUEUR NON NULLE > RETURN
            if (lg .eq. 1) then
                if (ivr(1) .eq. 1) then
                    valk(1) = car
                    valk(2) = nom
                    call u2mesk('A', 'MODELISA_90', 2, valk)
                    ier = ier + 1
                endif
                goto 9999
            else
! ---       SI MAILLE SEG2 (LONGUEUR=0) OU POI1 : 3 ANGLES
                call angvxy(val(1), val(4), angl)
                alpha = angl(1)
                beta = angl(2)
                gamma = angl(3)
! ---          ---------------------------------------------------------
! ---          REGLES DE SURCHARGE
                if (abs(zr(jad)) .gt. r8prem()) then
                    valk(1) = car
                    valk(2) = nom
                    valr(1) = zr(jad)
                    valr(2) = alpha
                    call u2mesg('A', 'MODELISA2_7', 2, valk, 0,&
                                ibid, 2, valr)
                endif
                if (abs(zr(jad+1)) .gt. r8prem()) then
                    valk(1) = car
                    valk(2) = nom
                    valr(1) = zr(jad+1)
                    valr(2) = beta
                    call u2mesg('A', 'MODELISA2_7', 2, valk, 0,&
                                ibid, 2, valr)
                endif
                if (abs(zr(jad+2)) .gt. r8prem()) then
                    valk(1) = car
                    valk(2) = nom
                    valr(1) = zr(jad+2)
                    valr(2) = gamma
                    call u2mesg('A', 'MODELISA2_7', 2, valk, 0,&
                                ibid, 2, valr)
                endif
! ---          ---------------------------------------------------------
                zr(jad) = alpha
                zr(jad+1) = beta
                zr(jad+2) = gamma
                goto 9999
            endif
        else
! --- ------------------------------------------------------------------
! ---    NOEUD
! ---       SI NOEUD (POI1) > AFFECTATION DES 3 ANGLES
            call angvxy(val(1), val(4), angl)
            alpha = angl(1)
            beta = angl(2)
            gamma = angl(3)
! ---       ------------------------------------------------------------
! ---       REGLES DE SURCHARGE
            if (abs(zr(jad)) .gt. r8prem()) then
                valk(1) = car
                valk(2) = nom
                valr(1) = zr(jad)
                valr(2) = alpha
                call u2mesg('A', 'MODELISA2_7', 2, valk, 0,&
                            ibid, 2, valr)
            endif
            if (abs(zr(jad+1)) .gt. r8prem()) then
                valk(1) = car
                valk(2) = nom
                valr(1) = zr(jad+1)
                valr(2) = beta
                call u2mesg('A', 'MODELISA2_7', 2, valk, 0,&
                            ibid, 2, valr)
            endif
            if (abs(zr(jad+2)) .gt. r8prem()) then
                valk(1) = car
                valk(2) = nom
                valr(1) = zr(jad+2)
                valr(2) = gamma
                call u2mesg('A', 'MODELISA2_7', 2, valk, 0,&
                            ibid, 2, valr)
            endif
! ---       ------------------------------------------------------------
            zr(jad) = alpha
            zr(jad+1) = beta
            zr(jad+2) = gamma
            goto 9999
        endif
    endif
!
! --- ------------------------------------------------------------------
! --- CARA = "VECT_Y"
    if (car .eq. carori(1)) then
! --- ------------------------------------------------------------------
! ---    MAILLE
        if (typ(1:6) .eq. 'MAILLE') then
! ---       SI LA MAILLE N EST PAS UN SEG2 > RETURN
            if (nutyma .ne. ntseg) then
                if (ivr(1) .eq. 1) then
                    valk(1) = car
                    valk(2) = nom
                    call u2mesk('A', 'MODELISA_91', 2, valk)
                    ier = ier + 1
                endif
                goto 9999
            endif
! ---       SI LA MAILLE (SEG2) EST DE LONGUEUR NULLE > GOTO 9999
            if (lg .eq. 0) then
                if (ivr(1) .eq. 1) then
                    valk(1) = car
                    valk(2) = nom
                    call u2mesk('A', 'MODELISA_88', 2, valk)
                    ier = ier + 1
                endif
                goto 9999
            endif
! ---       SI MAILLE = SEG2 LONGUEUR<>0 : AFFECTATION DE GAMMA SEUL
            call angvxy(x3, val(1), angl)
            alpha = angl(1)
            beta = angl(2)
            gamma = angl(3)
! ---       ---------------------------------------------------------
! ---       REGLES DE SURCHARGE
            if (abs(zr(jad+2)) .gt. r8prem()) then
                valk(1) = car
                valk(2) = nom
                valr(1) = zr(jad+2)
                valr(2) = gamma
                call u2mesg('A', 'MODELISA2_7', 2, valk, 0,&
                            ibid, 2, valr)
            endif
! ---       ---------------------------------------------------------
            zr(jad+2) = gamma
            goto 9999
        else
! --- ------------------------------------------------------------------
! ---    NOEUD
! ---       PAS D AFFECTATION SUR UN NOEUD POI1 > RETURN
            if (ivr(2) .eq. 1) then
                valk(1) = car
                valk(2) = nom
                call u2mesk('A', 'MODELISA_89', 2, valk)
                ier = ier + 1
            endif
            goto 9999
        endif
    endif
!
9999  continue
end subroutine
