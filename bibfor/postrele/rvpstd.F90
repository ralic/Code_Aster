subroutine rvpstd(valee, type, codir, valdir, valeq)
    implicit none
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
#include "asterc/r8vide.h"
    real(kind=8) :: valee(*), valeq(*), valdir(*)
    character(len=2) :: type
    integer :: codir, indir1(3), indir2(4), indir3(3)
!
!-----------------------------------------------------------------------
    integer :: i
!-----------------------------------------------------------------------
    data indir1 / 2 , 1 , 3 /
    data indir2 / 2 , 1 , 4 , 3 /
    data indir3 / 2 , 3 , 1  /
!
!**********************************************************************
!
!  OPERATION REALISEE
!  ------------------
!
!     CALCUL DE LA TRACE NORMALE EN UN POINT
!
!  ARGUMENTS EN ENTREE
!  -------------------
!
!     VALEE  : TABLE DES VALEUR DES CMP (REPERE GLOBAL)
!     TYPE     VAUT 'V3' POUR LES VECTEURS ET
!              'T2' POUR LES TENSEUR 2X2 ET 'T3' POUR LES 3X3
!     CODIR  : CODE LES DIRECTIONS ACTIVES
!     VALDIR : VALEURS DU VECTEUR DIRECTION (TJS X, Y, Z)
!
!  ARGUMENTS EN SORTIE
!  -------------------
!
!     VALEE  : TABLE DES VALEUR DES TRACES(REPERE GLOBAL)
!
!**********************************************************************
!
!==================== CORPS DE LA ROUTINE =============================
!
    if (codir .eq. 1) then
!
!     /* DIRECTION ACTIVE : X */
!
        if (type .eq. 'V3') then
!
            if (valee(1) .eq. r8vide()) then
                valeq(1) = 0.d0
            else
                valeq(1) = valee(1)*valdir(1)
            endif
!
        else if (type .eq. 'T3') then
!
            do 10 i = 1, 3
                if (valee(i) .eq. r8vide()) then
                    valeq(i) = 0.d0
                else
                    valeq(i) = valee(i)*valdir(1)
                endif
10          continue
!
        else
!
            do 12 i = 1, 4
                if (valee(i) .eq. r8vide()) then
                    valeq(i) = 0.d0
                else
                    valeq(i) = valee(i)*valdir(1)
                endif
12          continue
!
        endif
!
    else if (codir .eq. 2) then
!
!     /* DIRECTION ACTIVE : Y */
!
        if (type .eq. 'V3') then
!
            if (valee(1) .eq. r8vide()) then
                valeq(1) = 0.d0
            else
                valeq(1) = valee(1)*valdir(2)
            endif
!
        else if (type .eq. 'T3') then
!
            do 22 i = 1, 3
                if (valee(indir1(i)) .eq. r8vide()) then
                    valeq(i) = 0.d0
                else
                    valeq(i) = valee(indir1(i))*valdir(2)
                endif
22          continue
!
        else
!
            do 24 i = 1, 4
                if (valee(indir2(i)) .eq. r8vide()) then
                    valeq(i) = 0.d0
                else
                    valeq(i) = valee(indir2(i))*valdir(2)
                endif
24          continue
!
        endif
!
    else if (codir .eq. 3) then
!
!     /* DIRECTION ACTIVE : Z */
!
        if (type .eq. 'V3') then
!
            if (valee(1) .eq. r8vide()) then
                valeq(1) = 0.d0
            else
                valeq(1) = valee(1)*valdir(3)
            endif
!
        else
!
            do 30 i = 1, 3
                if (valee(indir3(i)) .eq. r8vide()) then
                    valeq(i) = 0.d0
                else
                    valeq(i) = valee(indir3(i))*valdir(3)
                endif
30          continue
!
        endif
!
    else if (codir .eq. 4) then
!
!     /* DIRECTION ACTIVE : X,Y */
!
        if (type .eq. 'V3') then
!
            do 40 i = 1, 2
                if (valee(i) .eq. r8vide()) valee(i) = 0.d0
40          continue
            valeq(1) = valee(1)*valdir(1) + valee(2)*valdir(2)
!
        else if (type .eq. 'T3') then
!
            do 42 i = 1, 5
                if (valee(i) .eq. r8vide()) valee(i) = 0.d0
42          continue
            valeq(1) = valee(1)*valdir(1) + valee(3)*valdir(2)
            valeq(2) = valee(3)*valdir(1) + valee(2)*valdir(2)
            valeq(3) = valee(4)*valdir(1) + valee(5)*valdir(2)
!
        else
!
            do 44 i = 1, 6
                if (valee(i) .eq. r8vide()) valee(i) = 0.d0
44          continue
            valeq(1) = valee(1)*valdir(1) + valee(3)*valdir(2)
            valeq(2) = valee(3)*valdir(1) + valee(2)*valdir(2)
            valeq(3) = valee(4)*valdir(1) + valee(6)*valdir(2)
            valeq(4) = valee(6)*valdir(1) + valee(5)*valdir(2)
!
        endif
!
    else if (codir .eq. 5) then
!
!     /* DIRECTION ACTIVE : X,Z */
!
        if (type .eq. 'V3') then
!
            do 50 i = 1, 2
                if (valee(i) .eq. r8vide()) valee(i) = 0.d0
50          continue
            valeq(1) = valee(1)*valdir(1) + valee(2)*valdir(3)
!
        else
!
            do 52 i = 1, 5
                if (valee(i) .eq. r8vide()) valee(i) = 0.d0
52          continue
            valeq(1) = valee(1)*valdir(1) + valee(4)*valdir(3)
            valeq(2) = valee(3)*valdir(1) + valee(5)*valdir(3)
            valeq(3) = valee(4)*valdir(1) + valee(2)*valdir(3)
!
        endif
!
    else if (codir .eq. 6) then
!
!     /* DIRECTION ACTIVE : Y,Z */
!
        if (type .eq. 'V3') then
!
            do 60 i = 1, 2
                if (valee(i) .eq. r8vide()) valee(i) = 0.d0
60          continue
            valeq(1) = valee(1)*valdir(2) + valee(2)*valdir(3)
!
        else
!
            do 62 i = 1, 5
                if (valee(i) .eq. r8vide()) valee(i) = 0.d0
62          continue
            valeq(1) = valee(3)*valdir(2) + valee(4)*valdir(3)
            valeq(2) = valee(1)*valdir(2) + valee(5)*valdir(3)
            valeq(3) = valee(5)*valdir(2) + valee(2)*valdir(3)
!
        endif
!
    else
!
!     /* DIRECTION ACTIVE : X,Y,Z */
!
        if (type .eq. 'V3') then
!
            do 70 i = 1, 3
                if (valee(i) .eq. r8vide()) valee(i) = 0.d0
70          continue
            valeq(1) = valee(1)*valdir(1) + valee(2)*valdir(2) + valee(3)*valdir(3)
!
        else
!
            do 72 i = 1, 6
                if (valee(i) .eq. r8vide()) valee(i) = 0.d0
72          continue
            valeq(1) = valee(1)*valdir(1) + valee(4)*valdir(2) + valee(5)*valdir(3)
            valeq(2) = valee(4)*valdir(1) + valee(2)*valdir(2) + valee(6)*valdir(3)
            valeq(3) = valee(5)*valdir(1) + valee(6)*valdir(2) + valee(3)*valdir(3)
!
        endif
!
    endif
!
end subroutine
