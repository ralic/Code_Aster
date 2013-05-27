subroutine num2k8(nomgd, tglok8, tlock8, nblk8, tind)
    implicit none
    integer :: nblk8, tind(*)
    character(len=8) :: nomgd, tglok8(*), tlock8(*)
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! ----------------------------------------------------------------------
!     COPIE DE NUMEK8
!     TIND(I) <-- INDICE DANS LE TABLEAU TGLOK8 DE L' ELEMEMT
!                 NUMERO I DE TLOCK8
!                 (NBLK8 : DIMENSION DE TLOCK8)
!
! ----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, j
!-----------------------------------------------------------------------
    if (nomgd(1:6) .eq. 'SIEF_R') then
        do 10, i = 1, nblk8, 1
        tind (i) = 0
!  COMPOSANTES TRAITEES: SIXX SIYY SIZZ SIXY SIXZ SIYZ
        do 20, j = 1, 6, 1
        if (tlock8(i) .eq. tglok8(j)) then
            tind(i) = j
            goto 10
        endif
20      continue
!  COMPOSANTES TRAITEES: NXX NYY NXY MXX MYY MXY
        do 22, j = 14, 19, 1
        if (tlock8(i) .eq. tglok8(j)) then
            tind(i) = j
            goto 10
        endif
22      continue
10      continue
!
    else if (nomgd(1:6).eq.'EPSI_R') then
!  COMPOSANTES TRAITEES: EPXX EPYY EPZZ EPXY EPXZ EPYZ
!                        EXX EYY EXY KXX KYY KXY
        do 30, i = 1, nblk8, 1
        tind (i) = 0
        do 40, j = 1, 12, 1
        if (tlock8(i) .eq. tglok8(j)) then
            tind(i) = j
            goto 30
        endif
40      continue
30      continue
!
    else
        do 50, i = 1, nblk8, 1
        tind (i) = 0
50      continue
    endif
!
end subroutine
