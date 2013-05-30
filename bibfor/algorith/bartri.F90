subroutine bartri(i1, i2, coor, poin)
    implicit   none
    include 'asterfort/barso1.h'
    include 'asterfort/u2mesk.h'
    integer :: i1, i2, poin(*)
    real(kind=8) :: coor(*)
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
!     BARSOUM : TRAITEMENT DES MAILLES "TRIA6" ET "TRIA7"
!-----------------------------------------------------------------------
!
    integer :: i, n1, n2, n3
!     ------------------------------------------------------------------
!
!     ------------------------------------------------------------------
!                       TRAITEMENT DES "POI1"
!     ------------------------------------------------------------------
    if (i1 .eq. 1 .and. i2 .eq. 0) then
        do 110 i = 1, 2
            if (i .eq. 1) then
                n1 = 1
                n2 = 2
                n3 = 4
            else
                n1 = 1
                n2 = 3
                n3 = 6
            endif
            call barso1(n1, n2, n3, coor, poin)
110      continue
    else if (i1.eq.2 .and. i2.eq.0) then
        do 120 i = 1, 2
            if (i .eq. 1) then
                n1 = 2
                n2 = 1
                n3 = 4
            else
                n1 = 2
                n2 = 3
                n3 = 5
            endif
            call barso1(n1, n2, n3, coor, poin)
120      continue
    else if (i1.eq.3 .and. i2.eq.0) then
        do 130 i = 1, 2
            if (i .eq. 1) then
                n1 = 3
                n2 = 1
                n3 = 6
            else
                n1 = 3
                n2 = 2
                n3 = 5
            endif
            call barso1(n1, n2, n3, coor, poin)
130      continue
!
!     ------------------------------------------------------------------
!                       TRAITEMENT DES "SEG3"
!     ------------------------------------------------------------------
    else if (i1+i2 .eq. 3) then
        do 210 i = 1, 2
            if (i .eq. 1) then
                n1 = 2
                n2 = 3
                n3 = 5
            else
                n1 = 1
                n2 = 3
                n3 = 6
            endif
            call barso1(n1, n2, n3, coor, poin)
210      continue
!
    else if (i1+i2 .eq. 5) then
        do 220 i = 1, 2
            if (i .eq. 1) then
                n1 = 3
                n2 = 1
                n3 = 6
            else
                n1 = 2
                n2 = 1
                n3 = 4
            endif
            call barso1(n1, n2, n3, coor, poin)
220      continue
!
    else if (i1+i2 .eq. 4) then
        do 230 i = 1, 2
            if (i .eq. 1) then
                n1 = 3
                n2 = 2
                n3 = 5
            else
                n1 = 1
                n2 = 2
                n3 = 4
            endif
            call barso1(n1, n2, n3, coor, poin)
230      continue
!
    else
        call u2mesk('F', 'ALGORITH_36', 1, 'TRIA')
!
    endif
!
end subroutine
