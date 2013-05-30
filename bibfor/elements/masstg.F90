subroutine masstg(matin, matout)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
! -----------------------------------------------------------
! ---  BUT : MISE SOUS LA FORME DES MATRICES DE MASSE DES
!            POU_D_TG(M) DE LA MATRICE ISSUE DE PTMA01
! -----------------------------------------------------------
    real(kind=8) :: matin(105), matout(105)
!
    integer :: i
!
    do 100 i = 1, 21
        matout(i) = matin(i)
100  continue
    do 102 i = 22, 28
        matout(i) = 0.d0
102  continue
    do 104 i = 29, 34
        matout(i) = matin(i-7)
104  continue
    matout(35) = 0.d0
    do 106 i = 36, 42
        matout(i) = matin(i-8)
106  continue
    matout(43) = 0.d0
    do 108 i = 44, 51
        matout(i) = matin(i-9)
108  continue
    matout(52) = 0.d0
    do 110 i = 53, 61
        matout(i) = matin(i-10)
110  continue
    matout(62) = 0.d0
    do 112 i = 63, 72
        matout(i) = matin(i-11)
112  continue
    matout(73) = 0.d0
    do 114 i = 74, 84
        matout(i) = matin(i-12)
114  continue
    matout(85) = 0.d0
    do 116 i = 86, 91
        matout(i) = matin(i-13)
116  continue
    do 118 i = 92, 105
        matout(i) = 0.d0
118  continue
!
end subroutine
