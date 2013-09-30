subroutine vddege(nomte, nb2, npgsn, xr, deggtg,&
                  deggt)
    implicit none
!
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
!
! CALCUL DE L'OPTION EDGE_ELNO POUR LES COQUE_3D
!
#include "asterfort/matrq9.h"
    character(len=16) :: nomte
    real(kind=8) :: xr(*), deggtg(72), deggt(8, 9), mat(9,9)
    integer :: i, i1, j, k, l1, nb2, npgsn, icmp, ino, ipg
!
    do 5 i = 1, nb2
        do 6 j = 1, 8
            deggt(j,i)=0.d0
 6      continue
 5  continue
!
    if (nomte .eq. 'MEC3QU9H') then
!
        call matrq9(mat)
        do 15 icmp = 1, 8
            do 20 ino = 1, nb2
                do 25 ipg = 1, npgsn
                    deggt(icmp,ino)=deggt(icmp,ino)&
                    +deggtg(8*(ipg-1)+icmp)*mat(ino,ipg)
25              continue
20          continue
15      continue
!
    else if (nomte.eq.'MEC3TR7H') then
!
    l1 =1600
!
        do 35 i = 1, nb2
            i1=l1+7*(i-1)
            do 40 k = 1, npgsn
                do 45 j = 1, 8
                    deggt(j,i)=deggt(j,i)+deggtg(8*(k-1)+j)*xr(i1+k)
45              continue
40          continue
35      continue
!
!     VALEURS AU NOEUD INTERNE OBTENUE PAR MOYENNE DES AUTRES
!
!       deggt(1,7)=(deggt(1,1)+deggt(1,2)+deggt(1,3))/3.d0
!        deggt(2,7)=(deggt(2,1)+deggt(2,2)+deggt(2,3))/3.d0
!        deggt(3,7)=(deggt(3,1)+deggt(3,2)+deggt(3,3))/3.d0
!        deggt(4,7)=(deggt(4,1)+deggt(4,2)+deggt(4,3))/3.d0
!        deggt(5,7)=(deggt(5,1)+deggt(5,2)+deggt(5,3))/3.d0
!        deggt(6,7)=(deggt(6,1)+deggt(6,2)+deggt(6,3))/3.d0
!        deggt(7,7)=(deggt(7,1)+deggt(7,2)+deggt(7,3))/3.d0
!        deggt(8,7)=(deggt(8,1)+deggt(8,2)+deggt(8,3))/3.d0
!
    endif
!
end subroutine
