subroutine vddege(nomte, nb1, npgsr, xr, deggtg,&
                  deggt)
    implicit none
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    character(len=16) :: nomte
    real(kind=8) :: xr(*), deggtg(8, *), deggt(8, 9)
    integer :: i, i1, j, k, l1, nb1, npgsr
!
    do 5 i = 1, nb1
        do 6 j = 1, 8
            deggt(j,i)=0.d0
 6      end do
 5  end do
!
    l1 =1452
!
    if (nomte .eq. 'MEC3QU9H') then
!
        do 15 i = 1, nb1
            i1=l1+4*(i-1)
            do 20 k = 1, npgsr
                deggt(1,i)=deggt(1,i)+deggtg(1,k)*xr(i1+k)
                deggt(2,i)=deggt(2,i)+deggtg(2,k)*xr(i1+k)
                deggt(3,i)=deggt(3,i)+deggtg(3,k)*xr(i1+k)
                deggt(4,i)=deggt(4,i)+deggtg(4,k)*xr(i1+k)
                deggt(5,i)=deggt(5,i)+deggtg(5,k)*xr(i1+k)
                deggt(6,i)=deggt(6,i)+deggtg(6,k)*xr(i1+k)
                deggt(7,i)=deggt(7,i)+deggtg(7,k)*xr(i1+k)
                deggt(8,i)=deggt(8,i)+deggtg(8,k)*xr(i1+k)
20          continue
15      continue
!
!     VALEURS AU NOEUD INTERNE OBTENUE PAR MOYENNE DES AUTRES
!
        deggt(1,9)=(deggt(1,5)+deggt(1,6)+deggt(1,7)+deggt(1,8))/4.d0
        deggt(2,9)=(deggt(2,5)+deggt(2,6)+deggt(2,7)+deggt(2,8))/4.d0
        deggt(3,9)=(deggt(3,5)+deggt(3,6)+deggt(3,7)+deggt(3,8))/4.d0
        deggt(4,9)=(deggt(4,5)+deggt(4,6)+deggt(4,7)+deggt(4,8))/4.d0
        deggt(5,9)=(deggt(5,5)+deggt(5,6)+deggt(5,7)+deggt(5,8))/4.d0
        deggt(6,9)=(deggt(6,5)+deggt(6,6)+deggt(6,7)+deggt(6,8))/4.d0
        deggt(7,9)=(deggt(7,5)+deggt(7,6)+deggt(7,7)+deggt(7,8))/4.d0
        deggt(8,9)=(deggt(8,5)+deggt(8,6)+deggt(8,7)+deggt(8,8))/4.d0
!
    else if (nomte.eq.'MEC3TR7H') then
!
        do 35 i = 1, nb1
            i1=l1+4*(i-1)
            do 40 k = 1, npgsr
                deggt(1,i)=deggt(1,i)+deggtg(1,k)*xr(i1+k)
                deggt(2,i)=deggt(2,i)+deggtg(2,k)*xr(i1+k)
                deggt(3,i)=deggt(3,i)+deggtg(3,k)*xr(i1+k)
                deggt(4,i)=deggt(4,i)+deggtg(4,k)*xr(i1+k)
                deggt(5,i)=deggt(5,i)+deggtg(5,k)*xr(i1+k)
                deggt(6,i)=deggt(6,i)+deggtg(6,k)*xr(i1+k)
                deggt(7,i)=deggt(7,i)+deggtg(7,k)*xr(i1+k)
                deggt(8,i)=deggt(8,i)+deggtg(8,k)*xr(i1+k)
40          continue
35      continue
!
!     VALEURS AU NOEUD INTERNE OBTENUE PAR MOYENNE DES AUTRES
!
        deggt(1,7)=(deggt(1,1)+deggt(1,2)+deggt(1,3))/3.d0
        deggt(2,7)=(deggt(2,1)+deggt(2,2)+deggt(2,3))/3.d0
        deggt(3,7)=(deggt(3,1)+deggt(3,2)+deggt(3,3))/3.d0
        deggt(4,7)=(deggt(4,1)+deggt(4,2)+deggt(4,3))/3.d0
        deggt(5,7)=(deggt(5,1)+deggt(5,2)+deggt(5,3))/3.d0
        deggt(6,7)=(deggt(6,1)+deggt(6,2)+deggt(6,3))/3.d0
        deggt(7,7)=(deggt(7,1)+deggt(7,2)+deggt(7,3))/3.d0
        deggt(8,7)=(deggt(8,1)+deggt(8,2)+deggt(8,3))/3.d0
!
    endif
!
end subroutine
