subroutine vdefge(nomte, nb1, npgsr, xr, epais,&
                  sigma, effgt)
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
    implicit none
!
    character(len=16) :: nomte
    real(kind=8) :: epais
    real(kind=8) :: xr(*), sigma(6, *), effgt(8, *), effgtg(8, 8)
    integer :: i, i1, j, k, l1, nb1, npgsr
!
    real(kind=8) :: demiep, wnc1, wnc2, wnc3, zic, zic1, zic2
    real(kind=8) :: zic3
!-----------------------------------------------------------------------
!
!     DANS LE CAS DE EFGE_ELNO, ON CALCULE AUX NB1 NOEUDS
!     ON NE CALCULE PAS AU NOEUD INTERNE
!
    do 5 i = 1, nb1
        do 6 j = 1, 8
            effgt(j,i)=0.d0
 6      end do
 5  end do
!
    l1 =1452
!
    demiep=epais/2.d0
!
    wnc1 = 0.33333333333333D0
    wnc2 = 1.33333333333333D0
    wnc3 = 0.33333333333333D0
    zic = -epais/2.d0
!
    zic1 = zic
    zic2 = zic1 + demiep
    zic3 = zic2 + demiep
!
    if (nomte .eq. 'MEC3QU9H') then
        effgtg(1,1)=demiep*(wnc1*sigma(1,1)+wnc2*sigma(1,5) +wnc3*&
        sigma(1,9))
        effgtg(1,2)=demiep*(wnc1*sigma(1,2)+wnc2*sigma(1,6) +wnc3*&
        sigma(1,10))
        effgtg(1,3)=demiep*(wnc1*sigma(1,3)+wnc2*sigma(1,7) +wnc3*&
        sigma(1,11))
        effgtg(1,4)=demiep*(wnc1*sigma(1,4)+wnc2*sigma(1,8) +wnc3*&
        sigma(1,12))
!
        effgtg(2,1)=demiep*(wnc1*sigma(2,1)+wnc2*sigma(2,5) +wnc3*&
        sigma(2,9))
        effgtg(2,2)=demiep*(wnc1*sigma(2,2)+wnc2*sigma(2,6) +wnc3*&
        sigma(2,10))
        effgtg(2,3)=demiep*(wnc1*sigma(2,3)+wnc2*sigma(2,7) +wnc3*&
        sigma(2,11))
        effgtg(2,4)=demiep*(wnc1*sigma(2,4)+wnc2*sigma(2,8) +wnc3*&
        sigma(2,12))
!
        effgtg(3,1)=demiep*(wnc1*sigma(4,1)+wnc2*sigma(4,5) +wnc3*&
        sigma(4,9))
        effgtg(3,2)=demiep*(wnc1*sigma(4,2)+wnc2*sigma(4,6) +wnc3*&
        sigma(4,10))
        effgtg(3,3)=demiep*(wnc1*sigma(4,3)+wnc2*sigma(4,7) +wnc3*&
        sigma(4,11))
        effgtg(3,4)=demiep*(wnc1*sigma(4,4)+wnc2*sigma(4,8) +wnc3*&
        sigma(4,12))
!
        effgtg(4,1)=demiep*(wnc1*zic1*sigma(1,1) +wnc2*zic2*sigma(1,5)&
        +wnc3*zic3*sigma(1,9))
        effgtg(4,2)=demiep*(wnc1*zic1*sigma(1,2) +wnc2*zic2*sigma(1,6)&
        +wnc3*zic3*sigma(1,10))
        effgtg(4,3)=demiep*(wnc1*zic1*sigma(1,3) +wnc2*zic2*sigma(1,7)&
        +wnc3*zic3*sigma(1,11))
        effgtg(4,4)=demiep*(wnc1*zic1*sigma(1,4) +wnc2*zic2*sigma(1,8)&
        +wnc3*zic3*sigma(1,12))
!
        effgtg(5,1)=demiep*(wnc1*zic1*sigma(2,1) +wnc2*zic2*sigma(2,5)&
        +wnc3*zic3*sigma(2,9))
        effgtg(5,2)=demiep*(wnc1*zic1*sigma(2,2) +wnc2*zic2*sigma(2,6)&
        +wnc3*zic3*sigma(2,10))
        effgtg(5,3)=demiep*(wnc1*zic1*sigma(2,3) +wnc2*zic2*sigma(2,7)&
        +wnc3*zic3*sigma(2,11))
        effgtg(5,4)=demiep*(wnc1*zic1*sigma(2,4) +wnc2*zic2*sigma(2,8)&
        +wnc3*zic3*sigma(2,12))
!
        effgtg(6,1)=demiep*(wnc1*zic1*sigma(4,1) +wnc2*zic2*sigma(4,5)&
        +wnc3*zic3*sigma(4,9))
        effgtg(6,2)=demiep*(wnc1*zic1*sigma(4,2) +wnc2*zic2*sigma(4,6)&
        +wnc3*zic3*sigma(4,10))
        effgtg(6,3)=demiep*(wnc1*zic1*sigma(4,3) +wnc2*zic2*sigma(4,7)&
        +wnc3*zic3*sigma(4,11))
        effgtg(6,4)=demiep*(wnc1*zic1*sigma(4,4) +wnc2*zic2*sigma(4,8)&
        +wnc3*zic3*sigma(4,12))
!
        effgtg(7,1)=demiep*(wnc1*sigma(5,1)+wnc2*sigma(5,5) +wnc3*&
        sigma(5,9))
        effgtg(7,2)=demiep*(wnc1*sigma(5,2)+wnc2*sigma(5,6) +wnc3*&
        sigma(5,10))
        effgtg(7,3)=demiep*(wnc1*sigma(5,3)+wnc2*sigma(5,7) +wnc3*&
        sigma(5,11))
        effgtg(7,4)=demiep*(wnc1*sigma(5,4)+wnc2*sigma(5,8) +wnc3*&
        sigma(5,12))
!
        effgtg(8,1)=demiep*(wnc1*sigma(6,1)+wnc2*sigma(6,5) +wnc3*&
        sigma(6,9))
        effgtg(8,2)=demiep*(wnc1*sigma(6,2)+wnc2*sigma(6,6) +wnc3*&
        sigma(6,10))
        effgtg(8,3)=demiep*(wnc1*sigma(6,3)+wnc2*sigma(6,7) +wnc3*&
        sigma(6,11))
        effgtg(8,4)=demiep*(wnc1*sigma(6,4)+wnc2*sigma(6,8) +wnc3*&
        sigma(6,12))
!
        do 15 i = 1, nb1
            i1=l1+4*(i-1)
            do 20 k = 1, npgsr
                effgt(1,i)=effgt(1,i)+effgtg(1,k)*xr(i1+k)
                effgt(2,i)=effgt(2,i)+effgtg(2,k)*xr(i1+k)
                effgt(3,i)=effgt(3,i)+effgtg(3,k)*xr(i1+k)
                effgt(4,i)=effgt(4,i)+effgtg(4,k)*xr(i1+k)
                effgt(5,i)=effgt(5,i)+effgtg(5,k)*xr(i1+k)
                effgt(6,i)=effgt(6,i)+effgtg(6,k)*xr(i1+k)
                effgt(7,i)=effgt(7,i)+effgtg(7,k)*xr(i1+k)
                effgt(8,i)=effgt(8,i)+effgtg(8,k)*xr(i1+k)
!
20          continue
15      continue
!
!     VALEURS AU NOEUD INTERNE OBTENUE PAR MOYENNE DES AUTRES
!
        effgt(1,9)=(effgt(1,5)+effgt(1,6)+effgt(1,7)+effgt(1,8))/4.d0
        effgt(2,9)=(effgt(2,5)+effgt(2,6)+effgt(2,7)+effgt(2,8))/4.d0
        effgt(3,9)=(effgt(3,5)+effgt(3,6)+effgt(3,7)+effgt(3,8))/4.d0
        effgt(4,9)=(effgt(4,5)+effgt(4,6)+effgt(4,7)+effgt(4,8))/4.d0
        effgt(5,9)=(effgt(5,5)+effgt(5,6)+effgt(5,7)+effgt(5,8))/4.d0
        effgt(6,9)=(effgt(6,5)+effgt(6,6)+effgt(6,7)+effgt(6,8))/4.d0
        effgt(7,9)=(effgt(7,5)+effgt(7,6)+effgt(7,7)+effgt(7,8))/4.d0
        effgt(8,9)=(effgt(8,5)+effgt(8,6)+effgt(8,7)+effgt(8,8))/4.d0
!
    else if (nomte.eq.'MEC3TR7H') then
!
        effgtg(1,1)=demiep*(wnc1*sigma(1,1)+wnc2*sigma(1,4) +wnc3*&
        sigma(1,7))
        effgtg(1,2)=demiep*(wnc1*sigma(1,2)+wnc2*sigma(1,5) +wnc3*&
        sigma(1,8))
        effgtg(1,3)=demiep*(wnc1*sigma(1,3)+wnc2*sigma(1,6) +wnc3*&
        sigma(1,9))
!
        effgtg(2,1)=demiep*(wnc1*sigma(2,1)+wnc2*sigma(2,4) +wnc3*&
        sigma(2,7))
        effgtg(2,2)=demiep*(wnc1*sigma(2,2)+wnc2*sigma(2,5) +wnc3*&
        sigma(2,8))
        effgtg(2,3)=demiep*(wnc1*sigma(2,3)+wnc2*sigma(2,6) +wnc3*&
        sigma(2,9))
!
        effgtg(3,1)=demiep*(wnc1*sigma(4,1)+wnc2*sigma(4,4) +wnc3*&
        sigma(4,7))
        effgtg(3,2)=demiep*(wnc1*sigma(4,2)+wnc2*sigma(4,5) +wnc3*&
        sigma(4,8))
        effgtg(3,3)=demiep*(wnc1*sigma(4,3)+wnc2*sigma(4,6) +wnc3*&
        sigma(4,9))
!
        effgtg(4,1)=demiep*(wnc1*zic1*sigma(1,1) +wnc2*zic2*sigma(1,4)&
        +wnc3*zic3*sigma(1,7))
        effgtg(4,2)=demiep*(wnc1*zic1*sigma(1,2) +wnc2*zic2*sigma(1,5)&
        +wnc3*zic3*sigma(1,8))
        effgtg(4,3)=demiep*(wnc1*zic1*sigma(1,3) +wnc2*zic2*sigma(1,6)&
        +wnc3*zic3*sigma(1,9))
!
        effgtg(5,1)=demiep*(wnc1*zic1*sigma(2,1) +wnc2*zic2*sigma(2,4)&
        +wnc3*zic3*sigma(2,7))
        effgtg(5,2)=demiep*(wnc1*zic1*sigma(2,2) +wnc2*zic2*sigma(2,5)&
        +wnc3*zic3*sigma(2,8))
        effgtg(5,3)=demiep*(wnc1*zic1*sigma(2,3) +wnc2*zic2*sigma(2,6)&
        +wnc3*zic3*sigma(2,9))
!
        effgtg(6,1)=demiep*(wnc1*zic1*sigma(4,1) +wnc2*zic2*sigma(4,4)&
        +wnc3*zic3*sigma(4,7))
        effgtg(6,2)=demiep*(wnc1*zic1*sigma(4,2) +wnc2*zic2*sigma(4,5)&
        +wnc3*zic3*sigma(4,8))
        effgtg(6,3)=demiep*(wnc1*zic1*sigma(4,3) +wnc2*zic2*sigma(4,6)&
        +wnc3*zic3*sigma(4,9))
!
        effgtg(7,1)=demiep*(wnc1*sigma(5,1)+wnc2*sigma(5,4) +wnc3*&
        sigma(5,7))
        effgtg(7,2)=demiep*(wnc1*sigma(5,2)+wnc2*sigma(5,5) +wnc3*&
        sigma(5,8))
        effgtg(7,3)=demiep*(wnc1*sigma(5,3)+wnc2*sigma(5,6) +wnc3*&
        sigma(5,9))
!
        effgtg(8,1)=demiep*(wnc1*sigma(6,1)+wnc2*sigma(6,4) +wnc3*&
        sigma(6,7))
        effgtg(8,2)=demiep*(wnc1*sigma(6,2)+wnc2*sigma(6,5) +wnc3*&
        sigma(6,8))
        effgtg(8,3)=demiep*(wnc1*sigma(6,3)+wnc2*sigma(6,6) +wnc3*&
        sigma(6,9))
!
        do 35 i = 1, nb1
            i1=l1+4*(i-1)
            do 40 k = 1, npgsr
                effgt(1,i)=effgt(1,i)+effgtg(1,k)*xr(i1+k)
                effgt(2,i)=effgt(2,i)+effgtg(2,k)*xr(i1+k)
                effgt(3,i)=effgt(3,i)+effgtg(3,k)*xr(i1+k)
                effgt(4,i)=effgt(4,i)+effgtg(4,k)*xr(i1+k)
                effgt(5,i)=effgt(5,i)+effgtg(5,k)*xr(i1+k)
                effgt(6,i)=effgt(6,i)+effgtg(6,k)*xr(i1+k)
                effgt(7,i)=effgt(7,i)+effgtg(7,k)*xr(i1+k)
                effgt(8,i)=effgt(8,i)+effgtg(8,k)*xr(i1+k)
!
40          continue
35      continue
!
!     VALEURS AU NOEUD INTERNE OBTENUE PAR MOYENNE DES AUTRES
!
        effgt(1,7)=(effgt(1,1)+effgt(1,2)+effgt(1,3))/3.d0
        effgt(2,7)=(effgt(2,1)+effgt(2,2)+effgt(2,3))/3.d0
        effgt(3,7)=(effgt(3,1)+effgt(3,2)+effgt(3,3))/3.d0
        effgt(4,7)=(effgt(4,1)+effgt(4,2)+effgt(4,3))/3.d0
        effgt(5,7)=(effgt(5,1)+effgt(5,2)+effgt(5,3))/3.d0
        effgt(6,7)=(effgt(6,1)+effgt(6,2)+effgt(6,3))/3.d0
        effgt(7,7)=(effgt(7,1)+effgt(7,2)+effgt(7,3))/3.d0
        effgt(8,7)=(effgt(8,1)+effgt(8,2)+effgt(8,3))/3.d0
!
    endif
!
end subroutine
