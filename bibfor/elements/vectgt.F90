subroutine vectgt(ind, nb1, xi, ksi3s2, intsx,&
                  zr, epais, vectn, vectg, vectt)
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
!
    implicit none
    integer :: ind, nb1, intsx
    real(kind=8) :: xi(3, *), zr(*), epais, vectn(9, 3)
    real(kind=8) :: vectg(2, 3), vectt(3, 3)
    real(kind=8) :: ksi3s2
!
!-----------------------------------------------------------------------
    integer :: i1, i2, intsx1, j, k, l1, l2
    integer :: l3
    real(kind=8) :: rnorm
!-----------------------------------------------------------------------
    if (ind .eq. 0) then
!
!     CALCULS AUX PTS D'INTEGRATION REDUITE
!
        l1= 12
        l2= 44
        l3= 76
    else if (ind.eq.1) then
!
!     CALCULS AUX PTS D'INTEGRATION NORMALE
!
        l1=135
        l2=207
        l3=279
!
    endif
!
!     CONSTRUCTION DU VECTEUR N AUX X PTS DE GAUSS. X = REDUIT OU NORMAL
!     (STOCKE DANS VECTT)
!
    intsx1=8*(intsx-1)
    i1=l1+intsx1
    do 15 k = 1, 3
        vectt(3,k)=0
        do 25 j = 1, nb1
            vectt(3,k)=vectt(3,k)+zr(i1+j)*vectn(j,k)
25      end do
15  end do
!
!     CONSTRUCTION DES VECTEURS GA AUX X PTS DE GAUSS
!
    i1=l2+intsx1
    i2=l3+intsx1
    do 40 k = 1, 3
        vectg(1,k)=0.d0
        vectg(2,k)=0.d0
        do 50 j = 1, nb1
            vectg(1,k)= vectg(1,k) +zr(i1+j)*(xi(k,j)+ksi3s2*epais*&
            vectn(j,k))
            vectg(2,k)= vectg(2,k) +zr(i2+j)*(xi(k,j)+ksi3s2*epais*&
            vectn(j,k))
50      end do
40  end do
!
!     CONSTRUCTION DES VECTEURS TA AUX X PTS DE GAUSS (T3=N)
!
    rnorm=sqrt(vectg(1,1)*vectg(1,1)&
     &             +vectg(1,2)*vectg(1,2)&
     &             +vectg(1,3)*vectg(1,3))
!
    do 60 k = 1, 3
        vectt(1,k)=vectg(1,k)/rnorm
60  end do
!
    vectt(2,1)= vectt(3,2)*vectt(1,3)-vectt(3,3)*vectt(1,2)
    vectt(2,2)= vectt(3,3)*vectt(1,1)-vectt(3,1)*vectt(1,3)
    vectt(2,3)= vectt(3,1)*vectt(1,2)-vectt(3,2)*vectt(1,1)
!
end subroutine
