subroutine vectci(intsn, nb1, xi, xr, rnormc)
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
    implicit none
!
    integer :: nb1, intsn
    real(kind=8) :: rnormc
    real(kind=8) :: xi(3, *), xr(*), vecta(2, 3), vectc(3)
!     REAL*8 VECTN(3),VECPTX(3,3)
!
!-----------------------------------------------------------------------
    integer :: i1, i2, intsx, j, k, l1, l2
!
!-----------------------------------------------------------------------
    l1=207
    l2=279
    intsx=8*(intsn-1)
    i1=l1+intsx
    i2=l2+intsx
!
!     CONSTRUCTION DES VECTEURS AA AUX X PTS D'INTEGRATION
!
    do 10 k = 1, 3
        vecta(1,k)=0.d0
        vecta(2,k)=0.d0
        do 20 j = 1, nb1
            vecta(1,k)=vecta(1,k)+xr(i1+j)*xi(k,j)
            vecta(2,k)=vecta(2,k)+xr(i2+j)*xi(k,j)
20      end do
10  end do
!
!     CONSTRUCTION DU VECTEUR C AUX X PTS D'INTEGRATION
!
    vectc(1)=vecta(1,2)*vecta(2,3)-vecta(1,3)*vecta(2,2)
    vectc(2)=vecta(1,3)*vecta(2,1)-vecta(1,1)*vecta(2,3)
    vectc(3)=vecta(1,1)*vecta(2,2)-vecta(1,2)*vecta(2,1)
!
!     NORME DU VECTEUR C AUX X PTS D'INTEGRATION
!
    rnormc=sqrt(vectc(1)*vectc(1)+vectc(2)*vectc(2)&
     &                                +vectc(3)*vectc(3))
!
!     CONSTRUCTION DES VECTEURS TA AUX X PTS D'INTEGRATION
!
!        VECTN(1)=VECTC(1)/RNORMC
!        VECTN(2)=VECTC(2)/RNORMC
!        VECTN(3)=VECTC(3)/RNORMC
!
!        RNORM=SQRT(VECTA(1,1)*VECTA(1,1)+VECTA(1,2)*VECTA(1,2)
!    &                                   +VECTA(1,3)*VECTA(1,3))
!
!        VECPTX(1,1)=VECTA(1,1)/RNORM
!        VECPTX(1,2)=VECTA(1,2)/RNORM
!        VECPTX(1,3)=VECTA(1,3)/RNORM
!
!        VECPTX(2,1)=VECTN(2)*VECPTX(1,3)-VECTN(3)*VECPTX(1,2)
!        VECPTX(2,2)=VECTN(3)*VECPTX(1,1)-VECTN(1)*VECPTX(1,3)
!        VECPTX(2,3)=VECTN(1)*VECPTX(1,2)-VECTN(2)*VECPTX(1,1)
!
!        VECPTX(3,1)=VECTN(1)
!        VECPTX(3,2)=VECTN(2)
!        VECPTX(3,3)=VECTN(3)
end subroutine
