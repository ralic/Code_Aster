subroutine vectan(nb1, nb2, xi, xr, vecta,&
                  vectn, vectpt)
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
    integer :: nb1, nb2, l1, l2, i1, i2, j, i, k
    real(kind=8) :: xi(3, *), xr(*)
    real(kind=8) :: vecta(9, 2, 3), vectn(9, 3), vectpt(9, 2, 3)
!
!-----------------------------------------------------------------------
    integer :: ib, l
    real(kind=8) :: rnorm
!-----------------------------------------------------------------------
    l1=828
    l2=900
!
!     CONSTRUCTION DES VECTEURS AA AUX NB2 NOEUDS I
!     (STOCKE DANS VECTA)
!
    do 10 i = 1, nb2
        i1=l1+8*(i-1)
        i2=l2+8*(i-1)
        do 20 k = 1, 3
            vecta(i,1,k)=0.d0
            vecta(i,2,k)=0.d0
            do 30 j = 1, nb1
                vecta(i,1,k)=vecta(i,1,k)+xr(i1+j)*xi(k,j)
                vecta(i,2,k)=vecta(i,2,k)+xr(i2+j)*xi(k,j)
30          end do
20      end do
!
!     CONSTRUCTION DU VECTEUR N AUX NB2 NOEUDS I
!     (STOCKE DANS VECTN)
!
        vectn(i,1)= vecta(i,1,2)*vecta(i,2,3) -vecta(i,1,3)*vecta(i,2,&
        2)
        vectn(i,2)= vecta(i,1,3)*vecta(i,2,1) -vecta(i,1,1)*vecta(i,2,&
        3)
        vectn(i,3)= vecta(i,1,1)*vecta(i,2,2) -vecta(i,1,2)*vecta(i,2,&
        1)
!
        rnorm=sqrt(vectn(i,1)*vectn(i,1)+vectn(i,2)*vectn(i,2)&
        +vectn(i,3)*vectn(i,3))
        vectn(i,1)=vectn(i,1)/rnorm
        vectn(i,2)=vectn(i,2)/rnorm
        vectn(i,3)=vectn(i,3)/rnorm
!
!     CONSTRUCTION DES VECTEURS TA AUX NOEUDS I
!     (STOCKE DANS VECTPT)
!
        rnorm=sqrt(vecta(i,1,1)*vecta(i,1,1) +vecta(i,1,2)*vecta(i,1,&
        2) +vecta(i,1,3)*vecta(i,1,3))
        do 25 k = 1, 3
            vectpt(i,1,k)=vecta(i,1,k)/rnorm
25      end do
!
        vectpt(i,2,1)= vectn(i,2)*vectpt(i,1,3) -vectn(i,3)*vectpt(i,&
        1,2)
        vectpt(i,2,2)= vectn(i,3)*vectpt(i,1,1) -vectn(i,1)*vectpt(i,&
        1,3)
        vectpt(i,2,3)= vectn(i,1)*vectpt(i,1,2) -vectn(i,2)*vectpt(i,&
        1,1)
10  end do
!
!     STOCKAGE DES NB2 MATRICES DE PASSAGE LOCALES GLOBALE (3,3) DANS XR
!
    do 40 ib = 1, nb2
        l=9*(ib-1)
        do 50 j = 1, 3
            do 60 i = 1, 3
                k=l+(j-1)*3+i
                if (i .le. 2) then
                    xr(1090+k)=vectpt(ib,i,j)
                else
                    xr(1090+k)=vectn(ib,j)
                endif
60          end do
50      end do
40  end do
!
end subroutine
