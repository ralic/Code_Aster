subroutine forsrg(intsn, nb1, nb2, xr, chgsrg,&
                  rnormc, vectpt, vecl1)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer :: intsn, nb1, nb2, intsx1
    real(kind=8) :: wgt, rnormc
    real(kind=8) :: xr(*), vecl1(42), chg(6), chgsrg(6, 8)
    real(kind=8) :: vectpt(9, 3, 3)
!
!     EFFORTS SURFACIQUES DONNES DANS LE REPERE GLOBAL
!
!-----------------------------------------------------------------------
    integer :: i, i1, i2, i3, i4, i5, in
    integer :: intsx2, j, k, l1, l2
!-----------------------------------------------------------------------
    wgt=xr(127-1+intsn)
!
    l1=135
    l2=459
    intsx1=8*(intsn-1)
    intsx2=9*(intsn-1)
!
    i1=l1+intsx1
    i2=l2+intsx2
!
!     INTERPOLATION DES CHARGES
!
    do 10 j = 1, 6
        chg(j)=0.d0
        do 20 in = 1, nb1
            chg(j)=chg(j)+chgsrg(j,in)*xr(i1+in)
20      end do
10  end do
!
    do 30 i = 1, nb1
!
        i3=5*(i-1)
        vecl1(i3+1)=vecl1(i3+1)+wgt*chg(1)*xr(i1+i)*rnormc
        vecl1(i3+2)=vecl1(i3+2)+wgt*chg(2)*xr(i1+i)*rnormc
        vecl1(i3+3)=vecl1(i3+3)+wgt*chg(3)*xr(i1+i)*rnormc
!
        do 40 k = 1, 3
            vecl1(i3+4)=vecl1(i3+4)+wgt*chg(3+k)*(-xr(i2+i))*vectpt(i,&
            2,k)
            vecl1(i3+5)=vecl1(i3+5)+wgt*chg(3+k)* xr(i2+i) *vectpt(i,&
            1,k)
40      end do
30  end do
!
    i4=5*nb1+1
    i5=5*nb1+2
    do 50 k = 1, 3
        vecl1(i4) =vecl1(i4)+wgt*chg(3+k)*(-xr(i2+nb2))*vectpt(i,2,k)
        vecl1(i5) =vecl1(i5)+wgt*chg(3+k)* xr(i2+nb2)* vectpt(i,1,k)
50  end do
!
end subroutine
