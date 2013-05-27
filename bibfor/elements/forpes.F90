subroutine forpes(intsn, nb1, xr, rho, epais,&
                  vpesan, rnormc, vecl1)
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
    integer :: intsn, nb1, intsx
    real(kind=8) :: wgt, rho
    real(kind=8) :: xr(*), vpesan(3), vecl1(42)
!
!-----------------------------------------------------------------------
    integer :: i, i1, i2, l1
    real(kind=8) :: epais, rnormc
!-----------------------------------------------------------------------
    wgt=xr(127-1+intsn)
!
    l1=135
    intsx=8*(intsn-1)
!
    i1=l1+intsx
!
    do 10 i = 1, nb1
        i2=5*(i-1)
        vecl1(i2+1)=vecl1(i2+1)+wgt*rho*epais*vpesan(1)*xr(i1+i)*&
        rnormc
        vecl1(i2+2)=vecl1(i2+2)+wgt*rho*epais*vpesan(2)*xr(i1+i)*&
        rnormc
        vecl1(i2+3)=vecl1(i2+3)+wgt*rho*epais*vpesan(3)*xr(i1+i)*&
        rnormc
10  end do
end subroutine
