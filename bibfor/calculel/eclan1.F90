subroutine eclan1(ipoini, mxnbpi, nsomm1, nterm1, i1,&
                  i2, i3, i4, i5, i6,&
                  i7, i8)
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
    implicit   none
    integer :: mxnbpi
!
    integer :: nsomm1(mxnbpi, *)
    integer :: nterm1(mxnbpi), work(8), k
    integer :: ipoini, i1, i2, i3, i4, i5, i6, i7, i8
!
    work(1)=i1
    work(2)=i2
    work(3)=i3
    work(4)=i4
    work(5)=i5
    work(6)=i6
    work(7)=i7
    work(8)=i8
    do 1, k=1,nterm1(ipoini)
    nsomm1(ipoini,k)=work(k)
    1 end do
!
end subroutine
