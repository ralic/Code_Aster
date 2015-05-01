subroutine btkb(ndimc, ndimx, nddle, wmatc, btild,&
                wmatcb, ktildi)
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
    integer :: ndimc, nddle
!
    real(kind=8) :: wmatc(ndimc, ndimc), wmatcb(ndimc, ndimx)
    real(kind=8) :: btild(ndimc, ndimx)
    real(kind=8) :: ktildi(ndimx, ndimx)
    integer :: i, j, k, ndimx
!-----------------------------------------------------------------------
!
    do 10 i = 1, ndimc
        do 20 j = 1, nddle
            wmatcb(i,j)=0.d0
            do 30 k = 1, ndimc
                wmatcb(i,j)=wmatcb(i,j)+wmatc(i,k)*btild(k,j)
30          end do
20      end do
10  end do
!
    do 40 i = 1, nddle
        do 50 j = 1, nddle
            ktildi(i,j)=0.d0
            do 60 k = 1, ndimc
                ktildi(i,j)=ktildi(i,j)+btild(k,i)*wmatcb(k,j)
60          end do
50      end do
40  end do
end subroutine
