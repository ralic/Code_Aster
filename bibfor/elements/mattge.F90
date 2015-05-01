subroutine mattge(nomte, dtild, sina, cosa, r,&
                  jacp, vf, dfds, rtangi)
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
#include "asterfort/btkb.h"
#include "blas/dscal.h"
    character(len=16) :: nomte
    real(kind=8) :: sina, cosa, r, jacp, vf(*), dfds(*)
    real(kind=8) :: mats(5, 9), mats1(3, 9), dtild(5, 5), dtild1(3, 3)
    real(kind=8) :: rtangi(9, 9)
    real(kind=8) :: dtilds(5, 9), dtildt(3, 9)
!
!
!     CALCULS DE LA MATRICE TANGENTE
!
!-----------------------------------------------------------------------
    integer :: i, j
!-----------------------------------------------------------------------
    if (nomte .eq. 'MECXSE3') then
!
        mats(1,1)=-sina*dfds(1)
        mats(1,2)= cosa*dfds(1)
        mats(1,3)= 0.d0
        mats(1,4)=-sina*dfds(2)
        mats(1,5)= cosa*dfds(2)
        mats(1,6)= 0.d0
        mats(1,7)=-sina*dfds(3)
        mats(1,8)= cosa*dfds(3)
        mats(1,9)= 0.d0
!
        mats(2,1)= 0.d0
        mats(2,2)= 0.d0
        mats(2,3)= dfds(1)
        mats(2,4)= 0.d0
        mats(2,5)= 0.d0
        mats(2,6)= dfds(2)
        mats(2,7)= 0.d0
        mats(2,8)= 0.d0
        mats(2,9)= dfds(3)
!
        mats(3,1)= vf(1)/r
        mats(3,2)= 0.d0
        mats(3,3)= 0.d0
        mats(3,4)= vf(2)/r
        mats(3,5)= 0.d0
        mats(3,6)= 0.d0
        mats(3,7)= vf(3)/r
        mats(3,8)= 0.d0
        mats(3,9)= 0.d0
!
        mats(4,1)= 0.d0
        mats(4,2)= 0.d0
        mats(4,3)=-sina*vf(1)/r
        mats(4,4)= 0.d0
        mats(4,5)= 0.d0
        mats(4,6)=-sina*vf(2)/r
        mats(4,7)= 0.d0
        mats(4,8)= 0.d0
        mats(4,9)=-sina*vf(3)/r
!
        mats(5,1)= cosa*dfds(1)
        mats(5,2)= sina*dfds(1)
        mats(5,3)= vf(1)
        mats(5,4)= cosa*dfds(2)
        mats(5,5)= sina*dfds(2)
        mats(5,6)= vf(2)
        mats(5,7)= cosa*dfds(3)
        mats(5,8)= sina*dfds(3)
        mats(5,9)= vf(3)
!
        call dscal(25, jacp, dtild, 1)
!
        call btkb(5, 9, 9, dtild, mats,&
                  dtilds, rtangi)
!
    else
!
        do 20 i = 1, 3
            do 30 j = 1, 3
                dtild1(i,j)=dtild(i,j)
30          end do
20      end do
!
        mats1(1,1)=-sina*dfds(1)
        mats1(1,2)= cosa*dfds(1)
        mats1(1,3)= 0.d0
        mats1(1,4)=-sina*dfds(2)
        mats1(1,5)= cosa*dfds(2)
        mats1(1,6)= 0.d0
        mats1(1,7)=-sina*dfds(3)
        mats1(1,8)= cosa*dfds(3)
        mats1(1,9)= 0.d0
!
        mats1(2,1)= 0.d0
        mats1(2,2)= 0.d0
        mats1(2,3)= dfds(1)
        mats1(2,4)= 0.d0
        mats1(2,5)= 0.d0
        mats1(2,6)= dfds(2)
        mats1(2,7)= 0.d0
        mats1(2,8)= 0.d0
        mats1(2,9)= dfds(3)
!
        mats1(3,1)= cosa*dfds(1)
        mats1(3,2)= sina*dfds(1)
        mats1(3,3)= vf(1)
        mats1(3,4)= cosa*dfds(2)
        mats1(3,5)= sina*dfds(2)
        mats1(3,6)= vf(2)
        mats1(3,7)= cosa*dfds(3)
        mats1(3,8)= sina*dfds(3)
        mats1(3,9)= vf(3)
!
        call dscal(9, jacp, dtild1, 1)
!
        call btkb(3, 9, 9, dtild1, mats1,&
                  dtildt, rtangi)
!
    endif
!
end subroutine
