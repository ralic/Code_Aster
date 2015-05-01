subroutine effi(nomte, sigmtd, vf, dfds, jacp,&
                sina, cosa, r, effint)
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
    character(len=16) :: nomte
    real(kind=8) :: sigmtd(*), sina, cosa, r, vf(*), dfds(*), jacp, effint(*)
    real(kind=8) :: mats(5, 9), effinb(9)
!
!     CALCULS DES EFFORTS INTERIEURS
!
!-----------------------------------------------------------------------
    integer :: i, k
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
        mats(4,3)= -sina*vf(1)/r
        mats(4,4)= 0.d0
        mats(4,5)= 0.d0
        mats(4,6)= -sina*vf(2)/r
        mats(4,7)= 0.d0
        mats(4,8)= 0.d0
        mats(4,9)= -sina*vf(3)/r
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
!     CONSTRUCTION DES EFFORTS INTERIEURS
!
        do 10 i = 1, 9
            effinb(i)=0.d0
            do 20 k = 1, 5
                effinb(i)=effinb(i)+mats(k,i)*sigmtd(k)
20          end do
            effint(i)=effint(i)+jacp*effinb(i)
10      end do
!
    else
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
        mats(3,1)= cosa*dfds(1)
        mats(3,2)= sina*dfds(1)
        mats(3,3)= vf(1)
        mats(3,4)= cosa*dfds(2)
        mats(3,5)= sina*dfds(2)
        mats(3,6)= vf(2)
        mats(3,7)= cosa*dfds(3)
        mats(3,8)= sina*dfds(3)
        mats(3,9)= vf(3)
!
        do 30 i = 1, 9
            effinb(i)=0.d0
            do 40 k = 1, 3
                effinb(i)=effinb(i)+mats(k,i)*sigmtd(k)
40          end do
            effint(i)=effint(i)+jacp*effinb(i)
30      end do
!
    endif
end subroutine
