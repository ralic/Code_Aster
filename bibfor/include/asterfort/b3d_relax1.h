!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
interface 
    subroutine b3d_relax1(spl0, s0, s1, vsigma, eta1,&
                          e1, eta2, e0, dt, sref1,&
                          spl1, depst, sref, wref, w,&
                          li, xx1, dam, dpic)
        real(kind=8) :: spl0
        real(kind=8) :: s0
        real(kind=8) :: s1
        real(kind=8) :: vsigma
        real(kind=8) :: eta1
        real(kind=8) :: e1
        real(kind=8) :: eta2
        real(kind=8) :: e0
        real(kind=8) :: dt
        real(kind=8) :: sref1
        real(kind=8) :: spl1
        real(kind=8) :: depst
        real(kind=8) :: sref
        real(kind=8) :: wref
        real(kind=8) :: w
        real(kind=8) :: li
        real(kind=8) :: xx1
        real(kind=8) :: dam
        real(kind=8) :: dpic
    end subroutine b3d_relax1
end interface 
