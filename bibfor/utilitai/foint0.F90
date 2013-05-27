subroutine foint0()
    implicit none
!     ------------------------------------------------------------------
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
!     REMISE A "ZERO" DU COMMON UTILISE PAR FOINT2
!     ------------------------------------------------------------------
!
    integer :: isvind, isvnxt, svnbpa, svpar, nextsv
    integer :: iaprol, iavale, iapara, luvale
    real(kind=8) :: svresu
    character(len=1) :: svtypf
    character(len=2) :: svprgd
    character(len=24) :: svinte
    character(len=16) :: svnomp
    character(len=19) :: svnomf
    common /ifosav/ mxsave, mxpara, svnbpa(4) , svpar(10,4) ,&
     &                isvnxt , isvind(4), nextsv(4)
    common /jfosav/ iaprol(4),iavale(4),iapara(4),luvale(4),lupara(4)
    common /rfosav/ svresu(4)
    common /kfosav/ svnomp(10,4) , svnomf(4) ,&
     &                svtypf(4) , svprgd(4) , svinte(4)
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, lupara, mxpara, mxsave
!-----------------------------------------------------------------------
    mxpara = 10
    mxsave = 4
    isvnxt = mxsave
    do 10 i = 1, mxsave
        svnomf(i)= '????????'
        svresu(i)= 0.d0
        isvind(i)= 1
!
        iaprol(i)=0
        iavale(i)=0
        luvale(i)=0
        iapara(i)=0
        lupara(i)=0
10  end do
    nextsv(1) = 2
    nextsv(2) = 3
    nextsv(3) = 4
    nextsv(4) = 1
!
end subroutine
