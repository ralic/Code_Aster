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
    subroutine b3d_srf(sigal6, vss33, spl6, long3, eps23,&
                       wfeff3, wfm6, vss33t, sigaf6, xnu0,&
                       e0, dtpic, rc, wref, tanphi,&
                       d66, erreur, dlt3, ssl6, ept0,&
                       sigel6, sref, fl3d, eve6, vve6,&
                       sve6, y1sy, tau1, tau2, cc2,&
                       teta1, eafluage, dt, vm6, maj0,&
                       depst6, sigef06, xloc)
#include "asterf_types.h"
        real(kind=8) :: sigal6(6)
        real(kind=8) :: vss33(3, 3)
        real(kind=8) :: spl6(6)
        real(kind=8) :: long3(3)
        real(kind=8) :: eps23(3)
        real(kind=8) :: wfeff3(3)
        real(kind=8) :: wfm6(6)
        real(kind=8) :: vss33t(3, 3)
        real(kind=8) :: sigaf6(6)
        real(kind=8) :: xnu0
        real(kind=8) :: e0
        real(kind=8) :: dtpic
        real(kind=8) :: rc
        real(kind=8) :: wref
        real(kind=8) :: tanphi
        real(kind=8) :: d66(6, 6)
        integer :: erreur
        real(kind=8) :: dlt3(3)
        real(kind=8) :: ssl6(6)
        real(kind=8) :: ept0
        real(kind=8) :: sigel6(6)
        real(kind=8) :: sref
        aster_logical :: fl3d
        real(kind=8) :: eve6(6)
        real(kind=8) :: vve6(6)
        real(kind=8) :: sve6(6)
        real(kind=8) :: y1sy
        real(kind=8) :: tau1
        real(kind=8) :: tau2
        real(kind=8) :: cc2
        real(kind=8) :: teta1
        real(kind=8) :: eafluage
        real(kind=8) :: dt
        real(kind=8) :: vm6(6)
        aster_logical :: maj0
        real(kind=8) :: depst6(6)
        real(kind=8) :: sigef06(6)
        real(kind=8) :: xloc
    end subroutine b3d_srf
end interface 
