subroutine dmatmc(fami, mater , time, poum ,ipg,&
                  ispg, repere, xyzgau, nbsig,d,&
                  l_modi_cp)
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/dmat3d.h"
#include "asterfort/dmatcp.h"
#include "asterfort/dmatdp.h"
#include "asterfort/lteatt.h"
!
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
!
    character(len=*), intent(in) :: fami
    integer, intent(in) :: mater
    real(kind=8), intent(in) :: time
    character(len=*), intent(in) :: poum
    integer, intent(in) :: ipg
    integer, intent(in) :: ispg
    real(kind=8), intent(in) :: repere(7)
    real(kind=8), intent(in) :: xyzgau(3)
    integer, intent(in) :: nbsig
    real(kind=8), intent(out) :: d(nbsig, nbsig)
    logical, optional, intent(in) :: l_modi_cp
!
! --------------------------------------------------------------------------------------------------
!
! Elementary computation
!
! Hooke matrix for iso-parametric elements
!
! --------------------------------------------------------------------------------------------------
!
! In  fami      : Gauss family for integration point rule
! In  mater     : material parameters
! In  time      : current time
! In  poum      : '-' or '+' for parameters evaluation (previous or current temperature)
! In  ipg       : current point gauss
! In  ispg      : current "sous-point" gauss
! In  repere    : definition of basis for orthotropic elasticity
! In  xyzgau    : coordinates for current Gauss point
! In  nbsig     : number of components for stress
! Out d         : Hooke matrix
! In  l_modi_cp : using plane strain Hooke matrix for plane stress case
!
! --------------------------------------------------------------------------------------------------
!
    if (lteatt('DIM_TOPO_MAILLE','3')) then
        ASSERT(nbsig.eq.6)
        call dmat3d(fami, mater , time, poum, ipg,&
                    ispg, repere, xyzgau, d)
    elseif (lteatt('FOURIER','OUI')) then
        ASSERT(nbsig.eq.6)
        call dmat3d(fami, mater , time, poum, ipg,&
                    ispg, repere, xyzgau, d)
    else if (lteatt('C_PLAN','OUI')) then
        ASSERT(nbsig.eq.4)
        call dmatcp(fami, mater, time, poum, ipg,&
                    ispg, repere, d)
        if (present(l_modi_cp)) then
            ASSERT(l_modi_cp)
            call dmatdp(fami, mater, time, poum, ipg,&
                        ispg, repere, d)
        else
            call dmatcp(fami, mater, time, poum, ipg,&
                        ispg, repere, d)            
        endif
    elseif (lteatt('D_PLAN','OUI').or. lteatt('AXIS','OUI')) then
        ASSERT(nbsig.eq.4)
        call dmatdp(fami, mater, time, poum, ipg,&
                    ispg, repere, d)
    else
        ASSERT(.false.)
    endif
!
end subroutine
