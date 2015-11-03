subroutine aporth(mesh, sdcont_defi, model_ndim, elem_mast_indx, poin_coor,&
                  tau1, tau2)
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8prem.h"
#include "asterfort/cfnben.h"
#include "asterfort/cfnumm.h"
#include "asterfort/aptypm.h"
#include "asterfort/assert.h"
#include "asterfort/cforth.h"
#include "asterfort/normev.h"
#include "asterfort/utmess.h"
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: mesh
    character(len=24), intent(in) :: sdcont_defi
    integer, intent(in) :: model_ndim
    integer, intent(in) :: elem_mast_indx
    real(kind=8), intent(in) :: poin_coor(3)
    real(kind=8), intent(inout) :: tau1(3)
    real(kind=8), intent(inout) :: tau2(3)
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing
!
! Orthogonalization of local basis
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  model_ndim       : dimension of model
! In  elem_mast_indx   : nearest master element index in contact datastructure
! In  poin_coor        : coordinate of contact point to project
! IO  tau1             : first tangent vector for local basis
! IO  tau2             : second tangent vector for local basis
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_beam
    character(len=8) :: elem_mast_type, elem_mast_name
    real(kind=8) :: noor
    integer :: niverr, elem_mast_ndim
    integer :: elem_mast_nume, elem_mast_nbnode
!
! --------------------------------------------------------------------------------------------------
!
!
! - No node excluded
!
    ASSERT(elem_mast_indx.ne.0)
!
! - Number of nodes
!
    call cfnben(sdcont_defi, elem_mast_indx, 'CONNEX', elem_mast_nbnode)
!
! - Index of master element
!
    call cfnumm(sdcont_defi, elem_mast_indx, elem_mast_nume)
!
! - Parameters of master element
!
    call aptypm(mesh          , elem_mast_nume, elem_mast_ndim, elem_mast_nbnode,&
                elem_mast_type, elem_mast_name)
    l_beam = (elem_mast_type(1:2).eq.'SE').and.(model_ndim.eq.3)
!
! - Orthogonalization of local basis
!
    if (l_beam) then
        call normev(tau1, noor)
        if (noor .le. r8prem()) then
            call utmess('F', 'APPARIEMENT_38', sk=elem_mast_name)
        endif
    else
        call cforth(model_ndim, tau1, tau2, niverr)
        if (niverr .eq. 1) then
            call utmess('F', 'APPARIEMENT_14', sk=elem_mast_name, nr=3, valr=poin_coor)
        else if (niverr.eq.2) then
            call utmess('F', 'APPARIEMENT_34', sk=elem_mast_name, nr=3, valr=poin_coor)
        endif
    endif
!
end subroutine
