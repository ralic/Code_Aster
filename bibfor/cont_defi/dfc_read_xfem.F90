subroutine dfc_read_xfem(sdcont      , keywf, mesh, model, model_ndim,&
                         nb_cont_zone)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/cfdisl.h"
#include "asterfort/limacx.h"
#include "asterfort/xmacon.h"
#include "asterfort/xconta.h"
#include "asterfort/xfem_rel_lin.h"
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
    character(len=8), intent(in) :: sdcont
    character(len=8), intent(in) :: mesh
    character(len=8), intent(in) :: model
    character(len=16), intent(in) :: keywf
    integer, intent(in) :: model_ndim
    integer, intent(in) :: nb_cont_zone
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! XFEM method - Read contact data
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont           : name of contact concept (DEFI_CONTACT)
! In  keywf            : factor keyword to read
! In  mesh             : name of mesh
! In  model            : name of model
! In  model_ndim       : dimension of model
! In  nb_cont_zone     : number of zones of contact
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: l_cont_xfem_gg
    character(len=24) :: sdcont_defi
!
! --------------------------------------------------------------------------------------------------
!
    sdcont_defi    = sdcont(1:8)//'.CONTACT'
    l_cont_xfem_gg = cfdisl(sdcont_defi,'CONT_XFEM_GG')
!
! - Read cracks
!
    call limacx(sdcont, keywf, model_ndim, nb_cont_zone)
!
! - Create/modify contact datastructure
!
    if (l_cont_xfem_gg) then
        call xmacon(sdcont, mesh, model)
    endif
!
! - Prepare informations for linear relations for LBB condition
!
    call xconta(sdcont, mesh, model, model_ndim)
!
! - Set linear relations between contact unknonws for LBB condition
!
    call xfem_rel_lin(sdcont, mesh, model, model_ndim)
!
end subroutine
