subroutine nmctcl(model, mesh, sdcont_defi, sdcont_solv)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/infdbg.h"
#include "asterfort/mmligr.h"
#include "asterfort/xmligr.h"
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
    character(len=8), intent(in) :: model
    character(len=8), intent(in) :: mesh
    character(len=24), intent(in) :: sdcont_defi 
    character(len=24), intent(in) :: sdcont_solv
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Continue/XFEM method
!
! Create elements for contact
!
! --------------------------------------------------------------------------------------------------
!
! In  model            : name of model
! In  mesh             : name of mesh
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  sdcont_solv      : name of contact solving datastructure
!
! --------------------------------------------------------------------------------------------------
!
    integer :: cont_form
    integer :: ifm, niv
    aster_logical :: l_cont_xfem_gg
    character(len=8) :: nomo
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('MECANONLINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> Create elements for contact'
    endif
!
! - Parameters
!
    cont_form      = cfdisi(sdcont_defi,'FORMULATION')
    l_cont_xfem_gg = cfdisl(sdcont_defi,'CONT_XFEM_GG')
    nomo           = model(1:8)
!
! - Create elements for contact
!
    if (cont_form .eq. 2) then
        call mmligr(mesh, nomo, sdcont_defi, sdcont_solv)
    elseif  (cont_form .eq. 3) then
        if (l_cont_xfem_gg) then
            call xmligr(mesh, nomo, sdcont_solv)
        endif
    else
        ASSERT(.false.)
    endif
!
end subroutine
