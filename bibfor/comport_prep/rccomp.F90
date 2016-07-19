subroutine rccomp(chmat, mesh)
!
implicit none
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/comp_comp_read.h"
#include "asterfort/comp_comp_save.h"
#include "asterfort/comp_init.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
!-
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=8), intent(in) :: chmat
    character(len=8), intent(in) :: mesh
!
! --------------------------------------------------------------------------------------------------
!
! Preparation of comportment (AFFE_MATERIAU)
!
! Prepare COMPOR <CARTE>
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh        : name of mesh
! In  chmat       : name material field
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_cmp, nocc
    character(len=16) :: keywordfact
    character(len=19) :: compor
    character(len=16), pointer :: v_info_valk(:) => null()
    integer, pointer :: v_info_vali(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    keywordfact = 'AFFE_COMPOR'
    compor      = chmat//'.COMPOR'
    call getfac(keywordfact, nocc)
    if (nocc .ne. 0) then
!
! ----- Create comportment informations objects
!
        AS_ALLOCATE(vk16 = v_info_valk, size = 16*nocc)
        AS_ALLOCATE(vi   = v_info_vali, size = 4*nocc)
!
! ----- Create COMPOR <CARTE>
!
        call comp_init(mesh, compor, 'G', nb_cmp)
!
! ----- Read informations from command file
!
        call comp_comp_read(v_info_valk, v_info_vali)
!
! ----- Save informations in COMPOR <CARTE>
!
        call comp_comp_save(mesh, compor, nb_cmp, v_info_valk, v_info_vali)
!
! ----- Clean it
!
        AS_DEALLOCATE(vk16 = v_info_valk)
        AS_DEALLOCATE(vi   = v_info_vali)
    endif
!
end subroutine
