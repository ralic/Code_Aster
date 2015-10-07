subroutine xfem_rel_lin(sdcont, mesh, model, nb_dim)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/aflrch.h"
#include "asterfort/cfdisl.h"
#include "asterfort/dismoi.h"
#include "asterfort/exixfe.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/xrelco.h"
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
! person_in_charge: samuel.geniaut at edf.fr
!
    character(len=8), intent(in) :: sdcont
    integer, intent(in) :: nb_dim
    character(len=8), intent(in) :: mesh
    character(len=8), intent(in) :: model
!
! --------------------------------------------------------------------------------------------------
!
! XFEM - Contact
!
! Linear relation between contact unknows for LBB condition
!
! --------------------------------------------------------------------------------------------------
!
! In  model          : name of model
! In  mesh           : name of mesh
! In  nb_dim         : dimension of space
! In  sdcont         : name of contact datastructure
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_crack_max
    parameter    (nb_crack_max=100)
!
    integer :: i_crack
    integer :: nb_crack, nb_rela_line, nb_edge
    character(len=24) :: sdcont_defi, sdline
    character(len=19) :: list_rela_line
    character(len=14) :: sdline_crack
    aster_logical :: l_edge_elim
    integer, pointer :: v_crack_nb(:) => null()
    character(len=24), pointer :: v_sdline(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    sdcont_defi    = sdcont(1:8)//'.CONTACT'
    list_rela_line = '&&CAXFEM.RLISTE'
    nb_rela_line   = 0
    l_edge_elim    = cfdisl(sdcont_defi,'ELIM_ARETE')
!
! - Access to cracks datastructure
!
    call jeveuo(model(1:8)//'.NFIS', 'L', vi=v_crack_nb)
    nb_crack = v_crack_nb(1)
    if (nb_crack .gt. nb_crack_max) then
        call utmess('F', 'XFEM_2', si=nb_crack_max)
    endif
!
! - Access to datastructure for linear relation
!
    sdline = sdcont_defi(1:16)//'.XNRELL'
    call jeveuo(sdline, 'L', vk24 = v_sdline)
!
! - Create linear relation for "VITAL" edges
!
    do i_crack = 1, nb_crack
        sdline_crack = v_sdline(i_crack)(1:14)
        call xrelco(mesh   , model, nb_dim, sdline_crack, nb_rela_line, list_rela_line,&
                    nb_edge)
    end do
!
! - Affectation of linear relations
!
    if (nb_edge .ne. 0) then
        call utmess('I','XFEM2_4', si = nb_edge)
        if (.not.l_edge_elim) then
            call aflrch(list_rela_line, sdcont, 'LIN')
        endif
    endif
!
    call jedema()
end subroutine
