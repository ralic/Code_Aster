subroutine CreateContactDS(ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    type(NL_DS_Contact), intent(out) :: ds_contact
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Contact management
!
! Create contact management datastructure
!
! --------------------------------------------------------------------------------------------------
!
! Out ds_contact       : datastructure for contact management
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: i_loop
    integer, parameter :: nb_loop_defi = 3
    character(len=4), parameter :: loop_type(nb_loop_defi) = (/'Geom','Fric','Cont'/)
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('MECANONLINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> . Create contact management datastructure'
    endif
!
! - Main parameters
!
    ds_contact%l_contact   = .false._1
    ds_contact%l_meca_cont = .false._1
    ds_contact%l_meca_unil = .false._1
    ds_contact%sdcont      = ' '
    ds_contact%l_form_cont = .false._1
    ds_contact%l_form_disc = .false._1
    ds_contact%l_form_xfem = .false._1
    ds_contact%l_form_lac  = .false._1
!
! - Name of datastructures
!
    ds_contact%sdcont_defi = '&&OP0070.DEFIC'
    ds_contact%sdunil_defi = '&&OP0070.DEFIU'
    ds_contact%sdcont_solv = '&&OP0070.RESOC'
    ds_contact%sdunil_solv = '&&OP0070.RESUC'
!
! - Name of <LIGREL> - Slave and contact elements
!
    ds_contact%ligrel_elem_slav = ' '
    ds_contact%l_elem_slav      = .false._1
    ds_contact%ligrel_elem_cont = ' '
    ds_contact%l_elem_cont      = .false._1
!
! - Name of <CHELEM> - Input field
!
    ds_contact%field_input      = ' '
!
! - Name of NUME_DOF for discrete friction methods
!
    ds_contact%nume_dof_frot    = ' '
!
! - Identity relations between dof
!
    ds_contact%l_iden_rela = .false._1
    ds_contact%iden_rela   = ' '
!
! - Relations between dof (QUAD8 in discrete methods or XFEM)
!
    ds_contact%l_dof_rela       = .false._1
    ds_contact%ligrel_dof_rela  = ' '
!
! - Management of loops
!
    ds_contact%nb_loop = nb_loop_defi
    ASSERT(ds_contact%nb_loop.le.ds_contact%nb_loop_maxi)
    do i_loop = 1, nb_loop_defi
        ds_contact%loop(i_loop)%type    = loop_type(i_loop)
        ds_contact%loop(i_loop)%conv    = .false._1
        ds_contact%loop(i_loop)%error   = .false._1
        ds_contact%loop(i_loop)%counter = 0
    end do
!
! - Field for CONT_NODE
!
    ds_contact%field_cont_node  = ' '
    ds_contact%fields_cont_node = ' '
    ds_contact%field_cont_perc  = ' '
!
! - Field for CONT_ELEM
!
    ds_contact%field_cont_elem  = ' '
!
! - Flag for (re) numbering
!
    ds_contact%l_renumber   = .false._1
!
! - Geometric loop control
!
    ds_contact%geom_maxi    = -1.d0
!
! - Get-off indicator
!
    ds_contact%l_getoff     = .false._1
!
! - First geometric loop
!
    ds_contact%l_first_geom = .false._1
!
! - Flag for pairing
!
    ds_contact%l_pair       = .false._1
!
! - Total number of patches (for LAC method)
!
    ds_contact%nt_patch     = 0
!
end subroutine
