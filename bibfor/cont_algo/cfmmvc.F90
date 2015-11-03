subroutine cfmmvc(ds_contact   , v_ncomp_jeux, v_ncomp_loca, v_ncomp_enti, v_ncomp_zone,&
                  nt_ncomp_poin)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/as_allocate.h"
#include "asterfort/cfdisi.h"
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
    type(NL_DS_Contact), intent(in) :: ds_contact
    real(kind=8), pointer, intent(out) :: v_ncomp_jeux(:)
    integer, pointer, intent(out) :: v_ncomp_loca(:)
    character(len=16), pointer, intent(out) :: v_ncomp_enti(:)
    integer, pointer, intent(out) :: v_ncomp_zone(:)
    integer, intent(out) :: nt_ncomp_poin
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Post-treatment for no computation methods
!
! Prepare datastructures
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
! Out v_ncomp_jeux     : pointer to save gaps
! Out v_ncomp_loca     : pointer to save index of node
! Out v_ncomp_enti     : pointer to save name of entities
! Out v_ncomp_zone     : pointer to save contact zone index
! Out nt_ncomp_poin    : number of points in no-computation mode
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nt_poin, nt_cont_poin
!
! --------------------------------------------------------------------------------------------------
!
    nt_poin      = cfdisi(ds_contact%sdcont_defi,'NTPT' )
    nt_cont_poin = cfdisi(ds_contact%sdcont_defi,'NTPC' )
    nt_ncomp_poin = nt_poin-nt_cont_poin
    ASSERT(nt_ncomp_poin.ge.1)
!
    AS_ALLOCATE(vr   = v_ncomp_jeux, size = nt_ncomp_poin)
    AS_ALLOCATE(vi   = v_ncomp_loca, size = nt_ncomp_poin)
    AS_ALLOCATE(vk16 = v_ncomp_enti, size = nt_ncomp_poin*2)
    AS_ALLOCATE(vi   = v_ncomp_zone, size = nt_ncomp_poin)
!
end subroutine
