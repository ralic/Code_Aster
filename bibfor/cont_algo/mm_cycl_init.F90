subroutine mm_cycl_init(ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfl.h"
#include "asterfort/mminfm.h"
#include "asterfort/mminfr.h"
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
    type(NL_DS_Contact), intent(in) :: ds_contact
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve - Cycling
!
! Initialization of data structures
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sdcont_cychis
    real(kind=8), pointer :: p_sdcont_cychis(:) => null()
    character(len=24) :: sdcont_cyccoe
    real(kind=8), pointer :: p_sdcont_cyccoe(:) => null()
    integer :: i_cont_poin
    integer :: zone_index, nb_cont_zone
    integer :: slave_elt_index, slave_elt_nb, slave_elt_shift, slave_elt_num
    integer :: slave_pt_index, slave_pt_nb
    real(kind=8) :: coef_cont, coef_frot
    aster_logical :: lveri
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    nb_cont_zone = cfdisi(ds_contact%sdcont_defi,'NZOCO' )
!
! - Access to cycling objects
!
    sdcont_cychis = ds_contact%sdcont_solv(1:14)//'.CYCHIS'
    sdcont_cyccoe = ds_contact%sdcont_solv(1:14)//'.CYCCOE'
    call jeveuo(sdcont_cychis, 'E', vr = p_sdcont_cychis)
    call jeveuo(sdcont_cyccoe, 'E', vr = p_sdcont_cyccoe)
!
! - Init history
!
    i_cont_poin = 1
    do zone_index = 1, nb_cont_zone
        lveri = mminfl(ds_contact%sdcont_defi,'VERIF' ,zone_index)
        slave_elt_nb = mminfi(ds_contact%sdcont_defi,'NBMAE' ,zone_index)
        slave_elt_shift = mminfi(ds_contact%sdcont_defi,'JDECME',zone_index)
        coef_cont = mminfr(ds_contact%sdcont_defi,'COEF_AUGM_CONT',zone_index)
        coef_frot = mminfr(ds_contact%sdcont_defi,'COEF_AUGM_FROT',zone_index)
        p_sdcont_cyccoe(6*(zone_index-1)+1) = coef_cont
        p_sdcont_cyccoe(6*(zone_index-1)+2) = coef_frot
        p_sdcont_cyccoe(6*(zone_index-1)+3) = +1.d12
        p_sdcont_cyccoe(6*(zone_index-1)+4) = -1.d12
        p_sdcont_cyccoe(6*(zone_index-1)+5) = +1.d12
        p_sdcont_cyccoe(6*(zone_index-1)+6) = -1.d12
        if (lveri) goto 25
!
! ----- Loop on slave elements
!
        do slave_elt_index = 1, slave_elt_nb
!
! --------- Absolute number of slave element
!
            slave_elt_num = slave_elt_shift + slave_elt_index
!
! --------- Number of points on slave element
!
            call mminfm(slave_elt_num, ds_contact%sdcont_defi, 'NPTM', slave_pt_nb)
!
! --------- Loop on points
!
            do slave_pt_index = 1, slave_pt_nb
                p_sdcont_cychis(25*(i_cont_poin-1)+2) = coef_cont
                p_sdcont_cychis(25*(i_cont_poin-1)+6) = coef_frot
                p_sdcont_cychis(25*(i_cont_poin-1)+25) = zone_index
                i_cont_poin = i_cont_poin + 1
            end do
        end do
 25     continue
    end do
!
    call jedema()
end subroutine
