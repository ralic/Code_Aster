subroutine mm_cycl_init(sd_cont_defi, sd_cont_solv)
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
    character(len=24), intent(in) :: sd_cont_defi
    character(len=24), intent(in) :: sd_cont_solv
!
! --------------------------------------------------------------------------------------------------
!
! Contact (continue method) - Cycling
!
! Initialization of data structures
!
! --------------------------------------------------------------------------------------------------
!
! In  sd_cont_solv : data structure for contact solving
! In  sd_cont_defi : data structure from contact definition
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sd_cycl_his
    real(kind=8), pointer :: p_cycl_his(:) => null()
    character(len=24) :: sd_cycl_coe
    real(kind=8), pointer :: p_cycl_coe(:) => null()
    integer :: point_index
    integer :: zone_index, zone_number
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
    zone_number = cfdisi(sd_cont_defi,'NZOCO' )
!
! - Access to cycling objects
!
    sd_cycl_his = sd_cont_solv(1:14)//'.CYCHIS'
    sd_cycl_coe = sd_cont_solv(1:14)//'.CYCCOE'
    call jeveuo(sd_cycl_his, 'E', vr = p_cycl_his)
    call jeveuo(sd_cycl_coe, 'E', vr = p_cycl_coe)
!
! - Init history
!
    point_index = 1
    do zone_index = 1, zone_number
        lveri = mminfl(sd_cont_defi,'VERIF' ,zone_index)
        slave_elt_nb = mminfi(sd_cont_defi,'NBMAE' ,zone_index)
        slave_elt_shift = mminfi(sd_cont_defi,'JDECME',zone_index)
        coef_cont = mminfr(sd_cont_defi,'COEF_AUGM_CONT',zone_index)
        coef_frot = mminfr(sd_cont_defi,'COEF_AUGM_FROT',zone_index)
        p_cycl_coe(6*(zone_index-1)+1) = coef_cont
        p_cycl_coe(6*(zone_index-1)+2) = coef_frot
        p_cycl_coe(6*(zone_index-1)+3) = +1.d99
        p_cycl_coe(6*(zone_index-1)+4) = -1.d99
        p_cycl_coe(6*(zone_index-1)+5) = +1.d99
        p_cycl_coe(6*(zone_index-1)+6) = -1.d99
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
            call mminfm(slave_elt_num, sd_cont_defi, 'NPTM', slave_pt_nb)
!
! --------- Loop on points
!
            do slave_pt_index = 1, slave_pt_nb
                p_cycl_his(25*(point_index-1)+2) = coef_cont
                p_cycl_his(25*(point_index-1)+6) = coef_frot
                p_cycl_his(25*(point_index-1)+25) = zone_index
                point_index = point_index + 1
            end do
        end do
 25     continue
    end do
!
    call jedema()
end subroutine
