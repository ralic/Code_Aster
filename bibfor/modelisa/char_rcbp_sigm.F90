subroutine char_rcbp_sigm(ligrmo, cabl_prec, cabl_sigm, l_first, nb_cmp_sief, &
                          nb_elem, nbec_sief, cmp_index_n)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/etenca.h"
#include "asterfort/jecreo.h"
#include "asterfort/jedetc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nocart.h"
#include "asterfort/u2mesk.h"
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
    character(len=8), intent(in) :: cabl_prec
    character(len=19), intent(in) :: cabl_sigm
    character(len=19), intent(in) :: ligrmo
    logical, intent(inout) :: l_first
    integer, intent(in) :: nb_cmp_sief
    integer, intent(in) :: nb_elem
    integer, intent(in) :: nbec_sief
    integer, intent(in) :: cmp_index_n
!
! --------------------------------------------------------------------------------------------------
!
! Loads affectation
!
! RELA_CINE_BP - Combine stresses
!
! --------------------------------------------------------------------------------------------------
!
! In  cabl_prec     : prestress information from CABLE_BP
! In  cabl_sigm     : stresses in cables
! In  ligrmo        : list of elements in model
! I/O l_first       : .true. if first stresses read
! In  nb_cmp_sief   : number of components of SIEF_R <GRANDEUR>
! In  nb_elem       : numbor ef elements in mesh
! In  nbec_sief     : number of coded integer of SIEF_R <GRANDEUR>
! In  cmp_index_n   : index in SIEF_R <GRANDEUR> for N
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: keywordfact
    character(len=8)  :: k8bid
    character(len=19) :: cabl_sigm_read
    integer :: iret, elem_nume, i_cmp
    integer :: jcmp, jdesc, jvale, jptma, jvalv
    integer :: jdesc_read, jvale_read, jptma_read
    integer :: icode_read, icode_n, icode
    integer :: iasm, ias
    integer :: iasm_read, ias_read
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    keywordfact = 'RELA_CINE_BP'
    icode_n = 2**cmp_index_n
!
! - Read stresses
!
    cabl_sigm_read = cabl_prec//'.CHME.SIGIN'
    call jeexin(cabl_sigm_read//'.DESC', iret)
    if (iret.eq.0) call u2mesk('F','CHARGES2_49', 1, cabl_prec)
!
    if (l_first) then
!
! ----- Reference field in cabl_sigm
!
        call copisd('CHAMP_GD', 'G', cabl_sigm_read, cabl_sigm)
        l_first = .false.
        call etenca(cabl_sigm, ligrmo, iret)
        ASSERT(iret.eq.0)
!
! ----- Only N component
!
        call jecreo(cabl_sigm//'.NCMP', 'V V K8')
        call jeecra(cabl_sigm//'.NCMP', 'LONMAX', nb_cmp_sief, ' ')
        call jecreo(cabl_sigm//'.VALV', 'V V R')
        call jeecra(cabl_sigm//'.VALV', 'LONMAX', nb_cmp_sief, ' ')
        call jeveuo(cabl_sigm//'.NCMP', 'E', jcmp)
        do i_cmp = 1, nb_cmp_sief
            zk8(jcmp+i_cmp-1) = ' '
        enddo
        zk8(jcmp) = 'N'
    else
        call etenca(cabl_sigm_read, ligrmo, iret)
        ASSERT(iret.eq.0)
        call jeveuo(cabl_sigm//'.DESC', 'L', jdesc)
        call jeveuo(cabl_sigm//'.VALE', 'L', jvale)
        call jeveuo(cabl_sigm//'.PTMA', 'L', jptma)
        call jeveuo(cabl_sigm//'.VALV', 'E', jvalv)
        call jeveuo(cabl_sigm_read//'.DESC', 'L', jdesc_read)
        call jeveuo(cabl_sigm_read//'.VALE', 'L', jvale_read)
        call jeveuo(cabl_sigm_read//'.PTMA', 'L', jptma_read)
!
! ----- Combine values with first field
!
        iasm       = zi(jdesc       +1)
        iasm_read  = zi(jdesc_read  +1)
        do elem_nume = 1, nb_elem
            ias        = zi(jptma      +elem_nume-1)
            ias_read   = zi(jptma_read +elem_nume-1)
            icode      = zi(jdesc      +3+2*iasm      + nbec_sief*(ias - 1))
            icode_read = zi(jdesc_read +3+2*iasm_read + nbec_sief*(ias_read-1))
            if (icode_read .eq. icode_n) then
                zr(jvalv) = zr(jvale_read + nb_cmp_sief*(ias_read-1))
                if (icode .eq. icode_n) then
                    zr(jvalv) = zr(jvalv) + zr(jvale + nb_cmp_sief*(ias_read-1))
                endif
                call nocart(cabl_sigm, 3, k8bid, 'NUM', 1,&
                            k8bid, elem_nume, ' ', 1)
            endif
        enddo
!
        call jelibe(cabl_sigm_read//'.DESC')
        call jelibe(cabl_sigm_read//'.VALE')
        call jelibe(cabl_sigm_read//'.PTMA')
        call jedetc('V', cabl_sigm_read, 1)
    endif
!
    call jedema()
end subroutine
