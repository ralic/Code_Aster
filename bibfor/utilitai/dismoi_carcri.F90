subroutine dismoi_carcri(questi_, nomobj_, repi, repk, ierd)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/carces.h"
#include "asterfort/cesexi.h"
#include "asterfort/cesred.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
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
!
    character(len=*), intent(in) :: nomobj_
    character(len=*), intent(in) :: questi_
    integer, intent(out) :: repi
    integer, intent(out) :: ierd
    character(len=*), intent(out) :: repk
!
! --------------------------------------------------------------------------------------------------
!
! DISMOI(CARCRI)
!
! --------------------------------------------------------------------------------------------------
!

! Out ierd : code -retour (0 si OK, 1 sinon)
!
! --------------------------------------------------------------------------------------------------
!
    character(len=19) :: cham_elem_s, chcalc, questi, nomobj
    integer :: iret, jcald, jcall, nb_elem, i_elem, iadc
    character(len=8) :: lcham(1)
    aster_logical :: l_post_incr
    character(len=8), pointer :: cesk(:) => null()
    real(kind=8), pointer :: cesv(:) => null()
    data  lcham/ 'POSTINCR'/
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    questi = questi_
    nomobj = nomobj_
    ASSERT(questi.eq.'POST_INCR')
    repk   = 'NON'
    repi   = 0
    ierd   = 0
    l_post_incr = .false.
    cham_elem_s = '&&DISMCO.CHTMP'
    chcalc      = '&&GVERLC.CHCALC'
!
! - Convert to cham_elem_s
!
    call carces(nomobj, 'ELEM', ' ', 'V', cham_elem_s,&
                'A'   , iret)
    if (iret.gt.0) then
        ierd = 1
        goto 999
    endif
!
! - Reduction on specific GRANDEUR
!
    call cesred(cham_elem_s, 0, [0], 1, lcham,&
                'V', chcalc)
    call detrsd('CHAM_ELEM_S', cham_elem_s)
!
! - Access to CHAM_ELEM_S
!
    call jeveuo(chcalc//'.CESD', 'L', jcald)
    call jeveuo(chcalc//'.CESV', 'L', vr=cesv)
    call jeveuo(chcalc//'.CESL', 'L', jcall)
    call jeveuo(chcalc//'.CESK', 'L', vk8=cesk)
    nb_elem = zi(jcald-1+1)
!
    do i_elem = 1, nb_elem
        call cesexi('C', jcald, jcall, i_elem, 1,&
                    1, 1, iadc)
        if (iadc .gt. 0) then
            if (cesv(iadc) .gt. 0.d0) then
                l_post_incr = .true.
            endif
        endif
    end do
!
999 continue
!
    if (l_post_incr) then
        repk = 'OUI'
    endif
    call detrsd('CHAM_ELEM_S', chcalc)
!
    call jedema()
!
end subroutine
