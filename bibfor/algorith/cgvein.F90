subroutine cgvein(resu, compor, iord0, l_temp)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/carces.h"
#include "asterfort/cesexi.h"
#include "asterfort/cesred.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer, intent(in) :: iord0
    character(len=8), intent(in) :: resu
    character(len=19), intent(in) :: compor
    logical, intent(in) :: l_temp
!
! --------------------------------------------------------------------------------------------------
!
! CALC_G
!
! Verifications supplementaires pour les comportements incrementaux (ELAS + ETAT_INIT ou GTP)
!
! --------------------------------------------------------------------------------------------------
!
! In resu   : nom du resultat
! In compor : carte comportement cree dans cgleco()
! In iord0  : premier NUME_ORDRE dans resu
! In l_temp : .true.  en presence de thermique
!             .false. sinon
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iret,  jcald, jcall,  nbma, iadc, ima
    character(len=8) :: noma
    character(len=16) :: k16ldc
    character(len=19) :: chcalc, chtmp
    logical :: lldcok
    character(len=16), pointer :: cesv(:) => null()
    character(len=8), pointer :: cesk(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - les relations de comportement dans la carte compor sont elles autorisees ?
!
    chtmp  = '&&CGVEIN_CHTMP'
    chcalc = '&&CGVEIN_CHCALC'
!
    call carces(compor, 'ELEM', ' ', 'V', chtmp, 'A', iret)
    call cesred(chtmp, 0, [0], 1, 'RELCOM', 'V', chcalc)
    call detrsd('CHAM_ELEM_S', chtmp)
    call jeveuo(chcalc//'.CESD', 'L', jcald)
    call jeveuo(chcalc//'.CESV', 'L', vk16=cesv)
    call jeveuo(chcalc//'.CESL', 'L', jcall)
    call jeveuo(chcalc//'.CESK', 'L', vk8=cesk)
!
    noma = cesk(1)
    nbma = zi(jcald-1+1)
!
    do ima = 1, nbma
!
        call cesexi('C', jcald, jcall, ima, 1, 1, 1, iadc)
        ASSERT(iadc .gt. 0)
        k16ldc = cesv(iadc)
!
!       seules relations de type COMP_INCR autorisees
        lldcok = k16ldc .eq. 'ELAS            ' .or.&
               & k16ldc .eq. 'VMIS_ISOT_LINE  ' .or.&
               & k16ldc .eq. 'VMIS_ISOT_TRAC  '
        if (.not.lldcok) call utmess('F', 'RUPTURE1_69', sk=k16ldc)
!
!       on interdit VMIS_ISOT_TRAC en presence de thermique
        if (k16ldc .eq. 'VMIS_ISOT_TRAC  ' .and. l_temp) call utmess('F', 'RUPTURE1_70')
    enddo
!
    call jedema()
!
end subroutine
