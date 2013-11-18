subroutine cgvein(resu, compor, iord0)
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
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
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
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iret, jcalv, jcald, jcall, jcalk, jadmat, jcvvar
    integer :: nvacr, ivarc, nbma, iadc, ima
    character(len=8) :: noma, chmat, k8b
    character(len=16) :: k16ldc
    character(len=19) :: chcalc, chtmp
    logical :: exivrc, exitem, lldcok
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - la variable de commande TEMP est-elle presente dans la sd cham_mater du resultat (exitem) ?
!
    call rsadpa(resu, 'L', 1, 'CHAMPMAT', iord0, 0, sjv=jadmat, styp=k8b)
    chmat = zk8(jadmat)
    call jeexin(chmat//'.CVRCVARC', iret)
    exivrc = iret .ne. 0
    exitem = .false.
!
    if (exivrc) then
        call jelira(chmat// '.CVRCVARC', 'LONMAX', ival=nvacr)
        call jeveuo(chmat// '.CVRCVARC', 'L', jcvvar)
        do ivarc = 1, nvacr
            exitem = zk8(jcvvar-1+ivarc) .eq. 'TEMP    '
            if (exitem) exit
        enddo
    endif
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
    call jeveuo(chcalc//'.CESV', 'L', jcalv)
    call jeveuo(chcalc//'.CESL', 'L', jcall)
    call jeveuo(chcalc//'.CESK', 'L', jcalk)
!
    noma = zk8(jcalk-1+1)
    nbma = zi(jcald-1+1)
!
    do ima = 1, nbma
!
        call cesexi('C', jcald, jcall, ima, 1, 1, 1, iadc)
        ASSERT(iadc .gt. 0)
        k16ldc = zk16(jcalv+iadc-1)
!
!       seules relations de type COMP_INCR autorisees
        lldcok = k16ldc .eq. 'ELAS            ' .or.&
               & k16ldc .eq. 'VMIS_ISOT_LINE  ' .or.&
               & k16ldc .eq. 'VMIS_ISOT_TRAC  '
        if (.not.lldcok) call utmess('F', 'RUPTURE1_69', sk=k16ldc)
!
!       on interdit VMIS_ISOT_TRAC en presence de thermique
        if (k16ldc .eq. 'VMIS_ISOT_TRAC  ' .and. exitem) call utmess('F', 'RUPTURE1_70')
    enddo
!
    call jedema()
!
end subroutine
