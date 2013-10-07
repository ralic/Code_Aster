subroutine gverlc(resu, compor, iord0)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/carces.h"
#include "asterfort/cesexi.h"
#include "asterfort/cesred.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/rsexch.h"
#include "asterfort/utmess.h"
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
! Check is CALG_G COMPOR <CARTE> is coherent with result COMPOR <CARTE>
!
! --------------------------------------------------------------------------------------------------
!
! In  resu   : name of result
! In  compor : name of COMPOR <CARTE>
! In  iord0  : first NUME_ORDRE in result 
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iret, jresv, jresd, jresl, jresk, jcalv, jcald, jcall, jcalk
    integer :: nbma, iadr, iadc, ima
    character(len=8) :: noma, nomail
    character(len=6) :: lcham(3)
    character(len=16) :: valk(3)
    character(len=19) :: chresu, chcalc, chtmp
    character(len=19) :: compor_resu
!
    data  lcham/ 'RELCOM', 'DEFORM', 'INCELA'/
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    chtmp  = '&&GVERLC_CHTMP'
    chresu = '&&GVERLC_CHRESU'
    chcalc = '&&GVERLC_CHCALC'
!
! - Sort COMPOR <CARTE>
!
    call carces(compor, 'ELEM', ' ', 'V', chtmp,&
                'A', iret)
    call cesred(chtmp,0,[0],3,lcham,&
                'V', chcalc)
    call detrsd('CHAM_ELEM_S', chtmp)
!
    call jeveuo(chcalc//'.CESD', 'L', jcald)
    call jeveuo(chcalc//'.CESV', 'L', jcalv)
    call jeveuo(chcalc//'.CESL', 'L', jcall)
    call jeveuo(chcalc//'.CESK', 'L', jcalk)
!
    noma = zk8(jcalk-1+1)
    nbma = zi(jcald-1+1)
!
! - COMPOR <CARTE> in result
!
    call rsexch(' ', resu, 'COMPORTEMENT', iord0, compor_resu, &
                iret)
    if (iret .eq. 0 ) then
        call carces(compor_resu, 'ELEM', ' ', 'V', chtmp,&
                    'A', iret)
        call cesred(chtmp, 0, [0], 3, lcham,&
                    'V', chresu)
        call detrsd('CHAM_ELEM_S', chtmp)
        call jeveuo(chresu//'.CESD', 'L', jresd)
        call jeveuo(chresu//'.CESV', 'L', jresv)
        call jeveuo(chresu//'.CESL', 'L', jresl)
        call jeveuo(chresu//'.CESK', 'L', jresk)
    endif

!
!     SI LA CARTE DE COMPORTEMENT (RESULTAT) N'EXISTE PAS,
!     CELA SIGNIFIE QUE LA SD RESULTAT A ETE PRODUITE PAR MECA_STATIQUE
!     ET QUE LA LOI DE COMPORTEMENT EST 'ELAS'.
    if (iret .ne. 0) then
!
! ----- No COMPOR <CARTE> in result: isotropic elastic only. If not -> alarm
!
        do ima = 1, nbma
            call cesexi('C', jcald, jcall, ima, 1,&
                        1, 1, iadc)
            if (iadc .gt. 0) then
                if (zk16(jcalv+iadc-1) .eq. 'ELAS') then
                    goto 10
                else
                    call jenuno(jexnum(noma//'.NOMMAI', ima), nomail)
                    valk(1) = 'ELAS'
                    valk(2) = zk16(jcalv+iadc-1)
                    valk(3) = nomail
                    call utmess('A', 'RUPTURE1_42', nk=3, valk=valk)
                    goto 999
                endif
            endif
10          continue
        enddo
!
!     SI LA CARTE DE COMPORTEMENT (RESULTAT) EXISTE,ALORS ON VERIFIE QUE
!     LE COMPORTEMENT AFFECTE A CHAQUE MAILLE CORRESPOND A CELUI ETABLI
!     DANS CALC_G (VOIR ROUTINE NMDORC)
!
    else
!
! ----- COMPOR <CARTE> in result: check
!
        do ima = 1,nbma
            call cesexi('C', jresd, jresl, ima, 1,&
                        1, 1, iadr)
            call cesexi('C', jcald, jcall, ima, 1,&
                        1, 1, iadc)
!
! --------- COMP_INCR -> only VMIS and ELAS
!
            if (iadr .gt. 0) then
                if (zk16(jresv+iadr-1+2)(1:9) .eq. 'COMP_INCR') then
                    if (zk16(jresv+iadr-1)(1:4) .eq. 'VMIS') then
                        call utmess('A', 'RUPTURE1_47')
                    else
                        if (zk16(jresv+iadr-1)(1:4) .ne. 'ELAS') then
                            call utmess('F', 'RUPTURE1_47')
                        endif
                    endif
                endif
            endif
!
            if (iadc .gt. 0 .and. iadr .gt. 0) then
!
                if (zk16(jresv+iadr-1) .eq. zk16(jcalv+iadc-1)) then
!
! ----------------- If not same deformation -> Alarm
!
                    if (zk16(jresv+iadr-1+1) .eq. zk16(jcalv+iadc-1+1)) then
                        goto 20
                    else
                        call jenuno(jexnum(noma//'.NOMMAI', ima), nomail)
                        valk(1)=zk16(jresv+iadr-1+1)
                        valk(2)=zk16(jcalv+iadc-1+1)
                        valk(3)=nomail
                        call utmess('A', 'RUPTURE1_45', nk=3, valk=valk)
                        goto 999
                    endif
                else
!
! ----------------- If not same compotment -> Alarm
!
                    call jenuno(jexnum(noma//'.NOMMAI', ima), nomail)
                    valk(1)=zk16(jresv+iadr-1)
                    valk(2)=zk16(jcalv+iadc-1)
                    valk(3)=nomail
                    call utmess('A', 'RUPTURE1_42', nk=3, valk=valk)
                    goto 999
                endif
            endif
20          continue
        end do
    endif
!
999 continue
!
    call detrsd('CHAM_ELEM_S', chresu)
    call detrsd('CHAM_ELEM_S', chcalc)
!
    call jedema()
!
end subroutine
