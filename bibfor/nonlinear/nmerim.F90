subroutine nmerim(sderro)
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
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
    character(len=24) :: sderro
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (SD ERREUR)
!
! EMISSION MESSAGE ERRREUR
!
! ----------------------------------------------------------------------
!
!
! IN  SDERRO : SD ERREUR
!
! ----------------------------------------------------------------------
!
    integer :: ieven, zeven
    character(len=24) :: errinf
    integer :: jeinfo
    character(len=24) :: erraac, erreni, errmsg
    integer :: jeeact, jeeniv, jeemsg
    integer :: icode
    character(len=9) :: teven
    character(len=24) :: meven
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SD
!
    errinf = sderro(1:19)//'.INFO'
    call jeveuo(errinf, 'L', jeinfo)
    zeven = zi(jeinfo-1+1)
!
    erraac = sderro(1:19)//'.EACT'
    erreni = sderro(1:19)//'.ENIV'
    errmsg = sderro(1:19)//'.EMSG'
    call jeveuo(erraac, 'L', jeeact)
    call jeveuo(erreni, 'L', jeeniv)
    call jeveuo(errmsg, 'L', jeemsg)
!
! --- EMISSION DES MESSAGES D'ERREUR
!
    do 10 ieven = 1, zeven
        icode = zi(jeeact-1+ieven)
        teven = zk16(jeeniv-1+ieven)(1:9)
        meven = zk24(jeemsg-1+ieven)
        if ((teven(1:3).eq.'ERR') .and. (icode.eq.1)) then
            if (meven .eq. ' ') then
                ASSERT(.false.)
            endif
            if (meven .eq. 'MECANONLINE10_1') then
                call utmess('I', 'MECANONLINE10_1')
            else if (meven.eq.'MECANONLINE10_2') then
                call utmess('I', 'MECANONLINE10_2')
            else if (meven.eq.'MECANONLINE10_3') then
                call utmess('I', 'MECANONLINE10_3')
            else if (meven.eq.'MECANONLINE10_4') then
                call utmess('I', 'MECANONLINE10_4')
            else if (meven.eq.'MECANONLINE10_5') then
                call utmess('I', 'MECANONLINE10_5')
            else if (meven.eq.'MECANONLINE10_6') then
                call utmess('I', 'MECANONLINE10_6')
            else if (meven.eq.'MECANONLINE10_7') then
                call utmess('I', 'MECANONLINE10_7')
            else if (meven.eq.'MECANONLINE10_8') then
                call utmess('I', 'MECANONLINE10_8')
            else if (meven.eq.'MECANONLINE10_9') then
                call utmess('I', 'MECANONLINE10_9')
            else if (meven.eq.'MECANONLINE10_10') then
                call utmess('I', 'MECANONLINE10_10')
            else if (meven.eq.'MECANONLINE10_11') then
                call utmess('I', 'MECANONLINE10_11')
            else if (meven.eq.'MECANONLINE10_12') then
                call utmess('I', 'MECANONLINE10_12')
            else if (meven.eq.'MECANONLINE10_20') then
                call utmess('I', 'MECANONLINE10_20')
            else if (meven.eq.'MECANONLINE10_24') then
                call utmess('I', 'MECANONLINE10_24')
            else
                ASSERT(.false.)
            endif
        endif
 10 end do
!
    call jedema()
end subroutine
