subroutine slecor(iunv, datset)
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
!  ROUTINE DE TRAITEMENT DES SYSTEMES DE COORDONNEES SUPERTAB
!  LE DATASET 18 EST OBSOLETE DEPUIS 1987
!  DATSET : IN :DATASET DES SYSTEMES DE COORDONNEES
! ======================================================================
! aslint: disable=
    implicit none
!     =================
!
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/juveca.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    aster_logical :: first
    character(len=6) :: moins1
    character(len=80) :: cbuf, kbid
    real(kind=8) :: rbid
    integer :: datset, ibid, iunv, inus, inum, icoor, jsys, iret, icol, ibid2
!
!  ------------ FIN DECLARATION -------------
!
!  -->N  D'UNITE LOGIQUE ASSOCIE AUX FICHIERS
    call jemarq()
!
    first=.true.
    moins1 = '    -1'
    inus = 10
    call jeexin('&&IDEAS.SYST', iret)
    if (iret .ne. 0) then
        call jedetr('&&IDEAS.SYST')
        if (datset .eq. 2420) then
            call utmess('A', 'STBTRIAS_3')
        else if (datset.eq.18) then
            call utmess('A', 'STBTRIAS_4')
        endif
    endif
    call wkvect('&&IDEAS.SYST', 'V V I', inus, jsys)
!
  1 continue
!
    read(iunv,'(A)') cbuf
    if (cbuf(1:6) .ne. moins1) then
!
        if (first) then
            if (datset .eq. 2420) then
                read(iunv,'(A)') kbid
                read(iunv,'(3I10)') inum,icoor,icol
            else if (datset.eq.18) then
                read(cbuf,'(5I10)') inum,icoor,ibid,icol,ibid2
            endif
        else
            if (datset .eq. 2420) then
                read(cbuf,'(3I10)') inum,icoor,icol
            else if (datset.eq.18) then
                read(cbuf,'(5I10)') inum,icoor,ibid,icol,ibid2
            endif
        endif
        if (inum .gt. inus) then
            inus = inum
            call juveca('&&IDEAS.SYST', inus)
            call jeveuo('&&IDEAS.SYST', 'E', jsys)
        endif
!
        zi(jsys-1+inum) = icoor
!
        if (datset .eq. 2420) then
            read(iunv,'(A)') kbid
            read(iunv,'(3(1PD25.16))') rbid
            read(iunv,'(3(1PD25.16))') rbid
            read(iunv,'(3(1PD25.16))') rbid
            read(iunv,'(3(1PD25.16))') rbid
        else
            read(iunv,'(A)') kbid
            read(iunv,'(6(1PE13.5))') rbid
            read(iunv,'(3(1PE13.5))') rbid
        endif
!
        first = .false.
!
        goto 1
!
    endif
    call jedema()
end subroutine
