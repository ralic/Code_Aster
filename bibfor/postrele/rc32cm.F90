subroutine rc32cm()
    implicit none
#include "jeveux.h"
#include "asterf_types.h"
#include "asterfort/jemarq.h"
#include "asterc/getfac.h"
#include "asterfort/jecrec.h"
#include "asterfort/getvis.h"
#include "asterfort/codent.h"
#include "asterfort/jecroc.h"
#include "asterfort/jexnom.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeveuo.h"
#include "asterfort/getvr8.h"
#include "asterfort/wkvect.h"
#include "asterfort/jedema.h"
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE B3200 et ZE200
!     LECTURE DU MOT CLE FACTEUR "CHAR_MECA"
!     ------------------------------------------------------------------
!
    integer :: nbchar, iocc, nume, n1, jchar, i, n2
    character(len=8) :: knumec
    integer :: jcorp
! DEB ------------------------------------------------------------------
    call jemarq()
!
    call getfac('CHAR_MECA', nbchar)
!-- si on est en B3200_T
    if (nbchar .eq. 0) goto 9999
!-- sinon
    call jecrec('&&RC3200.VALE_CHAR', 'V V R', 'NO', 'DISPERSE', 'VARIABLE',&
                nbchar)
!
    do 20, iocc = 1, nbchar, 1
!
        call getvis('CHAR_MECA', 'NUME_CHAR', iocc=iocc, scal=nume, nbret=n1)
        knumec = 'C       '
        call codent(nume, 'D0', knumec(2:8))
!
        call jecroc(jexnom('&&RC3200.VALE_CHAR', knumec))
        call jeecra(jexnom('&&RC3200.VALE_CHAR', knumec), 'LONMAX', 12)
        call jeecra(jexnom('&&RC3200.VALE_CHAR', knumec), 'LONUTI', 12)
        call jeveuo(jexnom('&&RC3200.VALE_CHAR', knumec), 'E', jchar)
!
!-- cas simple ou cas corps/tubulure ?
        do 30 i=1,12
            zr(jchar-1+i)=0.d0
 30 end do
!
        call getvr8('CHAR_MECA', 'MX', iocc=iocc, nbval=0, nbret=n2)
!
        if (n2 .ne. 0) then
            call getvr8('CHAR_MECA', 'FX', iocc=iocc, scal=zr(jchar-1+1), nbret=n1)
            call getvr8('CHAR_MECA', 'FY', iocc=iocc, scal=zr(jchar-1+2), nbret=n1)
            call getvr8('CHAR_MECA', 'FZ', iocc=iocc, scal=zr(jchar-1+3), nbret=n1)
            call getvr8('CHAR_MECA', 'MX', iocc=iocc, scal=zr(jchar-1+4), nbret=n1)
            call getvr8('CHAR_MECA', 'MY', iocc=iocc, scal=zr(jchar-1+5), nbret=n1)
            call getvr8('CHAR_MECA', 'MZ', iocc=iocc, scal=zr(jchar-1+6), nbret=n1)
!
        else
            call getvr8('CHAR_MECA', 'FX_TUBU', iocc=iocc, scal=zr(jchar-1+1), nbret=n1)
            call getvr8('CHAR_MECA', 'FY_TUBU', iocc=iocc, scal=zr(jchar-1+2), nbret=n1)
            call getvr8('CHAR_MECA', 'FZ_TUBU', iocc=iocc, scal=zr(jchar-1+3), nbret=n1)
            call getvr8('CHAR_MECA', 'MX_TUBU', iocc=iocc, scal=zr(jchar-1+4), nbret=n1)
            call getvr8('CHAR_MECA', 'MY_TUBU', iocc=iocc, scal=zr(jchar-1+5), nbret=n1)
            call getvr8('CHAR_MECA', 'MZ_TUBU', iocc=iocc, scal=zr(jchar-1+6), nbret=n1)
!
            call getvr8('CHAR_MECA', 'FX_CORP', iocc=iocc, scal=zr(jchar-1+7), nbret=n1)
            call getvr8('CHAR_MECA', 'FY_CORP', iocc=iocc, scal=zr(jchar-1+8), nbret=n1)
            call getvr8('CHAR_MECA', 'FZ_CORP', iocc=iocc, scal=zr(jchar-1+9), nbret=n1)
            call getvr8('CHAR_MECA', 'MX_CORP', iocc=iocc, scal=zr(jchar-1+10), nbret=n1)
            call getvr8('CHAR_MECA', 'MY_CORP', iocc=iocc, scal=zr(jchar-1+11), nbret=n1)
            call getvr8('CHAR_MECA', 'MZ_CORP', iocc=iocc, scal=zr(jchar-1+12), nbret=n1)
        endif
!
    20 end do
!
    call wkvect('&&RC3200.CORPS', 'V V L', 1, jcorp)
    if (n2 .ne. 0) then
        zl(jcorp) = .false.
    else
        zl(jcorp) = .true.
    endif
!
9999 continue
    call jedema()
end subroutine
