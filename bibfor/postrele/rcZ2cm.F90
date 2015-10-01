subroutine rcZ2cm()
    implicit none
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
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_ZE200
!     LECTURE DU MOT CLE FACTEUR "CHAR_MECA"
!
!     ------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/codent.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/wkvect.h"
!
    integer :: n1, n1t, iocc, ndim, nbchar, nume, jchar, jcorp
    character(len=8) :: knumec
    character(len=16) :: motclf
! DEB ------------------------------------------------------------------
    call jemarq()
!
    motclf = 'CHAR_MECA'
    call getfac(motclf, nbchar)
!
    ndim = 0
    do 10, iocc = 1, nbchar, 1
    call getvis(motclf, 'NUME_CHAR', iocc=iocc, scal=nume, nbret=n1)
    ndim = max (ndim, nume)
    10 end do
!
    call jecrec('&&RC3200.VALE_CHAR', 'V V R', 'NO', 'DISPERSE', 'VARIABLE',&
                nbchar)
!
    do 20, iocc = 1, nbchar, 1
!
    call getvis(motclf, 'NUME_CHAR', iocc=iocc, scal=nume, nbret=n1)
!
    knumec = 'C       '
    call codent(nume, 'D0', knumec(2:8))
!
!
    call jecroc(jexnom('&&RC3200.VALE_CHAR', knumec))
    call jeecra(jexnom('&&RC3200.VALE_CHAR', knumec), 'LONMAX', 6)
    call jeecra(jexnom('&&RC3200.VALE_CHAR', knumec), 'LONUTI', 6)
    call jeveuo(jexnom('&&RC3200.VALE_CHAR', knumec), 'E', jchar)
!
! ------ UN SEUL TENSEUR OU 2 ?
!
    call getvr8(motclf, 'MX', iocc=iocc, nbval=0, nbret=n1t)
!
    if (n1t .ne. 0) then
        call getvr8(motclf, 'MX', iocc=iocc, scal=zr(jchar-1+1), nbret=n1)
        call getvr8(motclf, 'MY', iocc=iocc, scal=zr(jchar-1+2), nbret=n1)
        call getvr8(motclf, 'MZ', iocc=iocc, scal=zr(jchar-1+3), nbret=n1)
!
    else
        call getvr8(motclf, 'MX_TUBU', iocc=iocc, scal=zr(jchar-1+1), nbret=n1)
        call getvr8(motclf, 'MY_TUBU', iocc=iocc, scal=zr(jchar-1+2), nbret=n1)
        call getvr8(motclf, 'MZ_TUBU', iocc=iocc, scal=zr(jchar-1+3), nbret=n1)
!
        call getvr8(motclf, 'MX_CORP', iocc=iocc, scal=zr(jchar-1+4), nbret=n1)
        call getvr8(motclf, 'MY_CORP', iocc=iocc, scal=zr(jchar-1+5), nbret=n1)
        call getvr8(motclf, 'MZ_CORP', iocc=iocc, scal=zr(jchar-1+6), nbret=n1)
    endif
!
    20 end do
!
    call wkvect('&&RC3200.CORPS', 'V V L', 1, jcorp)
    if (n1t .ne. 0) then
        zl(jcorp) = .false.
    else
        zl(jcorp) = .true.
    endif
!
    call jedema()
end subroutine
