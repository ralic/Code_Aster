subroutine op0159()
    implicit none
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
! person_in_charge: jacques.pellet at edf.fr
!     ------------------------------------------------------------------
!     OPERATEUR APPL_CINE_SCMB
!     BUT: APPLIQUER LES CHAR_CINE SUR LE SECOND MEMBRE
!     ------------------------------------------------------------------
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/chpver.h"
#include "asterfort/csmbgg.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jacopo.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mtdscr.h"
#include "asterfort/titre.h"
#include "asterfort/utmess.h"
#include "asterfort/vtdefs.h"
#include "asterfort/wkvect.h"
    character(len=19) :: vci19, secm19, csol19, mat19
    character(len=8) :: matr
    character(len=3) :: type, typ1
    integer :: lmat, nimpo, idvalc,  neq1, jval2, jtrav
    integer :: ifm, niv, neq, jvals, nb, imd, ier
!
    character(len=8) :: xsol, secmbr, vcine
    character(len=16) :: concep, nomcmd, metres
    complex(kind=8) :: cbid
    character(len=24), pointer :: refa(:) => null()
    cbid = dcmplx(0.d0, 0.d0)
!     ------------------------------------------------------------------
    call jemarq()
!
!
    call infmaj()
    call infniv(ifm, niv)
!
    call getres(xsol, concep, nomcmd)
    csol19=xsol
!
    call getvid('  ', 'MATR', scal=matr, nbret=nb)
    ASSERT(nb.eq.1)
!
    call getvid('  ', 'CHAM_NO', scal=secmbr, nbret=nb)
    ASSERT(nb.eq.1)
    call chpver('F', secmbr, 'NOEU', '*', ier)
    secm19=secmbr
!
!
    vcine = ' '
    call getvid('  ', 'CHAM_CINE', scal=vcine, nbret=nb)
    if (nb .eq. 1) call chpver('F', vcine, 'NOEU', '*', ier)
    vci19=vcine
!
    call dismoi('METH_RESO', matr, 'MATR_ASSE', repk=metres)
    if (metres .ne. 'LDLT' .and. metres .ne. 'MULT_FRONT' .and. metres .ne. 'MUMPS') then
        call utmess('F', 'ALGELINE4_1')
    endif
!
    mat19=matr
    call mtdscr(mat19)
    call jeveuo(mat19//'.&INT', 'E', lmat)
    if (lmat .eq. 0) then
        call utmess('F', 'ALGELINE3_40')
    endif
!
    if (zi(lmat+3) .eq. 1) then
        type='R'
    else if (zi(lmat+3).eq.2) then
        type='C'
    else
        ASSERT(.false.)
    endif
!
    nimpo=zi(lmat+7)
    if (vci19 .eq. ' ') then
        if (nimpo .ne. 0) then
            call utmess('F', 'ALGELINE3_41')
        endif
        idvalc=0
    else
        call jeveuo(vci19//'.VALE', 'L', idvalc)
        call jelira(vci19//'.VALE', 'TYPE', cval=type)
        if (((type.eq.'R').and.(zi(lmat+3).ne.1)) .or.&
            ((type.eq.'C') .and.(zi(lmat+3).ne.2))) then
            call utmess('F', 'ALGELINE3_42')
        endif
    endif
!
    call jeveuo(mat19//'.REFA', 'L', vk24=refa)
    if (refa(11) .eq. 'MATR_DISTR') then
        imd=1
    else
        imd=0
    endif
    ASSERT(secm19.ne.' ')
    ASSERT(csol19.ne.' ')
    if (csol19 .ne. secm19) then
        call detrsd('CHAMP_GD', csol19)
        call vtdefs(csol19, secm19, 'G', ' ')
    endif
!
    neq=zi(lmat+2)
    call jelira(secm19//'.VALE', 'LONMAX', neq1)
    call jelira(secm19//'.VALE', 'TYPE', cval=typ1)
    if ((neq1.ne.neq) .and. (imd.eq.0)) then
        call utmess('F', 'FACTOR_67')
    endif
    if (typ1 .ne. type) then
        call utmess('F', 'FACTOR_68')
    endif
    call jeveuo(secm19//'.VALE', 'L', jval2)
    if (imd .eq. 0) then
        call wkvect('&&APPLCINE.TRAV', 'V V '//type, neq, jtrav)
        call jacopo(neq, type, jval2, jtrav)
    else
        call wkvect('&&APPLCINE.TRAV', 'V V '//type, neq1, jtrav)
        call jacopo(neq1, type, jval2, jtrav)
    endif
!
    if (type .eq. 'R') then
!     ----------------------------------------
        if (idvalc .ne. 0) then
            call csmbgg(lmat, zr(jtrav), zr(idvalc), [cbid], [cbid],&
                        'R')
        endif
!
    else if (type.eq.'C') then
!     ----------------------------------------
        if (idvalc .ne. 0) then
            call csmbgg(lmat, [0.d0], [0.d0], zc(jtrav), zc(idvalc),&
                        'C')
        endif
    endif
!
!     RECOPIE DANS LE CHAMP SOLUTION SI IL Y A LIEU
    call jeveuo(csol19//'.VALE', 'E', jvals)
    if (imd .eq. 0) then
        call jacopo(neq, type, jtrav, jvals)
    else
        call jacopo(neq1, type, jtrav, jvals)
    endif
    call jedetr('&&APPLCINE.TRAV')
!
    call titre()
    call jedema()
end subroutine
