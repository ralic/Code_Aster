subroutine tbutnu(motfac, iocc, nomjv, nbinst, nomtab,&
                  prec, crit)
    implicit none
#include "jeveux.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsindi.h"
#include "asterfort/tbexv1.h"
#include "asterfort/u2mesg.h"
#include "asterfort/wkvect.h"
    integer :: iocc, nbinst
    real(kind=8) :: prec
    character(len=8) :: crit
    character(len=16) :: motfac
    character(len=*) :: nomjv, nomtab
!     ------------------------------------------------------------------
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
!     RECUPERER LES INSTANTS DANS UNE TABLE A PARTIR
!        DU MOT CLE  INST
!        DU MOT CLE  LIST_INST
!        PAR DEFAUT, TOUT_INST
!
!
    integer :: ibid, np, nc, n1, n2, jinstd, jinst, jordr, ii, nbval, nbtrou
    integer :: nutrou(1)
    real(kind=8) :: dinst
    real(kind=8) :: valr
    complex(kind=8) :: cbid
    character(len=8) :: k8b
    character(len=24) :: valk
    character(len=19) :: listr
! DEB ------------------------------------------------------------------
!
    call jemarq()
!
    call getvr8(motfac, 'PRECISION', iocc=iocc, scal=prec, nbret=np)
    call getvtx(motfac, 'CRITERE', iocc=iocc, scal=crit, nbret=nc)
!
    nbinst = 0
    call getvid(motfac, 'LIST_INST', iocc=iocc, scal=listr, nbret=n1)
    if (n1 .ne. 0) then
        call jeveuo(listr//'.VALE', 'L', jinstd)
        call jelira(listr//'.VALE', 'LONMAX', nbinst)
        call wkvect(nomjv, 'V V R', nbinst, jinst)
        call wkvect('&&TBUTNU.ORDRE', 'V V I', nbinst, jordr)
        do 10 ii = 1, nbinst
            zr(jinst+ii-1) = zr(jinstd+ii-1)
            zi(jordr+ii-1) = ii
10      continue
    endif
!
    call getvr8(motfac, 'INST', iocc=iocc, nbval=0, nbret=n2)
    if (n2 .ne. 0) then
        nbinst = -n2
        call wkvect(nomjv, 'V V R', nbinst, jinst)
        call getvr8(motfac, 'INST', iocc=iocc, nbval=nbinst, vect=zr(jinst),&
                    nbret=n2)
        call wkvect('&&TBUTNU.ORDRE', 'V V I', nbinst, jordr)
        do 12 ii = 1, nbinst
            zi(jordr+ii-1) = ii
12      continue
    endif
!
    call tbexv1(nomtab, 'INST', '&&TBUTNU.INST_D', 'V', nbval,&
                k8b)
    call jeveuo('&&TBUTNU.INST_D', 'L', jinstd)
    do 20 ii = 1, nbinst
        dinst = zr(jinst+ii-1)
        call rsindi('R8  ', jinstd, 1, jordr, ibid,&
                    dinst, k8b, cbid, prec, crit,&
                    nbval, nbtrou, nutrou, 1)
        if (nbtrou .lt. 1) then
            valr = dinst
            valk = nomtab
            call u2mesg('F', 'PREPOST5_74', 1, valk, 0,&
                        0, 1, valr)
        else if (nbtrou.gt.1) then
            valr = dinst
            valk = nomtab
            call u2mesg('F', 'PREPOST5_75', 1, valk, 0,&
                        0, 1, valr)
        endif
20  end do
!
    if (nbinst .eq. 0) then
        prec = 1.d-06
        crit = 'RELATIF'
        nbinst = nbval
        call wkvect(nomjv, 'V V R', nbinst, jinst)
        do 30 ii = 1, nbinst
            zr(jinst+ii-1) = zr(jinstd+ii-1)
30      continue
    else
        call jedetr('&&TBUTNU.ORDRE')
    endif
    call jedetr('&&TBUTNU.INST_D')
!
    call jedema()
end subroutine
