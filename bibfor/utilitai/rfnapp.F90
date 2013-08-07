subroutine rfnapp(nappe)
    implicit none
#include "jeveux.h"
!
#include "asterc/getres.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/foattr.h"
#include "asterfort/foimpr.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/lxlgut.h"
#include "asterfort/titre.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    character(len=19) :: nappe
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
! EXTRAIT UNE FONCTION D UNE NAPPE (ATTENTION, PAS D INTERPOLATION)
! POUR LE PARAMETRE DONNE
! ----------------------------------------------------------------------
    integer :: ifm, niv
    integer :: nv, np, nc, npar, jpara, indic, jprol, lpro, nbvr
    integer :: jval, lval, ival
    real(kind=8) :: valp, prec, vpar, delta
    character(len=8) :: k8b, crit
    character(len=16) :: nomcmd, typfon
    character(len=19) :: nomfon
    integer :: iarg
!     ------------------------------------------------------------------
!
    call jemarq()
!
    call infmaj()
    call infniv(ifm, niv)
!
    call getres(nomfon, typfon, nomcmd)
!
    call getvr8(' ', 'VALE_PARA_FONC', 0, iarg, 1,&
                valp, nv)
    call getvr8(' ', 'PRECISION', 0, iarg, 1,&
                prec, np)
    call getvtx(' ', 'CRITERE', 0, iarg, 1,&
                crit, nc)
!
    call jelira(nappe//'.PARA', 'LONUTI', npar)
    call jeveuo(nappe//'.PARA', 'L', jpara)
!
    do 10 indic = 1, npar
        vpar=zr(jpara+indic-1)
        if (crit .eq. 'RELATIF') then
            delta=abs((vpar-valp)/valp)
            if (delta .le. prec) goto 20
        else if (crit .eq. 'ABSOLU') then
            delta=abs(vpar-valp)
            if (delta .le. prec) goto 20
        endif
10  end do
    call u2mess('F', 'UTILITAI5_90')
20  continue
!
!     --- REMPLISSAGE DU .PROL ---
!
    call jeveuo(nappe//'.PROL', 'L', jprol)
    ASSERT(lxlgut(nomfon).le.24)
    call wkvect(nomfon//'.PROL', 'G V K24', 6, lpro)
    zk24(lpro) = 'FONCTION'
    zk24(lpro+1) = zk24(jprol+6+indic*2-1)
    zk24(lpro+2) = zk24(jprol+6)
    zk24(lpro+3) = zk24(jprol+3)
    zk24(lpro+4) = zk24(jprol+6+indic*2)
    zk24(lpro+5) = zk24(jprol+5)
!
    call jeveuo(jexnum(nappe//'.VALE', indic), 'L', jval)
    call jelira(jexnum(nappe//'.VALE', indic), 'LONMAX', nbvr)
    call wkvect(nomfon//'.VALE', 'G V R', nbvr, lval)
    do 30 ival = 1, nbvr
        zr(lval+ival-1)=zr(jval+ival-1)
30  end do
!
!     --- CREATION D'UN TITRE ---
    call titre()
    call foattr(' ', 1, nomfon)
    if (niv .gt. 1) call foimpr(nomfon, niv, ifm, 0, k8b)
!
    call jedema()
end subroutine
