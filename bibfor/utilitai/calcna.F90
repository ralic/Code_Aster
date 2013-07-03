subroutine calcna(nomfin, nomfon, nbvalp, valep, noparp,&
                  nbvalf, valef, noparf)
    implicit   none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/fointe.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/lxlgut.h"
#include "asterfort/wkvect.h"
    integer :: nbvalp, nbvalf
    real(kind=8) :: valep(*), valef(*)
    character(len=19) :: nomfin, nomfon
    character(len=24) :: noparp, noparf
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
!     CREATION DU SD FONCTION A PARTIR D'UNE FORMULE (NAPPE )
!     ------------------------------------------------------------------
    integer :: lont, i, ival, lval, lfon, lprol, lpara, ier
    real(kind=8) :: vale(2)
    character(len=16) :: nopara(2)
!     ------------------------------------------------------------------
!
    call jemarq()
!
!     --- CREATION ET REMPLISSAGE DE L'OBJET NOMFON.VALE ---
!
    lont = 2*nbvalf*nbvalp
    nopara(1) = noparf
    nopara(2) = noparp
!
    call jecrec(nomfon//'.VALE', ' G V R', 'NU', 'CONTIG', 'VARIABLE',&
                nbvalp)
    call jeecra(nomfon//'.VALE', 'LONT', lont, ' ')
    do 10 i = 1, nbvalp
        call jecroc(jexnum(nomfon//'.VALE', i))
        call jeecra(jexnum(nomfon//'.VALE', i), 'LONMAX', 2*nbvalf, ' ')
        call jeecra(jexnum(nomfon//'.VALE', i), 'LONUTI', 2*nbvalf, ' ')
        call jeveuo(jexnum(nomfon//'.VALE', i), 'E', lval)
        lfon = lval + nbvalf
        vale(2) = valep(i)
        do 20 ival = 0, nbvalf-1
            zr(lval+ival) = valef(ival+1)
            vale(1) = zr(lval+ival)
            call fointe('F', nomfin, 2, nopara, vale,&
                        zr(lfon+ival), ier)
20      continue
10  end do
!
!     --- CREATION ET REMPLISSAGE DE L'OBJET NOMFON.PROL ---
!
    call assert(lxlgut(nomfon).le.24)
    call wkvect(nomfon//'.PROL', 'G V K24', 7+2*nbvalp, lprol)
!
    zk24(lprol) = 'NAPPE           '
    zk24(lprol+1) = 'LIN LIN         '
    zk24(lprol+2) = noparp
    zk24(lprol+3) = 'TOUTRESU        '
    zk24(lprol+4) = 'EE              '
    zk24(lprol+5) = nomfon
    zk24(lprol+6) = noparf
    do 30 ival = 1, nbvalp
        zk24(lprol+6+(2*ival-1)) = 'LIN LIN         '
        zk24(lprol+6+(2*ival )) = 'EE              '
30  continue
!
!     --- CREATION ET REMPLISSAGE DE L'OBJET NOMFON.PARA ---
!
    call wkvect(nomfon//'.PARA', 'G V R', nbvalp, lpara)
    do 40 ival = 1, nbvalp
        zr(lpara+ival-1) = valep(ival)
40  continue
!
    call jedema()
end subroutine
