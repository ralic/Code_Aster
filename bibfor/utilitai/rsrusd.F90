subroutine rsrusd(nomsd, iordr)
    implicit none
#include "jeveux.h"
!
#include "asterc/isnnem.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/rsutch.h"
#include "asterfort/rsutrg.h"
    integer :: iordr
    character(len=*) :: nomsd
! ----------------------------------------------------------------------
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
! person_in_charge: jacques.pellet at edf.fr
!
!  BUT: "EFFACER" LA STRUCTURE DE DONNEES RESULTAT NOMSD
!        A PARTIR DU NUMERO D'ORDRE IORDR (Y COMPRIS IORDR)
!
! ----------------------------------------------------------------------
! IN  : NOMSD  : NOM DE LA STRUCTURE "RESULTAT"
! IN  : IORDR  : NUMERO D'ORDRE A PARTIR DUQUEL ON EFFACE TOUT
!       SI IORDR N'EST PAS UN NUMERO D'ORDRE DE NOMSD => ERREUR <F>
! ----------------------------------------------------------------------
!
! REMARQUES :
!------------
! ON NE REDIMENSIONNE PAS LES OBJETS JEVEUX MAIS :
!   - ON REMET LONUTI DE .ORDR A (IORDR-1)
!   - ON EFFACE LE CONTENU DE .ORDR AU DELA DE IORDR
!   - ON DETRUIT LES CHAMPS EXISTANTS AU DELA DE IORDR
!   - ON REMET LES NOMS DES CHAMPS A " "  AU DELA DE IORDR
!   - ON EFFACE LES VALEURS DES PARAMETRES AU DELA DE IORDR
!
!
    character(len=16) :: nomsy
    character(len=19) :: noms2, chextr
    character(len=24) :: nomobj
    integer ::  kordr, krang, irang, nbcham, nbordr, k, ibid, jtach
    integer :: nbormx, n1, n2, kk, iundef, jpara, ier1
    real(kind=8) :: rundef
    integer, pointer :: ordr(:) => null()
! ----------------------------------------------------------------------
    call jemarq()
!
    rundef = r8vide()
    iundef = isnnem()
!
    noms2 = nomsd
    call jelira(noms2//'.DESC', 'NOMMAX', nbcham)
    call jelira(noms2//'.ORDR', 'LONMAX', nbormx)
    call jeveuo(noms2//'.ORDR', 'E', vi=ordr)
!
    call rsutrg(noms2, iordr, irang, nbordr)
!
!     -- SI IORDR N'EST PAS TROUVE DANS NOMSD, ON VERIFIE
!        QUE IORDR > DERNIER NUMERO D'ORDRE
!        ET ON RESSORT SANS RIEN FAIRE :
    if (irang .eq. 0) then
        ASSERT(iordr.gt.ordr(nbordr))
        goto 40
    endif
!
!
!     -- ON DETRUIT ET ON EFFACE LES CHAMPS :
!     ---------------------------------------
    do 20 k = 1, nbcham
        call jenuno(jexnum(noms2//'.DESC', k), nomsy)
        call jenonu(jexnom(noms2//'.DESC', nomsy), ibid)
        call jeveuo(jexnum(noms2//'.TACH', ibid), 'E', jtach)
        do 10 krang = irang, nbordr
            kordr=ordr(krang)
            call rsutch(nomsd, nomsy, kordr, chextr, .true._1)
            call detrsd('CHAMP_GD', chextr)
            zk24(jtach-1+krang) = ' '
10      continue
20  end do
!
!
!     -- ON EFFACE LES PARAMETRES :
!     ------------------------------
    nomobj=noms2//'.RSPR'
    call jeexin(nomobj, ier1)
    if (ier1 .gt. 0) then
        call jelira(nomobj, 'LONMAX', n1)
        n2=n1/nbormx
        ASSERT(n1.eq.n2*nbormx)
        call jeveuo(nomobj, 'E', jpara)
        do 1, kk=n2*irang, n2*nbormx
        zr(jpara-1+kk)=rundef
 1      continue
    endif
!
!
    nomobj=noms2//'.RSPC'
    call jeexin(nomobj, ier1)
    if (ier1 .gt. 0) then
        call jelira(nomobj, 'LONMAX', n1)
        n2=n1/nbormx
        call jeveuo(nomobj, 'E', jpara)
        do 2, kk=n2*irang, n2*nbormx
        zc(jpara-1+kk) = dcmplx(rundef,rundef)
 2      continue
    endif
!
!
    nomobj=noms2//'.RSPI'
    call jeexin(nomobj, ier1)
    if (ier1 .gt. 0) then
        call jelira(nomobj, 'LONMAX', n1)
        n2=n1/nbormx
        call jeveuo(nomobj, 'E', jpara)
        do 3, kk=n2*irang, n2*nbormx
        zi(jpara-1+kk)=iundef
 3      continue
    endif
!
!
    nomobj=noms2//'.RSP8'
    call jeexin(nomobj, ier1)
    if (ier1 .gt. 0) then
        call jelira(nomobj, 'LONMAX', n1)
        n2=n1/nbormx
        call jeveuo(nomobj, 'E', jpara)
        do 4, kk=n2*irang, n2*nbormx
        zk8(jpara-1+kk)=' '
 4      continue
    endif
!
!
    nomobj=noms2//'.RS16'
    call jeexin(nomobj, ier1)
    if (ier1 .gt. 0) then
        call jelira(nomobj, 'LONMAX', n1)
        n2=n1/nbormx
        call jeveuo(nomobj, 'E', jpara)
        do 5, kk=n2*irang, n2*nbormx
        zk16(jpara-1+kk)=' '
 5      continue
    endif
!
!
    nomobj=noms2//'.RS24'
    call jeexin(nomobj, ier1)
    if (ier1 .gt. 0) then
        call jelira(nomobj, 'LONMAX', n1)
        n2=n1/nbormx
        call jeveuo(nomobj, 'E', jpara)
        do 6, kk=n2*irang, n2*nbormx
        zk24(jpara-1+kk)=' '
 6      continue
    endif
!
!
    nomobj=noms2//'.RS32'
    call jeexin(nomobj, ier1)
    if (ier1 .gt. 0) then
        call jelira(nomobj, 'LONMAX', n1)
        n2=n1/nbormx
        call jeveuo(nomobj, 'E', jpara)
        do 7, kk=n2*irang, n2*nbormx
        zk32(jpara-1+kk)=' '
 7      continue
    endif
!
!
    nomobj=noms2//'.RS80'
    call jeexin(nomobj, ier1)
    if (ier1 .gt. 0) then
        call jelira(nomobj, 'LONMAX', n1)
        n2=n1/nbormx
        call jeveuo(nomobj, 'E', jpara)
        do 8, kk=n2*irang, n2*nbormx
        zk80(jpara-1+kk)=' '
 8      continue
    endif
!
!     -- ON EFFACE .ORDR :
    call jeecra(noms2//'.ORDR', 'LONUTI', irang-1)
    do 30 krang = irang, nbormx
        ordr(krang)=0
30  end do
!
40  continue
    call jedema()
end subroutine
