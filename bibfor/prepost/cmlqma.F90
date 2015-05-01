subroutine cmlqma(nbmato, nbma, nbno, lima, typema,&
                  conniz, connoz, nofils, nbtyma, nomast,&
                  reftyp, nbref, impmai)
    implicit none
#include "jeveux.h"
!
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
    integer :: nbmato, nbma, nbno, lima(nbma), nbtyma, nbref(*), nofils(12, *)
    integer :: typema(*), reftyp(*), impmai(*)
    character(len=8) :: nomast(*)
    character(len=*) :: conniz, connoz
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
! ----------------------------------------------------------------------
!           MISE A JOUR DES MAILLES (CREA_MAILLAGE LINE_QUAD)
! ----------------------------------------------------------------------
! IN        NBMATO  NOMBRE TOTAL DE MAILLES DU MAILLAGE
! IN        NBMA    NOMBRE DE MAILLES DE LA LISTE DES MAILLES A TRAITER
! IN        NBNO    NOMBRE DE NOEUDS DU MAILLAGE INITIAL
! IN        LIMA    LISTE DES MAILLES A TRAITER
! VAR       TYPEMA  LISTE DES TYPES DES MAILLES
! IN        NDINIT  NUMERO INITIAL DES NOEUDS CREES
! IN        CONNEI  CONNECTIONS INITIALES (COLLECTION JEVEUX)
! IN/JXOUT  CONNEO  NOUVELLES CONNECTIONS (COLLECTION JEVEUX)
! IN        NOFILS  LISTE DES NOEUDS CREES PAR MAILLE A TRAITER
! ----------------------------------------------------------------------
!
!
    integer :: m, ma, tymain, tymaou, nbnoin, nbnoou, n, jmamo, jposma, jconxi
    integer :: jconxo, ifm, niv
    character(len=24) :: connei, conneo, mamo, posmai
! ----------------------------------------------------------------------
!
    call jemarq()
    connei = conniz
    conneo = connoz
    call infniv(ifm, niv)
!
! --- LISTE DES MAILLES MODIFIEES
!
    mamo = '&&CMLQMA.MAMO'
    posmai = '&&CMLQMA.POSMAI'
    call wkvect(mamo, 'V V L', nbmato, jmamo)
    call wkvect(posmai, 'V V I', nbmato, jposma)
    do 12 m = 1, nbmato
        zl(jmamo-1+m) = .false.
12  end do
!
    do 14 m = 1, nbma
        ma = lima(m)
        tymain = typema(ma)
        tymaou = reftyp(tymain)
        if (tymain .ne. tymaou) then
            zl(jmamo -1+ma) = .true.
            zi(jposma-1+ma) = m
            impmai(tymain) = impmai(tymain) + 1
        endif
14  end do
!
! --- CREATION DE LA CONNECTIVITE
!
    do 20 ma = 1, nbmato
!
! ------ ANCIENNE CONNECTIVITE
        call jelira(jexnum(connei, ma), 'LONMAX', nbnoin)
        call jeveuo(jexnum(connei, ma), 'L', jconxi)
!
! ------ NOUVEAU NOMBRE DE NOEUD POUR LA MAILLE COURANTE
        tymain = typema(ma)
        if (zl(jmamo-1 + ma)) then
            nbnoou = nbref(tymain)
        else
            nbnoou = nbnoin
        endif
!
! ------ NOUVELLE CONNECTIVITE
        call jeecra(jexnum(conneo, ma), 'LONMAX', nbnoou)
        call jeveuo(jexnum(conneo, ma), 'E', jconxo)
!
! ------ RECOPIE DES NOEUDS INCHANGES
        do 22 n = 1, nbnoin
            zi(jconxo-1+n) = zi(jconxi-1+n)
22      continue
!
! ------ INSERTION DES NOUVEAUX NOEUDS
        if (zl(jmamo-1+ma)) then
            do 24 n = nbnoin+1, nbnoou
                zi(jconxo-1+n) = nofils(n-nbnoin,zi(jposma-1+ma)) + nbno
24          continue
!
! ------ MODIFICATION DU TYPE
            tymaou = reftyp(tymain)
            typema(ma) = tymaou
        endif
!
20  end do
!
! --- IMPRESSION DU NOMBRE DE MAILLES LINE_QUAD
!
    if (niv .ge. 1) then
        write(ifm,1000) 1
        do 30 m = 1, nbtyma
            if (impmai(m) .ne. 0) then
                write(ifm,1002) impmai(m), nomast(m), nomast(reftyp(m)&
                )
            endif
30      continue
    endif
!
    1000 format('MOT CLE FACTEUR "LINE_QUAD", OCCURRENCE ',i4)
    1002 format('   TRANSFORMATION DE ',i6,' MAILLES ',a8,' EN ',a8)
!
    call jedetr(mamo)
    call jedetr(posmai)
    call jedema()
end subroutine
