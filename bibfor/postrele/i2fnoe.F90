subroutine i2fnoe(courbe, listnd)
    implicit none
!
#include "jeveux.h"
!
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
    character(len=24) :: listnd
    character(len=8) :: courbe
!
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
!     ------------------------------------------------------------------
!     SAISIE DES NOEUDS MIS EN JEU POUR LE POST-TRAITEMENT D' UN
!     CONCEPT DE TYPE COURBE OU SURFACE
!     ------------------------------------------------------------------
! IN  COURBE : K : NOM DU CONCEPT DE TYPE COURBE OU SURFACE
! OUT LISTND : K : NOM DE L' OJB S V I REALISANT LA SAISIE
!     ------------------------------------------------------------------
!     LE VECTEUR LISTND EST DE DIMENSION NBR_NOEUD_ACTIF ET
!     CONTIENT LA LISTE TRIEE DES NOEUDS ACTIFS
!     ------------------------------------------------------------------
!
!
!
    character(len=8) :: typcrb, nmaila
    character(len=15) :: nconec
    character(len=19) :: nmail1
    integer :: adr, pt, nbnd, nbtnd, alstnd, m, p, nbpart, im, in, n, i
    integer :: adrndm, adrm1, nbmail, nbndm, vlcmai, vlccou, asds, aconec
    character(len=1) :: k1bid
!
!================= CORPS DE LA ROUTINE ===============================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jemarq()
    nbnd = 0
    pt = 1
    call jeexin(courbe//'.NOMA', n)
    if (n .ne. 0) then
        call jeveuo(courbe//'.NOMA', 'L', adr)
    else
        call jeveuo(courbe//'.NOMMAIL', 'L', adr)
    endif
    nmaila = zk8(adr)
    nconec = nmaila//'.CONNEX'
    call dismoi('F', 'NB_NO_MAILLA', nmaila, 'MAILLAGE', nbtnd,&
                k1bid, i)
    call jeveuo(jexatr(nconec, 'LONCUM'), 'L', vlcmai)
    call jeveuo(jexnum(nconec, 1), 'L', aconec)
    call wkvect('&&I2FNOE.VEC.TEMP', 'V V I', nbtnd, adr)
    do 10, i = 1, nbtnd, 1
    zi(adr + i-1) = 0
    10 end do
    if (n .ne. 0) then
        call jelira(courbe//'.NSDS', 'LONMAX', nbpart, k1bid)
        call jeveuo(courbe//'.NSDS', 'L', asds)
        do 100, p = 1, nbpart, 1
        nmail1 = zk24(asds+p-1)(1:13)//'.MAIL'
        call jeveuo(jexatr(nmail1, 'LONCUM'), 'L', vlccou)
        call jelira(nmail1, 'NMAXOC', nbmail, k1bid)
        call jeveuo(jexnum(nmail1, 1), 'L', adrm1)
        do 110, im = 1, nbmail, 1
        m = zi(adrm1 + zi(vlccou + im-1)-1)
        adrndm = aconec + zi(vlcmai + m-1)-1
        nbndm = zi(vlcmai + m ) - zi(vlcmai + m-1)
        do 111, in = 1, nbndm, 1
        n = zi(adrndm + in-1)
        zi(adr + n-1) = 1
111      continue
110      continue
100      continue
    else
        call jeveuo(courbe//'.TYPCOURBE', 'L', im)
        typcrb = zk8(im)
        nmail1 = courbe//'.MAIL1'
        call jelira(nmail1, 'NMAXOC', nbpart, k1bid)
        do 400, p = 1, nbpart, 1
        call jelira(jexnum(nmail1, p), 'LONMAX', nbmail, k1bid)
        call jeveuo(jexnum(nmail1, p), 'L', adrm1)
        if (typcrb .eq. 'LISTMAIL') then
            nbmail = nbmail - 1
        endif
        do 410, im = 1, nbmail, 1
        m = zi(adrm1 + im-1)
!C             CALL JEVEUO(JEXNUM(NCONEC,M),'L',ADRNDM)
!C             CALL JELIRA(JEXNUM(NCONEC,M),'LONMAX',NBNDM)
        adrndm = aconec + zi(vlcmai + m-1)-1
        nbndm = zi(vlcmai + m ) - zi(vlcmai + m-1)
        do 411, in = 1, nbndm, 1
        n = zi(adrndm + in-1)
        zi(adr + n-1) = 1
411      continue
410      continue
400      continue
    endif
    do 500, i = 1, nbtnd, 1
    nbnd = nbnd + min(zi(adr + i-1),1)
    500 end do
    call wkvect(listnd, 'V V I', nbnd, alstnd)
    do 600, i = 1, nbtnd, 1
    if (zi(adr + i-1) .ne. 0) then
        zi(alstnd + pt-1) = i
        pt = pt + 1
    endif
    600 end do
    call jedetr('&&I2FNOE.VEC.TEMP')
    call jedema()
end subroutine
