subroutine op0003()
    implicit none
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
! person_in_charge: mathieu.courtois at edf.fr
!     LECTURE DE LA DEFINITION D'UNE FONCTION
!     STOCKAGE DANS UN OBJET DE TYPE FONCTION
! ----------------------------------------------------------------------
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/r8maem.h"
#include "asterfort/assert.h"
#include "asterfort/defcur.h"
#include "asterfort/foimpr.h"
#include "asterfort/foverf.h"
#include "asterfort/getvem.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lxlgut.h"
#include "asterfort/ordonn.h"
#include "asterfort/titre.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    integer :: i, iret, ibid, ival, jval, jfon, ifm
    integer :: jpar, lpara, lpar2, l, lpro, n1, n2, n3, n4, n5, niv
    integer :: jnoe, n, nbval, lval, lfon
    integer :: nbcoup, nbcou2
    integer :: nblr, nblv, nbrma, nbln, nbvc, nbvr, nbla, nblo
    real(kind=8) :: min1, min2, min3
    character(len=2) :: prolgd
    character(len=4) :: interp(2)
    character(len=8) :: k8b, nompar, nomres, nommai
    character(len=16) :: nomcmd, typfon, verif
    character(len=19) :: nomfon, listpa, listfo
    character(len=24) :: nommas
    character(len=24) :: valk(2)
    integer :: iarg
!     ------------------------------------------------------------------
!
! --- RECUPERATION DU NIVEAU D'IMPRESSION
    call infmaj()
    call infniv(ifm, niv)
    call jemarq()
!
    call getres(nomfon, typfon, nomcmd)
    verif = ' '
    call getvtx(' ', 'VERIF', scal=verif, nbret=n1)
    call getvr8(' ', 'VALE', nbval=0, nbret=n2)
    call getvr8(' ', 'VALE_C', nbval=0, nbret=n3)
    call getvr8(' ', 'ABSCISSE', nbval=0, nbret=nbla)
!
    call getvtx(' ', 'NOEUD_PARA', nbval=0, nbret=nbln)
    call getvid(' ', 'VALE_PARA', nbval=0, nbret=nblr)
    call getvr8(' ', 'VALE_Y', nbval=0, nbret=nblv)
    nbvr = abs(n2)
    nbvc = abs(n3)
    nbla = abs(nbla)
    nbln = abs(nbln)
    nblv = abs(nblv)
!
    if (nbvc .ne. 0) then
        if (nbvc .lt. 3) then
            call u2mess('F', 'UTILITAI2_66')
        endif
        nbcoup = nbvc / 3
        if (mod(nbvc,3) .ne. 0) then
            call u2mess('F', 'UTILITAI2_67')
        endif
        call wkvect('&&OP0003.TEMP.PARA', 'V V R', nbvc, lpara)
        call wkvect('&&OP0003.TEMP.PAR2', 'V V R', nbcoup, lpar2)
        call getvr8(' ', 'VALE_C', nbval=nbvc, vect=zr(lpara), nbret=l)
        do 2 i = 0, nbcoup-1
            zr(lpar2+i) = zr(lpara+3*i)
 2      continue
!        VERIF QUE LES PARAMETRES SONT STRICT CROISSANTS
        if (verif .eq. 'CROISSANT') then
            iret=2
            call foverf(zr(lpar2), nbcoup, iret)
            ASSERT(iret.eq.2)
        endif
    else if (nbvr.ne.0 .and. nbln.eq.0) then
        if (nbvr .lt. 2) then
            call u2mess('F', 'UTILITAI2_66')
        endif
        nbcoup = nbvr / 2
        if (mod(nbvr,2) .ne. 0) then
            call u2mess('F', 'UTILITAI2_68')
        endif
        call wkvect('&&OP0003.TEMP.PARA', 'V V R', nbvr, lpara)
        call wkvect('&&OP0003.TEMP.PAR2', 'V V R', nbcoup, lpar2)
        call getvr8(' ', 'VALE', nbval=nbvr, vect=zr(lpara), nbret=l)
        do 4 i = 0, nbcoup-1
            zr(lpar2+i) = zr(lpara+2*i)
 4      continue
!        VERIF QUE LES PARAMETRES SONT STRICT CROISSANTS
        if (verif .eq. 'CROISSANT') then
            iret=2
            call foverf(zr(lpar2), nbcoup, iret)
            ASSERT(iret.eq.2)
        endif
    else if (nbla.ne.0) then
        if (nbla .lt. 2) then
            call u2mess('F', 'UTILITAI2_66')
        endif
        call getvr8(' ', 'ORDONNEE', nbval=0, nbret=nblo)
        nblo = abs(nblo)
        if (nblo .ne. nbla) then
            call u2mess('F', 'UTILITAI2_77')
        endif
        call wkvect('&&OP0003.TEMP.PAR2', 'V V R', nbla, lpar2)
        call getvr8(' ', 'ABSCISSE', nbval=nbla, vect=zr(lpar2), nbret=l)
!        VERIF QUE LES PARAMETRES SONT STRICT CROISSANTS
        if (verif .eq. 'CROISSANT') then
            iret=2
            call foverf(zr(lpar2), nbla, iret)
            ASSERT(iret.eq.2)
        endif
    endif
!
    call getvtx(' ', 'NOM_PARA', scal=nompar, nbret=n1)
    call getvtx(' ', 'NOM_RESU', scal=nomres, nbret=n2)
    call getvtx(' ', 'INTERPOL', nbval=2, vect=interp, nbret=n3)
    if (n3 .eq. 1) interp(2) = interp(1)
    call getvtx(' ', 'PROL_GAUCHE', scal=prolgd(1:1), nbret=n4)
    call getvtx(' ', 'PROL_DROITE', scal=prolgd(2:2), nbret=n5)
!
!
    ASSERT(lxlgut(nomfon).le.24)
    call wkvect(nomfon//'.PROL', 'G V K24', 6, lpro)
    if (nbln .ne. 0) then
        zk24(lpro) = 'FONCTION'
        zk24(lpro+1) = interp(1)//interp(2)
        zk24(lpro+2) = nompar
        zk24(lpro+3) = nomres
        zk24(lpro+4) = prolgd
        zk24(lpro+5) = nomfon
    else
        if (nbvc .ne. 0) then
            zk24(lpro) = 'FONCT_C '
        else
            zk24(lpro) = 'FONCTION'
        endif
        zk24(lpro+1) = interp(1)//interp(2)
        zk24(lpro+2) = nompar
        zk24(lpro+3) = nomres
        zk24(lpro+4) = prolgd
        zk24(lpro+5) = nomfon
    endif
!
!     MINI DES ABSCISSES
    min1=r8maem()
!     MINI DES ORDONNEES
    min2=r8maem()
!     MINI DES ORDONNEES (PARTIE IMAGINAIRE)
    min3=r8maem()
!
    if (nbln .ne. 0) then
        call getvid(' ', 'MAILLAGE', scal=nommai, nbret=ibid)
        nommas = nommai//'.NOMMAI'
        call jelira(nommas, 'NOMUTI', nbrma)
!
        call wkvect('&&OP0003.VALEURS_LUES', 'V V R', nblv, jval)
        call wkvect('&&OP0003.NOEUDS_LUES', 'V V K8', nbln, jnoe)
        if (nblv .ne. nbln) then
            call u2mess('F', 'UTILITAI2_69')
        endif
        call getvr8(' ', 'VALE_Y', nbval=nblv, vect=zr(jval), nbret=n)
        call getvem(nommai, 'NOEUD', ' ', 'NOEUD_PARA', 0,&
                    iarg, nblv, zk8( jnoe), n)
        nbval = 2*(nbrma+1)
        call wkvect(nomfon//'.VALE', 'G V R', nbval, lval)
!
        k8b = interp(1)
        call defcur(zr(jval), zk8(jnoe), nblv, zr(lval), nbval,&
                    nommai, nbrma, prolgd, k8b)
!
        do 9 ival = 0, nblv-1
            if (zr(jval+ival) .lt. min1) min1=zr(jval+ival)
 9      continue
    else if (nbvc.ne.0) then
        call wkvect('&&OP0003.VALEURS_LUES', 'V V R', nbvc, jval)
        call getvr8(' ', 'VALE_C', nbval=nbvc, vect=zr(jval), nbret=n)
        call wkvect(nomfon//'.VALE', 'G V R', nbvc, lval)
        nbcoup = nbvc / 3
        lfon = lval + nbcoup - 1
        do 10 ival = 0, nbcoup-1
            zr(lval+ ival ) = zr(jval+3*ival)
            zr(lfon+(ival*2)+1) = zr(jval+3*ival+1)
            zr(lfon+(ival*2)+2) = zr(jval+3*ival+2)
            if (zr(lval+ival) .lt. min1) min1=zr(lval+ival)
            if (zr(lfon+(ival*2)+1) .lt. min2) min2=zr(lfon+(ival*2)+1)
            if (zr(lfon+(ival*2)+2) .lt. min3) min3=zr(lfon+(ival*2)+2)
10      continue
!
    else if (nbvr.ne.0 .and. nbln.eq.0) then
        call wkvect('&&OP0003.VALEURS_LUES', 'V V R', nbvr, jval)
        call getvr8(' ', 'VALE', nbval=nbvr, vect=zr(jval), nbret=n)
        call wkvect(nomfon//'.VALE', 'G V R', nbvr, lval)
        nbcoup = nbvr / 2
        lfon = lval + nbcoup
        do 20 ival = 0, nbcoup-1
            zr(lval+ival) = zr(jval+2*ival)
            zr(lfon+ival) = zr(jval+2*ival+1)
            if (zr(lval+ival) .lt. min1) min1=zr(lval+ival)
            if (zr(lfon+ival) .lt. min2) min2=zr(lfon+ival)
20      continue
!
    else if (nblr.ne.0) then
        call getvid(' ', 'VALE_PARA', scal=listpa, nbret=n1)
        call getvid(' ', 'VALE_FONC', scal=listfo, nbret=n1)
        call jelira(listpa//'.VALE', 'LONMAX', nbcoup)
        call jelira(listfo//'.VALE', 'LONMAX', nbcou2)
        if (nbcou2 .ne. nbcoup) then
            if (nbcou2 .lt. nbcoup) then
                valk(1) = listfo
                valk(2) = listpa
                call u2mesk('F', 'UTILITAI2_70', 2, valk)
            endif
            if (nbcoup .lt. nbcou2) then
                valk(1) = listpa
                valk(2) = listfo
                call u2mesk('F', 'UTILITAI2_70', 2, valk)
            endif
        endif
        nbval = nbcoup * 2
        call wkvect(nomfon//'.VALE', 'G V R', nbval, lval)
        call jeveuo(listpa//'.VALE', 'L', jpar)
        call jeveuo(listfo//'.VALE', 'L', jfon)
        lfon = lval + nbcoup
        do 30 ival = 0, nbcoup-1
            zr(lval+ival) = zr(jpar+ival)
            zr(lfon+ival) = zr(jfon+ival)
            if (zr(lval+ival) .lt. min1) min1=zr(lval+ival)
            if (zr(lfon+ival) .lt. min2) min2=zr(lfon+ival)
30      continue
    else if (nbla.ne.0) then
        call wkvect(nomfon//'.VALE', 'G V R', nbla*2, lval)
        call getvr8(' ', 'ABSCISSE', nbval=nbla, vect=zr(lval), nbret=n)
        call getvr8(' ', 'ORDONNEE', nbval=nbla, vect=zr(lval+nbla), nbret=n)
!
    endif
!
!     --- VERIFICATION QUE LES VALEURS SONT >=0 SI INTERPOLATION 'LOG'
!     JE NE VOIS PAS A QUOI CELA PEUT CORRESPONDRE SUR UNE FONCTION
!     A VALEURS COMPLEXES, MAIS AUTANT LE LAISSER POSSIBLE
    if (interp(2) .eq. 'LOG' .and. (nbvc.ne.0)) then
        call u2mess('F', 'UTILITAI5_92')
    endif
    if ((interp(1).eq.'LOG'.and.min1.le.0.d0) .or. (interp(2).eq.'LOG'.and.(min2.le.0.d0))) then
        call u2mess('F', 'UTILITAI2_71')
    endif
!
!     --- VERIFICATION QU'ON A BIEN CREER UNE FONCTION ---
!         ET REMISE DES ABSCISSES EN ORDRE CROISSANT
!         CE N'EST PAS LA PEINE SI LA CROISSANTE STRICTE A ETE IMPOSEE
    if (verif .ne. 'CROISSANT') then
        call ordonn(nomfon, 0)
    endif
!
!     --- CREATION D'UN TITRE ---
    call titre()
!
!     --- IMPRESSIONS ---
    if (niv .gt. 1) call foimpr(nomfon, niv, ifm, 0, k8b)
!
    call jedema()
end subroutine
