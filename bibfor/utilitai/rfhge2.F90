subroutine rfhge2(harmge)
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/copmod.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/lxlgut.h"
#include "asterfort/mdgep5.h"
#include "asterfort/posddl.h"
#include "asterfort/rstran.h"
#include "asterfort/utmess.h"
#include "asterfort/vprecu.h"
#include "asterfort/wkvect.h"
#include "asterfort/zxtrac.h"
!
    character(len=*) :: harmge
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     OPERATEUR "RECU_FONCTION"  MOT CLE "HARM_GENE"
!     ------------------------------------------------------------------
    character(len=4) :: interp(2)
    character(len=8) :: k8b, crit, noeud, cmp, noma, basemo
    character(len=8) :: intres
    character(len=14) :: nume
    character(len=16) :: nomcmd, typcon, nomcha
    character(len=19) :: nomfon, knume, kinst, resu
    character(len=24) :: nogno, valk(2)
    complex(kind=8) :: crep, cbid
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: iagno, ibid, idbase, iddl, idinsg, idvecg
    integer :: ie, ierd, ign2, ii, ino, inoeud, iordr, ldesc
    integer :: iret, itresu, jinst, jj, lfon, lg1, lg2
    integer :: lordr, lpro, lvar, n1, n2
    integer :: n3, nbinsg, nbmode, nbordr
    integer :: neq, ngn, numcmp
    real(kind=8) :: epsi
!-----------------------------------------------------------------------
    call jemarq()
!
    call getres(nomfon, typcon, nomcmd)
!
    resu = harmge
    interp(1) = 'NON '
    interp(2) = 'NON '
    intres = 'NON '
!
    call getvtx(' ', 'CRITERE', scal=crit, nbret=n1)
    call getvr8(' ', 'PRECISION', scal=epsi, nbret=n1)
    call getvtx(' ', 'INTERP_NUME', scal=intres, nbret=n1)
    call getvtx(' ', 'INTERPOL', nbval=2, vect=interp, nbret=n1)
    if (n1 .eq. 1) interp(2) = interp(1)
!
    noeud = ' '
    cmp = ' '
    call getvtx(' ', 'NOM_CMP', scal=cmp, nbret=n1)
    call getvtx(' ', 'NOM_CHAM', scal=nomcha, nbret=n2)
    call getvtx(' ', 'NOEUD', scal=noeud, nbret=n3)
    call getvtx(' ', 'GROUP_NO', scal=nogno, nbret=ngn)
!
    call jeexin(resu//'.'//nomcha(1:4), iret)
    if (iret .eq. 0) then
        call utmess('F', 'UTILITAI4_23', sk=nomcha)
    endif
    call jeveuo(resu//'.'//nomcha(1:4), 'L', itresu)
!
    knume = '&&RFHGE2.NUME_ORDR'
    kinst = '&&RFHGE2.FREQUENCE'
    call rstran(intres, resu, ' ', 1, kinst,&
                knume, nbordr, ie)
    if (ie .ne. 0) then
        call utmess('F', 'UTILITAI4_15')
    endif
    call jeexin(kinst, iret)
    if (iret .gt. 0) then
        call jeveuo(kinst, 'L', jinst)
        call jeveuo(knume, 'L', lordr)
    endif
!
!     --- CREATION DE LA FONCTION ---
!
    ASSERT(lxlgut(nomfon).le.24)
    call wkvect(nomfon//'.PROL', 'G V K24', 6, lpro)
    zk24(lpro) = 'FONCT_C         '
    zk24(lpro+1) = interp(1)//interp(2)
    zk24(lpro+2) = 'FREQ            '
    zk24(lpro+3) = nomcha
    zk24(lpro+4) = 'EE              '
    zk24(lpro+5) = nomfon(1:19)
!
! --- LA FONCTION EST LA CONCATENATION DE DEUX VECTEURS:
! --- ABSCISSES +  ( PARTIE REELLE | PARTIE IMAGINAIRE )
    call wkvect(nomfon//'.VALE', 'G V R', 3*nbordr, lvar)
!
    call jeveuo(resu//'.DESC', 'L', ldesc)
    nbmode = zi(ldesc+1)
    call getvis(' ', 'NUME_CMP_GENE', scal=numcmp, nbret=n1)
    lfon = lvar + nbordr
!
! --- CAS OU D'UNE VARIABLE GENERALISEE
!
    if (n1 .ne. 0) then
        if (numcmp .gt. nbmode) then
            call utmess('F', 'UTILITAI4_14')
        endif
!
        jj = 0
        if (intres(1:3) .ne. 'NON') then
! ---   CAS OU ON INTERPOLE
            call utmess('E', 'ALGORITH11_79')
        else
! ---   CAS OU ON N'INTERPOLE PAS
            do 41 iordr = 0, nbordr-1
                ii = zi(lordr+iordr)
                zr(lvar+iordr) = zr(jinst+iordr)
                crep = zc(itresu+nbmode*(ii-1)+numcmp-1)
                zr(lfon+jj) = dble(crep)
                jj = jj +1
                zr(lfon+jj) = dimag(crep)
                jj = jj +1
41          continue
        endif
    else
!
! --- CAS D'UNE VARIABLE PHYSIQUE
!
        call dismoi('F', 'BASE_MODALE', resu, 'RESU_DYNA', ibid,&
                    basemo, iret)
        call dismoi('F', 'NUME_DDL', basemo, 'RESU_DYNA', ibid,&
                    nume, iret)
        call dismoi('F', 'NOM_MAILLA', nume, 'NUME_DDL', ibid,&
                    noma, ie)
!
! ---   RECUPERATION DE LA BASE MODALE DANS UN VECTEUR DE TRAVAIL
        call dismoi('F', 'NB_EQUA', nume, 'NUME_DDL', neq,&
                    k8b, ie)
        call wkvect('&&RFHGE2.VECT.PROPRE', 'V V R', neq* nbmode, idbase)
        call copmod(basemo, 'DEPL', neq, nume, nbmode,&
                    'R', zr( idbase), [cbid])
!
! --- TRAITEMENT D'UN GROUP DE NOEUDS SEUELEMENT
        if (ngn .ne. 0) then
            call jenonu(jexnom(noma//'.GROUPENO', nogno), ign2)
            if (ign2 .le. 0) then
                call utmess('F', 'ELEMENTS_67', sk=nogno)
            endif
            call jeveuo(jexnum(noma//'.GROUPENO', ign2), 'L', iagno)
            ino = zi(iagno)
            call jenuno(jexnum(noma//'.NOMNOE', ino), noeud)
        endif
        call posddl('NUME_DDL', nume, noeud, cmp, inoeud,&
                    iddl)
        if (inoeud .eq. 0) then
            lg1 = lxlgut(noeud)
            call utmess('F', 'UTILITAI_92', sk=noeud(1:lg1))
        else if (iddl .eq. 0) then
            lg1 = lxlgut(noeud)
            lg2 = lxlgut(cmp)
            valk(1) = cmp(1:lg2)
            valk(2) = noeud(1:lg1)
            call utmess('F', 'UTILITAI_93', nk=2, valk=valk)
        endif
! --- INTERPOLATION PROPREMENT DITE (ESPACE PHYSIQUE)
        jj = 0
        if (intres(1:3) .ne. 'NON') then
! ---   CAS OU ON INTERPOLE
            call jeveuo(resu//'.DISC', 'L', idinsg)
            call jelira(resu//'.DISC', 'LONMAX', nbinsg)
            call wkvect('&&RFHGE2.VECTGENE', 'V V C', nbmode, idvecg)
            do 50 iordr = 0, nbordr-1
!             EXTRACTION ET INTERPOLATION
                call zxtrac(intres, epsi, crit, nbinsg, zr(idinsg),&
                            zr(jinst+iordr), zc(itresu), nbmode, zc(idvecg), ierd)
!             PASSAGE EN BASE PHYSIQUE
                call mdgep5(neq, nbmode, zr(idbase), zc(idvecg), iddl,&
                            crep)
!             REMPLISSAGE DES TROIS VECTEURS DE LA FONCTION
                zr(lvar+iordr) = zr(jinst+iordr)
                zr(lfon+jj) = dble(crep)
                jj = jj +1
                zr(lfon+jj) = dimag(crep)
                jj = jj +1
50          continue
            call jedetr('&&RFHGE2.VECTGENE')
!
        else
! ---   CAS OU ON N'INTERPOLE PAS
            do 51 iordr = 0, nbordr-1
                ii = zi(lordr+iordr)
!             PASSAGE EN BASE PHYSIQUE
                call mdgep5(neq, nbmode, zr(idbase), zc(itresu+nbmode*( ii-1)), iddl,&
                            crep)
                zr(lvar+iordr) = zr(jinst+iordr)
                zr(lfon+jj) = dble(crep)
                jj = jj +1
                zr(lfon+jj) = dimag(crep)
                jj = jj +1
51          continue
!
        endif
    endif
!
    call jedetr('&&RFHGE2.VECT.PROPRE')
!
!     ---------------------------------------------------------------
    call jedetr(knume)
    call jedetr(kinst)
!
    call jedema()
end subroutine
