subroutine focrr2(nomfon, resu, base, nomcha, maille,&
                  noeud, cmp, npoint, nusp, ivari,&
                  ier)
    implicit none
#include "jeveux.h"
!
#include "asterc/getres.h"
#include "asterc/gettco.h"
#include "asterc/getvid.h"
#include "asterc/getvr8.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/focrrs.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/lxlgut.h"
#include "asterfort/posddl.h"
#include "asterfort/rsbary.h"
#include "asterfort/rsexch.h"
#include "asterfort/rslipa.h"
#include "asterfort/rsutro.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/utch19.h"
#include "asterfort/wkvect.h"
    character(len=1) :: base
    character(len=8) :: maille, noeud, cmp
    character(len=16) :: nomcha
    character(len=19) :: nomfon, resu
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     RECUPERATION D'UNE FONCTION DANS UNE STRUCTURE "RESULTAT"
!     ------------------------------------------------------------------
! VAR : NOMFON : NOM DE LA FONCTION
! IN  : RESU   : NOM DE LA STRUCTURE RESULTAT
! IN  : BASE   : BASE OU L'ON CREE LA FONCTION
! IN  : NOMCHA : NOM DU CHAMP
! IN  : NOEUD  : NOEUD
! IN  : MAILLE : MAILE
! IN  : CMP    : COMPOSANTE
! IN  : NPOINT : NUMERO DU POINT ( CAS DES CHAM_ELEMS )
! IN  : NUSP   : NUMERO DU SOUS-POINT ( CAS DES CHAM_ELEMS )
! IN  : IVARI   : NUMERO DE LA CMP (POUR VARI_R)
! OUT : IER    : CODE RETOUR, = 0 : OK
!     ------------------------------------------------------------------
    character(len=1) :: type
    character(len=4) :: typch2
    character(len=8) :: k8b, noma, nogd, nomacc, noeuz
    character(len=16) :: nomcmd, typcon, typcha, typres
    character(len=19) :: listr, profch, profc2, ch1, ch2
    character(len=24) :: valk(2)
    real(kind=8) :: dimag
    real(kind=8) :: valr(2)
    complex(kind=8) :: valc1, valc2
    integer :: npoinz, nuspz
    integer :: iarg
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, i1, i2, iacces, ialexi, iatach, iatava
    integer :: ibid, iddl1, iddl2, ie, ier, ierd, ierr1
    integer :: ierr2, ii, inoeud, iordr, ip1, ip2, iposit
    integer :: iret, ivari, jinst, jlir8, l1, l2, lfon
    integer :: lg1, lg2, lpro, lval1, lval2, lvar
    integer :: n1, n2, n3, n4, nbinst, nbordr, npoint
    integer :: nusp, vali1, vali2
    real(kind=8) :: r1, r2, r8b, rbase, rval, valr1, valr2
!
!-----------------------------------------------------------------------
    call jemarq()
!
    ier = 0
    call getres(k8b, typcon, nomcmd)
    call gettco(resu, typres)
!
    call getvr8(' ', 'INST', 1, iarg, 0,&
                r8b, n1)
    call getvid(' ', 'LIST_INST', 1, iarg, 0,&
                k8b, n2)
    call getvr8(' ', 'FREQ', 1, iarg, 0,&
                r8b, n3)
    call getvid(' ', 'LIST_FREQ', 1, iarg, 0,&
                k8b, n4)
!
    if (typres(1:10) .eq. 'DYNA_HARMO') then
        nomacc = 'FREQ'
        if (n1+n2 .ne. 0) then
            call u2mess('F', 'UTILITAI_95')
        endif
        if (n3+n4 .eq. 0) then
            call focrrs(nomfon, resu, base, nomcha, maille,&
                        noeud, cmp, npoint, nusp, ivari,&
                        ier)
            goto 40
        endif
        if (n3 .ne. 0) then
            nbinst = -n3
            call wkvect('&&FOCRR2.INST', 'V V R', nbinst, jinst)
            call getvr8(' ', 'FREQ', 1, iarg, nbinst,&
                        zr(jinst), n3)
        else
            call getvid(' ', 'LIST_FREQ', 1, iarg, 1,&
                        listr, n4)
            call jeveuo(listr//'.VALE', 'L', jinst)
            call jelira(listr//'.VALE', 'LONMAX', nbinst, k8b)
        endif
    else
        nomacc = 'INST'
        if (n3+n4 .ne. 0) then
            call u2mess('F', 'UTILITAI_96')
        endif
        if (n1+n2 .eq. 0) then
            call focrrs(nomfon, resu, base, nomcha, maille,&
                        noeud, cmp, npoint, nusp, ivari,&
                        ier)
            goto 40
        endif
        if (n1 .ne. 0) then
            nbinst = -n1
            call wkvect('&&FOCRR2.INST', 'V V R', nbinst, jinst)
            call getvr8(' ', 'INST', 1, iarg, nbinst,&
                        zr(jinst), n1)
        else
            call getvid(' ', 'LIST_INST', 1, iarg, 1,&
                        listr, n2)
            call jeveuo(listr//'.VALE', 'L', jinst)
            call jelira(listr//'.VALE', 'LONMAX', nbinst, k8b)
        endif
    endif
!
!     --- REMPLISSAGE DU .PROL ---
    call assert(lxlgut(nomfon).le.24)
    call wkvect(nomfon//'.PROL', base//' V K24', 6, lpro)
    if (typres(1:10) .eq. 'DYNA_HARMO') then
        zk24(lpro) = 'FONCT_C'
    else
        zk24(lpro) = 'FONCTION'
    endif
    zk24(lpro+1) = 'NON NON '
    zk24(lpro+2) = nomacc
    zk24(lpro+3) = cmp
    zk24(lpro+4) = 'EE      '
    zk24(lpro+5) = nomfon
!
    if (typres(1:10) .eq. 'DYNA_HARMO') then
        call wkvect(nomfon//'.VALE', base//' V R', 3*nbinst, lvar)
    else
        call wkvect(nomfon//'.VALE', base//' V R', 2*nbinst, lvar)
    endif
    lfon = lvar + nbinst
!
    call jenonu(jexnom(resu//'.NOVA', nomacc), iacces)
    call assert(iacces.ne.0)
    call jeveuo(jexnum(resu//'.TAVA', iacces), 'L', iatava)
!
    call rslipa(resu, nomacc, '&&FOCRR2.LIR8', jlir8, nbordr)
!
!
!     -- ON REPERE QUELS SONT LES CHAMPS EXISTANT REELLEMENT:
    call wkvect('&&FOCRR2.LEXI', 'V V L', nbordr, ialexi)
    call jenonu(jexnom(resu//'.DESC', nomcha), ibid)
    call jeveuo(jexnum(resu//'.TACH', ibid), 'L', iatach)
    do 10 i = 1, nbordr
        if (zk24(iatach-1+i) (1:1) .eq. ' ') then
            zl(ialexi-1+i) = .false.
        else
            zl(ialexi-1+i) = .true.
        endif
10  end do
!
    rval = zr(jinst)
    call rsbary(zr(jlir8), nbordr, .false., zl(ialexi), rval,&
                i1, i2, iposit)
    call rsutro(resu, i1, ip1, ierr1)
    call rsexch('F', resu, nomcha, ip1, ch1,&
                ierd)
    call dismoi('F', 'TYPE_SUPERVIS', ch1, 'CHAMP', ibid,&
                typcha, ierd)
!
!               ----- EXTRACTION SUR UN "CHAM_NO" -----
!
    if (typcha(1:7) .eq. 'CHAM_NO') then
        call dismoi('F', 'PROF_CHNO', ch1, 'CHAM_NO', ibid,&
                    profch, ie)
        call dismoi('F', 'NOM_MAILLA', ch1, 'CHAM_NO', ibid,&
                    noma, ie)
        call posddl('CHAM_NO', ch1, noeud, cmp, inoeud,&
                    iddl1)
        if (inoeud .eq. 0) then
            lg1 = lxlgut(noeud)
            call u2mesk('F', 'UTILITAI_92', 1, noeud(1:lg1))
        else if (iddl1.eq.0) then
            lg1 = lxlgut(noeud)
            lg2 = lxlgut(cmp)
            valk(1) = cmp(1:lg2)
            valk(2) = noeud(1:lg1)
            call u2mesk('F', 'UTILITAI_93', 2, valk)
        endif
        iddl2 = iddl1
        do 20 iordr = 0, nbinst - 1
            call jemarq()
!
            rval = zr(jinst+iordr)
            call rsbary(zr(jlir8), nbordr, .false., zl(ialexi), rval,&
                        i1, i2, iposit)
            if (iposit .eq. -2) then
                valr (1) = rval
                call u2mesg('F', 'UTILITAI6_16', 0, ' ', 0,&
                            0, 1, valr)
!
!           -- PROLONGEMENT A GAUCHE:
!           -------------------------
            else if (iposit.eq.-1) then
                valr (1) = rval
                valr (2) = zr(jlir8)
                call u2mesg('F', 'UTILITAI6_17', 0, ' ', 0,&
                            0, 2, valr)
!
!           -- PROLONGEMENT A DROITE:
!           -------------------------
            else if (iposit.eq.1) then
                valr (1) = rval
                valr (2) = zr(jlir8+nbordr-1)
                call u2mesg('F', 'UTILITAI6_18', 0, ' ', 0,&
                            0, 2, valr)
            endif
!
            call rsutro(resu, i1, ip1, ierr1)
            call rsutro(resu, i2, ip2, ierr2)
            call assert(ierr1+ierr2.le.0)
            rbase = zr(jlir8-1+i2) - zr(jlir8-1+i1)
!
            call rsexch('F', resu, nomcha, ip1, ch1,&
                        l1)
            call rsexch('F', resu, nomcha, ip2, ch2,&
                        l2)
!
            call dismoi('F', 'PROF_CHNO', ch1, 'CHAM_NO', ibid,&
                        profc2, ie)
            if (profc2 .ne. profch) then
                profch = profc2
                call posddl('CHAM_NO', ch1, noeud, cmp, inoeud,&
                            iddl1)
                if (inoeud .eq. 0) then
                    lg1 = lxlgut(noeud)
                    call u2mesk('F', 'UTILITAI_92', 1, noeud(1:lg1))
                else if (iddl1.eq.0) then
                    lg1 = lxlgut(noeud)
                    lg2 = lxlgut(cmp)
                    valk(1) = cmp(1:lg2)
                    valk(2) = noeud(1:lg1)
                    call u2mesk('F', 'UTILITAI_93', 2, valk)
                endif
                iddl2 = iddl1
            endif
!
            if (rbase .eq. 0.0d0) then
                call jeveuo(ch1//'.VALE', 'L', lval1)
                zr(lvar+iordr) = rval
                zr(lfon+iordr) = zr(lval1+iddl1-1)
                goto 22
            endif
            r1 = (zr(jlir8-1+i2)-rval)/rbase
            r2 = (rval-zr(jlir8-1+i1))/rbase
!
            call dismoi('F', 'PROF_CHNO', ch2, 'CHAM_NO', ibid,&
                        profc2, ie)
            if (profc2 .ne. profch) then
                profch = profc2
                call posddl('CHAM_NO', ch2, noeud, cmp, inoeud,&
                            iddl2)
                if (inoeud .eq. 0) then
                    lg1 = lxlgut(noeud)
                    call u2mesk('F', 'UTILITAI_92', 1, noeud(1:lg1))
                else if (iddl2.eq.0) then
                    lg1 = lxlgut(noeud)
                    lg2 = lxlgut(cmp)
                    valk(1) = cmp(1:lg2)
                    valk(2) = noeud(1:lg1)
                    call u2mesk('F', 'UTILITAI_93', 2, valk)
                endif
            endif
!
            call jeveuo(ch1//'.VALE', 'L', lval1)
            call jeveuo(ch2//'.VALE', 'L', lval2)
            zr(lvar+iordr) = rval
            zr(lfon+iordr) = r1*zr(lval1+iddl1-1) + r2*zr(lval2+iddl2- 1)
!
            iddl1 = iddl2
22          continue
            call jedema()
20      continue
!
!               ----- EXTRACTION SUR UN "CHAM_ELEM" -----
!
    else if (typcha(1:9).eq.'CHAM_ELEM') then
        noeuz = noeud
        npoinz = npoint
        nuspz = nusp
! ---    VERIFICATION DE LA PRESENCE DES MOTS CLE GROUP_MA (OU MAILLE)
! ---    ET GROUP_NO (OU NOEUD OU POINT) DANS LE CAS D'UN CHAM_ELEM
!        -------------------------------------------------------------
        call dismoi('F', 'TYPE_CHAMP', ch1, 'CHAMP', ibid,&
                    typch2, ie)
        if (typch2 .eq. 'ELEM') then
            npoinz = 1
            nuspz = 1
            noeuz = ' '
            if (maille .eq. ' ') call u2mess('F', 'CHAMPS_11')
        else if (typch2.eq.'ELNO') then
            nuspz = 1
            if (maille .eq. ' ' .or. (noeud.eq.' ' .and. npoint.eq.0)) call u2mess('F',&
                                                                                   'CHAMPS_12')
        else
            if (maille .eq. ' ' .or. npoint .eq. 0) call u2mess('F', 'CHAMPS_13')
        endif
        call dismoi('F', 'NOM_MAILLA', ch1, 'CHAM_ELEM', ibid,&
                    noma, ie)
        call dismoi('F', 'NOM_GD', ch1, 'CHAM_ELEM', ibid,&
                    nogd, ie)
        call dismoi('F', 'TYPE_SCA', nogd, 'GRANDEUR', ibid,&
                    type, ie)
!
        ii = 0
        do 30 iordr = 0, nbinst - 1
            call jemarq()
!
            rval = zr(jinst+iordr)
            call rsbary(zr(jlir8), nbordr, .false., zl(ialexi), rval,&
                        i1, i2, iposit)
            if (iposit .eq. -2) then
                valr (1) = rval
                call u2mesg('F', 'UTILITAI6_16', 0, ' ', 0,&
                            0, 1, valr)
!
!           -- PROLONGEMENT A GAUCHE:
!           -------------------------
            else if (iposit.eq.-1) then
                valr (1) = rval
                valr (2) = zr(jlir8)
                call u2mesg('F', 'UTILITAI6_17', 0, ' ', 0,&
                            0, 2, valr)
!
!           -- PROLONGEMENT A DROITE:
!           -------------------------
            else if (iposit.eq.1) then
                valr (1) = rval
                valr (2) = zr(jlir8+nbordr-1)
                call u2mesg('F', 'UTILITAI6_18', 0, ' ', 0,&
                            0, 2, valr)
            endif
!
            call rsutro(resu, i1, ip1, ierr1)
            call rsutro(resu, i2, ip2, ierr2)
            call assert(ierr1+ierr2.le.0)
            rbase = zr(jlir8-1+i2) - zr(jlir8-1+i1)
!
            call rsexch('F', resu, nomcha, ip1, ch1,&
                        l1)
            call rsexch('F', resu, nomcha, ip2, ch2,&
                        l2)
!
            if (rbase .eq. 0.0d0) then
                call utch19(ch1, noma, maille, noeuz, npoinz,&
                            nuspz, ivari, cmp, type, valr1,&
                            valc1, vali1, iret)
                call assert(iret.eq.0)
                zr(lvar+iordr) = rval
                if (type .eq. 'R') then
                    zr(lfon+iordr) = valr1
                else
                    zr(lfon+ii) = dble(valc1)
                    ii = ii + 1
                    zr(lfon+ii) = dimag(valc1)
                    ii = ii + 1
                endif
                goto 32
            endif
            r1 = (zr(jlir8-1+i2)-rval)/rbase
            r2 = (rval-zr(jlir8-1+i1))/rbase
!
            call utch19(ch1, noma, maille, noeuz, npoinz,&
                        nuspz, ivari, cmp, type, valr1,&
                        valc1, vali1, iret)
            call assert(iret.eq.0)
            call utch19(ch2, noma, maille, noeuz, npoinz,&
                        nuspz, ivari, cmp, type, valr2,&
                        valc2, vali2, iret)
            call assert(iret.eq.0)
!
            zr(lvar+iordr) = rval
            if (type .eq. 'R') then
                zr(lfon+iordr) = r1*valr1 + r2*valr2
            else
                zr(lfon+ii) = dble(r1*valc1+r2*valc2)
                ii = ii + 1
                zr(lfon+ii) = dimag(r1*valc1+r2*valc2)
                ii = ii + 1
            endif
!
32          continue
            call jedema()
30      continue
    endif
!
! --- MENAGE
    call jedetr('&&FOCRR2.LEXI')
    call jedetr('&&FOCRR2.LIR8')
    call jedetr('&&FOCRR2.INST')
!
40  continue
!
    call jedema()
end subroutine
