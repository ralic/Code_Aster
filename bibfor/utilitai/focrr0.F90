subroutine focrr0(nomfon, interp, base, resu, nomcha,&
                  maille, noeud, cmp, npoint, nusp,&
                  ivari, nbordr, lordr)
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lxlgut.h"
#include "asterfort/posddl.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnopa.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/utch19.h"
#include "asterfort/wkvect.h"
    integer :: nbordr, lordr(*), npoint, ivari
    character(len=1) :: base
    character(len=8) :: interp, maille, noeud, cmp
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
!                  POUR TOUS LES NUMEROS D'ORDRE
!     ------------------------------------------------------------------
! VAR : NOMFON : NOM DE LA FONCTION
! IN  : INTERP : TYPE D'INTERPOLATION DE LA FONCTION
! IN  : BASE   : BASE OU L'ON CREE LA FONCTION
! IN  : RESU   : NOM DE LA STRUCTURE RESULTAT
! IN  : NOMCHA : NOM DU CHAMP
! IN  : NOEUD  : NOEUD
! IN  : MAILLE : MAILE
! IN  : CMP    : COMPOSANTE
! IN  : NPOINT : NUMERO DU POINT ( CAS DES CHAM_ELEMS )
! IN  : NUSP   : NUMERO DU SOUS-POINT ( CAS DES CHAM_ELEMS )
! IN  : IVARI   : NUMERO DE LA CMP (POUR VARI_R)
!     ------------------------------------------------------------------
    character(len=1) :: type
    character(len=24) :: valk(2)
    character(len=4) :: typch2
    character(len=8) :: k8b, noma, nogd, noeuz
    character(len=16) :: nomcmd, typcon, nomacc, typcha, typres
    character(len=19) :: profch, profc2, cham19
    complex(kind=8) :: valc
    integer :: npoinz, nuspz
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: ibid, iddl, ie, ierd, ii, inoeud, iordr
    integer :: iret, lacce, lfon, lg1, lg2, lpro, lvacc
    integer :: lvale, lvar, nbacc, nusp, vali
    real(kind=8) :: valr
!-----------------------------------------------------------------------
    call jemarq()
    call getres(k8b, typcon, nomcmd)
! GETTCO ne fonctionne pas avec les noms compose issus de sensibilite
!      CALL GETTCO(RESU,TYPRES)
    call dismoi('F', 'TYPE_RESU', resu(1:8), 'RESULTAT', ibid,&
                typres, ierd)
!
    call rsnopa(resu, 0, '&&FOCRR0.VAR.ACCES', nbacc, ibid)
    call jeexin('&&FOCRR0.VAR.ACCES', iret)
    if (iret .gt. 0) then
        call jeveuo('&&FOCRR0.VAR.ACCES', 'E', lvacc)
        nomacc = zk16(lvacc)
    else
        nomacc = ' '
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
    zk24(lpro+1) = interp
    zk24(lpro+2) = nomacc
    zk24(lpro+3) = cmp
    zk24(lpro+4) = 'EE      '
    zk24(lpro+5) = nomfon
!
    if (typres(1:10) .eq. 'DYNA_HARMO') then
        call wkvect(nomfon//'.VALE', base//' V R', 3*nbordr, lvar)
    else
        call wkvect(nomfon//'.VALE', base//' V R', 2*nbordr, lvar)
    endif
    lfon = lvar + nbordr
    call rsexch('F', resu, nomcha, lordr(1), cham19,&
                ie)
    call dismoi('F', 'TYPE_SUPERVIS', cham19, 'CHAMP', ibid,&
                typcha, ie)
!
    if (typcha(1:7) .eq. 'CHAM_NO') then
        call dismoi('F', 'PROF_CHNO', cham19, 'CHAM_NO', ibid,&
                    profch, ie)
        call dismoi('F', 'NOM_MAILLA', cham19, 'CHAM_NO', ibid,&
                    noma, ie)
        call posddl('CHAM_NO', cham19, noeud, cmp, inoeud,&
                    iddl)
        if (inoeud .eq. 0) then
            lg1 = lxlgut(noeud)
            call u2mesk('F', 'UTILITAI_92', 1, noeud(1:lg1))
        else if (iddl.eq.0) then
            lg1 = lxlgut(noeud)
            lg2 = lxlgut(cmp)
            valk(1) = cmp(1:lg2)
            valk(2) = noeud(1:lg1)
            call u2mesk('F', 'UTILITAI_93', 2, valk)
        endif
        ii = 0
        do 10 iordr = 1, nbordr
            call jemarq()
!
!           --- EXTRACTION DU CHAMP ET DE LA VALEUR DE L'ACCES ----
            call rsexch(' ', resu, nomcha, lordr(iordr), cham19,&
                        ie)
            if (ie .eq. 0) then
                call dismoi('F', 'PROF_CHNO', cham19, 'CHAM_NO', ibid,&
                            profc2, ie)
                if (profc2 .ne. profch) then
                    profch = profc2
                    call posddl('CHAM_NO', cham19, noeud, cmp, inoeud,&
                                iddl)
                    if (inoeud .eq. 0) then
                        lg1 = lxlgut(noeud)
                        call u2mesk('F', 'UTILITAI_92', 1, noeud(1:lg1))
                    else if (iddl.eq.0) then
                        lg1 = lxlgut(noeud)
                        lg2 = lxlgut(cmp)
                        valk(1) = cmp(1:lg2)
                        valk(2) = noeud(1:lg1)
                        call u2mesk('F', 'UTILITAI_93', 2, valk)
                    endif
                endif
                call rsadpa(resu, 'L', 1, nomacc, lordr(iordr),&
                            0, lacce, k8b)
                call jeveuo(cham19//'.VALE', 'L', lvale)
                if (typres(1:10) .eq. 'DYNA_HARMO') then
                    zr(lvar+iordr-1) = zr(lacce)
                    zr(lfon+ii) = dble(zc(lvale+iddl-1))
                    ii = ii + 1
                    zr(lfon+ii) = dimag(zc(lvale+iddl-1))
                    ii = ii + 1
                else
                    zr(lvar+iordr-1) = zr(lacce)
                    zr(lfon+iordr-1) = zr(lvale+iddl-1)
                endif
                call jelibe(cham19//'.VALE')
            endif
            call jedema()
10      continue
!
    else if (typcha(1:9).eq.'CHAM_ELEM') then
        noeuz = noeud
        npoinz = npoint
        nuspz = nusp
! ---    VERIFICATION DE LA PRESENCE DES MOTS CLE GROUP_MA (OU MAILLE)
! ---    ET GROUP_NO (OU NOEUD OU POINT) DANS LE CAS D'UN CHAM_ELEM
!        -------------------------------------------------------------
        call dismoi('F', 'TYPE_CHAMP', cham19, 'CHAMP', ibid,&
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
        call dismoi('F', 'NOM_MAILLA', cham19, 'CHAM_ELEM', ibid,&
                    noma, ie)
        call dismoi('F', 'NOM_GD', cham19, 'CHAM_ELEM', ibid,&
                    nogd, ie)
        call dismoi('F', 'TYPE_SCA', nogd, 'GRANDEUR', ibid,&
                    type, ie)
        ii = 0
        do 20 iordr = 1, nbordr
            call jemarq()
!
!           --- EXTRACTION DU CHAMP ET DE LA VALEUR DE L'ACCES ----
            call rsexch(' ', resu, nomcha, lordr(iordr), cham19,&
                        ie)
            if (ie .eq. 0) then
                call rsadpa(resu, 'L', 1, nomacc, lordr(iordr),&
                            0, lacce, k8b)
                call utch19(cham19, noma, maille, noeuz, npoinz,&
                            nuspz, ivari, cmp, type, valr,&
                            valc, vali, iret)
                if (iret .eq. 0) then
                    zr(lvar+iordr-1) = zr(lacce)
                    if (type .eq. 'R') then
                        zr(lfon+iordr-1) = valr
                    else
                        zr(lfon+ii) = dble(valc)
                        ii = ii + 1
                        zr(lfon+ii) = dimag(valc)
                        ii = ii + 1
                    endif
                endif
            endif
            call jedema()
20      continue
    else
        call u2mesk('F', 'UTILITAI_94', 1, typcha)
    endif
    call jedetr('&&FOCRR0.VAR.ACCES')
!
    call jedema()
end subroutine
