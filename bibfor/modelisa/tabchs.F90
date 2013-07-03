subroutine tabchs(tabin, typchs, base, nomgd, ma,&
                  chs)
!
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
!     TRAITEMENT DE COMMANDE:   CREA_CHAMP / OPTION: 'EXTR' / TABLE
!
!     CREATION D'UN CHAMP SIMPLE A PARTIR D'UNE TABLE
!
!
!     IN : TABIN  : NOM DE LA TABLE
!     IN : TYPCHS : TYPE DU CHAMP SIMPLE (NOEU/ELEM/ELNO/ELGA)
!     IN : BASE   : BASE DE CREATION (G/V)
!     IN : NOMGD  : NOM DE LA GRANDEUR
!     IN : MA     : NOM DU MAILLAGE
!     IN/JXOUT : CHS: NOM DU CHAMP SIMPLE A CREER
!
    implicit   none
!
!     ------------------------------------------------------------------
! 0.1. ==> ARGUMENT
!
!
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/cescre.h"
#include "asterfort/cesexi.h"
#include "asterfort/cnscre.h"
#include "asterfort/dismoi.h"
#include "asterfort/indiis.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/tbexip.h"
#include "asterfort/tbexve.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
#include "asterfort/verigd.h"
#include "asterfort/verima.h"
#include "asterfort/wkvect.h"
    character(len=1) :: base
    character(len=8) :: nomgd, ma
    character(len=16) :: typchs
    character(len=19) :: chs, tabin
!
!
! 0.2. ==> COMMUNS
!
!
!      ==> VARIABLES LOCALES
    integer :: jtbnp, jtblp, ncmp, jcmp, jcnsl, jcnsv, i
    integer :: vali(2), nblig, isp, jsp
    integer :: nbval, ibid
    integer :: nuno, numa, nbma, jcesd, nbssp
    integer :: jcesl, jcesv, jcesc, iad
    integer :: nbcol, nbno, ksp, kpt, jcon1, jcon2
    integer :: jcolma, jcolno, jcolpt, jcolsp, ipt
    integer :: icmp, ili, iret, jind, jobj2, jobj3, jpg
    character(len=8) :: k8b, nono, tsca, noma
    character(len=24) :: objlg, objr, objtmp
    character(len=24) :: valk(3)
    logical :: lmail, lnoeu, lpoin, lspoin
! ---------------------------------------------------------------------
!
!
    call jemarq()
!
    call jeveuo(tabin//'.TBNP', 'L', jtbnp)
    nbcol=zi(jtbnp-1+1)
    nblig=zi(jtbnp-1+2)
    call jeveuo(tabin//'.TBLP', 'L', jtblp)
!
    call dismoi('F', 'NB_MA_MAILLA', ma, 'MAILLAGE', nbma,&
                k8b, iret)
    call dismoi('F', 'TYPE_SCA', nomgd, 'GRANDEUR', ibid,&
                tsca, ibid)
    call assert(tsca.eq.'R')
    call jeveuo(ma//'.CONNEX', 'L', jcon1)
    call jeveuo(jexatr(ma//'.CONNEX', 'LONCUM'), 'L', jcon2)
!
!
!     1. VERIFICATION DES PARAMETRES DE LA TABLE :
!     --------------------------------------------
    call tbexip(tabin, 'MAILLE', lmail, tsca)
    call tbexip(tabin, 'NOEUD', lnoeu, tsca)
    call tbexip(tabin, 'POINT', lpoin, tsca)
    call tbexip(tabin, 'SOUS_POINT', lspoin, tsca)
!
    valk(1)=tabin(1:8)
    valk(2)=typchs
!
!     PRESENCE DU PARAMETRE MAILLE
    if (typchs .eq. 'EL' .and. .not.lmail) call u2mesk('F', 'MODELISA9_1', 2, valk)
!
!     PRESENCE/ABSENCE POUR CHAMPS NOEU :
    if (typchs .eq. 'NOEU') then
        if (.not.lnoeu) call u2mesk('F', 'MODELISA9_1', 2, valk)
        if (lmail .or. lpoin .or. lspoin) call u2mesk('F', 'MODELISA9_1', 2, valk)
    endif
!
!     PRESENCE/ABSENCE POUR CHAMPS ELGA :
    if (typchs .eq. 'ELGA') then
        if (.not.lpoin .or. lnoeu) call u2mesk('F', 'MODELISA9_1', 2, valk)
    endif
!
!     PRESENCE/ABSENCE POUR CHAMPS ELNO :
    if (typchs .eq. 'ELNO') then
        if (.not.lpoin .and. .not.lnoeu) call u2mesk('F', 'MODELISA9_1', 2, valk)
        if (lpoin .and. lnoeu) call u2mesk('F', 'MODELISA9_1', 2, valk)
    endif
!
!     PRESENCE/ABSENCE POUR CHAMPS ELEM :
    if (typchs .eq. 'ELEM') then
        if (lpoin .or. lnoeu) call u2mesk('F', 'MODELISA9_1', 2, valk)
    endif
!
!
!     2. RECUPERATION DES COLONNES DE LA TABLE :
!     -------------------------------------------
!     -- 2.1 COLONNE 'NOEUD' :
    if (lnoeu) then
!        ON VERIFIE QUE LES NOEUDS FOURNIS DANS LA TABLE
!        APPARTIENNENT AU MAILLAGE
        objtmp='&&TABCHS.NOEUD'
        call tbexve(tabin, 'NOEUD', objtmp, 'V', nbval,&
                    tsca)
        call jeveuo(objtmp, 'L', jcolno)
        call verima(ma, zk8(jcolno), nbval, 'NOEUD')
    endif
!
!     -- 2.2 COLONNE 'MAILLE' :
    if (lmail) then
!        ON VERIFIE QUE LES MAILLES FOURNIES DANS LA TABLE
!        APPARTIENNENT AU MAILLAGE
        objtmp='&&TABCHS.MAILLE'
        call tbexve(tabin, 'MAILLE', objtmp, 'V', nbval,&
                    tsca)
        call jeveuo(objtmp, 'L', jcolma)
        call verima(ma, zk8(jcolma), nbval, 'MAILLE')
    endif
!
!     -- 2.3 COLONNE 'POINT' :
    if (lpoin) then
        objtmp='&&TABCHS.POINT'
        call tbexve(tabin, 'POINT', objtmp, 'V', nbval,&
                    tsca)
        call jeveuo(objtmp, 'L', jcolpt)
    endif
!
!     -- 2.4 COLONNE 'SOUS_POINT' :
    if (lspoin) then
        objtmp='&&TABCHS.SPOINT'
        call tbexve(tabin, 'SOUS_POINT', objtmp, 'V', nbval,&
                    tsca)
        call jeveuo(objtmp, 'L', jcolsp)
    endif
!
!
!     -- ON REPERE LES COLONNES QUI CORRESPONDENT AUX CMPS.
!        CE SONT CELLES QUI NE SONT PAS : MAILLE, NOEUD, ...
    call wkvect('&&TABCHS.NCMP1', 'V V K24', nbcol, jcmp)
    call wkvect('&&TABCHS.NCMP2', 'V V I', nbcol, jind)
    ncmp=0
    do 10 i = 1, nbcol
        if (zk24(jtblp+4*(i-1)) .ne. 'MAILLE' .and. zk24(jtblp+4*(i-1)) .ne. 'NOEUD' .and.&
            zk24(jtblp+4*(i-1)) .ne. 'POINT' .and. zk24(jtblp+4*(i-1)) .ne. 'SOUS_POINT') then
            ncmp=ncmp+1
            zk24(jcmp+ncmp-1)=zk24(jtblp+4*(i-1))
            zi(jind+ncmp-1)=i
        endif
10  continue
!
!     ON VERIFIE QUE LE NOM ET LE TYPE DES COMPOSANTES
!        DE LA TABLE CORRESPONDENT A LA GRANDEUR LUE
    call verigd(nomgd, zk24(jcmp), ncmp, iret)
    if (iret .ne. 0) then
        call u2mesk('F', 'MODELISA9_2', ncmp, zk24(jcmp))
    endif
!
!
!
    if (typchs .eq. 'NOEU') then
!     ------------------------------------
!
! ---    CREATION DU CHAM_NO_S
        call cnscre(ma, nomgd, ncmp, zk24(jcmp), base,&
                    chs)
!
! ---    REMPLISSAGE DU CHAM_S
        call jeveuo(chs//'.CNSV', 'E', jcnsv)
        call jeveuo(chs//'.CNSL', 'E', jcnsl)
!
        do 30 icmp = 1, ncmp
            objlg=zk24(jtblp+4*(zi(jind+icmp-1)-1)+3)
            call jeveuo(objlg, 'L', jobj2)
            objr=zk24(jtblp+4*(zi(jind+icmp-1)-1)+2)
            call jeveuo(objr, 'L', jobj3)
            do 20 ili = 1, nblig
                if (zi(jobj2+ili-1) .eq. 1) then
                    nono=zk8(jcolno+ili-1)
                    call jenonu(jexnom(ma//'.NOMNOE', nono), nuno)
                    call assert(nuno.gt.0)
                    zr(jcnsv+(nuno-1)*ncmp+icmp-1)=zr(jobj3+ili-1)
                    zl(jcnsl+(nuno-1)*ncmp+icmp-1)=.true.
                endif
20          continue
30      continue
!
!
!
    else if (typchs(1:2).eq.'EL') then
!     ------------------------------------
!
        if (typchs .eq. 'ELNO') then
!          POUR LES CHAMPS ELNO :
!           - SI LNOEU, ON CALCULE '&&TABCHS.POINT'
!           - ON VERIFIE QUE LE NUMERO DE POINT EST POSSIBLE
            if (lnoeu) then
                call assert(.not.lpoin)
                call wkvect('&&TABCHS.POINT', 'V V I', nblig, jcolpt)
            endif
            do 40 ili = 1, nblig
                noma=zk8(jcolma+ili-1)
                call jenonu(jexnom(ma//'.NOMMAI', noma), numa)
                nbno=zi(jcon2-1+numa+1)-zi(jcon2-1+numa)
                if (lpoin) then
                    ipt=zi(jcolpt-1+ili)
                else
                    call assert(lnoeu)
                    nono=zk8(jcolno-1+ili)
                    call jenonu(jexnom(ma//'.NOMNOE', nono), nuno)
                    ipt=indiis(zi(jcon1-1+zi(jcon2-1+numa)),nuno,1,&
                    nbno)
                    zi(jcolpt-1+ili)=ipt
                endif
                if (ipt .eq. 0 .or. ipt .gt. nbno) then
                    valk(1)=tabin
                    valk(2)=noma
                    valk(3)=nono
                    vali(1)=ipt
                    vali(2)=nbno
                    call u2mesg('F', 'MODELISA9_5', 3, valk, 2,&
                                vali, 0, 0.d0)
                endif
40          continue
        endif
!
!
!       CALCUL DU NOMBRE DE SOUS_POINT PAR ELEMENT (&&TABCHS.SP_TOT):
!       CALCUL DE NBSSP : MAX DU NOMBRE DE SOUS_POINT
        if (lspoin) then
            call wkvect('&&TABCHS.SP_TOT', 'V V I', nbma, jsp)
            nbssp=1
            do 50 ili = 1, nblig
                ksp=zi(jcolsp+ili-1)
                call assert(ksp.gt.0)
                nbssp=max(nbssp,ksp)
                noma=zk8(jcolma+ili-1)
                call jenonu(jexnom(ma//'.NOMMAI', noma), numa)
                call assert(numa.gt.0)
                zi(jsp-1+numa)=max(zi(jsp-1+numa),ksp)
50          continue
        else
            nbssp=1
        endif
!
!
!       CALCUL DU NOMBRE DE POINTS DE GAUSS PAR ELEMENT
!       (&&TABCHS.PG_TOT):
        if (typchs .eq. 'ELGA') then
            call wkvect('&&TABCHS.PG_TOT', 'V V I', nbma, jpg)
            do 60 ili = 1, nblig
                kpt=zi(jcolpt+ili-1)
                call assert(kpt.gt.0)
                noma=zk8(jcolma+ili-1)
                call jenonu(jexnom(ma//'.NOMMAI', noma), numa)
                zi(jpg-1+numa)=max(zi(jpg-1+numa),kpt)
60          continue
        endif
!
!
!       CREATION DU CHAM_ELEM_S VIERGE :
        if (nbssp .eq. 1) then
            if (typchs .eq. 'ELNO' .or. typchs .eq. 'ELEM') then
                call cescre(base, chs, typchs, ma, nomgd,&
                            ncmp, zk24(jcmp), - 1, -1, -ncmp)
            else if (typchs.eq.'ELGA') then
                call cescre(base, chs, typchs, ma, nomgd,&
                            ncmp, zk24(jcmp), zi(jpg), -1, -ncmp)
            endif
        else
            if (typchs .eq. 'ELNO' .or. typchs .eq. 'ELEM') then
                call cescre(base, chs, typchs, ma, nomgd,&
                            ncmp, zk24(jcmp), - 1, zi(jsp), -ncmp)
            else if (typchs.eq.'ELGA') then
                call cescre(base, chs, typchs, ma, nomgd,&
                            ncmp, zk24(jcmp), zi(jpg), zi(jsp), -ncmp)
            endif
        endif
!
!
! ---   REMPLISSAGE DU CHAM_S
        call jeveuo(chs//'.CESD', 'L', jcesd)
        call jeveuo(chs//'.CESV', 'E', jcesv)
        call jeveuo(chs//'.CESL', 'E', jcesl)
        call jeveuo(chs//'.CESC', 'L', jcesc)
!
!
        do 80 icmp = 1, ncmp
            objlg=zk24(jtblp+4*(zi(jind+icmp-1)-1)+3)
            call jeveuo(objlg, 'L', jobj2)
            objr=zk24(jtblp+4*(zi(jind+icmp-1)-1)+2)
            call jeveuo(objr, 'L', jobj3)
!
            do 70 ili = 1, nblig
                if (zi(jobj2+ili-1) .eq. 0) goto 70
!
                noma=zk8(jcolma+ili-1)
                call jenonu(jexnom(ma//'.NOMMAI', noma), numa)
!
                ipt=1
                if (lpoin) ipt=zi(jcolpt+ili-1)
!
                isp=1
                if (lspoin) isp=zi(jcolsp+ili-1)
!
                nono=' '
                if (lnoeu) then
                    nono=zk8(jcolno+ili-1)
                    ipt=zi(jcolpt+ili-1)
                endif
!
                call cesexi('S', jcesd, jcesl, numa, ipt,&
                            isp, icmp, iad)
                call assert(iad.ne.0)
                if (iad .lt. 0) then
                    iad=-iad
                else
                    valk(1)=tabin(1:8)
                    valk(2)=noma
                    valk(3)=nono
                    vali(1)=ipt
                    vali(2)=isp
                    call u2mesg('F', 'MODELISA9_6', 3, valk, 2,&
                                vali, 0, 0.d0)
                endif
                zr(jcesv+iad-1)=zr(jobj3+ili-1)
                zl(jcesl+iad-1)=.true.
70          continue
80      continue
    endif
!
    call jedetr('&&TABCHS.MAILLE')
    call jedetr('&&TABCHS.NOEUD')
    call jedetr('&&TABCHS.POINT')
    call jedetr('&&TABCHS.SPOINT')
    call jedetr('&&TABCHS.NCMP1')
    call jedetr('&&TABCHS.NCMP2')
    call jedetr('&&TABCHS.PG_TOT')
    call jedetr('&&TABCHS.SP_TOT')
!
    call jedema()
end subroutine
