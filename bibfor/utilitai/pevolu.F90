subroutine pevolu(resu, modele, nbocc)
    implicit none
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/alchml.h"
#include "asterfort/assert.h"
#include "asterfort/chpchd.h"
#include "asterfort/chsut1.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnscno.h"
#include "asterfort/codent.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nopar2.h"
#include "asterfort/pebpct.h"
#include "asterfort/reliem.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsorac.h"
#include "asterfort/rsutnu.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/utflmd.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: nbocc
    character(len=8) :: modele
    character(len=19) :: resu
!
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
!     OPERATEUR :  POST_ELEM
!     TRAITEMENT DU MOT CLE-FACTEUR : "VOLUMOGRAMME"
!     ------------------------------------------------------------------
!
    integer :: nr, nd, np, nc, ni, no, nli, nlo, iret, ibid, nbma, nbordr, jno
    integer :: nn
    integer :: nbpar, nbpmax, iocc, inum, numo, jin, nbmato, iresma, ncmpm, ifm
    integer :: nbcmp, nbint, jbpct, ivalr, ii, i, ib, jvalr, jvali, jvalk, niv
    integer :: jlicmp, jlicm2, jlicm1, nucmp, ivali, bfix, ivol(2), tord(1)
    parameter(nbpmax=13)
    character(len=4) :: tych, ki
    character(len=8) :: mailla, crit, k8b, resuco, chamg, typpar(nbpmax), nomgd
    character(len=8) :: typmcl(1), tout, nomcmp, infoma, ncpini
    character(len=8) :: nopar, norme
    real(kind=8) :: prec, inst, borne(2), voltot, seuil
    complex(kind=8) :: c16b
    character(len=19) :: knum, kins, lisins, cham, cham2, chamtm, celmod, ligrel
    character(len=19) :: tmpcha, cham3
    character(len=16) :: nompar(nbpmax), mocles(1), optio2, nomcha, valk, valr
    character(len=16) :: vali
    character(len=24) :: mesmai, mesmaf, mesmae, borpct, valk2(5), grouma
    logical :: exiord, toneut, lseuil
!     ------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
! --- 1- RECUPERATION DU MAILLAGE ET DU NOMBRE DE MAILLES
!     ===================================================
    call dismoi('NOM_MAILLA', modele, 'MODELE', repk=mailla)
    call dismoi('NB_MA_MAILLA', mailla, 'MAILLAGE', repi=nbmato)
!
!
! --- 2- RECUPERATION DU RESULTAT ET DES NUMEROS D'ORDRE
!     ==================================================
    call getvid(' ', 'RESULTAT', scal=resuco, nbret=nr)
    call getvid(' ', 'CHAM_GD', scal=chamg, nbret=nd)
!
    call getvr8(' ', 'PRECISION', scal=prec, nbret=np)
    call getvtx(' ', 'CRITERE', scal=crit, nbret=nc)
    call getvr8(' ', 'INST', nbval=0, nbret=ni)
    call getvis(' ', 'NUME_ORDRE', nbval=0, nbret=no)
    call getvid(' ', 'LIST_INST', nbval=0, nbret=nli)
    call getvid(' ', 'LIST_ORDRE', nbval=0, nbret=nlo)
!
    valr = '&&PEVOLU.VALR'
    vali = '&&PEVOLU.VALI'
    valk = '&&PEVOLU.VALK'
    knum = '&&PEVOLU.NUME_ORDRE'
    kins = '&&PEVOLU.INST'
    mesmai = '&&PEVOLU.MES_MAILLES'
    mesmaf = '&&PEVOLU.MAILLES_FILTRE'
    borpct = '&&PEVOLU_BORNES_PCT'
!
    exiord=.false.
    toneut=.false.
!
    if (nd .ne. 0) then
!
        nbordr = 1
        call wkvect(knum, 'V V I', nbordr, jno)
        zi(jno) = 1
        exiord=.true.
!
    else
!
!       -- NUME_ORDRE --
        if (no .ne. 0) then
            exiord=.true.
            nbordr=-no
            call wkvect(knum, 'V V I', nbordr, jno)
            call getvis(' ', 'NUME_ORDRE', nbval=nbordr, vect=zi(jno), nbret=iret)
        endif
!
!       -- LIST_ORDRE --
        if (nlo .ne. 0) then
            exiord=.true.
            call getvid(' ', 'LIST_ORDRE', scal=lisins, nbret=iret)
            call jeveuo(lisins // '.VALE', 'L', jno)
            call jelira(lisins // '.VALE', 'LONMAX', nbordr)
        endif
!
!       -- INST --
        if (ni .ne. 0) then
            nbordr=-ni
            call wkvect(kins, 'V V R', nbordr, jin)
            call getvr8(' ', 'INST', nbval=nbordr, vect=zr(jin), nbret=iret)
        endif
!
!       -- LIST_INST --
        if (nli .ne. 0) then
            call getvid(' ', 'LIST_INST', scal=lisins, nbret=iret)
            call jeveuo(lisins // '.VALE', 'L', jin)
            call jelira(lisins // '.VALE', 'LONMAX', nbordr)
        endif
!
!       -- TOUT_ORDRE --
        nn=nli+ni+no+nlo
        if (nn .eq. 0) then
            exiord=.true.
            call rsutnu(resuco, ' ', 0, knum, nbordr,&
                        prec, crit, iret)
            call jeveuo(knum, 'L', jno)
        endif
!
    endif
!
!
! --- 3- CREATION DE LA TABLE
!     =======================
    call tbcrsd(resu, 'G')
    if (nr .ne. 0) then
        nbpar=11
        nompar(1) ='RESULTAT'
        nompar(2) ='NOM_CHAM'
        nompar(3) ='NUME_ORDRE'
        nompar(4) ='INST'
        nompar(5) ='NOM_CMP'
        nompar(6) ='GROUP_MA'
        nompar(7) ='RESTRICTION'
        nompar(8) ='INTERVALLE'
        nompar(9) ='BORNE_INF'
        nompar(10)='BORNE_SUP'
        nompar(11)='DISTRIBUTION'
        typpar(1) ='K8'
        typpar(2) ='K16'
        typpar(3) ='I'
        typpar(4) ='R'
        typpar(5) ='K8'
        typpar(6) ='K24'
        typpar(7) ='K8'
        typpar(8) ='I'
        typpar(9) ='R'
        typpar(10)='R'
        typpar(11)='R'
    else
        nbpar=8
        nompar(1)='CHAM_GD'
        nompar(2)='NOM_CMP'
        nompar(3)='GROUP_MA'
        nompar(4)='RESTRICTION'
        nompar(5)='INTERVALLE'
        nompar(6)='BORNE_INF'
        nompar(7)='BORNE_SUP'
        nompar(8)='DISTRIBUTION'
        typpar(1)='K8'
        typpar(2)='K8'
        typpar(3)='K24'
        typpar(4)='K8'
        typpar(5)='I'
        typpar(6)='R'
        typpar(7)='R'
        typpar(8)='R'
    endif
    call tbajpa(resu, nbpar, nompar, typpar)
!
! --- 4- REMPLISSAGE DE LA TABLE
!     ==========================
!
!     --- ON PARCOURT LES OCCURENCES DU MOT CLE 'VOLUMOGRAMME':
!     ---------------------------------------------------------
    if (nr .eq. 0) then
        tmpcha='TMP_CHAMP_GD'
        call copisd('CHAMP', 'V', chamg, tmpcha)
    endif
!
    do iocc = 1, nbocc
!
!
!
!     --- BOUCLE SUR LES NUMEROS D'ORDRE:
!     ----------------------------------
!
        do inum = 1, nbordr
!
!      -- 4.1 RECUPERATION DU CHAMP --
!
            if (nr .ne. 0) then
!         --  RESULTAT --
                if (exiord) then
!           - ORDRE -
                    numo=zi(jno+inum-1)
                    call rsadpa(resuco, 'L', 1, 'INST', numo,&
                                0, sjv=jin, styp=k8b)
                    inst=zr(jin)
                else
!           - INST -
                    inst=zr(jin+inum-1)
                    call rsorac(resuco, 'INST', 0, zr(jin+inum-1), k8b,&
                                c16b, prec, crit, tord, nbordr,&
                                iret)
                    numo=tord(1)
                endif
                call getvtx('VOLUMOGRAMME', 'NOM_CHAM', iocc=iocc, scal=nomcha, nbret=iret)
                if (iret .eq. 0) then
                    call utmess('F', 'POSTELEM_4')
                endif
!
                call rsexch(' ', resuco, nomcha, numo, cham2,&
                            iret)
!
            else
!         -- CHAM_GD --
                numo = nbordr
                cham2 = tmpcha
                nomcha= chamg
            endif
!
            call dismoi('TYPE_CHAMP', cham2, 'CHAMP', repk=tych, arret='C',&
                        ier=iret)
            call dismoi('NOM_GD', cham2, 'CHAMP', repk=nomgd, arret='C',&
                        ier=iret)
!
            if (nomgd(6:6) .eq. 'C') goto 10
!
            if (tych(1:2) .ne. 'EL') then
!
!          --- 1. TRANSFORMATION DU CHAMP EN CHAMP NEUTRE:
!              - CHANGEMENT DE LA GRANDEUR EN NEUT_R
!              - CHAMGEMENT DES COMPOSANTES EN X1,X2,X3,...
                toneut=.true.
                chamtm='&&PEVOLU.CHS1'
                call cnocns(cham2, 'V', chamtm)
                call jeveuo(chamtm//'.CNSC', 'L', jlicmp)
                call jelira(chamtm//'.CNSC', 'LONMAX', ncmpm)
                call jedetr('&&PEVOLU.CMP1')
                call wkvect('&&PEVOLU.CMP1', 'V V K8', ncmpm, jlicm1)
                call jedetr('&&PEVOLU.CMP2')
                call wkvect('&&PEVOLU.CMP2', 'V V K8', ncmpm, jlicm2)
                do i = 1, ncmpm
                    call codent(i, 'G', ki)
                    zk8(jlicm2+i-1)='X'//ki(1:len(ki))
                    zk8(jlicm1+i-1)=zk8(jlicmp+i-1)
                end do
                call chsut1(chamtm, 'NEUT_R', ncmpm, zk8(jlicm1), zk8(jlicm2),&
                            'V', chamtm)
!
                cham3='&&PEEINT.CHAM_3'
                call cnscno(chamtm, ' ', 'NON', 'V', cham3,&
                            'F', ibid)
                call detrsd('CHAM_NO_S', chamtm)
!
!           --- 2. CHANGEMENT DE DISCRETISATION : NOEU -> ELGA
                optio2 ='TOU_INI_ELGA'
                call dismoi('NOM_GD', cham3, 'CHAMP', repk=nomgd, arret='C',&
                            ier=iret)
                nopar = nopar2(optio2,nomgd,'OUT')
                celmod = '&&PEVOLU.CELMOD'
                ligrel = modele//'.MODELE'
                call alchml(ligrel, optio2, nopar, 'V', celmod,&
                            ib, ' ')
                if (ib .ne. 0) then
                    valk2(1)=ligrel
                    valk2(2)=nopar
                    valk2(3)=optio2
                    call utmess('F', 'UTILITAI3_23', nk=3, valk=valk2)
                endif
                cham='&&CHPCHD.CHAM'
                call chpchd(cham3, 'ELGA', celmod, 'OUI', 'V',&
                            cham)
                call detrsd('CHAMP', celmod)
                call detrsd('CHAMP', cham3)
!
            else
                cham=cham2
            endif
!
            call dismoi('TYPE_CHAMP', cham, 'CHAMP', repk=tych, arret='C',&
                        ier=iret)
!
!      -- 4.2 RECUPERATION DE LA COMPOSANTE --
!
            call getvtx('VOLUMOGRAMME', 'NOM_CMP', iocc=iocc, scal=nomcmp, nbret=nbcmp)
            ncpini=nomcmp
            if (toneut) then
                nucmp=indik8(zk8(jlicm1),nomcmp,1,ncmpm)
                nomcmp=zk8(jlicm2+nucmp-1)
            endif
!
!      -- 4.3 RECUPERATION DES MAILLES --
!
            call getvtx('VOLUMOGRAMME', 'TOUT', iocc=iocc, scal=tout, nbret=iret)
            if (iret .ne. 0) then
                mocles(1) = 'TOUT'
                typmcl(1) = 'TOUT'
                grouma='-'
            else
                mocles(1) = 'GROUP_MA'
                typmcl(1) = 'GROUP_MA'
                call getvtx('VOLUMOGRAMME', 'GROUP_MA', iocc=iocc, scal=grouma, nbret=iret)
            endif
!
!         - MAILLES FOURNIES PAR L'UTILISATEUR -
            call reliem(modele, mailla, 'NU_MAILLE', 'VOLUMOGRAMME', iocc,&
                        1, mocles, typmcl, mesmai, nbma)
!
            mesmae=mesmai
!         - MAILLES EVENTUELLEMENT FILTREES EN FONCTION DE LA DIMENSION
!           GEOMETRIQUE (2D OU 3D)
            call getvtx('VOLUMOGRAMME', 'TYPE_MAILLE', iocc=iocc, scal=infoma, nbret=iret)
            if (iret .ne. 0) then
                mesmae=mesmaf
                if (infoma .eq. '2D') then
                    iresma=2
                else if (infoma.eq.'3D') then
                    iresma=3
                else
                    ASSERT(.false.)
                endif
                call utflmd(mailla, mesmai, nbma, iresma, ' ',&
                            nbma, mesmaf)
            else
                infoma='-'
            endif
!
!      -- 4.4 CALCUL DES INTERVALLES ET DE LA DISTRIBUTION --
!
!        - ON RECUPERE LE NOMBRE D'INTERVALLES OU LE SEUIL ET LA NORME
!        - POUR CALCUL RELATIF OU ABSOLU
            call getvis('VOLUMOGRAMME', 'NB_INTERV', iocc=iocc, scal=nbint, nbret=iret)
            seuil = 0.d0
            lseuil = .false.
            call getvr8('VOLUMOGRAMME', 'SEUIL', iocc=iocc, scal=seuil, nbret=iret)
            if (iret .ne. 0) then
                nbint = 2
                lseuil = .true.
            endif
            call getvtx('VOLUMOGRAMME', 'NORME', iocc=iocc, scal=norme, nbret=iret)
            call getvr8('VOLUMOGRAMME', 'BORNES', iocc=iocc, nbval=2, vect=borne,&
                        nbret=iret)
            if (iret .ne. 0) then
                bfix=1
                ASSERT(borne(1).lt.borne(2))
            else
                borne(1)=0.d0
                borne(2)=0.d0
                bfix=0
            endif
!        - ON CREE UN TABLEAU POUR RECUEILLIR POUR CHAQUE INTERVALLE
!         LES 3 VALEURS SUIVANTES:
!             . LA BORNE INF,
!             . LA BORNE SUP,
!             . LE VALEUR DE REPARTITION DE LA COMPOSANTE
            call wkvect(borpct, 'V V R', 3*nbint, jbpct)
            call pebpct(modele, nbma, mesmae, cham, nomcmp,&
                        3*nbint, bfix, borne, norme, seuil,&
                        lseuil, zr(jbpct), voltot)
!
!      -- 4.5 ON REMPLIT LA TABLE --
!
            if (nompar(1) .eq. 'RESULTAT') then
                call wkvect(valk, 'V V K24', 5, jvalk)
                zk24(jvalk) =resuco
                zk24(jvalk+1)=nomcha
                zk24(jvalk+2)=ncpini
                zk24(jvalk+3)=grouma
                zk24(jvalk+4)=infoma
                call wkvect(valr, 'V V R', 4, jvalr)
                zr(jvalr)=inst
                ivalr=1
                call wkvect(vali, 'V V I', 2, jvali)
                zi(jvali)=numo
                ivali=1
            else
                call wkvect(valk, 'V V K24', 4, jvalk)
                zk24(jvalk) =nomcha
                zk24(jvalk+1)=ncpini
                zk24(jvalk+2)=grouma
                zk24(jvalk+3)=infoma
                call wkvect(valr, 'V V R', 3, jvalr)
                ivalr=0
                call wkvect(vali, 'V V I', 1, jvali)
                ivali=0
            endif
!
!        - POUR CHAQUE INTERVALLE, ON AJOUTE UNE LIGNE A LA TABLE :
            do ii = 1, nbint
                zr(jvalr+ivalr) =zr(jbpct+3*(ii-1))
                zr(jvalr+ivalr+1)=zr(jbpct+3*(ii-1)+1)
                zr(jvalr+ivalr+2)=zr(jbpct+3*(ii-1)+2)
                zi(jvali+ivali)=ii
                call tbajli(resu, nbpar, nompar, zi(jvali), zr(jvalr),&
                            [c16b], zk24(jvalk), 0)
            end do
!
!     IMPRESSION DU VOLUME TOTAL CONCERNE PAR LE CALCUL :
!      - SOIT LE VOLUME TOTAL DEFINI PAR L ENTITE TOPOLOGIQUE
!      - SOIT POUR CELUI DONT LES VALEURS SONT COMPRISES DANS LES
!        BORNES SI CELLES-CI SONT RENSEIGNEES PAR L UTILISATEUR
            ivol(1)=iocc
            ivol(2)=inum
            call utmess('I', 'UTILITAI7_14', ni=2, vali=ivol, sr=voltot)
!
!      -- 4.5 NETTOYAGE POUR L'OCCURRENCE SUIVANTE --
!CC          CALL DETRSD('CHAMP',CHAM2)
            call jedetr(valr)
            call jedetr(vali)
            call jedetr(valk)
            call jedetr(mesmaf)
            call jedetr(mesmai)
            call jedetr(borpct)
            call jedetr(mesmaf)
!
!     --- FIN DE LA BOUCLE SUR LES NUMEROS D'ORDRE:
!     ---------------------------------------------
        end do
!
!     --- FIN DE LA BOUCLE SUR LES OCCURRENCES DU MOT-CLE VOLUMOGRAMME
!     ----------------------------------------------------------------
 10     continue
    end do
!
    if (nr .eq. 0) then
        call detrsd('CHAMP', tmpcha)
    endif
!
!
    call jedema()
!
end subroutine
