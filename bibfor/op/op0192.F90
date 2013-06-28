subroutine op0192()
!_____________________________________________________________________
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!                   LIRE_CHAMP
!_____________________________________________________________________
!
! person_in_charge: nicolas.sellenet at edf.fr
    implicit none
!
! 0.1. ==> ARGUMENTS
!
!
! 0.2. ==> COMMUNS
!
! 0.3. ==> VARIABLES LOCALES
!
#   include "jeveux.h"
#   include "asterc/getres.h"
#   include "asterc/getvid.h"
#   include "asterc/getvis.h"
#   include "asterc/getvr8.h"
#   include "asterc/getvtx.h"
#   include "asterfort/codent.h"
#   include "asterfort/copisd.h"
#   include "asterfort/detrsd.h"
#   include "asterfort/dismoi.h"
#   include "asterfort/infmaj.h"
#   include "asterfort/jedema.h"
#   include "asterfort/jemarq.h"
#   include "asterfort/lrchme.h"
#   include "asterfort/lrvema.h"
#   include "asterfort/lrvemo.h"
#   include "asterfort/as_mficlo.h"
#   include "asterfort/as_mfiope.h"
#   include "asterfort/as_mfinvr.h"
#   include "asterfort/u2mesk.h"
#   include "asterfort/u2mess.h"
#   include "asterfort/ulisog.h"
#   include "asterfort/wkvect.h"
    character(len=6) :: nompro
    parameter ( nompro = 'OP0192' )
!
    integer :: ednono
    parameter (ednono=-1)
    integer :: ednopt
    parameter (ednopt=-1)
!
    character(len=7) :: lcmpva
    parameter ( lcmpva = 'NOM_CMP' )
    character(len=11) :: lcmpvm
    parameter ( lcmpvm = 'NOM_CMP_MED' )
!
    integer :: iaux, jaux, iret
    integer :: iinst, idfimd
    integer :: unite, imaj, imin, irel
    integer :: codret, iver, typent
    integer :: numpt, numord
    integer :: nbcmpv, jcmpva, jcmpvm
    integer :: nbma, jnbpgm, jnbpmm
    integer :: ednoeu
    parameter (ednoeu=3)
    integer :: edmail
    parameter (edmail=0)
    integer :: ednoma
    parameter (ednoma=4)
    integer :: edlect
    parameter (edlect=0)
!
    real(kind=8) :: inst
    real(kind=8) :: prec
!
    character(len=1) :: saux01
    character(len=8) :: chanom, nomaas, nomo, nomgd
    character(len=19) :: chatmp
    character(len=8) :: typech, param
    character(len=8) :: crit, saux08, k8b
    character(len=3) :: prolz
    character(len=16) :: nomcmd, format, tych
    character(len=24) :: option
    character(len=64) :: nochmd, nomamd
    character(len=72) :: rep
    character(len=200) :: nofimd
    character(len=255) :: kfic
!
    character(len=24) :: ncmpva, ncmpvm
    character(len=24) :: valk(2)
    integer :: iarg
!
! DEB ------------------------------------------------------------------
!====
! 1. PREALABLES
!====
!
    call jemarq()
!
! 1.1. ==> RECUPERATION DU NIVEAU D'IMPRESSION
!
    call infmaj()
!
!====
! 2. DETERMINATION DES OPTIONS DE LA COMMANDE :
!====
!
! 2.1. ==> FORMAT DU FICHIER
!
    call getvtx(' ', 'FORMAT', 0, iarg, 1,&
                format, iaux)
!
    if (format .eq. 'MED') then
        call getvtx(' ', 'NOM_MED', 0, iarg, 1,&
                    nochmd, iaux)
        if (iaux .eq. 0) then
            call u2mess('F', 'MED_96')
        endif
    else
        call u2mesk('F', 'UTILITAI3_17', 1, format)
    endif
!
! 2.2. ==> TYPE DE CHAMP A LIRE
!
    call getvtx(' ', 'TYPE_CHAM', 0, iarg, 1,&
                tych, iaux)
    call getres(chanom, typech, nomcmd)
    nomgd = tych(6:13)
    if (tych(1:11) .eq. 'ELGA_SIEF_R') then
        option = 'RAPH_MECA'
        param = 'PCONTPR'
    else if (tych(1:11).eq.'ELGA_EPSI_R') then
        option = 'EPSI_ELGA'
        param = 'PDEFOPG'
    else if (tych(1:11).eq.'ELGA_VARI_R') then
        option = 'RAPH_MECA'
        param = 'PVARIPR'
!       ELSE IF (TYCH(1:9).EQ.'ELGA_EQUI_R') THEN
!          OPTION = 'SIEQ_ELGA'
!          PARAM  = 'PCONTEQ'
    else if (tych(1:11).eq.'ELGA_SOUR_R') then
        option = 'SOUR_ELGA'
        param = 'PSOUR_R'
    else if (tych(1:4).eq.'ELGA') then
!        AUTRES CHAMPS ELGA : NON PREVU
        call u2mesk('F', 'UTILITAI2_94', 1, tych)
    else
!        CHAMPS ELNO OU AUTRES :
        option=' '
        param=' '
    endif
!
! - -  VERIFICATIONS - -
!
    if (tych(1:2) .eq. 'EL') then
        call getvid(' ', 'MODELE', 0, iarg, 1,&
                    nomo, iaux)
        call lrvemo(nomo)
    endif
!
! 2.3. ==> NOM DES COMPOSANTES VOULUES
!
    ncmpva = '&&'//nompro//'.'//lcmpva
    ncmpvm = '&&'//nompro//'.'//lcmpvm
!
    call getvtx(' ', 'NOM_CMP_IDEM', 0, iarg, 1,&
                rep, iaux)
!
! 2.3.1. ==> C'EST PAR IDENTITE DE NOMS
!
    if (iaux .ne. 0) then
!
        if (rep .eq. 'OUI') then
            nbcmpv = 0
        else
            call u2mesk('F', 'UTILITAI3_18', 1, rep)
        endif
!
    else
!
! 2.3.2. ==> C'EST PAR ASSOCIATION DE LISTE
!
        call getvtx(' ', lcmpva, 0, iarg, 0,&
                    rep, iaux)
        if (iaux .lt. 0) then
            nbcmpv = -iaux
        endif
!
        call getvtx(' ', lcmpvm, 0, iarg, 0,&
                    rep, iaux)
        if (-iaux .ne. nbcmpv) then
            valk(1) = lcmpva
            valk(2) = lcmpvm
            call u2mesk('F', 'UTILITAI2_95', 2, valk)
        endif
!
        if (nbcmpv .gt. 0) then
            call wkvect(ncmpva, 'V V K8', nbcmpv, jcmpva)
            call getvtx(' ', lcmpva, 0, iarg, nbcmpv,&
                        zk8(jcmpva), iaux)
            call wkvect(ncmpvm, 'V V K16', nbcmpv, jcmpvm)
            call getvtx(' ', lcmpvm, 0, iarg, nbcmpv,&
                        zk16(jcmpvm), iaux)
        endif
!
    endif
!
! 2.4a ==> PROLONGEMENT PAR ZERO OU NOT A NUMBER
!
    call getvtx(' ', 'PROL_ZERO', 0, iarg, 1,&
                prolz, iaux)
    if (prolz .ne. 'OUI') then
        prolz = 'NAN'
    endif
!
! 2.4b ==> UNITE LOGIQUE LIE AU FICHIER
!
    call getvis(' ', 'UNITE', 0, iarg, 1,&
                unite, iaux)
!
! 2.5. ==> NOM DU MODELE, NOM DU MAILLAGE ASTER ASSOCIE
!
    call getvid(' ', 'MODELE', 0, iarg, 1,&
                nomo, iaux)
!
    call getvid(' ', 'MAILLAGE', 0, iarg, 1,&
                nomaas, iaux)
    if (iaux .eq. 0) then
        call dismoi('F', 'NOM_MAILLA', nomo, 'MODELE', iaux,&
                    nomaas, codret)
        if (codret .ne. 0) then
            call u2mess('F', 'UTILITAI3_19')
        endif
    endif
!
! 2.6. ==> NOM DU MAILLAGE MED ASSOCIE
!
    call getvtx(' ', 'NOM_MAIL_MED', 0, iarg, 1,&
                nomamd, iaux)
!
    if (iaux .eq. 0) then
        nomamd = ' '
    endif
!
! 2.7. CARACTERISTIQUES TEMPORELLES
! 2.7.1. ==> NUMERO D'ORDRE EVENTUEL
!
    call getvis(' ', 'NUME_ORDRE', 0, iarg, 1,&
                numord, iaux)
    if (iaux .eq. 0) then
        numord = ednono
    endif
!
! 2.7.2. ==> NUMERO DE PAS DE TEMPS EVENTUEL
!
    call getvis(' ', 'NUME_PT', 0, iarg, 1,&
                numpt, jaux)
    if (jaux .eq. 0) then
        numpt = ednopt
    endif
!
! 2.7.3. ==> SI NI NUMERO D'ORDRE, NI NUMERO DE PAS DE TEMPS, IL Y A
!            PEUT-ETRE UNE VALEUR D'INSTANT
!
    if (iaux .eq. 0 .and. jaux .eq. 0) then
!
        call getvr8(' ', 'INST', 1, iarg, 1,&
                    inst, iinst)
!
        if (iinst .ne. 0) then
            call getvr8(' ', 'PRECISION', 1, iarg, 1,&
                        prec, iaux)
            call getvtx(' ', 'CRITERE', 1, iarg, 1,&
                        crit, iaux)
        endif
!
    else
        iinst = 0
    endif
!
!====
! 3. APPEL DE LA LECTURE AU FORMAT MED
!====
!
    if (tych(1:4) .eq. 'ELGA') then
        call dismoi('F', 'NB_MA_MAILLA', nomaas, 'MAILLAGE', nbma,&
                    k8b, iret)
        call wkvect('&&OP0150_NBPG_MAILLE', 'V V I', nbma, jnbpgm)
        call wkvect('&&OP0150_NBPG_MED', 'V V I', nbma, jnbpmm)
    else
        jnbpgm=1
        jnbpmm=1
    endif
!
    if (format .eq. 'MED') then
!
        chatmp = '&&OP0192.TEMPOR'
        if (tych(1:4) .eq. 'NOEU') then
            typent=ednoeu
        else if (tych(1:4).eq.'ELNO') then
!
!          DETERMINATION DU TYPE D'ENTITE CAR SELON LA VERSION MED,
!               TYPENT =4 ('MED_NOEUD_MAILLE') OU
!                      =0 ('MED_MAILLE' POUR LES VERSIONS ANTERIEURES)
!          NOM DU FICHIER MED
            call ulisog(unite, kfic, saux01)
            if (kfic(1:1) .eq. ' ') then
                call codent(unite, 'G', saux08)
                nofimd = 'fort.'//saux08
            else
                nofimd = kfic(1:200)
            endif
            call as_mfiope(idfimd, nofimd, edlect, iret)
            call as_mfinvr(idfimd, imaj, imin, irel, iret)
            call as_mficlo(idfimd, iret)
!          ON VERIFIE LA VERSION DU FICHIER A LA VERSION 2.3.3
            typent=ednoma
            iver= imaj*100 + imin*10 + irel
            if (iver .lt. 233) then
                typent=edmail
                call u2mesk('A', 'MED_53', 1, nochmd)
            else
                typent=ednoma
            endif
        else
            typent=edmail
        endif
!
        call lrvema(nomaas, unite, nochmd)
!
        call lrchme(chatmp, nochmd, nomamd, nomaas, tych(1:8),&
                    nomgd, typent, nbcmpv, ncmpva, ncmpvm,&
                    prolz, iinst, numpt, numord, inst,&
                    crit, prec, unite, option, param,&
                    zi(jnbpgm), zi( jnbpmm), codret)
!
        call copisd('CHAMP_GD', 'G', chatmp, chanom)
        if (tych(1:2) .eq. 'NO') then
            call detrsd('CHAM_NO', chatmp)
        else
            call detrsd('CHAM_ELEM', chatmp)
        endif
!
    endif
!
!====
! 4. LA FIN
!====
!
    call jedema()
! FIN ------------------------------------------------------------------
end subroutine
