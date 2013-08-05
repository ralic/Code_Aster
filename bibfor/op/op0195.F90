subroutine op0195()
    implicit  none
!     -----------------------------------------------------------------
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
! person_in_charge: jacques.pellet at edf.fr
!     COMMANDE CREA_CHAMP
!     -----------------------------------------------------------------
#include "jeveux.h"
#include "asterc/cheksd.h"
#include "asterc/getres.h"
#include "asterc/getvid.h"
#include "asterc/getvtx.h"
#include "asterfort/alchml.h"
#include "asterfort/assert.h"
#include "asterfort/caraff.h"
#include "asterfort/celver.h"
#include "asterfort/chcore.h"
#include "asterfort/chpass.h"
#include "asterfort/chpchd.h"
#include "asterfort/chpeva.h"
#include "asterfort/chprec.h"
#include "asterfort/chreco.h"
#include "asterfort/cnoaff.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnonor.h"
#include "asterfort/cnscno.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/imprsd.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nopar2.h"
#include "asterfort/titre.h"
#include "asterfort/u195tb.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/varaff.h"
#include "asterfort/x195cb.h"
    integer :: n1, ib, ifm, niv, iret, i11, i12, test, ibid
    character(len=3) :: prol0
    character(len=4) :: tychr, tych
    character(len=8) :: kbid, mo, ma, chou, nomgd, nomgd2
    character(len=8) :: tsca, nogd, nomgd1, nompar, ma2, ta
    character(len=16) :: tychr1, opera, optio2, typco, option
    character(len=19) :: ligrel, chatmp, celmod, prchn1, cns1, ch1, prchn2, chin
    character(len=8) :: nu1
!     -----------------------------------------------------------------
    logical :: dbg
    character(len=24) :: valk(4)
    integer :: iarg
!     ------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
    call infniv(ifm, niv)
!
    dbg=.true.
    dbg=.false.
!
!
! 1- CALCUL DE:
!      OPERA: OPERATION A EFFECTUER
!      MO: MODELE (OU ' ')
!      MA: MAILLAGE (OU ' ')
!      CHOU  : CHAMP RESULTAT
!      TYCHR : TYPE DU CHAMP RESULTAT (CART/NOEU/ELNO/ELGA/ELEM)
!      NOMGD : GRANDEUR ASSOCIEE A CHOU
!      PROL0 :/'OUI' POUR PROLONGER PAR ZERO LE CHAMP RESULTAT
!             (POUR LES CHAM_ELEM ET LES CHAM_NO POUR LESQUELS ON
!              VEUT IMPOSER LA NUMEROTATION DES DDLS).
!      OPTION: OPTION PERMETTANT D'ALLOUER UN CHAM_ELEM "MODELE"
!
!     ------------------------------------------------------------------
!
    call getvtx(' ', 'OPERATION', 0, iarg, 1,&
                opera, ib)
!
    call getvid(' ', 'MODELE', 0, iarg, 1,&
                mo, n1)
    if (n1 .eq. 0) mo = ' '
    call getvid(' ', 'MAILLAGE', 0, iarg, 1,&
                ma, n1)
    if (n1 .eq. 0) ma = ' '
    if (mo .ne. ' ') then
        call dismoi('F', 'NOM_MAILLA', mo, 'MODELE', ib,&
                    ma2, ib)
        if ((ma.ne.' ') .and. (ma.ne.ma2)) call u2mess('F', 'UTILITAI3_21')
        ma = ma2
    endif
!
    call getres(chou, typco, kbid)
    call exisd('CHAMP', chou, test)
    if (test .eq. 1) then
        if (.not.((opera.eq.'ASSE').or.(opera.eq.'COMB'))) call u2mess('F', 'UTILITAI3_43')
    endif
    call getvtx(' ', 'TYPE_CHAM', 0, iarg, 1,&
                tychr1, ib)
    tychr = tychr1(1:4)
    nomgd = tychr1(6:13)
    call dismoi('F', 'TYPE_SCA', nomgd, 'GRANDEUR', ib,&
                tsca, ib)
!
    prol0=' '
    call getvtx(' ', 'PROL_ZERO', 0, iarg, 1,&
                prol0, ib)
    if ((prol0.eq.'NON') .and. (tsca.eq.'R')) prol0='NAN'
!
    call getvtx(' ', 'OPTION', 0, iarg, 1,&
                option, n1)
    if (n1 .eq. 0) option = ' '
!
!
! 2.  CALCUL DE LIGREL,CELMOD  + QUELQUES VERIFICATIONS   :
!     -------------------------------------------------------------
    ligrel = ' '
    celmod = ' '
!
    if (tychr(1:2) .eq. 'EL') then
        if ((opera.eq.'AFFE') .or. (opera.eq.'ASSE') .or. ( opera.eq.'DISC')) then
            if (mo .eq. ' ') call u2mess('F', 'UTILITAI3_22')
            ligrel = mo//'.MODELE'
!
!         -- CALCUL D'UN CHAM_ELEM "MODELE" : CELMOD
!         ---------------------------------------------------
            if (option .eq. ' ') then
                optio2 = 'TOU_INI_'//tychr
!           -- SI OPERATION 'ASSE', IL Y A PEUT-ETRE UNE MEILLEURE
!              OPTION A CHOISIR PAR DEFAUT QUE TOU_INI_ELXX
                if (opera .eq. 'ASSE') then
                    call getvid('ASSE', 'CHAM_GD', 1, iarg, 1,&
                                chin, ib)
                    call dismoi('F', 'NOM_MAILLA', chin, 'CHAMP', ib,&
                                ma2, ib)
                    if (ma2 .ne. ma) then
                        valk(1) = chin
                        valk(2) = mo
                        valk(3) = ma2
                        valk(4) = ma
                        call u2mesk('F', 'CALCULEL4_59', 4, valk)
                    endif
!
                    call jeexin(chin//'.CELK', iret)
                    call dismoi('F', 'NOM_GD', chin, 'CHAMP', ib,&
                                nomgd2, ib)
                    if (iret .ne. 0 .and. nomgd .eq. nomgd2) then
                        call dismoi('F', 'NOM_OPTION', chin, 'CHAM_ELEM', ib,&
                                    optio2, ib)
                    endif
                endif
!
            else
                optio2 = option
            endif
            nompar = nopar2(optio2,nomgd,'INOUT')
            celmod = '&&OP0195.CELMOD'
            call alchml(ligrel, optio2, nompar, 'V', celmod,&
                        ib, ' ')
            if (ib .ne. 0) then
                valk(1)=ligrel
                valk(2)=nompar
                valk(3)=optio2
                call u2mesk('F', 'UTILITAI3_23', 3, valk)
            endif
!
!         VERIFICATION DU TYPE DE CELMOD : ELGA/ELNO/ELEM :
            call jeveuo(celmod//'.CELK', 'L', ib)
            if (zk24(ib-1+3) .ne. tychr) then
                valk(1) = optio2
                valk(2) = tychr
                call u2mesk('F', 'UTILITAI3_24', 2, valk)
            endif
        endif
    endif
!
!
!
! 3.  TRAITEMENT DU MOT CLE OPERATION :
!     -------------------------------------------------------------
!
!
    if (opera .eq. 'NORMALE') then
!     -----------------------------------------
        if (tychr .eq. 'NOEU') then
            if (nomgd .ne. 'GEOM_R') call u2mesk('F', 'UTILITAI3_25', 1, opera)
            call cnonor(mo, nomgd, 'G', chou)
!
        else
            valk(1) = opera
            valk(2) = tychr
            call u2mesk('F', 'UTILITAI3_26', 2, valk)
        endif
!
!
!
    else if (opera.eq.'AFFE') then
!     -----------------------------------------
        if (tychr .eq. 'NOEU') then
            call cnoaff(ma, nomgd, 'G', chou)
!
        else if (tychr.eq.'CART') then
            call caraff(ma, nomgd, 'G', chou)
!
        else if (tychr(1:2).eq.'EL') then
            chatmp = '&&OP0195.CHATMP'
            if (nomgd .eq. 'VARI_R') then
                call varaff(ma, nomgd, 'V', chatmp)
                call chpchd(chatmp, tychr, celmod, prol0, 'G',&
                            chou)
                call detrsd('CHAM_ELEM_S', chatmp)
            else
                call caraff(ma, nomgd, 'V', chatmp)
                call chpchd(chatmp, tychr, celmod, prol0, 'G',&
                            chou)
                call detrsd('CARTE', chatmp)
            endif
        endif
!
!
    else if (opera.eq.'ASSE') then
!     -----------------------------------------
        call chpass(tychr, ma, celmod, nomgd, prol0,&
                    chou)
!
!
    else if (opera.eq.'COMB') then
!     -----------------------------------------
        call x195cb(tychr, nomgd, chou)
!
!
    else if (opera.eq.'EVAL') then
!     -----------------------------------------
        call chpeva(chou)
!
!
    else if (opera(1:3).eq.'R2C') then
!     -----------------------------------------
        call chcore(chou)
!
!
    else if (opera(1:3).eq.'C2R') then
!     -----------------------------------------
        call chreco(chou)
!
!
    else if (opera.eq.'DISC') then
!     -----------------------------------------
        call getvid(' ', 'CHAM_GD', 0, iarg, 1,&
                    chin, ib)
        call dismoi('F', 'NOM_GD', chin, 'CHAMP', ib,&
                    nomgd2, ib)
        if (nomgd .ne. nomgd2) then
!          -- EXCEPTION : NOMGD='VARI_R' ET NOMGD2='VAR2_R'
            if (.not. (nomgd.eq.'VARI_R'.and.nomgd2.eq.'VAR2_R')) then
                valk(1) = chin
                valk(2) = tychr1
                call u2mesk('F', 'UTILITAI3_27', 2, valk)
            endif
        endif
        call chpchd(chin, tychr, celmod, prol0, 'G',&
                    chou)
!
!
    else if (opera.eq.'EXTR') then
!     -----------------------------------------
        call getvid(' ', 'TABLE', 0, iarg, 1,&
                    ta, n1)
        if (n1 .eq. 0) then
            call chprec(chou)
!
        else
            call u195tb(chou)
        endif
    endif
!
!
! 4.  SI ON A CREE UN CHAM_NO, ON PEUT IMPOSER SA NUMEROTATION :
! --------------------------------------------------------------
    if (tychr .eq. 'NOEU') then
        call getvid(' ', 'CHAM_NO', 0, iarg, 1,&
                    ch1, i11)
        call getvid(' ', 'NUME_DDL', 0, iarg, 1,&
                    nu1, i12)
        if ((i11+i12) .gt. 0) then
            call dismoi('F', 'NOM_GD', chou, 'CHAMP', ib,&
                        nogd, ib)
            prchn1 = ' '
            if (i11 .gt. 0) then
                call dismoi('F', 'PROF_CHNO', ch1, 'CHAM_NO', ib,&
                            prchn1, ib)
                call dismoi('F', 'NOM_GD', ch1, 'CHAM_NO', ib,&
                            nomgd1, ib)
            endif
            if (i12 .gt. 0) then
                call dismoi('F', 'PROF_CHNO', nu1, 'NUME_DDL', ib,&
                            prchn1, ib)
                call dismoi('F', 'NOM_GD', nu1, 'NUME_DDL', ib,&
                            nomgd1, ib)
            endif
!
            if (nomgd1 .ne. nogd) then
!           -- ON ACCEPTE LES COUPLES XXXX_R / XXXX_C :
                if (nomgd1(5:7) .eq. '_R ' .or. nomgd1(5:7) .eq. '_C ') then
                    if (nogd(5:7) .eq. '_R ' .or. nogd(5:7) .eq. '_C ') then
                        if (nogd(1:4) .eq. nomgd1(1:4)) goto 1
                    endif
                endif
                valk (1) = nomgd1
                valk (2) = nogd
                call u2mesk('F', 'UTILITAI6_5', 2, valk)
            endif
 1          continue
!
            call dismoi('F', 'PROF_CHNO', chou, 'CHAM_NO', ib,&
                        prchn2, ib)
            if (prchn1 .ne. prchn2) then
                cns1 = '&&OP0195.CNS1'
                call cnocns(chou, 'V', cns1)
                if (prchn2(1:8) .eq. chou(1:8)) call detrsd('PROF_CHNO', prchn2)
                prol0='NON'
                call getvtx(' ', 'PROL_ZERO', 0, iarg, 1,&
                            prol0, ibid)
                call cnscno(cns1, prchn1, prol0, 'G', chou,&
                            'F', ibid)
                call detrsd('CHAM_NO_S', cns1)
            endif
        endif
    endif
!
!
! 5.  AJOUT DU TITRE :
! -----------------------------------------------------
    call titre()
!
!
! 6.  SI INFO:2    ON IMPRIME LE CHAMP RESULTAT :
! ----------------------------------------------
    if (niv .eq. 2) call imprsd('CHAMP', chou, ifm, 'CHAMP RESULTAT DE LA COMMANDE CREA_CHAMP :')
!
!
! 7.  VERIFICATION PROL_ZERO='NON' POUR LES CHAM_ELEM :
! ------------------------------------------------------
    if ((tychr(1:2).eq.'EL') .and. (prol0.eq.'NAN')) then
        call celver(chou, 'PAS_NAN', 'STOP', iret)
    endif
    if (dbg .and. tychr(1:2) .eq. 'EL') then
        call cheksd(chou, 'SD_CHAM_ELEM', iret)
        ASSERT(iret.eq.0)
    endif
!
!
! 8.  VERIFICATION DE LA COHERENCE DU MAILLAGE SOUS-JACENT :
! ---------------------------------------------------------
    if (ma .ne. ' ') then
        call dismoi('F', 'NOM_MAILLA', chou, 'CHAMP', ib,&
                    ma2, ib)
        valk(1)=ma2
        valk(2)=ma
        if (ma .ne. ma2) call u2mesk('F', 'CALCULEL4_78', 2, valk)
    endif
!
!
! 9.  VERIFICATION DE LA COHERENCE DU CHAMP CREE AVEC TYPE_CHAM :
! ---------------------------------------------------------------
    call dismoi('F', 'TYPE_CHAMP', chou, 'CHAMP', ib,&
                tych, ib)
    if (tych .ne. tychr) then
        valk(1)=tychr
        valk(2)=tych
        call u2mesk('F', 'CALCULEL4_70', 2, valk)
    endif
!
    call dismoi('F', 'NOM_GD', chou, 'CHAMP', ib,&
                nogd, ib)
    if (nogd .ne. nomgd) then
        valk(1)=nomgd
        valk(2)=nogd
        call u2mesk('F', 'CALCULEL4_71', 2, valk)
    endif
!
!
    call jedema()
!
end subroutine
