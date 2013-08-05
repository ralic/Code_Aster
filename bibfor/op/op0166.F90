subroutine op0166()
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
! person_in_charge: jacques.pellet at edf.fr
!
!     COMMANDE:  PROJ_CHAMP
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
!
! 0.2. ==> COMMUNS
! ----------------------------------------------------------------------
!
!
!
! 0.3. ==> VARIABLES LOCALES
!
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterc/getvid.h"
#include "asterc/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/pjelga.h"
#include "asterfort/pjspma.h"
#include "asterfort/pjtyco.h"
#include "asterfort/pjxxch.h"
#include "asterfort/pjxxco.h"
#include "asterfort/pjxxpr.h"
#include "asterfort/titre.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
    integer :: iret
    integer :: ie, ibid, n1, n2, n3
    integer :: jpjk1
    logical :: isole
    logical :: lnoeu, lelno, lelem, lelga
    character(len=4) :: tychv, typcal
    character(len=8) :: k8b, noma1, noma2, noma3, resuin, prol0, projon, norein
    character(len=8) :: nomo1, nomo2, moa1, moa2, cnref, nomare, noca
    character(len=16) :: typres, nomcmd, lcorre(2), corru
    character(len=19) :: resuou, cham1, method, rtyp
    character(len=19) :: ligre1, ligre2
    character(len=24) :: valk(4)
    integer :: iarg, nbocc
!
!
!
! DEB ------------------------------------------------------------------
    call jemarq()
    call infmaj()
    call titre()
    lelga = .false.
!
!
!     0- CALCUL DE TYPCAL =
!     ------------------------------------------------------------
!       / '1ET2' : ON CALCULE LA SD_CORRESP_2_MAILLA ET ON PROJETTE
!                  LES CHAMPS
!       / '1'    : ON CALCULE LA SD_CORRESP_2_MAILLA ET ON S'ARRETE
!       / '2'    : ON UTILISE LA SD_CORRESP_2_MAILLA DEJA CALCULEE
    call getvtx(' ', 'PROJECTION', 1, iarg, 1,&
                projon, n1)
    call getvid(' ', 'MATR_PROJECTION', 1, iarg, 1,&
                corru, n2)
    if (n2 .eq. 0) corru=' '
    if (projon .eq. 'NON') then
        typcal='1'
        ASSERT(n2.eq.0)
    else
        ASSERT(projon.eq.'OUI')
        if (corru .ne. ' ') then
            typcal='2'
        else
            typcal='1ET2'
        endif
    endif
!
!
!
!     1- CALCUL DE RESUOU, TYPRES, METHOD, ISOLE, CHAM1, RESUIN :
!     ------------------------------------------------------------
!        RESUOU : NOM DU CONCEPT RESULTAT
!        TYPRES : TYPE DU RESULTAT (CHAM_GD OU SD_RESULTAT)
!        METHOD : METHODE CHOISIE POUR LA PROJECTION
!        ISOLE  : .TRUE.  : ON NE PROJETTE QU'UN CHAMP ISOLE
!                 .FALSE. : ON PROJETTE UNE SD_RESULTAT
!        CHAM1  : NOM DU CHAMP A PROJETER (SI ISOLE)
!        RESUIN : NOM DE LA SD_RESULTAT A PROJETER (SI .NOT.ISOLE)
    call getres(resuou, typres, nomcmd)
    call getvtx(' ', 'METHODE', 1, iarg, 1,&
                method, n1)
    if (n1 .eq. 0) method=' '
    if (projon .eq. 'OUI') then
        call getvid(' ', 'RESULTAT', 1, iarg, 1,&
                    resuin, n2)
        if (n2 .eq. 1) then
            isole=.false.
            cham1=' '
            call dismoi('F', 'NOM_MAILLA', resuin, 'RESULTAT', ibid,&
                        nomare, ibid)
            norein = resuin
        else
            isole=.true.
            call getvid(' ', 'CHAM_GD', 1, iarg, 1,&
                        cham1, n3)
            norein = cham1(1:8)
            call dismoi('F', 'NOM_MAILLA', cham1, 'CHAMP', ibid,&
                        nomare, ibid)
            ASSERT(n3.eq.1)
            resuin=' '
        endif
    else
        cham1=' '
        resuin=' '
        nomare=' '
    endif
!
!
!     LIMITATION DE LA METHODE ECLA_PG :
!      IL N'EST PAS POSSIBLE DE PROJETER UNE SD_RESULTAT
!
    if ((method.eq.'ECLA_PG') .and. (.not.isole)) then
        call u2mess('F', 'CALCULEL5_9')
    endif
!
!
!
!
!     2- CALCUL DE NOMA1, NOMA2, MOA1, MOA2, CNREF, NOCA:
!     ---------------------------------------------
!        NOMA1 : NOM DU MAILLAGE "1"
!        NOMA2 : NOM DU MAILLAGE "2"
!        NOMO1 : NOM DU MODELE "1"  (OU ' ')
!        NOMO2 : NOM DU MODELE "2"  (OU ' ')
!        MOA1  : NOMO1 SI NON ' '. SINON : NOMA1
!        MOA2  : NOMO2 SI NON ' '. SINON : NOMA2
!        CNREF : NOM DU CHAM_NO "MODELE" "2" (SI NUAGE_DEG_0/1)
    if (typcal .eq. '1' .or. typcal .eq. '1ET2') then
        call getvid(' ', 'MODELE_1', 1, iarg, 1,&
                    nomo1, n1)
        if (n1 .eq. 1) then
            call dismoi('F', 'NOM_MAILLA', nomo1, 'MODELE', ibid,&
                        noma1, ie)
            moa1=nomo1
        else
            nomo1=' '
            call getvid(' ', 'MAILLAGE_1', 1, iarg, 1,&
                        noma1, n2)
            ASSERT(n2.eq.1)
            moa1=noma1
        endif
!
        call getvid(' ', 'MODELE_2', 1, iarg, 1,&
                    nomo2, n1)
        if (n1 .eq. 1) then
            call dismoi('F', 'NOM_MAILLA', nomo2, 'MODELE', ibid,&
                        noma2, ie)
            moa2=nomo2
        else
            nomo2=' '
            call getvid(' ', 'MAILLAGE_2', 1, iarg, 1,&
                        noma2, n2)
            ASSERT(n2.eq.1)
            moa2=noma2
        endif
!
!
!     VERIFICATION DE LA COHERENCE ENTRE LES MAILLAGES ASSOCIES
!      1. AU RESULTAT (OU CHAM) A PROJETER
!      2. AU MODELE (OU MAILLAGE) FOURNI EN ENTREE
!
        if ((noma1.ne.nomare) .and. (projon.eq.'OUI')) then
            valk(1) = moa1
            valk(2) = norein
            valk(3) = noma1
            valk(4) = nomare
            call u2mesk('F', 'CALCULEL4_59', 4, valk)
        endif
!
        call getvid(' ', 'CHAM_NO_REFE', 1, iarg, 1,&
                    cnref, n1)
        if (n1 .eq. 1) then
            call dismoi('F', 'NOM_MAILLA', cnref, 'CHAMP', ibid,&
                        noma3, ie)
            if (noma3 .ne. noma2) then
                valk(1)=cnref
                valk(2)=noma3
                valk(3)=noma2
                call u2mesk('F', 'CALCULEL2_6', 3, valk)
            endif
        else
            cnref=' '
        endif
    endif
!
    if (method .eq. 'SOUS_POINT') then
!       RECUPERATION DU CARA_ELEM
        call getvid(' ', 'CARA_ELEM', 1, iarg, 1,&
                    noca, n1)
        ASSERT(n1.ne.0)
!       LE MOT-CLE 'MODELE_2' EST OBLIGATOIRE
        call getvid(' ', 'MODELE_2', 1, iarg, 1,&
                    nomo2, n1)
        if (n1 .eq. 0) then
            call u2mess('F', 'CALCULEL5_40')
        endif
!       VIS_A_VIS EST INTERDIT AVEC SOUS_POINT
        call getfac('VIS_A_VIS', nbocc)
        if (nbocc .ne. 0) call u2mess('F', 'CALCULEL5_31')
        if (.not. isole) then
            call dismoi('F', 'TYPE_RESU', resuin, 'RESULTAT', ibid,&
                        rtyp, ie)
            if (rtyp .ne. 'EVOL_THER') call u2mess('F', 'CALCULEL5_30')
        endif
    else
        noca = ' '
    endif
!
!
!     3- CALCUL DE LA SD_LCORRESP_2_MAILLA :
!     ------------------------------------
!     LA SD_LCORRESP_2_MAILLA EST CONSTITUEE D'UNE LISTE DE DEUX SD :
!        LA 1RE EST UNE SD_CORRESP_2_MAILLA
!        LA 2DE EST UNE SD_CORRESP_2_MAILLA PARTICULIERE
!          ELLE EST UTILISEE POUR LA PROJECTION DE CHAM_ELEM (ELGA)
!          ELLE COMPORTE PJEF_EL (TABLEAU AUXILIAIRE)
!
    if (typcal .eq. '1' .or. typcal .eq. '1ET2') then
        lcorre(1)='&&OP0166.CORRES'
        lcorre(2)='&&OP0166.CORRE2'
        call pjxxco(typcal, method, lcorre, isole, resuin,&
                    cham1, moa1, moa2, noma1, noma2,&
                    cnref, noca)
    endif
!
!     3.1 : SI TYPCAL='1', IL FAUT S'ARRETER LA :
    if (typcal .eq. '1') goto 9999
!
!
!
!     4- PROJECTION DES CHAMPS :
!     ------------------------------------
!
!
!     4.1 : SI TYPCAL='2', IL FAUT SURCHARGER LCORR(1) ET
!           EVENTUELLEMENT RECUPERER MODELE_2
    if (typcal .eq. '2') then
        lcorre(1)=corru
        lcorre(2)=' '
        call jeveuo(corru//'.PJXX_K1', 'L', jpjk1)
!        -- LES MOA1 ET MOA2 STOCKES SONT LES MAILLAGES
        moa1=zk24(jpjk1-1+1)(1:8)
        moa2=zk24(jpjk1-1+2)(1:8)
        if (moa1 .ne. nomare) then
            valk(1) = moa1
            valk(2) = norein
            valk(3) = moa1
            valk(4) = nomare
            call u2mesk('F', 'CALCULEL4_59', 4, valk)
        endif
!        -- POUR POUVOIR PROJETER LES CHAM_ELEM, IL FAUT MODELE_2
        call getvid(' ', 'MODELE_2', 1, iarg, 1,&
                    nomo2, n1)
        if (n1 .eq. 1) then
            call dismoi('F', 'NOM_MAILLA', nomo2, 'MODELE', ibid,&
                        noma2, ie)
            if (moa2 .ne. noma2) then
                valk(1) = moa2
                valk(2) = noma2
                call u2mesk('F', 'CALCULEL4_72', 2, valk)
            endif
            moa2=nomo2
        endif
    endif
!
!       1- CAS CHAMP ISOLE :
!       =====================
    if (isole) then
!
        if (method(1:10) .eq. 'NUAGE_DEG_') then
!
!       ---- METHODE 'NUAGE_DEG' :
!       ----   ON NE PEUT PROJETER QUE DES CHAM_NO
            tychv = ' '
            call pjxxch(lcorre(1), cham1, resuou, tychv, ' ',&
                        'NON', ' ', 'G', iret)
            ASSERT(iret.eq.0)
!
        else
!
!       ---- AUTRE METHODE :
!       ---- ON PEUT PROJETER DES CHAM_NO OU DES CHAM_ELEM
!       ---- ON INTERDIT LE MOT-CLE 'TYPE_CHAM' POUR UN CHAMP ISOLE
!
            call getvtx(' ', 'TYPE_CHAM', 1, iarg, 0,&
                        tychv, n1)
            if (n1 .ne. 0) then
                call getvtx(' ', 'TYPE_CHAM', 1, iarg, 1,&
                            tychv, n1)
            else
                tychv=' '
            endif
            if (tychv .eq. 'NOEU') then
                call u2mess('F', 'CALCULEL5_36')
            endif
!
!       ---- ON DETERMINE LE TYPE DE CHAMP A PROJETER
!
            call pjtyco(isole, k8b, cham1, lnoeu, lelno,&
                        lelem, lelga)
!
            if (lnoeu) then
!       ------ CAS OU IL Y A UN CHAM_NO
                if (method .eq. 'ECLA_PG') then
                    valk(1) = method
                    call u2mesk('F', 'CALCULEL5_32', 1, valk)
                endif
                if (method .eq. 'SOUS_POINT') then
                    ligre2 = nomo2//'.MODELE'
                    prol0='NON'
                    call pjspma(lcorre(1), cham1, resuou, prol0, ligre2,&
                                noca, 'G', iret)
                else
                    tychv = ' '
                    call pjxxch(lcorre(1), cham1, resuou, tychv, ' ',&
                                'NON', ' ', 'G', iret)
                    ASSERT(iret.eq.0)
                endif
            else if (lelno) then
!       ------ CAS OU IL Y A UN CHAM_ELEM (ELNO)
                if (method .eq. 'ECLA_PG') then
                    valk(1) = method
                    valk(2) = 'ELNO'
                    call u2mesk('F', 'CALCULEL5_33', 2, valk)
                endif
!       ------   LE MOT-CLE 'MODELE_2' EST OBLIGATOIRE
                call getvtx(' ', 'PROL_ZERO', 1, iarg, 1,&
                            prol0, ie)
                call getvid(' ', 'MODELE_2', 1, iarg, 1,&
                            nomo2, n1)
                if (n1 .eq. 0) then
                    call u2mess('F', 'CALCULEL5_37')
                endif
!
                ligre2 = nomo2//'.MODELE'
                if (method .eq. 'SOUS_POINT') then
                    call pjspma(lcorre(1), cham1, resuou, prol0, ligre2,&
                                noca, 'G', iret)
                else
                    tychv = ' '
                    call pjxxch(lcorre(1), cham1, resuou, tychv, ' ',&
                                prol0, ligre2, 'G', iret)
                    ASSERT(iret.eq.0)
                endif
            else if (lelem) then
!       ------ CAS OU IL Y A UN CHAM_ELEM (ELEM)
                if ((method.eq.'ECLA_PG') .or. (method.eq.'SOUS_POINT')) then
                    valk(1) = method
                    valk(2) = 'ELEM'
                    call u2mesk('F', 'CALCULEL5_33', 2, valk)
                endif
!       ------   LE MOT-CLE 'MODELE_2' EST OBLIGATOIRE
                call getvtx(' ', 'PROL_ZERO', 1, iarg, 1,&
                            prol0, ie)
                call getvid(' ', 'MODELE_2', 1, iarg, 1,&
                            nomo2, n1)
                if (n1 .eq. 0) then
                    call u2mess('F', 'CALCULEL5_37')
                endif
                ligre2 = nomo2//'.MODELE'
                tychv = ' '
                call pjxxch(lcorre(1), cham1, resuou, tychv, ' ',&
                            prol0, ligre2, 'G', iret)
                ASSERT(iret.eq.0)
!
            else if (lelga) then
!       ------ CAS OU IL Y A UN CHAM_ELEM (ELGA)
                if ((method.eq.'COLLOCATION') .or. ( method.eq.'SOUS_POINT')) then
                    valk(1) = method
                    valk(2) = 'ELGA'
                    call u2mesk('F', 'CALCULEL5_33', 2, valk)
                endif
!       ------  LES MOTS-CLES 'MODELE_1' ET 'MODELE_2' SONT OBLIGATOIRES
                call getvtx(' ', 'PROL_ZERO', 1, iarg, 1,&
                            prol0, ie)
                call getvid(' ', 'MODELE_1', 1, iarg, 1,&
                            nomo1, n1)
                if (n1 .eq. 0) then
                    call u2mess('F', 'CALCULEL5_35')
                endif
                call getvid(' ', 'MODELE_2', 1, iarg, 1,&
                            nomo2, n1)
                if (n1 .eq. 0) then
                    call u2mess('F', 'CALCULEL5_37')
                endif
                ligre1 = nomo1//'.MODELE'
                ligre2 = nomo2//'.MODELE'
!
                call pjelga(nomo2, cham1, ligre1, prol0, lcorre(2),&
                            resuou, ligre2, iret)
                ASSERT(iret.eq.0)
!
            endif
!
        endif
!
!
!       2- CAS SD_RESULTAT :
!       =====================
    else
        call pjxxpr(resuin, resuou(1:8), moa1, moa2, lcorre(1),&
                    'G', noca, method)
    endif
!
9999  continue
!
    if (typcal .ne. '2') then
        call detrsd('CORRESP_2_MAILLA', lcorre(1))
    endif
!
    if (lelga) then
        call detrsd('CORRESP_2_MAILLA', lcorre(2))
        call detrsd('MAILLAGE', '&&PJELC2')
    endif
!
    if (method .eq. 'SOUS_POINT') then
        call detrsd('MAILLAGE', '&&PJSPCO')
    endif
!
    call jedema()
end subroutine
