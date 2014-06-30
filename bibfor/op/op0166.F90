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
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
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
#include "asterfort/utmess.h"
    integer :: iret
    integer :: ie, n1, n2, n3
    logical(kind=1) :: isole
    logical(kind=1) :: lnoeu, lelno, lelem, lelga
    character(len=4) :: tychv, typcal
    character(len=8) :: k8b, noma1, noma2, noma3, resuin, prol0, projon, norein
    character(len=8) :: nomo1, nomo2, moa1, moa2, cnref, nomare, noca
    character(len=16) :: typres, nomcmd, lcorre(2), corru
    character(len=19) :: resuou, cham1, method, rtyp
    character(len=19) :: ligre1, ligre2
    character(len=24) :: valk(4)
    integer :: nbocc
    character(len=24), pointer :: pjxx_k1(:) => null()
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
    call getvtx(' ', 'PROJECTION', scal=projon, nbret=n1)
    call getvid(' ', 'MATR_PROJECTION', scal=corru, nbret=n2)
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
    call getvtx(' ', 'METHODE', scal=method, nbret=n1)
    if (n1 .eq. 0) method=' '
    if (projon .eq. 'OUI') then
        call getvid(' ', 'RESULTAT', scal=resuin, nbret=n2)
        if (n2 .eq. 1) then
            isole=.false.
            cham1=' '
            call dismoi('NOM_MAILLA', resuin, 'RESULTAT', repk=nomare)
            norein = resuin
        else
            isole=.true.
            call getvid(' ', 'CHAM_GD', scal=cham1, nbret=n3)
            norein = cham1(1:8)
            call dismoi('NOM_MAILLA', cham1, 'CHAMP', repk=nomare)
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
        call utmess('F', 'CALCULEL5_9')
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
        call getvid(' ', 'MODELE_1', scal=nomo1, nbret=n1)
        if (n1 .eq. 1) then
            call dismoi('NOM_MAILLA', nomo1, 'MODELE', repk=noma1)
            moa1=nomo1
        else
            nomo1=' '
            call getvid(' ', 'MAILLAGE_1', scal=noma1, nbret=n2)
            ASSERT(n2.eq.1)
            moa1=noma1
        endif
!
        call getvid(' ', 'MODELE_2', scal=nomo2, nbret=n1)
        if (n1 .eq. 1) then
            call dismoi('NOM_MAILLA', nomo2, 'MODELE', repk=noma2)
            moa2=nomo2
        else
            nomo2=' '
            call getvid(' ', 'MAILLAGE_2', scal=noma2, nbret=n2)
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
            call utmess('F', 'CALCULEL4_59', nk=4, valk=valk)
        endif
!
        call getvid(' ', 'CHAM_NO_REFE', scal=cnref, nbret=n1)
        if (n1 .eq. 1) then
            call dismoi('NOM_MAILLA', cnref, 'CHAMP', repk=noma3)
            if (noma3 .ne. noma2) then
                valk(1)=cnref
                valk(2)=noma3
                valk(3)=noma2
                call utmess('F', 'CALCULEL2_6', nk=3, valk=valk)
            endif
        else
            cnref=' '
        endif
    endif
!
    if (method .eq. 'SOUS_POINT') then
!       RECUPERATION DU CARA_ELEM
        call getvid(' ', 'CARA_ELEM', scal=noca, nbret=n1)
        ASSERT(n1.ne.0)
!       LE MOT-CLE 'MODELE_2' EST OBLIGATOIRE
        call getvid(' ', 'MODELE_2', scal=nomo2, nbret=n1)
        if (n1 .eq. 0) then
            call utmess('F', 'CALCULEL5_40')
        endif
!       VIS_A_VIS EST INTERDIT AVEC SOUS_POINT
        call getfac('VIS_A_VIS', nbocc)
        if (nbocc .ne. 0) then
            call utmess('F', 'CALCULEL5_31')
        endif
        if (.not. isole) then
            call dismoi('TYPE_RESU', resuin, 'RESULTAT', repk=rtyp)
            if (rtyp .ne. 'EVOL_THER') then
                call utmess('F', 'CALCULEL5_30')
            endif
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
    if (typcal .eq. '1') goto 999
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
        call jeveuo(corru//'.PJXX_K1', 'L', vk24=pjxx_k1)
!        -- LES MOA1 ET MOA2 STOCKES SONT LES MAILLAGES
        moa1=pjxx_k1(1)(1:8)
        moa2=pjxx_k1(2)(1:8)
        if (moa1 .ne. nomare) then
            valk(1) = moa1
            valk(2) = norein
            valk(3) = moa1
            valk(4) = nomare
            call utmess('F', 'CALCULEL4_59', nk=4, valk=valk)
        endif
!        -- POUR POUVOIR PROJETER LES CHAM_ELEM, IL FAUT MODELE_2
        call getvid(' ', 'MODELE_2', scal=nomo2, nbret=n1)
        if (n1 .eq. 1) then
            call dismoi('NOM_MAILLA', nomo2, 'MODELE', repk=noma2)
            if (moa2 .ne. noma2) then
                valk(1) = moa2
                valk(2) = noma2
                call utmess('F', 'CALCULEL4_72', nk=2, valk=valk)
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
            call getvtx(' ', 'TYPE_CHAM', nbval=0, nbret=n1)
            if (n1 .ne. 0) then
                call getvtx(' ', 'TYPE_CHAM', scal=tychv, nbret=n1)
            else
                tychv=' '
            endif
            if (tychv .eq. 'NOEU') then
                call utmess('F', 'CALCULEL5_36')
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
                    call utmess('F', 'CALCULEL5_32', sk=valk(1))
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
                    call utmess('F', 'CALCULEL5_33', nk=2, valk=valk)
                endif
!       ------   LE MOT-CLE 'MODELE_2' EST OBLIGATOIRE
                call getvtx(' ', 'PROL_ZERO', scal=prol0, nbret=ie)
                call getvid(' ', 'MODELE_2', scal=nomo2, nbret=n1)
                if (n1 .eq. 0) then
                    call utmess('F', 'CALCULEL5_37')
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
                    call utmess('F', 'CALCULEL5_33', nk=2, valk=valk)
                endif
!       ------   LE MOT-CLE 'MODELE_2' EST OBLIGATOIRE
                call getvtx(' ', 'PROL_ZERO', scal=prol0, nbret=ie)
                call getvid(' ', 'MODELE_2', scal=nomo2, nbret=n1)
                if (n1 .eq. 0) then
                    call utmess('F', 'CALCULEL5_37')
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
                    call utmess('F', 'CALCULEL5_33', nk=2, valk=valk)
                endif
!       ------  LES MOTS-CLES 'MODELE_1' ET 'MODELE_2' SONT OBLIGATOIRES
                call getvtx(' ', 'PROL_ZERO', scal=prol0, nbret=ie)
                call getvid(' ', 'MODELE_1', scal=nomo1, nbret=n1)
                if (n1 .eq. 0) then
                    call utmess('F', 'CALCULEL5_35')
                endif
                call getvid(' ', 'MODELE_2', scal=nomo2, nbret=n1)
                if (n1 .eq. 0) then
                    call utmess('F', 'CALCULEL5_37')
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
999 continue
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
