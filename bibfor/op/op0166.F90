subroutine op0166()
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! --------------------------------------------------------------------------------------------------
!
!     COMMANDE:  PROJ_CHAMP
!
! --------------------------------------------------------------------------------------------------
!
    implicit none
!
#include "asterf_types.h"
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
#include "asterfort/exi_fiss.h"
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iret, ie, n1, n2, n3, nbocc
    aster_logical :: isole, lnoeu, lelno, lelem, lelga, lxfem
    character(len=4) :: tychv, typcal
    character(len=8) :: k8b, noma1, noma2, noma3, resuin, prol0, projon, norein
    character(len=8) :: nomo1, nomo2, moa1, moa2, cnref, nomare, noca
    character(len=16) :: typres, nomcmd, lcorre(2), corru, nomgd
    character(len=19) :: resuou, cham1, method, rtyp, ligre1, ligre2
    character(len=24) :: valk(4)
    character(len=24), pointer :: pjxx_k1(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
    call titre()
    lelga = .false.
    lxfem=.false._1
!
! --------------------------------------------------------------------------------------------------
!   calcul de typcal :
!       / '1et2' : on calcule la SD_CORRESP_2_MAILLA et on projette
!   les champs :
!       / '1'    : on calcule la SD_CORRESP_2_MAILLA et on s'arrête
!       / '2'    : on utilise la SD_CORRESP_2_MAILLA déjà calculée
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
! --------------------------------------------------------------------------------------------------
!   calcul de resuou, typres, method, isole, cham1, resuin :
!       resuou : nom du concept résultat
!       typres : type du résultat (CHAM_GD ou SD_RESULTAT)
!       method : méthode choisie pour la projection
!       isole  : .true.  : on ne projette qu'un champ isole
!       .false. : on projette une SD_RESULTAT
!       cham1  : nom du champ à projeter (si isole)
!       resuin : nom de la SD_RESULTAT à projeter (si .not.isole)
    call getres(resuou, typres, nomcmd)
    call getvtx(' ', 'METHODE', scal=method, nbret=n1)
    if (n1 .eq. 0) method=' '
    lxfem=lxfem.and.method.eq.'COLLOCATION'
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
            ASSERT(n3.eq.1)
            norein = cham1(1:8)
            call dismoi('NOM_MAILLA', cham1, 'CHAMP', repk=nomare)
            resuin=' '
        endif
    else
        cham1=' '
        resuin=' '
        nomare=' '
    endif
    lxfem=lxfem.and.n2.eq.1
! --------------------------------------------------------------------------------------------------
!   limitation de la méthode ECLA_PG :
!       il n'est pas possible de projeter une SD_RESULTAT
    if ((method.eq.'ECLA_PG') .and. (.not.isole)) then
        call utmess('F', 'CALCULEL5_9')
    endif
!
! --------------------------------------------------------------------------------------------------
!   calcul de noma1, noma2, moa1, moa2, cnref, noca :
!       noma1 : nom du maillage "1"
!       noma2 : nom du maillage "2"
!       nomo1 : nom du modèle "1"  (ou ' ')
!       nomo2 : nom du modèle "2"  (ou ' ')
!       moa1  : nomo1 si non ' '. sinon : noma1
!       moa2  : nomo2 si non ' '. sinon : noma2
!       cnref : nom du CHAM_NO "modèle" "2" (si NUAGE_DEG_0/1)
    if (typcal .eq. '1' .or. typcal .eq. '1ET2') then
        call getvid(' ', 'MODELE_1', scal=nomo1, nbret=n1)
        if (n1 .eq. 1) then
            call dismoi('NOM_MAILLA', nomo1, 'MODELE', repk=noma1)
            moa1=nomo1
            lxfem=exi_fiss(nomo1)
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
!       vérification de la cohérence entre les maillages associes :
!           1. au résultat (ou cham) à projeter
!           2. au modèle (ou maillage) fourni en entrée
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
! --------------------------------------------------------------------------------------------------
!   méthode SOUS_POINT :
!       données  : CARA_ELEM, MODELE_2
!       valide  sur RESULTAT : EVOL_THER
!       interdit : VIS_A_VIS
!       valide sur CHAMP     : ELNO, NOEU (test de vérification fait après)
    if (method .eq. 'SOUS_POINT') then
!       récupération du CARA_ELEM
        call getvid(' ', 'CARA_ELEM', scal=noca, nbret=n1)
        if (n1 .eq. 0) then
            valk='CARA_ELEM'
            call utmess('F', 'CALCULEL5_40',nk=1,valk=valk)
        endif
!       le mot-clef 'MODELE_2' est obligatoire
        call getvid(' ', 'MODELE_2', scal=nomo2, nbret=n1)
        if (n1 .eq. 0) then
            valk(1)='MODELE_2'
            call utmess('F', 'CALCULEL5_40',nk=1,valk=valk)
        endif
!       VIS_A_VIS est interdit avec SOUS_POINT
        call getfac('VIS_A_VIS', nbocc)
        if (nbocc .ne. 0) then
            valk(1)='VIS_A_VIS'
            call utmess('F', 'CALCULEL5_31',nk=1,valk=valk)
        endif
        if (.not. isole) then
            call dismoi('TYPE_RESU', resuin, 'RESULTAT', repk=rtyp)
            if (rtyp .ne. 'EVOL_THER') then
                call utmess('F', 'CALCULEL5_30')
            endif
        endif
!       Si c'est un champ isolé on va chercher sa grandeur
        if ( isole ) then
            call dismoi('NOM_GD', cham1, 'CHAMP', repk=nomgd)
!           En fonction de NOM_GD :
!               Projection sur famille MATER, option INI_SP_MATER : TEMP_R  HYDR_R  NEUT_R
!               Projection sur famille RIGI,  option INI_SP_RIGI  : SIEF_R
            if ( (nomgd.ne.'SIEF_R').and.(nomgd.ne.'TEMP_R').and. &
                 (nomgd.ne.'HYDR_R').and.(nomgd.ne.'NEUT_R') ) then
                valk(1)=nomgd
                call utmess('F', 'CALCULEL5_29',nk=1,valk=valk)
            endif
            if ( nomgd.eq.'SIEF_R' ) then
                method = 'SOUS_POINT_RIGI'
            else
                method = 'SOUS_POINT_MATER'
            endif
        else
!           Si c'est un résultat, seules les EVOL_VARC sont prises en compte.
            method = 'SOUS_POINT_MATER'
        endif
    else
        noca = ' '
    endif
!
! --------------------------------------------------------------------------------------------------
!   calcul de la SD_LCORRESP_2_MAILLA. Elle est constituée d'une liste de deux sd :
!       - la 1ère est une SD_CORRESP_2_MAILLA
!       - la 2de est une SD_CORRESP_2_MAILLA particulière utilisée pour la projection de
!         CHAM_ELEM (elga) et comporte PJEF_EL (tableau auxiliaire)
    if (typcal .eq. '1' .or. typcal .eq. '1ET2') then
        lcorre(1)='&&OP0166.CORRES'
        lcorre(2)='&&OP0166.CORRE2'
        call pjxxco(typcal, method, lcorre, isole, resuin,&
                    cham1, moa1, moa2, noma1, noma2,&
                    cnref, noca)
    endif
!   si typcal='1', il faut s'arrêter la
    if (typcal .eq. '1') goto 999
! --------------------------------------------------------------------------------------------------
!   projection des champs :
!       si typcal='2', il faut surcharger lcorr(1) et éventuellement récupérer MODELE_2
    if (typcal .eq. '2') then
        lcorre(1)=corru
        lcorre(2)=' '
        call jeveuo(corru//'.PJXX_K1', 'L', vk24=pjxx_k1)
!       les moa1 et moa2 stockés sont les maillages
        moa1=pjxx_k1(1)(1:8)
        moa2=pjxx_k1(2)(1:8)
        if (moa1 .ne. nomare) then
            valk(1) = moa1
            valk(2) = norein
            valk(3) = moa1
            valk(4) = nomare
            call utmess('F', 'CALCULEL4_59', nk=4, valk=valk)
        endif
!       pour pouvoir projeter les CHAM_ELEM, il faut MODELE_2
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
! --------------------------------------------------------------------------------------------------
!   cas champ isolé
    if (isole) then
!
        if (method(1:10) .eq. 'NUAGE_DEG_') then
!           méthode 'NUAGE_DEG' : on ne peut projeter que des CHAM_NO
            tychv = ' '
            call pjxxch(lcorre(1), cham1, resuou, tychv, ' ',&
                        'NON', ' ', 'G', iret)
            ASSERT(iret.eq.0)
        else
!           autre méthode :
!               on peut projeter des CHAM_NO ou des CHAM_ELEM
!               on interdit le mot-clef 'TYPE_CHAM' pour un champ isole
            call getvtx(' ', 'TYPE_CHAM', nbval=0, nbret=n1)
            if (n1 .ne. 0) then
                call getvtx(' ', 'TYPE_CHAM', scal=tychv, nbret=n1)
            else
                tychv=' '
            endif
            if (tychv .eq. 'NOEU') then
                call utmess('F', 'CALCULEL5_36')
            endif
!           on détermine le type de champ à projeter
            call pjtyco(isole, k8b, cham1, lnoeu, lelno,&
                        lelem, lelga)
!
            if (lnoeu) then
!               cas ou il y a un CHAM_NO (NOEU)
                if (method .eq. 'ECLA_PG') then
                    valk(1) = method
                    call utmess('F', 'CALCULEL5_32', sk=valk(1))
                endif
                if (method(1:10) .eq. 'SOUS_POINT') then
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
!
            else if (lelno) then
!               cas ou il y a un CHAM_ELEM (ELNO)
                if (method .eq. 'ECLA_PG') then
                    valk(1) = method
                    valk(2) = 'ELNO'
                    call utmess('F', 'CALCULEL5_33', nk=2, valk=valk)
                endif
!               le mot-clef 'MODELE_2' est obligatoire
                call getvtx(' ', 'PROL_ZERO', scal=prol0, nbret=ie)
                call getvid(' ', 'MODELE_2', scal=nomo2, nbret=n1)
                if (n1 .eq. 0) then
                    call utmess('F', 'CALCULEL5_37')
                endif
!
                ligre2 = nomo2//'.MODELE'
                if (method(1:10) .eq. 'SOUS_POINT') then
                    prol0='NON'
                    call pjspma(lcorre(1), cham1, resuou, prol0, ligre2,&
                                noca, 'G', iret)
                else
                    tychv = ' '
                    call pjxxch(lcorre(1), cham1, resuou, tychv, ' ',&
                                prol0, ligre2, 'G', iret)
                    ASSERT(iret.eq.0)
                endif
!
            else if (lelem) then
!               cas ou il y a un CHAM_ELEM (ELEM)
                if ((method.eq.'ECLA_PG') .or. (method(1:10).eq.'SOUS_POINT')) then
                    valk(1) = method
                    valk(2) = 'ELEM'
                    call utmess('F', 'CALCULEL5_33', nk=2, valk=valk)
                endif
!               le mot-clef 'MODELE_2' est obligatoire
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
!               cas ou il y a un CHAM_ELEM (ELGA)
                if ((method.eq.'COLLOCATION') .or. ( method(1:10).eq.'SOUS_POINT')) then
                    valk(1) = method
                    valk(2) = 'ELGA'
                    call utmess('F', 'CALCULEL5_33', nk=2, valk=valk)
                endif
!               les mots-clefs 'MODELE_1' et 'MODELE_2' sont obligatoires
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
        endif
!
! --------------------------------------------------------------------------------------------------
!   cas SD_RESULTAT :
    else
        call pjxxpr(resuin, resuou(1:8), moa1, moa2, lcorre(1),&
                    'G', noca, method, xfem=lxfem)
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
