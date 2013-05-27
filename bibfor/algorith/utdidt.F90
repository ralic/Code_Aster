subroutine utdidt(getset, sddisc, typque, iocc, quest,&
                  valr, vali, valk)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! TOLE CRP_20
! person_in_charge: samuel.geniaut at edf.fr
!
    implicit      none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/dfllvd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    integer :: iocc, vali
    real(kind=8) :: valr
    character(len=1) :: getset
    character(len=4) :: typque
    character(len=*) :: quest, valk
    character(len=19) :: sddisc
!
! ----------------------------------------------------------------------
!
!
! ROUTINE UTILITAIRE SUR LA DISCRETISATION TEMPORELLE
!   ACCES AUX SD LOCALES !! ET NON A LA SD DE L'OPERATEUR DEFI_LIST_INST
!   MAIS L'ARCHITECTURE ETANT LA MEME, LES OBJETS DOIVENT ETRE EN
!   CONFORMITE AVEC LA ROUTINE OP0028 (DEFI_LIST_INST)
!
! ----------------------------------------------------------------------
!
! IN  GETSET : 'L' -> LECTURE
!              'E' -> ECRITURE
! IN  SDDISC : SDDISC LOCALE A OP00700
! IN  TYPQUE : TYPE DE DEMANDE (LIST, ECHE OU ADAP)
! IN  IOCC   : NUMERO OCCURRENCE (POUR ECHEC/ADAPT)
! IN  QUEST  : QUESTION
! I/O VALI   : VALEUR ENTIERE
! I/O VALR   : VALEUR REELLE
! I/O VALK   : VALEUR CHAINE
!
!
!
!
!
    integer :: leevr, leevk, lesur, laevr, latpr, latpk
    integer :: iechec, iadapt
    character(len=24) :: tpsinf, tpsrpc, tpspil
    integer :: jlinr, jreapc, jpil
    character(len=24) :: tpsevr, tpsevk, tpsesu
    integer :: jeevr, jeevk, jesur
    character(len=24) :: tpsavr, tpsavk, tpstpr, tpstpk
    integer :: jaevr, jaevk, jatpr, jatpk
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    if (getset .eq. 'L') then
        valk = ' '
        vali = 0
        valr = 0.d0
    endif
!
! --- TAILLE DES VECTEURS
!
    leevr = dfllvd('LEEVR')
    leevk = dfllvd('LEEVK')
    lesur = dfllvd('LESUR')
    laevr = dfllvd('LAEVR')
    latpr = dfllvd('LATPR')
    latpk = dfllvd('LATPK')
!
    call assert(typque.eq.'LIST'.or. typque.eq.'ECHE'.or. typque.eq.'ADAP')
!
    call assert(getset.eq.'L'.or.getset.eq.'E')
!
!     ------------------------------------------------------------------
!                     QUESTION SUR LA LISTE
!     ------------------------------------------------------------------
!
    if (typque .eq. 'LIST') then
        tpsinf = sddisc(1:19)//'.LINF'
        call jeveuo(tpsinf, getset, jlinr)
!
        if (quest .eq. 'METHODE') then
            if (getset .eq. 'L') then
                vali = nint(zr(jlinr-1+1))
                if (vali .eq. 1) valk = 'MANUEL'
                if (vali .eq. 2) valk = 'AUTO'
            else if (getset.eq.'E') then
                if (valk .eq. 'MANUEL') then
                    zr(jlinr-1+1) = 1
                else if (valk.eq.'AUTO') then
                    zr(jlinr-1+1) = 2
                else
                    call assert(.false.)
                endif
            endif
!
        else if (quest.eq.'PAS_MINI') then
            if (getset .eq. 'L') then
                valr = zr(jlinr-1+2)
            else if (getset.eq.'E') then
                zr(jlinr-1+2) = valr
            endif
!
        else if (quest.eq.'PAS_MAXI') then
            if (getset .eq. 'L') then
                valr = zr(jlinr-1+3)
            else if (getset.eq.'E') then
                zr(jlinr-1+3) = valr
            endif
!
        else if (quest.eq.'NB_PAS_MAXI') then
            if (getset .eq. 'L') then
                vali = nint(zr(jlinr-1+4))
            else if (getset.eq.'E') then
                zr(jlinr-1+4) = vali
            endif
!
        else if (quest.eq.'DTMIN') then
            if (getset .eq. 'L') then
                valr = zr(jlinr-1+5)
            else if (getset.eq.'E') then
                zr(jlinr-1+5) = valr
            endif
!
        else if (quest.eq.'DT-') then
            if (getset .eq. 'L') then
                valr = zr(jlinr-1+6)
            else if (getset.eq.'E') then
                zr(jlinr-1+6) = valr
            endif
!
        else if (quest.eq.'EXIS_DECOUPE') then
            if (getset .eq. 'L') then
                vali = nint(zr(jlinr-1+7))
                if (vali .eq. 0) valk = 'NON'
                if (vali .eq. 1) valk = 'OUI'
            else if (getset.eq.'E') then
                if (valk .eq. 'NON') then
                    zr(jlinr-1+7) = 0
                else if (valk.eq.'OUI') then
                    zr(jlinr-1+7) = 1
                else
                    write(6,*) 'VALK: ',valk
                    call assert(.false.)
                endif
            endif
!
        else if (quest.eq.'EXIS_REAC_PRECOND') then
            if (getset .eq. 'L') then
                vali = nint(zr(jlinr-1+11))
                if (vali .eq. 0) valk = 'NON'
                if (vali .eq. 1) valk = 'OUI'
            else if (getset.eq.'E') then
                if (valk .eq. 'NON') then
                    zr(jlinr-1+11) = 0
                else if (valk.eq.'OUI') then
                    zr(jlinr-1+11) = 1
                else
                    write(6,*) 'VALK: ',valk
                    call assert(.false.)
                endif
            endif
!
        else if (quest.eq.'NBINST') then
            if (getset .eq. 'L') then
                vali = nint(zr(jlinr-1+8))
            else if (getset.eq.'E') then
                zr(jlinr-1+8) = vali
            endif
!
        else if (quest.eq.'NECHEC') then
            if (getset .eq. 'L') then
                vali = nint(zr(jlinr-1+9))
            else if (getset.eq.'E') then
                zr(jlinr-1+9) = vali
            endif
!
        else if (quest.eq.'NADAPT') then
            if (getset .eq. 'L') then
                vali = nint(zr(jlinr-1+10))
            else if (getset.eq.'E') then
                zr(jlinr-1+10) = vali
            endif
!
        else
            call assert(.false.)
!
        endif
!
!     ------------------------------------------------------------------
!                     QUESTION SUR L'ECHEC
!     ------------------------------------------------------------------
!
    else if (typque.eq.'ECHE') then
        tpsevr = sddisc(1:19)//'.EEVR'
        tpsevk = sddisc(1:19)//'.EEVK'
        tpsesu = sddisc(1:19)//'.ESUR'
        tpsrpc = sddisc(1:19)//'.REPC'
        tpspil = sddisc(1:19)//'.EPIL'
        call jeveuo(tpsevr, getset, jeevr)
        call jeveuo(tpsevk, getset, jeevk)
        call jeveuo(tpsesu, getset, jesur)
        call jeveuo(tpsrpc, getset, jreapc)
        call jeveuo(tpspil, getset, jpil)
        iechec = iocc
!
        if (quest .eq. 'NOM_EVEN') then
            if (getset .eq. 'L') then
!
                vali = nint(zr(jeevr-1+leevr*(iechec-1)+1))
!
                if (vali .eq. 0) valk = 'ERRE'
                if (vali .eq. 1) valk = 'DELTA_GRANDEUR'
                if (vali .eq. 2) valk = 'COLLISION'
                if (vali .eq. 3) valk = 'INTERPENETRATION'
                if (vali .eq. 4) valk = 'DIVE_RESI'
                if (vali .eq. 5) valk = 'INSTABILITE'
            else if (getset.eq.'E') then
                if (valk .eq. 'ERRE') then
                    zr(jeevr-1+leevr*(iechec-1)+1) = 0.d0
                else if (valk.eq.'DELTA_GRANDEUR') then
                    zr(jeevr-1+leevr*(iechec-1)+1) = 1.d0
                else if (valk.eq.'COLLISION') then
                    zr(jeevr-1+leevr*(iechec-1)+1) = 2.d0
                else if (valk.eq.'INTERPENETRATION') then
                    zr(jeevr-1+leevr*(iechec-1)+1) = 3.d0
                else if (valk.eq.'DIVE_RESI') then
                    zr(jeevr-1+leevr*(iechec-1)+1) = 4.d0
                else if (valk.eq.'INSTABILITE') then
                    zr(jeevr-1+leevr*(iechec-1)+1) = 5.d0
                else
                    call assert(.false.)
                endif
            endif
!
        else if (quest.eq.'ACTION') then
            if (getset .eq. 'L') then
                vali = nint(zr(jeevr-1+leevr*(iechec-1)+2))
                if (vali .eq. 0) valk = 'ARRET'
                if (vali .eq. 1) valk = 'REAC_PRECOND'
                if (vali .eq. 2) valk = 'DECOUPE'
                if (vali .eq. 3) valk = 'ITER_SUPPL'
                if (vali .eq. 4) valk = 'AUTRE_PILOTAGE'
                if (vali .eq. 5) valk = 'ADAPT_COEF_PENA'
                if (vali .eq. 6) valk = 'CONTINUE'
            else if (getset.eq.'E') then
                if (valk .eq. 'ARRET') then
                    zr(jeevr-1+leevr*(iechec-1)+2) = 0.d0
                else if (valk.eq.'REAC_PRECOND') then
                    zr(jeevr-1+leevr*(iechec-1)+2) = 1.d0
                else if (valk.eq.'DECOUPE') then
                    zr(jeevr-1+leevr*(iechec-1)+2) = 2.d0
                else if (valk.eq.'ITER_SUPPL') then
                    zr(jeevr-1+leevr*(iechec-1)+2) = 3.d0
                else if (valk.eq.'AUTRE_PILOTAGE') then
                    zr(jeevr-1+leevr*(iechec-1)+2) = 4.d0
                else if (valk.eq.'ADAPT_COEF_PENA') then
                    zr(jeevr-1+leevr*(iechec-1)+2) = 5.d0
                else if (valk.eq.'CONTINUE') then
                    zr(jeevr-1+leevr*(iechec-1)+2) = 6.d0
                else
                    call assert(.false.)
                endif
            endif
!
        else if (quest.eq.'VERIF_EVEN') then
            if (getset .eq. 'L') then
                vali = nint(zr(jeevr-1+leevr*(iechec-1)+3))
                if (vali .eq. 0) valk = 'OUI'
                if (vali .eq. 1) valk = 'NON'
            else if (getset.eq.'E') then
                if (valk .eq. 'OUI') then
                    zr(jeevr-1+leevr*(iechec-1)+3) = 0
                else if (valk.eq.'NON') then
                    zr(jeevr-1+leevr*(iechec-1)+3) = 1
                else
                    call assert(.false.)
                endif
            endif
!
! ----- PARAMETRES EVENEMENT 'DELTA_GRANDEUR'
!
        else if (quest.eq.'NOM_CHAM') then
            if (getset .eq. 'L') then
                valk = zk16(jeevk-1+leevk*(iechec-1)+1)
            else if (getset.eq.'E') then
                zk16(jeevk-1+leevk*(iechec-1)+1) = valk
            endif
!
        else if (quest.eq.'NOM_CMP') then
            if (getset .eq. 'L') then
                valk = zk16(jeevk-1+leevk*(iechec-1)+2)
            else if (getset.eq.'E') then
                zk16(jeevk-1+leevk*(iechec-1)+2) = valk
            endif
!
        else if (quest.eq.'CRIT_COMP') then
            if (getset .eq. 'L') then
                valk = zk16(jeevk-1+leevk*(iechec-1)+3)
            else if (getset.eq.'E') then
                zk16(jeevk-1+leevk*(iechec-1)+3) = valk
            endif
!
        else if (quest.eq.'VALE_REF') then
            if (getset .eq. 'L') then
                valr = zr(jeevr-1+leevr*(iechec-1)+5)
            else if (getset.eq.'E') then
                zr(jeevr-1+leevr*(iechec-1)+5) = valr
            endif
!
! ----- PARAMETRES EVENEMENT 'INTERPENETRATION'
!
        else if (quest.eq.'PENE_MAXI') then
            if (getset .eq. 'L') then
                valr = zr(jeevr-1+leevr*(iechec-1)+6)
            else if (getset.eq.'E') then
                zr(jeevr-1+leevr*(iechec-1)+6) = valr
            endif
!
! ----- PARAMETRES ACTION 'DECOUPE'
!
        else if (quest.eq.'SUBD_METHODE') then
            if (getset .eq. 'L') then
                vali = nint(zr(jesur-1+lesur*(iechec-1)+1))
                if (vali .eq. 0) valk = 'AUCUNE'
                if (vali .eq. 1) valk = 'MANUEL'
                if (vali .eq. 2) valk = 'AUTO'
            else if (getset.eq.'E') then
                if (valk .eq. 'AUCUNE') then
                    zr(jesur-1+lesur*(iechec-1)+1) = 0
                else if (valk .eq. 'MANUEL') then
                    zr(jesur-1+lesur*(iechec-1)+1) = 1
                else if (valk .eq. 'AUTO') then
                    zr(jesur-1+lesur*(iechec-1)+1) = 2
                else
                    call assert(.false.)
                endif
            endif
!
        else if (quest.eq.'SUBD_METHODE_AUTO') then
            if (getset .eq. 'L') then
                vali = nint(zr(jesur-1+lesur*(iechec-1)+10))
                if (vali .eq. 1) valk = 'COLLISION'
                if (vali .eq. 2) valk = 'EXTRAPOLE'
            else if (getset.eq.'E') then
                if (valk .eq. 'COLLISION') then
                    zr(jesur-1+lesur*(iechec-1)+10) = 1
                else if (valk .eq. 'EXTRAPOLE') then
                    zr(jesur-1+lesur*(iechec-1)+10) =21
                else
                    call assert(.false.)
                endif
            endif
!
        else if (quest.eq.'SUBD_PAS') then
            if (getset .eq. 'L') then
                vali = nint(zr(jesur-1+lesur*(iechec-1)+2))
            else if (getset.eq.'E') then
                zr(jesur-1+lesur*(iechec-1)+2) = vali
            endif
!
        else if (quest.eq.'SUBD_PAS_MINI') then
            if (getset .eq. 'L') then
                valr = zr(jesur-1+lesur*(iechec-1)+3)
            else if (getset.eq.'E') then
                zr(jesur-1+lesur*(iechec-1)+3) = valr
            endif
!
        else if (quest.eq.'SUBD_NIVEAU') then
            if (getset .eq. 'L') then
                vali = nint(zr(jesur-1+lesur*(iechec-1)+4))
            else if (getset.eq.'E') then
                zr(jesur-1+lesur*(iechec-1)+4) = vali
            endif
!
        else if (quest.eq.'SUBD_INST') then
            if (getset .eq. 'L') then
                valr = zr(jesur-1+lesur*(iechec-1)+5)
            else if (getset.eq.'E') then
                zr(jesur-1+lesur*(iechec-1)+5) = valr
            endif
!
        else if (quest.eq.'SUBD_DUREE') then
            if (getset .eq. 'L') then
                valr = zr(jesur-1+lesur*(iechec-1)+6)
            else if (getset.eq.'E') then
                zr(jesur-1+lesur*(iechec-1)+6) = valr
            endif
!
        else if (quest.eq.'SUBD_RATIO') then
            if (getset .eq. 'L') then
                vali = nint(zr(jesur-1+lesur*(iechec-1)+9))
            else if (getset.eq.'E') then
                zr(jesur-1+lesur*(iechec-1)+9) = vali
            endif
!
! ---- PARAMETRES ACTION 'REAC_PRECOND'
!
        else if (quest.eq.'ESSAI_REAC_PRECOND') then
            if (getset .eq. 'L') then
                vali = zi(jreapc)
            else if (getset.eq.'E') then
                zi(jreapc) = vali
            endif
!
! ----- PARAMETRES ACTION 'ITER_SUPPL'
!
        else if (quest.eq.'PCENT_ITER_PLUS') then
            if (getset .eq. 'L') then
                valr = zr(jesur-1+lesur*(iechec-1)+7)
            else if (getset.eq.'E') then
                zr(jesur-1+lesur*(iechec-1)+7) = valr
            endif
!
! ----- PARAMETRES ACTION 'ADAPT_COEF_PENA'
!
        else if (quest.eq.'COEF_MAXI') then
            if (getset .eq. 'L') then
                valr = zr(jesur-1+lesur*(iechec-1)+8)
            else if (getset.eq.'E') then
                zr(jesur-1+lesur*(iechec-1)+8) = valr
            endif
!
! ----- PARAMETRES ACTION 'AUTRE_PILOTAGE'
!
        else if (quest.eq.'CHOIX_SOLU_PILO') then
            if (getset .eq. 'L') then
                vali = zi(jpil)
                if (vali .eq. 1) valk = 'NATUREL'
                if (vali .eq. 2) valk = 'AUTRE'
            else if (getset.eq.'E') then
                if (valk .eq. 'NATUREL') then
                    zi(jpil)=1
                else if (valk.eq.'AUTRE') then
                    zi(jpil)=2
                else
                    call assert(.false.)
                endif
            endif
!
        else if (quest.eq.'ESSAI_ITER_PILO') then
            if (getset .eq. 'L') then
                vali = zi(jpil+1)
            else if (getset.eq.'E') then
                zi(jpil+1) = vali
            endif
!
        else
            call assert(.false.)
!
        endif
!
!     ------------------------------------------------------------------
!                     QUESTION SUR L'ADAPTATION
!     ------------------------------------------------------------------
!
    else if (typque.eq.'ADAP') then
        tpsavr = sddisc(1:19)//'.AEVR'
        tpsavk = sddisc(1:19)//'.AEVK'
        tpstpr = sddisc(1:19)//'.ATPR'
        tpstpk = sddisc(1:19)//'.ATPK'
        tpspil = sddisc(1:19)//'.EPIL'
        call jeveuo(tpsavr, getset, jaevr)
        call jeveuo(tpsavk, getset, jaevk)
        call jeveuo(tpstpr, getset, jatpr)
        call jeveuo(tpstpk, getset, jatpk)
        call jeveuo(tpspil, getset, jpil)
        iadapt = iocc
!
        if (quest .eq. 'NOM_EVEN') then
            if (getset .eq. 'L') then
                vali = nint(zr(jaevr-1+laevr*(iadapt-1)+1))
                if (vali .eq. 0) valk = 'AUCUN'
                if (vali .eq. 1) valk = 'TOUT_INST'
                if (vali .eq. 2) valk = 'SEUIL_SANS_FORMULE'
                if (vali .eq. 3) valk = 'SEUIL_AVEC_FORMULE'
            else if (getset.eq.'E') then
                if (valk .eq. 'AUCUN') then
                    zr(jaevr-1+laevr*(iadapt-1)+1) = 0
                else if (valk.eq.'TOUT_INST') then
                    zr(jaevr-1+laevr*(iadapt-1)+1) = 1
                else if (valk.eq.'SEUIL_SANS_FORMULE') then
                    zr(jaevr-1+laevr*(iadapt-1)+1) = 2
                else if (valk.eq.'SEUIL_AVEC_FORMULE') then
                    zr(jaevr-1+laevr*(iadapt-1)+1) = 3
                else
                    call assert(.false.)
                endif
            endif
!
        else if (quest.eq.'NB_INCR_SEUIL') then
            if (getset .eq. 'L') then
                vali = nint(zr(jaevr-1+laevr*(iadapt-1)+2))
            else if (getset.eq.'E') then
                zr(jaevr-1+laevr*(iadapt-1)+2) = 1
            endif
!
        else if (quest.eq.'NOM_PARA') then
            if (getset .eq. 'L') then
                vali = nint(zr(jaevr-1+laevr*(iadapt-1)+3))
                if (vali .eq. 1) valk = 'NB_ITER_NEWT'
                if (vali .eq. 2) valk = 'DP'
            else if (getset.eq.'E') then
                if (valk .eq. 'NB_ITER_NEWT') then
                    zr(jaevr-1+laevr*(iadapt-1)+3) = 1
                else if (valk.eq.'SEUIL_AVEC_FORMULE') then
                    zr(jaevr-1+laevr*(iadapt-1)+3) = 2
                else
                    call assert(.false.)
                endif
            endif
!
        else if (quest.eq.'CRIT_COMP') then
            if (getset .eq. 'L') then
                vali = nint(zr(jaevr-1+laevr*(iadapt-1)+4))
                if (vali .eq. 1) valk = 'LT'
                if (vali .eq. 2) valk = 'GT'
                if (vali .eq. 3) valk = 'LE'
                if (vali .eq. 4) valk = 'GE'
            else if (getset.eq.'E') then
                if (valk .eq. 'LT') then
                    zr(jaevr-1+laevr*(iadapt-1)+4) = 1
                else if (valk.eq.'GT') then
                    zr(jaevr-1+laevr*(iadapt-1)+4) = 2
                else if (valk.eq.'LE') then
                    zr(jaevr-1+laevr*(iadapt-1)+4) = 3
                else if (valk.eq.'GE') then
                    zr(jaevr-1+laevr*(iadapt-1)+4) = 4
                else
                    call assert(.false.)
                endif
            endif
!
        else if (quest.eq.'VALE') then
            if (getset .eq. 'L') then
                valr = zr(jaevr-1+laevr*(iadapt-1)+5)
                vali = nint(zr(jaevr-1+laevr*(iadapt-1)+5))
            else if (getset.eq.'E') then
                zr(jaevr-1+laevr*(iadapt-1)+5) = valr
            endif
!
        else if (quest.eq.'NB_EVEN_OK') then
            if (getset .eq. 'L') then
                valr = zr(jaevr-1+laevr*(iadapt-1)+6)
                vali = nint(valr)
            else if (getset.eq.'E') then
                zr(jaevr-1+laevr*(iadapt-1)+6) = vali
            endif
!
        else if (quest.eq.'METHODE') then
            if (getset .eq. 'L') then
                vali = nint(zr(jatpr-1+latpr*(iadapt-1)+1))
                if (vali .eq. 1) valk = 'FIXE'
                if (vali .eq. 2) valk = 'DELTA_GRANDEUR'
                if (vali .eq. 3) valk = 'ITER_NEWTON'
                if (vali .eq. 4) valk = 'FORMULE'
                if (vali .eq. 5) valk = 'IMPLEX'
            else if (getset.eq.'E') then
                if (valk .eq. 'FIXE') then
                    zr(jatpr-1+latpr*(iadapt-1)+1) = 1
                else if (valk.eq.'DELTA_GRANDEUR') then
                    zr(jatpr-1+latpr*(iadapt-1)+1) = 2
                else if (valk.eq.'ITER_NEWTON') then
                    zr(jatpr-1+latpr*(iadapt-1)+1) = 3
                else if (valk.eq.'FORMULE') then
                    zr(jatpr-1+latpr*(iadapt-1)+1) = 4
                else if (valk.eq.'IMPLEX') then
                    zr(jatpr-1+latpr*(iadapt-1)+1) = 5
                else
                    call assert(.false.)
                endif
            endif
!
        else if (quest.eq.'PCENT_AUGM') then
            if (getset .eq. 'L') then
                valr = zr(jatpr-1+latpr*(iadapt-1)+2)
            else if (getset.eq.'E') then
                zr(jatpr-1+latpr*(iadapt-1)+2) = valr
            endif
!
        else if (quest.eq.'VALE_REF') then
            if (getset .eq. 'L') then
                valr = zr(jatpr-1+latpr*(iadapt-1)+3)
            else if (getset.eq.'E') then
                zr(jatpr-1+latpr*(iadapt-1)+3) = valr
            endif
!
        else if (quest.eq.'NU_CMP') then
            if (getset .eq. 'L') then
                valr = zr(jatpr-1+latpr*(iadapt-1)+4)
                vali = nint(valr)
            else if (getset.eq.'E') then
                zr(jatpr-1+latpr*(iadapt-1)+4) = vali
            endif
!
        else if (quest.eq.'NB_ITER_NEWTON_REF') then
            if (getset .eq. 'L') then
                valr = zr(jatpr-1+latpr*(iadapt-1)+5)
                vali = nint(valr)
            else if (getset.eq.'E') then
                zr(jatpr-1+latpr*(iadapt-1)+5) = vali
            endif
!
        else if (quest.eq.'NOM_CHAM') then
            if (getset .eq. 'L') then
                valk = zk16(jatpk-1+latpk*(iadapt-1)+2)
            else if (getset.eq.'E') then
                zk16(jatpk-1+latpk*(iadapt-1)+2) = valk
            endif
!
        else if (quest.eq.'NOM_CMP') then
            if (getset .eq. 'L') then
                valk = zk16(jatpk-1+latpk*(iadapt-1)+3)
            else if (getset.eq.'E') then
                zk16(jatpk-1+latpk*(iadapt-1)+3) = valk
            endif
!
        else
            call assert(.false.)
!
        endif
!
    endif
!
    call jedema()
end subroutine
