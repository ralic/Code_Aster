subroutine dfllsv(lisifr, lisevr, lisevk, lisesu, isauve,&
                  even, action, submet, subaut, pasmin,&
                  nbrpas, niveau, pcplus, cmmaxi, delcol,&
                  durdec, penmax, cricmp, valere, nocham,&
                  nocmp)
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
! TOLE CRP_21
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/dfllvd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/u2mess.h'
    character(len=24) :: lisifr, lisevr, lisevk, lisesu
    integer :: isauve
    real(kind=8) :: pasmin, pcplus, penmax
    character(len=16) :: even, action
    character(len=16) :: submet, subaut
    integer :: nbrpas, niveau
    real(kind=8) :: valere
    character(len=16) :: nocham, nocmp, cricmp
    real(kind=8) :: cmmaxi, delcol, durdec
!
! ----------------------------------------------------------------------
!
! OPERATEUR DEFI_LIST_INST
!
! ECRITURE DES INFORMATIONS DANS LA SD LIST
!
! ----------------------------------------------------------------------
!
!
! IN  LISIFR : NOM DE L'OBJET INFOS SUR LA LISTE
! IN  LISEVR : NOM DE L'OBJET INFOS ECHEC - REELS
! IN  LISEVK : NOM DE L'OBJET INFOS ECHEC - CHAINES
! IN  LISESU : NOM DE L'OBJET INFOS ECHEC - OPTIONS DECOUPE
! IN  ISAUVE : OCCURRENCE POUR LA SAUVEGARDE
! IN  EVEN   : NOM DE L'EVENEMENT
! IN  ACTION : NOM DE L'ACTION
! IN  SUBMET : TYPE DE SUBDIVISION
! IN  SUBAUT : TYPE DE SUBDIVISION AUTOMATIQUE
! IN  PASMIN : VALEUR DE SUBD_PAS_MINI
! IN  NBRPAS : VALEUR DE SUBD_PAS
! IN  NIVEAU : VALEUR DE SUBD_NIVEAU
! IN  PCPLUS : VALEUR DE PCENT_ITER_PLUS
! IN  PENMAX : VALEUR DE PENE_MAXI
! IN  CRICMP : VALEUR DE CRIT_COMP
! IN  VALERE : VALEUR DE VALE_REF
! IN  NOCHAM : VALEUR DE NOM_CHAM
! IN  NOCMP  : VALEUR DE NOM_CMP
! IN  CMMAXI : VALEUR DE COEF_MULT_MAXI
! IN  DELCOL : VALEUR DE DELTAT_COLLISION
! IN  DURDEC : VALEUR DE DUREE_DECOUPE
!
!
!
!
    integer :: leevr, leevk, lesur
    integer :: jlinr, jeevr, jeevk, jesur
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- TAILLE DES VECTEURS
!
    leevr = dfllvd('LEEVR')
    leevk = dfllvd('LEEVK')
    lesur = dfllvd('LESUR')
!
! --- ACCES SD
!
    call jeveuo(lisifr, 'E', jlinr)
    call jeveuo(lisevr, 'E', jeevr)
    call jeveuo(lisevk, 'E', jeevk)
    call jeveuo(lisesu, 'E', jesur)
!
! --- ALARME SI LE RE-DECOUPAGE EN CAS D'ERREUR EST DEBRAYE
!
    if (even .eq. 'ERRE') then
        if (action .eq. 'ARRET') call u2mess('I', 'DISCRETISATION_9')
    endif
!
! --- AU MOINS UNE ACTION DE SOUS-DECOUPAGE A ETE DEFINIE
!
    if (action .eq. 'DECOUPE') then
        zr(jlinr-1+7) = 1.d0
    endif
!
! --- ACTION DE REACTUALISATION DU PRECONDITIONNEUR LDLT_SP ACTIVEE
!
    if (action .eq. 'REAC_PRECOND') then
        zr(jlinr-1+11) = 1.d0
    endif
!
! --- TYPE D'EVENEMENT
!
    if (even .eq. 'ERRE') then
        zr(jeevr-1+leevr*(isauve-1)+1) = 0.d0
    else if (even.eq.'DELTA_GRANDEUR') then
        zr(jeevr-1+leevr*(isauve-1)+1) = 1.d0
    else if (even.eq.'COLLISION') then
        zr(jeevr-1+leevr*(isauve-1)+1) = 2.d0
    else if (even.eq.'INTERPENETRATION') then
        zr(jeevr-1+leevr*(isauve-1)+1) = 3.d0
    else if (even.eq.'DIVE_RESI') then
        zr(jeevr-1+leevr*(isauve-1)+1) = 4.d0
    else if (even.eq.'INSTABILITE') then
        zr(jeevr-1+leevr*(isauve-1)+1) = 5.d0
    else
        call assert(.false.)
    endif
!
! --- ACTION SI EVENEMENT DECLENCHE
!
    if (action .eq. 'ARRET') then
        zr(jeevr-1+leevr*(isauve-1)+2) = 0.d0
    else if (action.eq.'REAC_PRECOND') then
        zr(jeevr-1+leevr*(isauve-1)+2) = 1.d0
    else if (action.eq.'DECOUPE') then
        zr(jeevr-1+leevr*(isauve-1)+2) = 2.d0
    else if (action.eq.'ITER_SUPPL') then
        zr(jeevr-1+leevr*(isauve-1)+2) = 3.d0
    else if (action.eq.'AUTRE_PILOTAGE') then
        zr(jeevr-1+leevr*(isauve-1)+2) = 4.d0
    else if (action.eq.'ADAPT_COEF_PENA') then
        zr(jeevr-1+leevr*(isauve-1)+2) = 5.d0
    else if (action.eq.'CONTINUE') then
        zr(jeevr-1+leevr*(isauve-1)+2) = 6.d0
    else
        call assert(.false.)
    endif
!
! --- EVENEMENT 'DELTA_GRANDEUR' - PARAMETRES
!
    if (even .eq. 'DELTA_GRANDEUR') then
        zr (jeevr-1+leevr*(isauve-1)+5) = valere
        zk16(jeevk-1+leevk*(isauve-1)+1) = nocham
        zk16(jeevk-1+leevk*(isauve-1)+2) = nocmp
        zk16(jeevk-1+leevk*(isauve-1)+3) = cricmp
    endif
!
! --- EVENEMENT 'INTERPENETRATION' - PARAMETRES
!
    if (even .eq. 'INTERPENETRATION') then
        zr(jeevr-1+leevr*(isauve-1)+6) = penmax
    endif
!
! --- ACTION 'DECOUPE' - PARAMETRES
!
    if (submet .eq. 'MANUEL') then
        zr(jesur-1+lesur*(isauve-1)+1) = 1.d0
        zr(jesur-1+lesur*(isauve-1)+2) = nbrpas
        zr(jesur-1+lesur*(isauve-1)+3) = pasmin
        zr(jesur-1+lesur*(isauve-1)+4) = niveau
    else if (submet.eq.'AUTO') then
        zr(jesur-1+lesur*(isauve-1)+1) = 2.d0
        zr(jesur-1+lesur*(isauve-1)+3) = pasmin
        zr(jesur-1+lesur*(isauve-1)+5) = delcol
        zr(jesur-1+lesur*(isauve-1)+6) = durdec
        if (subaut .eq. 'COLLISION') then
            zr(jesur-1+lesur*(isauve-1)+10) = 1.d0
        else if (subaut.eq.'EXTRAPOLE') then
            zr(jesur-1+lesur*(isauve-1)+10) = 2.d0
        else
            call assert(.false.)
        endif
    endif
!
! --- ACTION 'ITER_SUPPL' - PARAMETRES
!
    if (action .eq. 'ITER_SUPPL') then
        zr(jesur-1+lesur*(isauve-1)+7) = pcplus
    endif
!
! --- ACTION 'ADAPT_COEF_PENA' - PARAMETRES
!
    if (action .eq. 'ADAPT_COEF_PENA') then
        zr(jesur-1+lesur*(isauve-1)+8) = cmmaxi
    endif
!
    call jedema()
!
end subroutine
