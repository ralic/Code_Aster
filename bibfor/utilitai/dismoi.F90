subroutine dismoi(arret, questi, nomob, typeco, repi,&
                  repk, ierd)
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/dismca.h'
    include 'asterfort/dismce.h'
    include 'asterfort/dismch.h'
    include 'asterfort/dismcm.h'
    include 'asterfort/dismcn.h'
    include 'asterfort/dismco.h'
    include 'asterfort/dismcp.h'
    include 'asterfort/dismcr.h'
    include 'asterfort/dismct.h'
    include 'asterfort/dismes.h'
    include 'asterfort/dismff.h'
    include 'asterfort/dismgd.h'
    include 'asterfort/dismic.h'
    include 'asterfort/dismlg.h'
    include 'asterfort/dismli.h'
    include 'asterfort/dismma.h'
    include 'asterfort/dismme.h'
    include 'asterfort/dismml.h'
    include 'asterfort/dismmo.h'
    include 'asterfort/dismms.h'
    include 'asterfort/dismne.h'
    include 'asterfort/dismns.h'
    include 'asterfort/dismnu.h'
    include 'asterfort/dismph.h'
    include 'asterfort/dismpm.h'
    include 'asterfort/dismpn.h'
    include 'asterfort/dismre.h'
    include 'asterfort/dismrs.h'
    include 'asterfort/dismte.h'
    include 'asterfort/dismtm.h'
    include 'asterfort/dismxf.h'
    include 'asterfort/u2mesk.h'
    integer :: repi, ierd
    character(len=*) :: arret, questi, nomob, typeco, repk
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     IN:
!       ARRET  : 'F' : ERREUR FATALE SI IERD /=0
!                'C' : ON CONTINUE MEME SI IERD /=0
!       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
!       NOMOB  : NOM D'UN OBJET DE CONCEPT DONNE
!       TYPECO : TYPE DU CONCEPT NOMOB
!     OUT:
!       REPI   : REPONSE ( SI ENTIERE )
!       REPK   : REPONSE ( SI CHAINE DE CARACTERES )
!       IERD   : CODE RETOUR (0--> OK, 1--> PB )
!
!     ------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    character(len=32) :: nomo1, repk1
    character(len=24) :: typec1, quest1
    character(len=1) :: arret2
    integer :: repi1, ierd1
!
! DEB-------------------------------------------------------------------
!
    arret2=arret
    call assert(arret2.eq.'F'.or.arret2.eq.'C')
!
    repi1=99999
    ierd1=0
    repk1='????????'
!
!     --ON RECOPIE LES ARGUMENTS "CARACTERE" EN ENTREE
!       DANS DES VARIABLES DE LONGUEUR FIXE (K24) :
!
!
    typec1 = typeco
    nomo1 = nomob
    quest1 = questi
!
!     -- SUIVANT LE TYPE DU CONCEPT, ON APPELLE DIFFERENTS DISMIJ:
!
    if (typec1 .eq. 'MATR_ASSE') then
        call dismms(quest1, nomo1(1:19), repi1, repk1, ierd1)
    else if (typec1.eq.'RESULTAT') then
        call dismrs(quest1, nomo1(1:8), repi1, repk1, ierd1)
    else if (typec1.eq.'CATALOGUE') then
        call dismct(quest1, nomo1(1:1), repi1, repk1, ierd1)
    else if (typec1.eq.'INCONNU') then
        call dismic(quest1, nomo1(1:19), repi1, repk1, ierd1)
    else if (typec1.eq.'MACR_ELEM_STAT') then
        call dismml(quest1, nomo1(1:8), repi1, repk1, ierd1)
    else if (typec1.eq.'CHAM_MATER') then
        call dismcm(quest1, nomo1(1:8), repi1, repk1, ierd1)
    else if (typec1.eq.'CARA_ELEM') then
        call dismcr(quest1, nomo1(1:8), repi1, repk1, ierd1)
    else if (typec1.eq.'CHAM_NO') then
        call dismcn(quest1, nomo1(1:19), repi1, repk1, ierd1)
    else if (typec1.eq.'CHAM_NO_S') then
        call dismns(quest1, nomo1(1:19), repi1, repk1, ierd1)
    else if (typec1.eq.'CARTE') then
        call dismca(quest1, nomo1(1:19), repi1, repk1, ierd1)
    else if (typec1.eq.'CHAMP') then
        call dismcp(quest1, nomo1(1:19), repi1, repk1, ierd1)
    else if (typec1.eq.'GRANDEUR') then
        call dismgd(quest1, nomo1(1:8), repi1, repk1, ierd1)
    else if (typec1.eq.'PHENOMENE') then
        call dismph(quest1, nomo1(1:16), repi1, repk1, ierd1)
    else if (typec1.eq.'PHEN_MODE') then
        call dismpm(quest1, nomo1(1:32), repi1, repk1, ierd1)
    else if (typec1.eq.'NUME_DDL') then
        call dismnu(quest1, nomo1(1:14), repi1, repk1, ierd1)
    else if (typec1.eq.'PROF_CHNO') then
        call dismpn(quest1, nomo1(1:19), repi1, repk1, ierd1)
    else if (typec1.eq.'MATR_ELEM') then
        call dismme(quest1, nomo1(1:19), repi1, repk1, ierd1)
    else if (typec1.eq.'VECT_ELEM') then
        call dismme(quest1, nomo1(1:19), repi1, repk1, ierd1)
    else if (typec1.eq.'LIGREL') then
        call dismlg(quest1, nomo1(1:19), repi1, repk1, ierd1)
    else if (typec1.eq.'MAILLAGE') then
        call dismma(quest1, nomo1(1:8), repi1, repk1, ierd1)
    else if (typec1.eq.'CHARGE') then
        call dismch(quest1, nomo1(1:8), repi1, repk1, ierd1)
    else if (typec1.eq.'MODELE') then
        call dismmo(quest1, nomo1(1:8), repi1, repk1, ierd1)
    else if (typec1.eq.'CHAM_ELEM') then
        call dismce(quest1, nomo1(1:19), repi1, repk1, ierd1)
    else if (typec1.eq.'CHAM_ELEM_S') then
        call dismes(quest1, nomo1(1:19), repi1, repk1, ierd1)
    else if (typec1.eq.'RESUELEM') then
        call dismre(quest1, nomo1(1:19), repi1, repk1, ierd1)
    else if (typec1.eq.'INTERF_DYNA') then
        call dismli(quest1, nomo1(1:8), repi1, repk1, ierd1)
    else if (typec1.eq.'NUME_EQUA') then
        call dismne(quest1, nomo1(1:19), repi1, repk1, ierd1)
    else if (typec1.eq.'TYPE_ELEM') then
        call dismte(quest1, nomo1(1:16), repi1, repk1, ierd1)
    else if (typec1.eq.'TYPE_MAILLE') then
        call dismtm(quest1, nomo1(1:8), repi1, repk1, ierd1)
    else if (typec1.eq.'FISS_XFEM') then
        call dismxf(quest1, nomo1(1:8), repi1, repk1, ierd1)
    else if (typec1.eq.'FOND_FISS') then
        call dismff(quest1, nomo1(1:8), repi1, repk1, ierd1)
    else if (typec1.eq.'CARTE_COMPOR') then
        call dismco(quest1, nomo1(1:19), repi1, repk1, ierd1)
    else
        if (arret2 .eq. 'F') then
            repk1 = typeco
            call u2mesk('F', 'UTILITAI_65', 1, repk1)
        endif
    endif
!
!
!     -- ON NE DOIT PAS SORTIR DE DISMOI SI IERD1/=0 ET ARRET='F'
    if (ierd1 .ne. 0 .and. arret2 .eq. 'F') call assert(.false.)
!
    repk = repk1
    repi = repi1
    ierd = ierd1
end subroutine
