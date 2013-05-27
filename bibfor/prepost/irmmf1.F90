subroutine irmmf1(fid, nomamd, typent, nbrent, nbgrou,&
                  nomgen, nufaen, nomast, prefix, typgeo,&
                  nomtyp, nmatyp, infmed, nivinf, ifm)
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
! person_in_charge: nicolas.sellenet at edf.fr
!-----------------------------------------------------------------------
!     ECRITURE DU MAILLAGE - FORMAT MED - LES FAMILLES - 1
!        -  -     -                 -         -          -
!-----------------------------------------------------------------------
!     L'ENSEMBLE DES FAMILLES EST L'INTERSECTION DE L'ENSEMBLE
!     DES GROUPES : UN ENTITE/MAILLE APPARAIT AU PLUS DANS 1 FAMILLE
!     TABLE  NUMEROS DES FAMILLES POUR LES ENTITES  <-> TABLE  DES COO
!     TABLES NUMEROS DES FAMILLES POUR MAILLE/TYPE <-> TABLES DES CNX
!     PAR CONVENTION, LES FAMILLES DE ENTITES SONT NUMEROTEES >0 ET LES
!     FAMILLES DE MAILLES SONT NUMEROTEES <0. LA FAMILLE NULLE EST
!     DESTINEE AUX ENTITES / ELEMENTS SANS FAMILLE.
!     ENTREE:
!   FID    : IDENTIFIANT DU FICHIER MED
!   NOMAMD : NOM DU MAILLAGE MED
!   TYPENT : TYPE D'ENTITES : 0, POUR DES NOEUDS, 1 POUR DES MAILLES
!   NBRENT : NOMBRE D'ENTITES A TRAITER
!   NBGROU : NOMBRE DE GROUPES D'ENTITES
!   NOMGEN : VECTEUR NOMS DES GROUPES D'ENTITES
!   NOMAST : NOM DU MAILLAGE ASTER
!   PREFIX : PREFIXE POUR LES TABLEAUX DES RENUMEROTATIONS
!   TYPGEO : TYPE MED POUR CHAQUE MAILLE
!   NOMTYP : NOM DES TYPES POUR CHAQUE MAILLE
!   NMATYP : NOMBRE DE MAILLES PAR TYPE
! TABLEAUX DE TRAVAIL
!   NUFAEN : NUMERO DE FAMILLE POUR CHAQUE ENTITE
!            PAR DEFAUT, L'ALLOCATION AVEC JEVEUX A TOUT MIS A 0. CELA
!            SIGNIFIE QUE LES ENTITES APPARTIENNENT A LA FAMILLE NULLE.
! DIVERS
!   INFMED : NIVEAU DES INFORMATIONS SPECIFIQUES A MED A IMPRIMER
!   NIVINF : NIVEAU DES INFORMATIONS GENERALES
!   IFM    : UNITE LOGIQUE DU FICHIER DE MESSAGE
!-----------------------------------------------------------------------
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
    include 'jeveux.h'
    include 'asterfort/irmmf2.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/wkvect.h'
    integer :: fid
    integer :: typent, nbrent, nbgrou
    integer :: nufaen(nbrent)
    integer :: typgeo(*), nmatyp(*)
    integer :: infmed
    integer :: ifm, nivinf
!
    character(len=6) :: prefix
    character(len=8) :: nomast
    character(len=24) :: nomgen(*)
    character(len=8) :: nomtyp(*)
    character(len=*) :: nomamd
!
! 0.2. ==> COMMUNS
!
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'IRMMF1' )
!
    integer :: tygeno
    parameter (tygeno=0)
!
    integer :: iaux
    integer :: nbec
    integer :: adtabx, adnufa, adnogr, adnofe
!
    real(kind=8) :: raux
!
    character(len=7) :: saux07
    character(len=24) :: nufacr, nogrfa, nofaex, tabaux
!
!====
! 1. S'IL EXISTE DES GROUPES, ON ALLOUE DES TABLEAUX DE TRAVAIL POUR
!    POUVOIR CONSTRUIRE LES FAMILLES
!====
!
    if (nivinf .ge. 2) then
        write (ifm,1001) nompro
        1001 format( 60('='),/,'DEBUT DU PROGRAMME ',a)
    endif
!
    if (infmed .ge. 2) then
!
        if (typent .eq. tygeno) then
            saux07 = 'NOEUDS '
        else
            saux07 = 'MAILLES'
        endif
        write (ifm,1002) saux07, saux07, nbrent, nbgrou
        1002 format( /,'CONSTRUCTION DES FAMILLES DE ',a,&
     &        /,'. NOMBRE DE ',a,' :',i12,&
     &        /,'. NOMBRE DE GROUPES :',i5)
!
    endif
!
    if (nbgrou .ne. 0) then
!
!====
! 2. S'IL EXISTE DES GROUPES, ON ALLOUE DES TABLEAUX DE TRAVAIL POUR
!    POUVOIR CONSTRUIRE LES FAMILLES
!====
!                 12   345678   9012345678901234
        nufacr = '&&'//nompro//'.NU_FAM_CRE     '
        nogrfa = '&&'//nompro//'.NOM_GR_FAM     '
        nofaex = '&&'//nompro//'.NOM_FAM_EX     '
        tabaux = '&&'//nompro//'.TABL_AUXIL     '
!
!       VECTEUR NUMEROS DES FAMILLES D'ENTITES CREES = NBRENT MAX
        call wkvect(nufacr, 'V V I', nbrent, adnufa)
!
!       VECTEUR NOMS DES GROUPES D'ENTITES / FAMILLE = NB GRP MAX
        call wkvect(nogrfa, 'V V K80', nbgrou, adnogr)
!
!       VECTEUR NOMS DES FAMILLES = NB FAMILLE MAX
!       AU PIRE, IL Y A UNE FAMILLE PAR ENTITE. MAIS EN FAIT, C'EST
!       UNE PARTITION SELON LES GROUPES : IL Y EN A AU PIRE 2**NBGROU-1
!       ON CHOISIT DONC LE MIN DES 2
!       POURQUOI 2**NBGROU-1 ?
!       SOIT L'ENTITE APPARTIENT A 1 GROUPE  ==> NBGROU CHOIX
!       SOIT L'ENTITE APPARTIENT A 2 GROUPES ==> ARR(NBGROU,2) CHOIX
!       SOIT L'ENTITE APPARTIENT A 3 GROUPES ==> ARR(NBGROU,3) CHOIX
!       ...
!       SOIT L'ENTITE APPARTIENT A (NBGROU-1) GROUPES
!                                        ==> ARR(NBGROU,NBGROU-1) CHOIX
!       SOIT L'ENTITE APPARTIENT AUX NBGROU GROUPES ==> 1 CHOIX
!       AU TOTAL : NBGROU + ARR(NBGROU,2) + ARR(NBGROU,3) + ...
!                     ... + ARR(NBGROU,NBGROU-1) + 1
!       ON REMARQUE QUE CE SONT LES COEFFICIENTS D'UNE LIGNE DU TRIANGLE
!       DE PASCAL (HONNEUR AUX AUVERGNATS)
!       DONC LA SOMME VAUT (1+1)*NBGROU-1 - 1
!
        raux = log(dble(nbrent)) / log(2.d0)
        if (nbgrou .lt. int(raux)) then
            iaux = 2**nbgrou - 1
        else
            iaux = nbrent
        endif
        call wkvect(nofaex, 'V V K80', iaux, adnofe)
!
!       ON UTILISE DES TABLEAUX DE BITS POUR ENREGISTRER LA PRESENCE
!       D'UNE ENTITE DANS UN GROUPE : 30 PAR ENTIER INT*4 NBEC ENTIERS
!
        nbec = (nbgrou-1) / 30 + 1
!       COLOSSAL ALLOC DU TABLEAU CROISE DES ENTITES X GROUPES
!
        call wkvect(tabaux, 'V V I', nbrent*nbec, adtabx)
!
!====
! 3. CREATION ET ECRITURE DES FAMILLES
!====
!
        call irmmf2(fid, nomamd, typent, nbrent, nbgrou,&
                    nomgen, nbec, nomast, prefix, typgeo,&
                    nomtyp, nmatyp, nufaen, zi( adnufa), zk80(adnogr),&
                    zk80(adnofe), zi(adtabx), infmed, nivinf, ifm)
!
!====
! 4. MENAGE
!====
!
        call jedetr(nufacr)
        call jedetr(nogrfa)
        call jedetr(nofaex)
        call jedetr(tabaux)
!
    endif
!
    if (nivinf .ge. 2) then
!
        write (ifm,4001) nompro
        4001 format(/,'FIN DU PROGRAMME ',a,/,60('-'))
!
    endif
!
end subroutine
