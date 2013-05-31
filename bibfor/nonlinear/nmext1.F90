subroutine nmext1(noma, champ, typcha, nomcha, nomchs,&
                  nbma, nbno, nbpi, nbspi, nbcmp,&
                  extrga, extrch, extrcp, listno, listma,&
                  listpi, listsp, listcp, chnoeu, chgaus,&
                  chelga)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
! aslint: disable=W1504
    implicit      none
    include 'asterfort/nmext2.h'
    include 'asterfort/nmext3.h'
    character(len=8) :: noma
    character(len=24) :: nomcha, nomchs
    integer :: nbcmp, nbno, nbma
    integer :: nbpi, nbspi
    character(len=19) :: champ
    character(len=4) :: typcha
    character(len=24) :: listno, listma, listpi, listcp, listsp
    character(len=8) :: extrga, extrch, extrcp
    character(len=19) :: chgaus, chnoeu, chelga
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (EXTRACTION - UTILITAIRE)
!
! EXTRAIRE LES VALEURS ET STOCKAGE DANS VECTEURS TEMPORAIRES
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  TYPCHA : TYPE DU CHAMP
! IN  NOMCHA : NOM DU CHAMP
! IN  NOMCHS : NOM DU CHAMP SIMPLE
! IN  CHAMP  : CHAMP OBSERVE
! IN  NBCMP  : NOMBRE DE COMPOSANTES DANS LA SD
! IN  NBNO   : NOMBRE DE NOEUDS DANS LA SD
! IN  NBMA   : NOMBRE DE MAILLES DANS LA SD
! IN  NBPI   : NOMBRE DE POINTS D'INTEGRATION
! IN  NBSPI  : NOMBRE DE SOUS-POINTS D'INTEGRATION
! IN  EXTRGA : TYPE D'EXTRACTION SUR UNE MAILLE
! IN  EXTRCH : TYPE D'EXTRACTION SUR LE CHAMP
! IN  EXTRCP : TYPE D'EXTRACTION SUR LES COMPOSANTES
! IN  LISTNO : LISTE CONTENANT LES NOEUDS
! IN  LISTMA : LISTE CONTENANT LES MAILLES
! IN  LISTCP : LISTE DES COMPOSANTES
! IN  LISTPI : LISTE CONTENANT LES POINTS D'EXTRACTION
! IN  LISTSP : LISTE CONTENANT LES SOUS-POINTS D'EXTRACTION
! IN  CHNOEU : VECTEUR DE TRAVAIL CHAMPS AUX NOEUDS
! IN  CHELGA : VECTEUR DE TRAVAIL CHAMPS AUX ELEMENTS
! IN  CHGAUS : VECTEUR DE TRAVAIL CHAMPS AUX POINTS DE GAUSS
!
! ----------------------------------------------------------------------
!
!
!
! --- VALEUR NODALES
!
    if (typcha .eq. 'NOEU') then
        call nmext2(noma, champ, nbcmp, nbno, extrch,&
                    extrcp, listno, listcp, chnoeu)
    endif
!
! --- VALEURS AUX POINTS DE GAUSS
!
    if (typcha .eq. 'ELGA') then
        call nmext3(noma, champ, nomcha, nomchs, nbcmp,&
                    nbma, nbpi, nbspi, extrga, extrch,&
                    extrcp, listma, listpi, listsp, listcp,&
                    chgaus, chelga)
    endif
!
end subroutine
