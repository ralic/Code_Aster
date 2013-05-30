subroutine nmextn(typcha, extrcp, extrga, extrch, nbno,&
                  nbma, nbcmp, nbpi, nbspi, nbext)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit      none
    include 'asterfort/nmexto.h'
    integer :: nbext
    character(len=4) :: typcha
    integer :: nbno, nbma, nbcmp, nbpi, nbspi
    character(len=8) :: extrcp, extrga, extrch
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (EXTRACTION - LECTURE)
!
! DECOMPTE DES POINTS D'EXTRACTION
!
! ----------------------------------------------------------------------
!
!
! IN  TYPCHA : TYPE DU CHAMP 'NOEU' OU 'ELGA'
! IN  EXTRCP : TYPE D'EXTRACTION SUR LES COMPOSANTES
!               ' ' POUR LES VALEURS OU NOM DE LA FORMULE
! IN  EXTRGA : TYPE D'EXTRACTION SUR UNE MAILLE
! IN  EXTRCH : TYPE D'EXTRACTION SUR LE CHAMP
! IN  NBNO   : LONGUEUR DE LA LISTE DES NOEUDS (-1 SI TOUS NOEUDS)
! IN  NBMA   : LONGUEUR DE LA LISTE DES MAILLES (-1 SI TOUTES MAILLES)
! IN  NBPI   : NOMBRE DE POINTS D'INTEGRATION
! IN  NBSPI  : NOMBRE DE SOUS-POINTS D'INTEGRATION
! IN  NCMP   : NOMBRE DE COMPOSANTES
! OUT NBEXT  : NOMBRE TOTAL D'EXTRACTIONS VALIDES POUR CETTE OCCURRENCE
!
! ----------------------------------------------------------------------
!
    integer :: nfor, npoin, nlieu
!
! ----------------------------------------------------------------------
!
!
! --- INITIALISATIONS
!
    nbext = 0
!
! --- NOMBRE DE COMPOSANTES A EXTRAIRE
!
    call nmexto('COMP', typcha, extrcp, extrga, extrch,&
                nbno, nbma, nbcmp, nbpi, nbspi,&
                nfor)
!
! --- NOMBRE DE POINTS A EXTRAIRE
!
    call nmexto('POIN', typcha, extrcp, extrga, extrch,&
                nbno, nbma, nbcmp, nbpi, nbspi,&
                npoin)
!
! --- NOMBRE DE LIEUX A EXTRAIRE
!
    call nmexto('LIEU', typcha, extrcp, extrga, extrch,&
                nbno, nbma, nbcmp, nbpi, nbspi,&
                nlieu)
!
! --- NOMBRE D'EXTRACTIONS
!
    nbext = nlieu * npoin * nfor
!
end subroutine
