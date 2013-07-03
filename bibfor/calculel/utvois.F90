subroutine utvois(typmac, lmaj, nbf, nsomf, poinc1,&
                  poinc2, elrefe, ndegre)
!-----------------------------------------------------------------------
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
! person_in_charge: olivier.boiteau at edf.fr
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:  CALCUL DES CARACTERISTIQUES DES MAILLES
!                          VOISINES POUR AERER TE0003 ET RECUPERATION
!                          DES ADRESSES JEVEUX DE LEUR GEOMETRIE/FFORME
!
! IN TYPMAC  : TYPE DE L'ELEMENT COURANT.
! IN LMAJ    : FLAG EGALE A TRUE SI ON A UNE SOURCE NON NULLE.
! OUT NBF    : NBRE D'ARETES (EN 2D) OU DE FACES (EN 3D)
! OUT NSOMF  : NBRE DE NOEUDS SOMMETS DE L'ARETE (EN 2D) OU DE(S)
!              (LA) FACE(S) (EN 3D).
! OUT POINC1/2 : POIDS DE NEWTON-COTES (POUR LES ARETES EN 2D)
! OUT ELREFE : TYPE D'ARETES OU DE(S) FACE(S)
! OUT NDEGRE   : DEGRE D'INTERPOLATION
!   -------------------------------------------------------------------
!     SUBROUTINES APPELLEES:
!       JEVEUX: JEEXIN,JEVEUO.
!     FONCTIONS INTRINSEQUES:
!       AUCUNE.
!   -------------------------------------------------------------------
!     ASTER INFORMATIONS:
!       20/09/01 (OB): CREATION POUR SIMPLIFIER TE0003.F.
!       05/02/02 (OB): EXTENSION AUX EFS LUMPES.
!       03/07/09 (GN): DEPLACEMENT DU 3D
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/u2mess.h"
    integer :: nbf, nsomf, ndegre
    real(kind=8) :: poinc1, poinc2
    logical :: lmaj
    character(len=8) :: elrefe, typmac
!
!
! DECLARATION VARIABLES LOCALES
    character(len=1) :: noeu
    character(len=2) :: form
!
    elrefe = '        '
!               12345678
!
! CAS 2D
!
    form = typmac(1:2)
! CALCUL NBRE ARETES
    call assert(form.eq.'TR'.or.form.eq.'QU')
    if (form .eq. 'TR') then
        nbf = 3
    else if (form.eq.'QU') then
        nbf = 4
    endif
    noeu = typmac(5:5)
! CALCUL NBRE SOMMETS ARETES ET POIDS DE NEWTON-COTES DE L'ARETE
    call assert(noeu .eq. '6' .or. noeu&
                .eq. '8' .or. noeu .eq.&
                '9' .or. noeu .eq. '3'&
                .or. noeu .eq. '4')
    if (noeu .eq. '6' .or. noeu .eq. '8' .or. noeu .eq. '9') then
        nsomf = 3
        ndegre = 2
        elrefe = 'SEG3  '
! INIT. POIDS DE NEWTON-COTES (POINTS EXTREMES 1/POINT CENTRAL 2)
        poinc1 = 1.d0/3.d0
        poinc2 = 4.d0/3.d0
    else if (noeu.eq.'3' .or. noeu.eq.'4') then
        nsomf = 2
        ndegre = 1
        elrefe = 'SEG2  '
        poinc1 = 1.d0
        poinc2 = 0.d0
    endif
!
!
! MAUVAIS CALCUL EN P1 SI FORCE VOLUMIQUE NON NULLE
    if ((ndegre.eq.1) .and. lmaj) call u2mess('A', 'CALCULEL5_34')
end subroutine
