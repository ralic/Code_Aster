subroutine greihm(perman, ndim, mecani, press1, press2,&
                  tempe, dimdef, dimcon)
    implicit none
#include "asterf_types.h"
#include "asterfort/lteatt.h"
!
    aster_logical :: perman
    integer :: mecani(8), press1(9), press2(9), tempe(5)
    integer :: dimdef, dimcon
    integer :: ndim
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   TABLEAU MECANI :
!   MECANI(1) = 1 : IL Y A UNE EQUATION MECANIQUE
!               0 : SINON
!   MECANI(2) = ADRESSE DANS LES TABLEAUX DES DEFORMATIONS
!               GENERALISEES AU POINT DE GAUSS DES
!               DEFORMATIONS CORRESPONDANT A LA MECANIQUE
!   MECANI(3) = ADRESSE DANS LES TABLEAUX DES CONTRAINTES
!               GENERALISEES AU POINT DE GAUSS DES
!               CONTRAINTES CORRESPONDANT A LA MECANIQUE
!   MECANI(4) = ADRESSE DANS LES TABLEAUX DES DÉFORMATIONS
!               GENERALISEES AU POINT DE GAUSS DES
!               DEFORMATION CORRESPONDANT A LA CONTRAINTE
!               MECANIQUE
!   MECANI(5) = ADRESSE DANS LES TABLEAUX DES CONTRAINTES
!               GENERALISEES AU POINT DE GAUSS DES
!               CONTRAINTES CORRESPONDANT A LA CONTRAINTE
!               MECANIQUE
!   MECANI(6) = NOMBRE DE DEFORMATIONS MECANIQUES
!   MECANI(7) = NOMBRE DE CONTRAINTES MECANIQUES
!   MECANI(8) = NOMBRE DE CONTRAINTES MECANIQUES LAGRANGE
!
!   TABLEAU PRESS1 :
!   PRESS1(1) = 1 : IL Y A UNE EQUATION SUR LA PREMIERE PRESSION
!               0 : SINON
!   PRESS1(2) = NOMBRE DE PHASES POUR LE CONSTITUANT 1
!   PRESS1(3) = ADRESSE DANS LES TABLEAUX DES DEFORMATIONS
!               GENERALISEES AU POINT DE GAUSS
!   PRESS1(4) = ADRESSE DANS LES TABLEAUX DES DEFORMATIONS
!               GENERALISEES AU POINT DE GAUSS CONTRAINTE HYDRO
!   PRESS1(5) = ADRESSE DANS LES TABLEAUX DES CONTRAINTES
!               GENERALISEES AU POINT DE GAUSS DES CONTRAINTES
!               CORRESPONDANT A LA PREMIERE PHASE DU 1ER CONSTITUANT
!   PRESS1(6) = ADRESSE DANS LES TABLEAUX DES CONTRAINTES
!               GENERALISEES AU POINT DE GAUSS DES CONTRAINTES
!               CORRESPONDANT A LA DEUXIEME PHASE DU 1ER CONSTITUANT
!   PRESS1(7) = ADRESSE DANS LES TABLEAUX DES CONTRAINTES
!               GENERALISEES AU POINT DE GAUSS DES CONTRAINTES
!               CORRESPONDANT A LA CONTRAINTE D'EGALITE DES PRESSIONS
!   PRESS1(8) = NOMBRE DE DEFORMATIONS PRESSION
!   PRESS1(9) = NOMBRE DE CONTRAINTES POUR CHAQUE PHASE DU CONSTITUANT 1
!
!
!   TABLEAU PRESS2 :
!   PRESS2(1) = 1 : IL Y A UNE EQUATION SUR LA SECONDE PRESSION
!               0 : SINON
!   PRESS2(2) = NOMBRE DE PHASES POUR LE CONSTITUANT 2
!   PRESS2(3) = ADRESSE DANS LES TABLEAUX DES DEFORMATIONS
!               GENERALISEES AU POINT DE GAUSS
!   PRESS2(4) = ADRESSE DANS LES TABLEAUX DES CONTRAINTES
!               GENERALISEES AU POINT DE GAUSS DES CONTRAINTES
!               CORRESPONDANT A LA PREMIERE PHASE DU 2ND CONSTITUANT
!   PRESS2(5) = ADRESSE DANS LES TABLEAUX DES CONTRAINTES
!               GENERALISEES AU POINT DE GAUSS DES CONTRAINTES
!               CORRESPONDANT A LA DEUXIEME PHASE DU 2ND CONSTITUANT
!   PRESS2(7) = NOMBRE DE DEFORMATIONS PRESSION
!   PRESS2(8) = NOMBRE DE CONTRAINTES POUR CHAQUE PHASE DU CONSTITUANT 2
!
!   TABLEAU TEMPE :
!   TEMPE(1)  = 1 : IL Y A UNE EQUATION THERMIQUE
!               0 : SINON
!   TEMPE(2)  = ADRESSE DANS LES TABLEAUX DES DEFORMATIONS
!               GENERALISEES AU POINT DE GAUSS DES
!               DEFORMATIONS CORRESPONDANT A LA THERMIQUE
!   TEMPE(3)  = ADRESSE DANS LES TABLEAUX DES CONTRAINTES
!               GENERALISEES AU POINT DE GAUSS DES
!               CONTRAINTES CORRESPONDANT A LA THERMIQUE
!   TEMPE(4)  = NOMBRE DE DEFORMATIONS THERMIQUES
!   TEMPE(5)  = NOMBRE DE CONTRAINTES THERMIQUES
!
    integer :: iaux
!
!====
! 1. REPERAGE DES CALCULS A FAIRE : MECANIQUE, HYDRAULIQUE, ETC.
!====
!
! =====================================================================
! --- INITIALISATION DES GRANDEURS PRESENTES SELON LA MODELISATION ----
! --- EN THM ----------------------------------------------------------
! =====================================================================
! =====================================================================
! --- SI MODELISATION = HM --------------------------------------------
! =====================================================================
    if (lteatt('MODTHM','HM')) then
        mecani(1) = 1
        press1(1) = 1
        press2(1) = 0
        tempe(1) = 0
        press1(2) = 1
        press2(2) = 0
    endif
!
! 2. CALCUL PREALABLE DES ADRESSES LOCALES DES VARIABLES
! =====================================================================
! 2.1. LES AUTRES VALEURS DES TABLEAUX MECANI,PRESS1,PRESS2,TEMPE -----
! --- SE DEFINISSENT AUTOMATIQUEMENT : --------------------------------
! --- NOMBRE DE DEFORMATIONS ET DE CONTRAINTES DE CHAQUE PROBLEME -----
! =====================================================================
!====
!
    if (mecani(1) .eq. 1) then
        mecani(6) = ndim
        mecani(7) = ndim+1
        mecani(8) = 0
    else
        mecani(6) = 0
        mecani(7) = 0
        mecani(8) = ndim
    endif
!
!  EN MODE PERMANENT POUR LES PROBLEMES HYDRAULIQUES ET/OU THERMIQUE,
!  IL N'Y A PLUS DE VARIABLES SCALAIRES. IL NE RESTE QUE LES FLUX.
!
    if (perman) then
        iaux = 0
    else
        iaux = 1
    endif
!
    if (press1(1) .eq. 1) then
        press1(8) = ndim
        press1(9) = iaux + ndim-1
        if (tempe(1) .eq. 1) press1(9) = press1(9) + 1
    else
        press1(8) = 0
        press1(9) = 0
    endif
!
    if (press2(1) .eq. 1) then
        press2(7) = ndim
        press2(8) = iaux + ndim-1
        if (tempe(1) .eq. 1) press2(8) = press2(8) + 1
    else
        press2(7) = 0
        press2(8) = 0
    endif
!
    if (tempe(1) .eq. 1) then
        tempe(4) = 1 + ndim
        tempe(5) = 1 + ndim
    else
        tempe(4) = 0
        tempe(5) = 0
    endif
!
! =====================================================================
! 2.2. ADRESSE DES SOUS-TABLEAUX DANS LES DEFORMATIONS PHYSIQUES, LES -
!      DEFORMATIONS GENERALISEES ET LES CONTRAINTES GENERALISEES ------
! =====================================================================
!
! 2.2.1. ==> DEFORMATIONS ET CONTRAINTES EN MECANIQUE
!
    if (mecani(1) .eq. 1) then
        mecani(2) = 1
        mecani(3) = 1
    else
        mecani(2) = 0
        mecani(3) = 0
    endif
!
! 2.2.2. ==> DEFORMATIONS ET CONTRAINTES POUR LA PREMIERE PRESSION
!
    if (press1(1) .eq. 1) then
        press1(3) = mecani(6) + mecani(2)
        press1(5) = mecani(7) + mecani(3)
        press1(6) = press1(5)
        if (press1(2) .eq. 2) then
            press1(6) = press1(6) + press1(9)
        endif
    endif
!
! 2.2.3. ==> DEFORMATIONS ET CONTRAINTES POUR LA SECONDE PRESSION
!
    if (press2(1) .eq. 1) then
        press2(3) = press1(3) + press1(8)
        press2(4) = press1(5) + press1(2)*press1(9)
        press2(5) = press2(4)
        if (press2(2) .eq. 2) then
            press2(5) = press2(5) + press2(8)
        endif
    endif
!
! ADRESSES DES CONTRAINTES CORRESPONDANTS AUX MULTIPLICATEURS DE
! LAGRANGE MECANIQUES ET HYDRAULIQUES
    mecani(4)=press1(3)+press1(2)*press1(8)+press2(2)*press2(7)-ndim
    mecani(5)=press1(5)+press1(9)*press1(2)+press2(8)*press2(2)-ndim
!
    if (press1(1) .eq. 1) then
        press1(4) = mecani(4)+ ndim
        press1(7) = mecani(5)+ndim
    endif
!
    if (press2(1) .eq. 1) then
        press2(6) = press1(7) + 4
    endif
!
!
! 2.2.4. ==> DEFORMATIONS ET CONTRAINTES POUR LA TEMPERATURE
!
    if (tempe(1) .eq. 1) then
        tempe(2) = mecani(6) + press1(8) + press2(7) + 1
        tempe(3) = mecani(7) + press1(2)*press1(9) + press2(2)*press2( 8) + 1
    endif
!
! =====================================================================
! 2.3. DIMENSION DES DEPLACEMENTS, DEFORMATIONS ET CONTRAINTES --------
! =====================================================================
    dimdef=mecani(6)+press1(8)+press2(7)+tempe(4)+&
     &                     (press1(1)+press2(1))*4 +mecani(8)
    dimcon = mecani(7) + mecani(8)
    dimcon = dimcon +press1(9)*press1(2)+press2(8)*press2(2) + (press1(1)+press2(1))*4+tempe(5)
!
! =====================================================================
!
end subroutine
