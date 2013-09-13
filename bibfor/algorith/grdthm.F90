subroutine grdthm(nomte, perman, vf, ndim, mecani,&
                  press1, press2, tempe, dimdep, dimdef,&
                  dimcon, nmec, np1, np2)
    implicit none
#include "asterfort/utmess.h"
    logical :: perman, vf
    integer :: mecani(5), press1(7), press2(7), tempe(5)
    integer :: dimdep, dimdef, dimcon
    integer :: ndim, nmec, np1, np2
    character(len=16) :: nomte
! =====================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   MECANI(4) = NOMBRE DE DEFORMATIONS MECANIQUES
!   MECANI(5) = NOMBRE DE CONTRAINTES MECANIQUES
!
!   TABLEAU PRESS1 :
!   PRESS1(1) = 1 : IL Y A UNE EQUATION SUR LA PREMIERE PRESSION
!               0 : SINON
!   PRESS1(2) = NOMBRE DE PHASES POUR LE CONSTITUANT 1
!   PRESS1(3) = ADRESSE DANS LES TABLEAUX DES DEFORMATIONS
!               GENERALISEES AU POINT DE GAUSS
!   PRESS1(4) = ADRESSE DANS LES TABLEAUX DES CONTRAINTES
!               GENERALISEES AU POINT DE GAUSS DES CONTRAINTES
!               CORRESPONDANT A LA PREMIERE PHASE DU 1ER CONSTITUANT
!   PRESS1(5) = ADRESSE DANS LES TABLEAUX DES CONTRAINTES
!               GENERALISEES AU POINT DE GAUSS DES CONTRAINTES
!               CORRESPONDANT A LA DEUXIEME PHASE DU 1ER CONSTITUANT
!   PRESS1(6) = NOMBRE DE DEFORMATIONS PRESSION
!   PRESS1(7) = NOMBRE DE CONTRAINTES POUR CHAQUE PHASE DU CONSTITUANT 1
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
!   PRESS2(6) = NOMBRE DE DEFORMATIONS PRESSION
!   PRESS2(7) = NOMBRE DE CONTRAINTES POUR CHAQUE PHASE DU CONSTITUANT 2
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
!=========================================
    if (vf) then
        if (nomte(2:4) .eq. 'HH2') then
            mecani(1) = 0
            tempe(1) = 0
!
            press1(1) = 1
            press1(2) = 2
            press1(3) = 1
            press1(4) = 1
            press1(5) = 3
            press1(6) = 1+ndim
            press1(7) = 2
!
            press2(1) = 1
            press2(2) = 2
            press2(3) = 1+ndim+1
            press2(4) = 5
            press2(5) = 7
            press2(6) = 1+ndim
            press2(7) = 2
!
            dimdep = ndim*mecani(1) + press1(1) + press2(1) + tempe(1)
            dimdef = press1(6) + press2(6)
            dimcon = 8
        else
            call utmess('F', 'VOLUFINI_10', sk=nomte)
        endif
    else
!
! =====================================================================
! --- SI MODELISATION = THHM ------------------------------------------
! =====================================================================
        mecani(1)=0
        press1(1)=0
        press2(1)=0
        press1(2)=0
        press2(2)=0
        tempe(1) =0
        if (nomte(1:4) .eq. 'THHM') then
            mecani(1) = 1
            press1(1) = 1
            press2(1) = 1
            tempe(1) = 1
            press1(2) = 2
            press2(2) = 1
        endif
! ======================================================================
! --- -- SI MODELISATION = THH2M ---------------------------------------
! ======================================================================
        if (nomte(1:5) .eq. 'THH2M') then
            mecani(1) = 1
            press1(1) = 1
            press2(1) = 1
            tempe(1) = 1
            press1(2) = 2
            press2(2) = 2
        endif
! =====================================================================
! --- SI MODELISATION = HM --------------------------------------------
! =====================================================================
        if (nomte(1:2) .eq. 'HM') then
            mecani(1) = 1
            press1(1) = 1
            press2(1) = 0
            tempe(1) = 0
            press1(2) = 1
            press2(2) = 0
        endif
! =====================================================================
! --- SI MODELISATION = HHM -------------------------------------------
! =====================================================================
! --- ON RESERVE DE LA PLACE POUR LES TROIS CONSTITUANTS. MAIS IL EST -
! --- POSSIBLE DE N'EN REMPLIR QUE DEUX EN LAISSANT LE DERNIER A ZERO -
! =====================================================================
        if (nomte(1:3) .eq. 'HHM') then
            mecani(1) = 1
            press1(1) = 1
            press2(1) = 1
            tempe(1) = 0
            press1(2) = 2
            press2(2) = 1
        endif
! =====================================================================
! --- SI MODELISATION = HH2M ------------------------------------------
! =====================================================================
! --- ON RESERVE DE LA PLACE POUR LES TROIS CONSTITUANTS. MAIS IL EST -
! --- POSSIBLE DE N'EN REMPLIR QUE DEUX EN LAISSANT LE DERNIER A ZERO -
! =====================================================================
        if (nomte(1:4) .eq. 'HH2M') then
            mecani(1) = 1
            press1(1) = 1
            press2(1) = 1
            tempe(1) = 0
            press1(2) = 2
            press2(2) = 2
        endif
! =====================================================================
! --- SI MODELISATION = THH -------------------------------------------
! =====================================================================
        if (nomte(1:4) .eq. 'THH_') then
            mecani(1) = 0
            press1(1) = 1
            press2(1) = 1
            tempe(1) = 1
            press1(2) = 2
            press2(2) = 1
        endif
! =====================================================================
! --- SI MODELISATION = THH2 ------------------------------------------
! =====================================================================
        if (nomte(1:5) .eq. 'THH2_') then
            mecani(1) = 0
            press1(1) = 1
            press2(1) = 1
            tempe(1) = 1
            press1(2) = 2
            press2(2) = 2
        endif
! =====================================================================
! --- SI MODELISATION = H --------------------------------------------
! =====================================================================
        if (nomte(1:2) .eq. 'H_') then
            mecani(1) = 0
            press1(1) = 1
            press2(1) = 0
            tempe(1) = 0
            press1(2) = 1
            press2(2) = 0
        endif
! =====================================================================
! --- SI MODELISATION = HH -------------------------------------------
! =====================================================================
        if (nomte(1:3) .eq. 'HH_') then
            mecani(1) = 0
            press1(1) = 1
            press2(1) = 1
            tempe(1) = 0
            press1(2) = 2
            press2(2) = 1
        endif
! =====================================================================
! --- SI MODELISATION = HH2 -------------------------------------------
! =====================================================================
        if (nomte(1:4) .eq. 'HH2_') then
            mecani(1) = 0
            press1(1) = 1
            press2(1) = 1
            tempe(1) = 0
            press1(2) = 2
            press2(2) = 2
        endif
! =====================================================================
! --- SI MODELISATION = THV -------------------------------------------
! =====================================================================
        if (nomte(1:4) .eq. 'THV_') then
            mecani(1) = 0
            press1(1) = 1
            press2(1) = 0
            tempe(1) = 1
            press1(2) = 2
            press2(2) = 0
        endif
! =====================================================================
! --- SI MODELISATION = THM -------------------------------------------
! =====================================================================
        if (nomte(1:4) .eq. 'THM_') then
            mecani(1) = 1
            press1(1) = 1
            press2(1) = 0
            tempe(1) = 1
            press1(2) = 1
            press2(2) = 0
        endif
!
!===
! 2. CALCUL PREALABLE DES ADRESSES LOCALES DES VARIABLES
! =====================================================================
! 2.1. LES AUTRES VALEURS DES TABLEAUX MECANI,PRESS1,PRESS2,TEMPE -----
! --- SE DEFINISSENT AUTOMATIQUEMENT : --------------------------------
! --- NOMBRE DE DEFORMATIONS ET DE CONTRAINTES DE CHAQUE PROBLEME -----
! =====================================================================
!====
!
!  ATTENTION LE NOMBRE DE PARAMETRES MECANIQUES AUGMENTE
!  LE SCALAIRE SIP DEVIENT UN TENSEUR A 6 COMPOSANTES
!  DANS LE CAS ISOTROPE SIP EST UN TENSEUR ISOTROPE
!
        if (mecani(1) .eq. 1) then
            mecani(4) = ndim + 6
            mecani(5) = 6 + 6
            nmec = ndim
        else
            mecani(4) = 0
            mecani(5) = 0
            nmec = 0
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
            press1(6) = 1 + ndim
            press1(7) = iaux + ndim
            np1 = 1
            if (tempe(1) .eq. 1) press1(7) = press1(7) + 1
        else
            press1(6) = 0
            press1(7) = 0
            np1 = 0
        endif
!
        if (press2(1) .eq. 1) then
            press2(6) = 1 + ndim
            press2(7) = iaux + ndim
            np2 = 1
            if (tempe(1) .eq. 1) press2(7) = press2(7) + 1
        else
            press2(6) = 0
            press2(7) = 0
            np2 = 0
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
            press1(3) = mecani(4) + 1
            press1(4) = mecani(5) + 1
            if (press1(2) .eq. 2) press1(5) = press1(4) + press1(7)
        endif
!
! 2.2.3. ==> DEFORMATIONS ET CONTRAINTES POUR LA SECONDE PRESSION
!
        if (press2(1) .eq. 1) then
            press2(3) = press1(3) + press1(6)
            press2(4) = press1(4) + press1(2)*press1(7)
            if (press2(2) .eq. 2) press2(5) = press2(4) + press2(7)
        endif
!
! 2.2.4. ==> DEFORMATIONS ET CONTRAINTES POUR LA TEMPERATURE
!
        if (tempe(1) .eq. 1) then
            tempe(2) = mecani(4) + press1(6) + press2(6) + 1
            tempe(3) = mecani(5) + press1(2)*press1(7) + press2(2)* press2(7) + 1
        endif
!
! =====================================================================
! 2.3. DIMENSION DES DEPLACEMENTS, DEFORMATIONS ET CONTRAINTES --------
! =====================================================================
!
!
        dimdep = ndim*mecani(1) + press1(1) + press2(1) + tempe(1)
        dimdef = mecani(4) + press1(6) + press2(6) + tempe(4)
        dimcon = mecani(5) + press1(2)*press1(7) + press2(2)*press2(7) + tempe(5)
!=====================================================================
!
! =====================================================================
    endif
!
end subroutine
