subroutine xgrdhm(nomte, ndim, mecani, press1, press2,&
                  tempe, enrmec, dimdef, dimcon, nmec,&
                  np1, np2, nenr, dimenr)
    implicit none
!
#   include "asterfort/teattr.h"
    integer :: mecani(5), press1(7), press2(7), tempe(5)
    integer :: dimdef, dimcon, ier
    integer :: ndim, nmec, np1, np2, i
    character(len=8) :: enr
    character(len=16) :: nomte
!
! DECLARATIONS POUR XFEM
    integer :: enrmec(3), dimenr
    integer :: nenr
!
! =====================================================================
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
!   TABLEAU ENRMEC :
!   ENRMEC(1) = 1 : IL Y A ENRICHISSEMENT PAS FONCTION HEAVISIDE
!   ENRMEC(2) = ADRESSE DANS LES TABLEAUX DES DEFORMATIONS
!               GENERALISEES AU POINT DE GAUSS
!   ENRMEC(3) = NOMBRE DE DEFORMATIONS POUR L'ENRICHISSEMENT
!               MECANIQUE (TYPE HEAVISIDE)
    integer :: iaux
!
!====
! 1. REPERAGE DES CALCULS A FAIRE : MECANIQUE, HYDRAULIQUE, ETC.
!====
!
! =====================================================================
! --- INITIALISATION DES GRANDEURS PRESENTES SELON LA MODELISATION ----
! --- EN THM ----------------------------------------------------------
!======================================================================
    do i = 1, 5
        mecani(i)=0
        tempe(i)=0
    end do
    do i = 1, 7
        press1(i)=0
        press2(i)=0
    end do
    do i = 1, 3
        enrmec(i)=0
    end do
    np2=0
! =====================================================================
! --- SI MODELISATION = HM --------------------------------------------
! =====================================================================
    call teattr(nomte, 'S', 'THM', enr, ier)
    if (enr .eq. 'OUI') then
        mecani(1) = 1
        press1(1) = 1
        press1(2) = 1
    endif
! =====================================================================
! --- ON VERIFIE LA NATURE DE L'ELEMENT HM-XFEM -----------------------
! =====================================================================
    call teattr(nomte, 'S', 'XFEM', enr, ier)
    if (enr(1:2) .eq. 'XH') then
        enrmec(1)=1
    endif
!
! 2. CALCUL PREALABLE DES ADRESSES LOCALES DES VARIABLES
! =====================================================================
! 2.1. LES AUTRES VALEURS DES TABLEAUX MECANI,PRESS1,PRESS2,TEMPE -----
! --- SE DEFINISSENT AUTOMATIQUEMENT : --------------------------------
! --- NOMBRE DE DEFORMATIONS ET DE CONTRAINTES DE CHAQUE PROBLEME -----
! =====================================================================
!
!  ATTENTION LE NOMBRE DE PARAMETRES MECANIQUES AUGMENTE
!  LE SCALAIRE SIP DEVIENT UN TENSEUR A 6 COMPOSANTES
!  DANS LE CAS ISOTROPE SIP EST UN TENSEUR ISOTROPE
!
    if (mecani(1) .eq. 1) then
        mecani(4) = ndim + 6
        mecani(5) = 6+6
        nmec = ndim
    endif
!
    iaux = 1
!
    if (press1(1) .eq. 1) then
        press1(6) = 1 + ndim
        press1(7) = iaux + ndim
        np1 = 1
    endif
!
    if (enrmec(1) .eq. 1) then
        enrmec(3) = ndim + 6
        nenr=ndim
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
    endif
!
! 2.2.2. ==> DEFORMATIONS ET CONTRAINTES POUR LA PREMIERE PRESSION
!
    if (press1(1) .eq. 1) then
        press1(3) = mecani(4) + 1
        press1(4) = mecani(5) + 1
    endif
!
! 2.2.3. ==> DEFORMATIONS POUR L'ENRICHISSEMENT HEAVISIDE (XFEM)
!
    if (enrmec(1) .eq. 1) then
        enrmec(2) = mecani(4) + press1(6) + 1
    endif
!
! =====================================================================
! 2.3. DIMENSION DES DEFORMATIONS ET CONTRAINTES ----------------------
! =====================================================================
    dimdef = mecani(4) + press1(6)
    dimcon = mecani(5) + press1(2)*press1(7)
! DIMENSION INTERMEDIAIRE UTILISEE POUR L'ASSEMBLAGE EN XFEM
    dimenr = mecani(4) + press1(6) + enrmec(3)
!=====================================================================
!
! =====================================================================
!
end subroutine
