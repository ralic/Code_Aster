subroutine grdthm(perman, vf, ndim,&
                  mecani, press1, press2, tempe)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/utmess.h"
#include "asterfort/lteatt.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    aster_logical, intent(in) :: perman
    aster_logical, intent(in) :: vf
    integer, intent(in)  :: ndim
    integer, intent(out) :: mecani(5)
    integer, intent(out) :: press1(7)
    integer, intent(out) :: press2(7)
    integer, intent(out) :: tempe(5)
!
! --------------------------------------------------------------------------------------------------
!
! THM - Initializations
!
! Get dimensions
!
! --------------------------------------------------------------------------------------------------
!
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
! --------------------------------------------------------------------------------------------------
!
    mecani(:) = 0
    press1(:) = 0
    press2(:) = 0
    tempe(:)  = 0
!
! - Main parameters: mechanic, thermic, hydraulic
!
    if (lteatt('MECA','OUI')) then
        mecani(1) = 1 
    endif
    if (lteatt('THER','OUI')) then
        tempe(1)  = 1 
    endif
    if (lteatt('HYDR1','1')) then
        press1(1) = 1
        press1(2) = 1 
    endif
    if (lteatt('HYDR2','1')) then
        press2(1) = 1
        press2(2) = 1 
    endif
    if (lteatt('HYDR1','2')) then
        press1(1) = 1
        press1(2) = 2 
    endif
    if (lteatt('HYDR2','2')) then
        press2(1) = 1
        press2(2) = 2 
    endif
!
! - Number of (generalized) stress/strain components - Mechanic
!
    if (mecani(1) .eq. 1) then
        mecani(4) = ndim + 6
        mecani(5) = 6 + 6
    endif
!
! - Number of (generalized) stress/strain components - Thermic
!
    if (tempe(1) .eq. 1) then
        tempe(4) = 1 + ndim
        tempe(5) = 1 + ndim
    endif
!
! - Number of (generalized) stress/strain components - Hydraulic
!
    if (press1(1) .eq. 1) then
        press1(6) = 1 + ndim
        if (perman) then
            press1(7) = ndim
        else
            press1(7) = ndim + 1
        endif
        if (tempe(1) .eq. 1) then
            press1(7) = press1(7) + 1
        endif
    endif
    if (press2(1) .eq. 1) then
        press2(6) = 1 + ndim
        if (perman) then
            press2(7) = ndim
        else
            press2(7) = ndim + 1
        endif
        if (tempe(1) .eq. 1) then
            press2(7) = press2(7) + 1
        endif
    endif
    if (vf) then
        press1(7) = 2
        press2(7) = 2
    endif
!
! - Index for adress in (generalized) vectors - Mechanic
!
    if (mecani(1) .eq. 1) then
        mecani(2) = 1
        mecani(3) = 1
    endif
!
! - Index for adress in (generalized) vectors - Hydraulic
!
    if (press1(1) .eq. 1) then
        press1(3) = mecani(4) + 1
        press1(4) = mecani(5) + 1
        if (press1(2) .eq. 2) then
            press1(5) = press1(4) + press1(7)
        endif
    endif
!
    if (press2(1) .eq. 1) then
        press2(3) = press1(3) + press1(6)
        press2(4) = press1(4) + press1(2)*press1(7)
        if (press2(2) .eq. 2) then
            press2(5) = press2(4) + press2(7)
        endif
    endif
!
! - Index for adress in (generalized) vectors - Thermic
!
    if (tempe(1) .eq. 1) then
        tempe(2) = mecani(4) + press1(6) + press2(6) + 1
        tempe(3) = mecani(5) + press1(2)*press1(7) + press2(2)* press2(7) + 1
    endif
!
end subroutine
