subroutine cfnewj(ndim, coorde, coordp, norm, jeu)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit    none
    integer :: ndim
    real(kind=8) :: jeu
    real(kind=8) :: coorde(3), coordp(3)
    real(kind=8) :: norm(3)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - APPARIEMENT)
!
! CALCUL DU JEU SUR LA NORMALE
!
! ----------------------------------------------------------------------
!
!
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  COORDE : COORDONNEES DU NOEUD ESCLAVE E
! IN  COORDP : COORDONNEES DE LA PROJECTION DU NOEUD ESCLAVE E
! IN  NORM   : NORMALE
! OUT JEU    : JEU
!
! ----------------------------------------------------------------------
!
!
!
! --- CALCUL JEU
!
    jeu = (coordp(1)-coorde(1))*norm(1) + (coordp(2)-coorde(2))*norm(2)
    if (ndim .eq. 3) then
        jeu = jeu + (coordp(3)-coorde(3))*norm(3)
    endif
!
end subroutine
