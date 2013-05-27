subroutine mmmjev(ndim, norm, vitpe, vitpm, jeuvit)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
    integer :: ndim
    real(kind=8) :: vitpe(3), vitpm(3)
    real(kind=8) :: norm(3)
    real(kind=8) :: jeuvit
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! CALCUL DU JEU ACTUALISE - SAUT DE VITESSE
!
! ----------------------------------------------------------------------
!
!
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NORM   : VALEUR DE LA NORMALE
! IN  VITPE  : VITESSE DU POINT DE CONTACT
! IN  VITPM  : VITESSE DU PROJETE DU POINT DE CONTACT
! OUT JEUVIT : VALEUR DU JEU (SAUT DE VITESSE NORMAL)
!
! ----------------------------------------------------------------------
!
    integer :: idim
!
! ----------------------------------------------------------------------
!
    jeuvit = 0.d0
    do 10 idim = 1, ndim
        jeuvit = jeuvit + (vitpe(idim)-vitpm(idim)) * norm(idim)
10  end do
!
end subroutine
