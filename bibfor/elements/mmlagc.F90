subroutine mmlagc(lambds, dlagrc, iresof, lambda)
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
    implicit none
    integer :: iresof
    real(kind=8) :: lambds, lambda, dlagrc
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - CALCUL)
!
! CHOIX DU SEUIL DE FROTTEMENT (PRESSION DE CONTACT)
!
! ----------------------------------------------------------------------
!
!
! IN  IRESOF : ALGO. DE RESOLUTION POUR LE FROTTEMENT
!              0 - POINT FIXE
!              1 - NEWTON COMPLET
! IN  LAMBDS : VALEUR DU MULT. DE CONTACT (SEUIL FIXE)
! IN  DLAGRC : INCREMENT DEPDEL DU LAGRANGIEN DE CONTACT
! OUT LAMBDA : LAGRANGIEN DE CONTACT
!
! ----------------------------------------------------------------------
!
    lambda = lambds
!
    if (iresof .ne. 0) then
        if (dlagrc .ne. 0) lambda = dlagrc
    endif
!
end subroutine
