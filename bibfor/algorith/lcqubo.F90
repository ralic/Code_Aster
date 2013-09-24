subroutine lcqubo(ep0, ep1, l0, l1, etamin, etamax, vide, etam, etap)
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
    implicit none
    real(kind=8),intent(in) :: ep0(6), ep1(6), l0, l1, etamin, etamax
    real(kind=8),intent(out):: etam, etap
    logical,intent(out)     :: vide
!
! ----------------------------------------------------------------------
!  BORNES POUR LE PILOTAGE RELATIF AU CRITERE QUADRATIQUE
!  ON NE FAIT RIEN A L'HEURE ACTUELLE
!    CHI**2 + L0 + L1*ETA = 0
! ----------------------------------------------------------------------
!  IN  EP0    DEFORMATION FIXE
!  IN  EP1    DEFORMATION PILOTEE
!  IN  L0,L1  COMPOSANTES DU TERME AFFINE
!  IN  ETAMIN BORNE MIN INITIALE
!  IN  ETAMAX BORNE MAX INITIALE
!  OUT VIDE   CODE RETOUR: T->PAS DE SOLUTION ; F->BORNES
!  OUT ETAM   NOUVELLE BORNE MIN
!  OUT ETAP   NOUVELLE BORNE MAX
! ----------------------------------------------------------------------
    etam = etamin
    etap = etamax
    vide = etamax.le.etamin
end subroutine
