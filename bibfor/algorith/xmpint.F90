subroutine xmpint(ndim, npte, nfaes, jpcpi, jpccf,&
                  geopi)
!
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
!
    implicit none
#include "jeveux.h"
    integer :: jpcpi, jpccf
    integer :: ndim, nfaes, npte
    real(kind=8) :: geopi(18)
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (METHODE XFEM-GG - TE)
!
! CALCUL DES COORDONNEES REELLE POUR LES POINTS
! D'INTERSECTION CONSTITUENT LA MAILLE DE CONTACT
! DANS L'ELEMENT DE CONTACT HYBRIDE X-FEM
!
! ----------------------------------------------------------------------
!
! ----------------------------------------------------------------------
! ROUTINE SPECIFIQUE A L'APPROCHE <<GRANDS GLISSEMENTS AVEC XFEM>>,
! TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
! ----------------------------------------------------------------------
!
!
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NFAES  : NUMERO DE LA FACETTE ESCLAVE
! IN  JPCPI  : COORDONNÉES DES POINTS D'INTERSECTION DANS L'ELEM DE REF
! IN  JPCCF  : NUM LOCAUX DES NOEUDS DES FACETTES DE CONTACT
! OUT GEOPI  : COORDONNÉES REELES DES POINTS D'INTERSECTION
!
!
!
!
    integer :: i, j
!
! ----------------------------------------------------------------------
!
!
    do 30 i = 1, npte
! --- BOUCLE SUR LES POINTS D'INTERSECTION DE LA FACETTE
        do 40 j = 1, ndim
            geopi(ndim*(i-1)+j) = zr( jpcpi-1+ndim*(zi(jpccf-1+npte*( nfaes-1)+i)-1 )+j )
40      continue
30  continue
!
end subroutine
