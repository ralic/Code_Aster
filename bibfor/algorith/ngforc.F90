subroutine ngforc(nddl, neps, npg, w, b,&
                  ni2ldc, sigmam, fint)
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
    implicit none
!
    include 'blas/dgemv.h'
    integer :: nddl, neps, npg
    real(kind=8) :: w(0:npg-1), ni2ldc(0:neps-1), b(neps*npg*nddl)
    real(kind=8) :: sigmam(0:neps*npg-1), fint(nddl)
! ----------------------------------------------------------------------
! OPTION FORC_NODA - FORMULATION GENERIQUE
! ----------------------------------------------------------------------
! IN  NDDL    : NOMBRE DE DEGRES DE LIBERTE
! IN  NEPS    : NOMBRE DE COMPOSANTES DE DEFORMATION ET CONTRAINTE
! IN  NPG     : NOMBRE DE POINTS DE GAUSS
! IN  W       : POIDS DES POINTS DE GAUSS
! IN  B       : MATRICE CINEMATIQUE : DEFORMATION = B.DDL
! IN  LI2LDC  : CONVERSION CONTRAINTE STOCKEE -> CONTRAINTE LDC (RAC2)
! IN  SIGMAM  : CONTRAINTES A L'INSTANT PRECEDENT
! OUT FINT    : FORCES INTERIEURES
! ----------------------------------------------------------------------
    integer :: npgmax, epsmax
    parameter (npgmax=27,epsmax=20)
! ----------------------------------------------------------------------
    integer :: nepg, ieg
    real(kind=8) :: sigm(0:epsmax*npgmax-1)
! ----------------------------------------------------------------------
!
!    INITIALISATION
    nepg = neps*npg
!
!    CONTRAINTE AVEC RAC2 ET POIDS DU POINT DE GAUSS
    do 10 ieg = 0, nepg-1
        sigm(ieg) = sigmam(ieg)*ni2ldc(mod(ieg,neps))*w(ieg/neps)
10  end do
!
!    FINT = SOMME(G) WG.BT.SIGMA
    call dgemv('T', nepg, nddl, 1.d0, b,&
               nepg, sigm, 1, 0.d0, fint,&
               1)
!
end subroutine
