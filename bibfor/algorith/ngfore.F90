subroutine ngfore(nddl, neps, npg, w, b,&
                  ni2ldc, sigref, fref)
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
    integer :: nddl, neps, npg
    real(kind=8) :: w(0:npg-1), ni2ldc(0:neps-1), b(0:neps*npg-1, nddl)
    real(kind=8) :: sigref(0:neps-1), fref(nddl)
! ----------------------------------------------------------------------
!     REFE_FORC_NODA - FORMULATION GENERIQUE
! ----------------------------------------------------------------------
! IN  NDDL    : NOMBRE DE DEGRES DE LIBERTE
! IN  NEPS    : NOMBRE DE COMPOSANTES DE DEFORMATION ET CONTRAINTE
! IN  NPG     : NOMBRE DE POINTS DE GAUSS
! IN  W       : POIDS DES POINTS DE GAUSS
! IN  B       : MATRICE CINEMATIQUE : DEFORMATION = B.DDL
! IN  LI2LDC  : CONVERSION CONTRAINTE STOCKEE -> CONTRAINTE LDC (RAC2)
! IN  SIGREF  : CONTRAINTES DE REFERENCE (PAR COMPOSANTE)
! OUT FREF    : FORCES DE REFERENCE
! ----------------------------------------------------------------------
    integer :: npgmax, epsmax
    parameter (npgmax=27,epsmax=20)
! ----------------------------------------------------------------------
    integer :: nepg, ieg, i
    real(kind=8) :: sigpds(0:epsmax*npgmax-1)
! ----------------------------------------------------------------------
!
!    INITIALISATION
    nepg = neps*npg
!
!    CONTRAINTE AVEC RAC2 ET POIDS DU POINT DE GAUSS
    do 10 ieg = 0, nepg-1
        sigpds(ieg) = sigref(mod(ieg,neps))*ni2ldc(mod(ieg,neps)) * w(ieg/neps)
10  end do
!
!    FINT = SOMME(G) W(G).ABS(BT).SIGREF
    do 20 i = 1, nddl
        fref(i)=0
        do 30 ieg = 0, nepg-1
            fref(i) = fref(i) + abs(b(ieg,i))*sigpds(ieg)
30      continue
20  end do
!
end subroutine
