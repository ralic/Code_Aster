subroutine ngfint(option, typmod, ndim, nddl, neps,&
                  npg, w, b, compor, fami,&
                  mat, angmas, lgpg, crit, instam,&
                  instap, ddlm, ddld, ni2ldc, sigmam,&
                  vim, sigmap, vip, fint, matr,&
                  codret)
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
! aslint: disable=W1504
    implicit none
!
#include "asterfort/assert.h"
#include "asterfort/codere.h"
#include "asterfort/nmcomp.h"
#include "asterfort/r8inir.h"
#include "blas/dgemm.h"
#include "blas/dgemv.h"
    character(len=8) :: typmod(*)
    character(len=*) :: fami
    character(len=16) :: option, compor(*)
!
    integer :: ndim, nddl, neps, npg, mat, lgpg, codret
    real(kind=8) :: w(0:npg-1), ni2ldc(0:neps-1), b(neps, npg, nddl)
    real(kind=8) :: angmas(3), crit(*), instam, instap
    real(kind=8) :: ddlm(nddl), ddld(nddl)
    real(kind=8) :: sigmam(0:neps*npg-1), sigmap(0:neps*npg-1)
    real(kind=8) :: vim(lgpg, npg), vip(lgpg, npg), matr(nddl, nddl), fint(nddl)
! ----------------------------------------------------------------------
!     RAPH_MECA, RIGI_MECA_* ET FULL_MECA_*
! ----------------------------------------------------------------------
! IN  OPTION  : OPTION DE CALCUL
! IN  TYPMOD  : TYPE DE MODEELISATION                              (LDC)
! IN  NDIM    : DIMENSION DE L'ESPACE                              (LDC)
! IN  NDDL    : NOMBRE DE DEGRES DE LIBERTE
! IN  NEPS    : NOMBRE DE COMPOSANTES DE DEFORMATION ET CONTRAINTE
! IN  NPG     : NOMBRE DE POINTS DE GAUSS
! IN  W       : POIDS DES POINTS DE GAUSS
! IN  B       : MATRICE CINEMATIQUE : DEFORMATION = B.DDL
! IN  COMPOR  : COMPORTEMENT                                       (LDC)
! IN  MAT     : MATERIAU CODE                                      (LDC)
! IN  ANGMAS  : ANGLE DU REPERE LOCAL                              (LDC)
! IN  LGPG    : LONGUEUR DU TABLEAU DES VARIABLES INTERNES
! IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX                     (LDC)
! IN  INSTAM  : INSTANT PRECEDENT                                  (LDC)
! IN  INSTAP  : INSTANT DE CALCUL                                  (LDC)
! IN  DDLM    : DDL A L'INSTANT PRECEDENT
! IN  DDLD    : INCREMENT DES DDL
! IN  LI2LDC  : CONVERSION CONTRAINTE STOCKEE -> CONTRAINTE LDC (RAC2)
! IN  SIGMAM  : CONTRAINTES A L'INSTANT PRECEDENT
! IN  VIM     : VARIABLES INTERNES A L'INSTANT PRECEDENT
! OUT SIGMAP  : CONTRAINTES DE CAUCHY (RAPH_MECA   ET FULL_MECA_*)
! OUT VIP     : VARIABLES INTERNES    (RAPH_MECA   ET FULL_MECA_*)
! OUT FINT    : FORCES INTERIEURES    (RAPH_MECA   ET FULL_MECA_*)
! OUT MATR    : MATRICE DE RIGIDITE   (RIGI_MECA_* ET FULL_MECA_*)
! OUT CODRET  : CODE RETOUR
! ----------------------------------------------------------------------
    integer :: nnomax, npgmax, epsmax, ddlmax
    parameter (nnomax=27,npgmax=27,epsmax=20,ddlmax=15*nnomax)
! ----------------------------------------------------------------------
    logical :: resi, rigi
    integer :: nepg, g, ieg, cod(npgmax)
    real(kind=8) :: sigm(0:epsmax*npgmax-1), sigp(0:epsmax*npgmax-1)
    real(kind=8) :: epsm(0:epsmax*npgmax-1), epsd(0:epsmax*npgmax-1)
    real(kind=8) :: dsidep(0:epsmax*epsmax*npgmax-1), dum
    real(kind=8) :: ktgb(0:epsmax*npgmax*ddlmax-1)
! ----------------------------------------------------------------------
    integer :: os, dos
    os(g) = (g-1)*neps
    dos(g) = (g-1)*neps*neps
! ----------------------------------------------------------------------
!
! - INITIALISATION
!
    call assert(npg.le.npgmax)
    call assert(neps.le.epsmax)
    call assert(nddl.le.ddlmax)
    nepg = neps*npg
!
    resi = option(1:9).eq.'FULL_MECA' .or. option(1:9).eq.'RAPH_MECA'
    rigi = option(1:9).eq.'FULL_MECA' .or. option(1:9).eq.'RIGI_MECA'
!
    if (rigi) call r8inir(neps*nepg, 0.d0, dsidep, 1)
    if (resi) call r8inir(nepg, 0.d0, sigp, 1)
!
    do 5 g = 1, npg
        cod(g)=0
 5  end do
!
!
!
! - CALCUL DES DEFORMATIONS GENERALISEES
!
    call dgemv('N', nepg, nddl, 1.d0, b,&
               nepg, ddlm, 1, 0.d0, epsm,&
               1)
    call dgemv('N', nepg, nddl, 1.d0, b,&
               nepg, ddld, 1, 0.d0, epsd,&
               1)
!
!
!
! - CALCUL DE LA LOI DE COMPORTEMENT
!
!    FORMAT LDC DES CONTRAINTES (AVEC RAC2)
    do 10 ieg = 0, nepg-1
        sigm(ieg) = sigmam(ieg)*ni2ldc(mod(ieg,neps))
10  end do
!
!    LOI DE COMPORTEMENT EN CHAQUE POINT DE GAUSS
    do 20 g = 1, npg
        call nmcomp(fami, g, 1, ndim, typmod,&
                    mat, compor, crit, instam, instap,&
                    neps, epsm(os(g)), epsd(os(g)), neps, sigm(os(g)),&
                    vim(1, g), option, angmas, 1, dum,&
                    sigp(os(g)), vip(1, g), neps*neps, dsidep( dos(g)), 1,&
                    dum, cod(g))
        if (cod(g) .eq. 1) goto 9000
20  end do
!
!    FORMAT RESULTAT DES CONTRAINTES (SANS RAC2)
    if (resi) then
        do 30 ieg = 0, nepg-1
            sigmap(ieg) = sigp(ieg)/ni2ldc(mod(ieg,neps))
30      continue
    endif
!
!
!
! - FORCE INTERIEURE
!
    if (resi) then
!
!      PRISE EN CHARGE DU POIDS DU POINT DE GAUSS
        do 40 ieg = 0, nepg-1
            sigp(ieg) = sigp(ieg)*w(ieg/neps)
40      continue
!
!      FINT = SOMME(G) WG.BT.SIGMA
        call dgemv('T', nepg, nddl, 1.d0, b,&
                   nepg, sigp, 1, 0.d0, fint,&
                   1)
!
    endif
!
!
!
! - CALCUL DE LA MATRICE DE RIGIDITE (STOCKAGE PAR LIGNES SUCCESSIVES)
!
    if (rigi) then
!
!      PRISE EN CHARGE DU POIDS DU POINT DE GAUSS  WG.DSIDEP
        do 50 ieg = 0, neps*nepg-1
            dsidep(ieg) = dsidep(ieg)*w(ieg/(neps*neps))
50      continue
!
!      CALCUL DES PRODUITS INTERMEDIAIRES (WG.DSIDEP).B POUR CHAQUE G
        do 60 g = 1, npg
            call dgemm('N', 'N', neps, nddl, neps,&
                       1.d0, dsidep(dos(g)), neps, b(1, g, 1), nepg,&
                       0.d0, ktgb(os(g)), nepg)
60      continue
!
!      CALCUL DU PRODUIT FINAL SOMME(G) BT. ((WG.DSIDEP).B)  TRANSPOSE
        call dgemm('T', 'N', nddl, nddl, nepg,&
                   1.d0, ktgb, nepg, b, nepg,&
                   0.d0, matr, nddl)
!
    endif
!
!
!
! - SYNTHESE DU CODE RETOUR
9000  continue
    if (resi) call codere(cod, npg, codret)
!
end subroutine
