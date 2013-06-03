subroutine prelog(ndim, lgpg, vim, gn, lamb,&
                  logl, fm, fp, epsml, deps,&
                  tn, resi, iret)
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
! ----------------------------------------------------------------------
!  BUT:  CALCUL DES GRANDES DEFORMATIONS  LOG 2D (D_PLAN ET AXI) ET 3D
!     SUIVANT ARTICLE MIEHE APEL LAMBRECHT CMAME 2002
! ----------------------------------------------------------------------
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  LGPG    : DIMENSION DU VECTEUR DES VAR. INTERNES POUR 1 PT GAUSS
! IN  VIM     : VARIABLES INTERNES EN T-
! OUT GN      : TERMES UTILES AU CALCUL DE TL DANS POSLOG
! OUT LAMB    : TERMES UTILES AU CALCUL DE TL DANS POSLOG
! OUT LOGL    : TERMES UTILES AU CALCUL DE TL DANS POSLOG
! OUT FM      : GRADIENT TRANSFORMATION EN T-
! OUT FP      : GRADIENT TRANSFORMATION EN T+
! OUT EPSML   : DEFORAMTIONS LOGARITHMIQUES EN T-
! OUT DEPS    : ACCROISSEEMENT DE DEFORMATIONS LOGARITHMIQUES
! OUT TN      : CONTRAINTES ASSOCIEES AUX DEF. LOGARITHMIQUES EN T-
! IN  RESI    : .TRUE. SI FULL_MECA/RAPH_MECA .FALSE. SI RIGI_MECA_TANG
!
    implicit none
    include 'asterfort/deflog.h'
    include 'asterfort/r8inir.h'
    include 'blas/dcopy.h'
    integer :: i, ndim, lgpg, ivtn, iret
    real(kind=8) :: vim(lgpg)
    real(kind=8) :: fm(3, 3), fp(3, 3), epsml(6), epspl(6)
    real(kind=8) :: tn(6), deps(6), gn(3, 3), lamb(3), logl(3)
    logical :: resi
! ---------------------------------------------------------------------
!
    call deflog(ndim, fm, epsml, gn, lamb,&
                logl, iret)
!
    if (resi) then
        call deflog(ndim, fp, epspl, gn, lamb,&
                    logl, iret)
        do 35 i = 1, 6
            deps(i)=epspl(i)-epsml(i)
35      continue
    else
        do 34 i = 1, 6
            deps(i)=0.d0
34      continue
    endif
!
!     --------------------------------
!     CALCUL DES CONTRAINTES TN INSTANT PRECEDENT
!     pour gagner du temps : on stocke TN comme variable interne
!     --------------------------------
    ivtn=lgpg-6+1
    call r8inir(6, 0.d0, tn, 1)
    call dcopy(2*ndim, vim(ivtn), 1, tn, 1)
!
end subroutine
