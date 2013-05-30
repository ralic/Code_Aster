subroutine xmvec0(ndim, jnne, nnc, nfaes, dlagrc,&
                  hpg, ffc, jacobi, cface, jpcai,&
                  coefcr, coefcp, lpenac, typmai, jddle,&
                  nconta, nfhe, lmulti, heavno, vtmp)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! TOLE CRP_21
!
    implicit none
    include 'asterfort/xplma2.h'
    integer :: ndim, jnne(3), nnc, nfaes, cface(3, 5), jpcai, jddle(2), nconta
    integer :: nfhe, heavno(8)
    real(kind=8) :: dlagrc, hpg, ffc(9), jacobi, coefcr, coefcp
    real(kind=8) :: vtmp(336)
    character(len=8) :: typmai
    logical :: lpenac, lmulti
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE XFEMGG - CALCUL ELEM.)
!
! VECTEUR SECOND MEMBRE SI PAS DE CONTACT (X-FEM)
!
! ----------------------------------------------------------------------
! ROUTINE SPECIFIQUE A L'APPROCHE <<GRANDS GLISSEMENTS AVEC XFEM>>,
! TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
! ----------------------------------------------------------------------
!
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
! IN  DLAGRC : LAGRANGE DE CONTACT AU POINT D'INTÉGRATION
! IN  HPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
! IN  FFC    : FONCTIONS DE FORME DU POINT DE CONTACT
! IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
! IN  COEFCA : COEF_REGU_CONT
! IN  COEFFS : COEF_STAB_CONT
! IN  COEFFP : COEF_PENA_CONT
! IN  DDLES  : NOMBRE DE DDLS D'UN NOEUD SOMMET ESCLAVE
! I/O VTMP   : VECTEUR SECOND MEMBRE ELEMENTAIRE DE CONTACT/FROTTEMENT
! ----------------------------------------------------------------------
!
    integer :: i, in, pl, xoula, nne, nnes, ddles
!
! ----------------------------------------------------------------------
!
! --------------------- CALCUL DE {L2_CONT}-----------------------------
!
    nne=jnne(1)
    nnes=jnne(2)
    ddles=jddle(1)
!
    do 10 i = 1, nnc
        call xplma2(ndim, nne, nnes, ddles, i,&
                    nfhe, pl)
        if (lmulti) pl = pl + (heavno(i)-1)*ndim
        if (lpenac) then
            vtmp(pl) = -hpg*jacobi*dlagrc*ffc(i)/coefcp
        else
            vtmp(pl) = -hpg*jacobi*dlagrc*ffc(i)/coefcr
        endif
10  end do
!
end subroutine
