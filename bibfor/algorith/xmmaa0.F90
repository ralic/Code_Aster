subroutine xmmaa0(ndim, nnc, jnne, hpg, nfaes,&
                  cface, ffc, jacobi, jpcai, coefcr,&
                  coefcp, lpenac, typmai, jddle, nconta,&
                  nfhe, lmulti, heavno, mmat)
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
!
    implicit none
    include 'asterfort/xplma2.h'
    integer :: ndim, nnc, jnne(3), nfaes, jddle(2), nconta
    integer :: cface(5, 3), jpcai, nfhe, heavno(8)
    real(kind=8) :: mmat(336, 336)
    real(kind=8) :: hpg, ffc(8), jacobi, coefcr, coefcp
    character(len=8) :: typmai
    logical :: lpenac, lmulti
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE XFEMGG - CALCUL ELEM.)
!
! CALCUL DE C POUR LE CONTACT METHODE CONTINUE
! CAS SANS CONTACT (XFEM)
!
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
! IN  NNC    : NOMBRE DE NOEUDS DE CONTACT
! IN  NNE    : NOMBRE TOTAL DE NOEUDS DE LA MAILLE ESCLAVE
! IN  NNES   : NOMBRE DE NOEUDS SOMMETS DE LA MAILLE ESCLAVE
! IN  HPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
! IN  NFAES  : NUMERO DE LA FACETTE DE CONTACT ESCLAVE
! IN  CFACE  : MATRICE DE CONECTIVITE DES FACETTES DE CONTACT
! IN  FFC    : FONCTIONS DE FORME DU POINT DE CONTACT DANS ELC
! IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
! IN  JPCAI  : POINTEUR VERS LE VECTEUR DES ARRETES ESCLAVES
!              INTERSECTEES
! IN  COEFCA : COEF_REGU_CONT
! IN  TYPMAI : NOM DE LA MAILLE ESCLAVE D'ORIGINE (QUADRATIQUE)
! IN  DDLES : NOMBRE DE DDLS D'UN NOEUD SOMMET ESCLAVE
! I/O MMAT   : MATRICE ELEMENTAIRE DE CONTACT/FROTTEMENT
!
! ----------------------------------------------------------------------
!
    integer :: i, j, ini, inj, pli, plj, xoula, nne, nnes, ddles
!
! ----------------------------------------------------------------------
!
    nne=jnne(1)
    nnes=jnne(2)
    ddles=jddle(1)
!
! ---  BOUCLE SUR LES NOEUDS PORTANT DES DDL DE CONTACT
    do 10 i = 1, nnc
! --- BOUCLE SUR LES NOEUDS PORTANT DES DDL DE CONTACT
        do 20 j = 1, nnc
            call xplma2(ndim, nne, nnes, ddles, i,&
                        nfhe, pli)
            if (lmulti) pli = pli + (heavno(i)-1)*ndim
            call xplma2(ndim, nne, nnes, ddles, j,&
                        nfhe, plj)
            if (lmulti) plj = plj + (heavno(j)-1)*ndim
            if (lpenac) then
                mmat(pli,plj) = -hpg*ffc(j)*ffc(i)*jacobi/coefcp
            else
                mmat(pli,plj) = -hpg*ffc(j)*ffc(i)*jacobi/coefcr
            endif
20      continue
10  end do
!
end subroutine
