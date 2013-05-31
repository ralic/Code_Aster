subroutine xmvec1(ndim, jnne, ndeple, nnc, jnnm,&
                  hpg, nfaes, ffc, ffe, ffm,&
                  jacobi, dlagrc, jpcai, cface, coefcr,&
                  coefcp, lpenac, jeu, norm, typmai,&
                  nsinge, nsingm, rre, rrm, nconta,&
                  jddle, jddlm, nfhe, nfhm, lmulti,&
                  heavno, heavfa, vtmp)
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
! aslint: disable=W1504
    implicit none
    include 'asterfort/indent.h'
    include 'asterfort/xplma2.h'
    integer :: ndim, jnne(3), jnnm(3), nnc, nfaes
    integer :: jpcai, cface(3, 5), nsinge, nsingm
    real(kind=8) :: hpg, ffc(9), jacobi, ffe(20), ffm(20)
    real(kind=8) :: dlagrc, jeu, norm(3), coefcr, coefcp, rre, rrm
    real(kind=8) :: vtmp(336)
    character(len=8) :: typmai
    integer :: nconta, ndeple, jddle(2), jddlm(2)
    integer :: nfhe, nfhm, heavno(8), heavfa(*)
    logical :: lpenac, lmulti
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE XFEMGG - CALCUL ELEM.)
!
! VECTEUR SECOND MEMBRE SI CONTACT AVEC COMPLIANCE (XFEM)
!
! ----------------------------------------------------------------------
! ROUTINE SPECIFIQUE A L'APPROCHE <<GRANDS GLISSEMENTS AVEC XFEM>>,
! TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
! ----------------------------------------------------------------------
!
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
! IN  NNES   : NOMBRE DE NOEUDS SOMMETS DE LA MAILLE ESCLAVE
! IN  NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
! IN  HPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
! IN  NFAES  : NUMERO DE LA FACETTE DE CONTACT ESCLAVE
! IN  FFC    : FONCTIONS DE FORME DU POINT DE CONTACT DANS ELC
! IN  FFE    : FONCTIONS DE FORME DU POINT DE CONTACT DANS ESC
! IN  FFM    : FONCTIONS DE FORME DE LA PROJECTION DU PTC DANS MAIT
! IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
! IN  DLAGRC : LAGRANGE DE CONTACT AU POINT D'INTÉGRATION
! IN  JPCAI  : POINTEUR VERS LE VECT DES ARRETES ESCLAVES INTERSECTEES
! IN  CFACE  : MATRICE DE CONECTIVITE DES FACETTES DE CONTACT
! IN  COEFCA : COEF_REGU_CONT
! IN  JEU    : VALEUR DU JEU
! IN  NORM   : VALEUR DE LA NORMALE AU POINT DE CONTACT
! IN  NSINGE  : NOMBRE DE FONCTION SINGULIERE ESCLAVE
! IN  NSINGM  : NOMBRE DE FONCTION SINGULIERE MAITRE
! IN  RRE    : SQRT LST ESCLAVE
! IN  RRM    : SQRT LST MAITRE
! I/O VTMP   : VECTEUR SECOND MEMBRE ELEMENTAIRE DE CONTACT/FROTTEMENT
! ----------------------------------------------------------------------
    integer :: i, j, k, ii, in, pl, xoula, iin, nddle
    integer :: nne, nnes, nnem, nnm, nnms, ddles, ddlem, ddlms, ddlmm
    integer :: ifh, iddl
    real(kind=8) :: vv, iescl(6), imait(6)
! ----------------------------------------------------------------------
!
!
! --- INITIALISATION
!
    iescl(1) = 1
    iescl(2) =-1
    iescl(2+nfhe)=-rre
    imait(1) = 1
    imait(2) = 1
    imait(2+nfhm)= rrm
!
! --------------------- CALCUL DE [L1_CONT]-----------------------------
!
    nne=jnne(1)
    nnes=jnne(2)
    nnem=jnne(3)
    nnm=jnnm(1)
    nnms=jnnm(2)
    ddles=jddle(1)
    ddlem=jddle(2)
    ddlms=jddlm(1)
    ddlmm=jddlm(2)
    nddle = ddles*nnes+ddlem*nnem
!
    if (nnm .ne. 0) then
!
        do 10 j = 1, ndim
            do 20 i = 1, ndeple
                call indent(i, ddles, ddlem, nnes, iin)
                if (lpenac) then
                    vv = hpg*jacobi*dlagrc* ffe(i)*norm(j)
                else
                    vv = hpg*jacobi*(dlagrc-coefcr*jeu) * ffe(i)*norm( j)
                endif
                if (lmulti) then
                    do 15 ifh = 1, nfhe
                        iescl(1+ifh)=heavfa(nfhe*(i-1)+ifh)
15                  continue
                endif
                do 25 iddl = 1, 1+nfhe+nsinge
                    ii = iin + (iddl-1)*ndim + j
                    vtmp(ii) = -iescl(iddl)*vv
25              continue
20          continue
            do 30 i = 1, nnm
                call indent(i, ddlms, ddlmm, nnms, iin)
                iin = iin + nddle
                if (lpenac) then
                    vv = hpg*jacobi*dlagrc* ffm(i)*norm(j)
                else
                    vv = hpg*jacobi*(dlagrc-coefcr*jeu) * ffm(i)*norm( j)
                endif
                if (lmulti) then
                    do 35 ifh = 1, nfhm
                        imait(1+ifh)=heavfa(nfhe*nne+nfhm*(i-1)+ifh)
35                  continue
                endif
                do 45 iddl = 1, 1+nfhm+nsingm
                    ii = iin + (iddl-1)*ndim + j
                    vtmp(ii) = imait(iddl)*vv
45              continue
30          continue
10      end do
    else
        do 50 j = 1, ndim
            do 60 i = 1, ndeple
! --- BLOCS ES,SI
                if (lpenac) then
                    vv = hpg*jacobi*dlagrc* ffe(i)*norm(j)
                else
                    vv = hpg*jacobi*(dlagrc-coefcr*jeu) * ffe(i)*norm( j)
                endif
                call indent(i, ddles, ddlem, nnes, iin)
                ii = iin + j
                vtmp(ii) = rre * vv
60          continue
50      end do
    endif
!
! --------------------- CALCUL DE [L2]----------------------------------
!
    do 40 i = 1, nnc
        call xplma2(ndim, nne, nnes, ddles, i,&
                    nfhe, pl)
        if (lmulti) pl = pl + (heavno(i)-1)*ndim
        if (lpenac) then
            vtmp(pl) = -hpg*jacobi*(dlagrc/coefcp+jeu) *ffc(i)
        else
            vtmp(pl) = -hpg*jacobi*jeu*ffc(i)
        endif
40  end do
!
end subroutine
