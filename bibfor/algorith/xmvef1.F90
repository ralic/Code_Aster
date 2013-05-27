subroutine xmvef1(ndim, jnne, jnnm, ndeple, nnc,&
                  nfaes, cface, hpg, ffc, ffe,&
                  ffm, jacobi, jpcai, dlagrc, dlagrf,&
                  coeffr, coeffp, lpenaf, coefff, tau1,&
                  tau2, rese, mproj, coefcr, coefcp,&
                  jeu, typmai, nsinge, nsingm, rre,&
                  rrm, nvit, nconta, jddle, jddlm,&
                  nfhe, vtmp)
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
    include 'asterfort/indent.h'
    include 'asterfort/xplma2.h'
    integer :: ndim, nnc, jnne(3), jnnm(3), nfaes, jpcai, cface(5, 3)
    integer :: nsinge, nsingm, nvit, jddle(2), jddlm(2), nfhe
    real(kind=8) :: hpg, ffc(9), ffe(20), ffm(20), jacobi
    real(kind=8) :: dlagrc, dlagrf(2), coefcp, jeu
    real(kind=8) :: coefff, coeffr, coeffp, rre, rrm, coefcr
    real(kind=8) :: tau1(3), tau2(3), rese(3), mproj(3, 3), vtmp(336)
    integer :: nconta, ndeple
    character(len=8) :: typmai
    logical :: lpenaf
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE XFEMGG - CALCUL ELEM.)
!
! CALCUL DU SECOND MEMBRE POUR LE FROTTEMENT
! CAS AVEC CONTACT
!
! ----------------------------------------------------------------------
! ROUTINE SPECIFIQUE A L'APPROCHE <<GRANDS GLISSEMENTS AVEC XFEM>>,
! TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
! ----------------------------------------------------------------------
!
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
! IN  NNES   : NOMBRE DE NOEUDS SOMMETS DE LA MAILLE ESCLAVE
! IN  NNC    : NOMBRE DE NOEUDS DE CONTACT
! IN  NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
! IN  NFAES  : NUMERO DE LA FACETTE DE CONTACT ESCLAVE
! IN  CFACE  : MATRICE DE CONECTIVITE DES FACETTES DE CONTACT
! IN  HPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
! IN  FFC    : FONCTIONS DE FORME DU POINT DE CONTACT DANS ELC
! IN  FFE    : FONCTIONS DE FORME DU POINT DE CONTACT DANS ESC
! IN  FFM    : FONCTIONS DE FORME DE LA PROJECTION DU PTC DANS MAIT
! IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
! IN  JPCAI  : POINTEUR VERS LE VECT DES ARRETES ESCLAVES INTERSECTEES
! IN  COEFFA : COEF_REGU_FROT
! IN  COEFFF : COEFFICIENT DE FROTTEMENT DE COULOMB
! IN  TAU1   : PREMIERE TANGENTE
! IN  TAU2   : SECONDE TANGENTE
! IN  RESE   : PROJECTION DE LA BOULE UNITE POUR LE FROTTEMENT
! IN  MPROJ  : MATRICE DE L'OPERATEUR DE PROJECTION
! IN  DLAGRF : LAGRANGES DE FROTTEMENT AU POINT D'INTÉGRATION
! IN  TYPMAI : NOM DE LA MAILLE ESCLAVE D'ORIGINE (QUADRATIQUE)
! IN  NSINGE : NOMBRE DE FONCTION SINGULIERE ESCLAVE
! IN  NSINGM : NOMBRE DE FONCTION SINGULIERE MAITRE
! IN  RRE    : SQRT LST ESCLAVE
! IN  RRM    : SQRT LST MAITRE
! IN  NVIT   : POINT VITAL OU PAS
! IN  INADH  : POINT ADHERENT OU PAS
! I/O VTMP   : VECTEUR SECOND MEMBRE ELEMENTAIRE DE CONTACT/FROTTEMENT
! ----------------------------------------------------------------------
    integer :: i, j, k, ii, ini, pli, xoula, iin, nddle
    integer :: nne, nnes, nnem, nnm, nnms, ddles, ddlem, ddlms, ddlmm
    real(kind=8) :: vectt(3), tt(2), vv, t
! ----------------------------------------------------------------------
!
! --- INITIALISATIONS
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
    do 100 i = 1, 3
        vectt(i) = 0.d0
100  end do
    do 110 i = 1, 2
        tt(i) = 0.d0
110  end do
!
! --- CALCUL DE RESE.C(*,I)
!
    do 120 i = 1, ndim
        do 130 k = 1, ndim
            vectt(i) = rese(k)*mproj(k,i) + vectt(i)
130      continue
120  end do
!
! --- CALCUL DE T.(T-P)
!
    do 140 i = 1, ndim
        t = dlagrf(1)*tau1(i)+dlagrf(2)*tau2(i)-rese(i)
        tt(1)= t*tau1(i)+tt(1)
        if (ndim .eq. 3) tt(2)= t*tau2(i)+tt(2)
140  end do
!
! --------------------- CALCUL DE [L1_FROT]-----------------------------
!
    if (nnm .ne. 0) then
!
        do 10 j = 1, ndim
            do 20 i = 1, ndeple
! --- BLOCS ES,CL ; ES,EN ; (ES,SI)
                if (nconta .eq. 3 .and. ndim .eq. 3) then
                    vv = jacobi*hpg*coefff*(dlagrc-coefcr*jeu)*vectt( j)*ffe(i)
                else
                    vv = jacobi*hpg*coefff*dlagrc*vectt(j)*ffe(i)
                endif
                call indent(i, ddles, ddlem, nnes, iin)
                ii = iin + j
                vtmp(ii) = -vv
                ii = ii + ndim
                vtmp(ii) = vv
                do 25 k = 1, nsinge
                    ii = ii + ndim
                    vtmp(ii) = rre * vv
25              continue
20          continue
            do 30 i = 1, nnm
                if (nconta .eq. 3 .and. ndim .eq. 3) then
                    vv = jacobi*hpg*coefff* (dlagrc-coefcr*jeu)*vectt( j)*ffm(i)
                else
                    vv = jacobi*hpg*coefff* dlagrc*vectt(j)*ffm(i)
                endif
                call indent(i, ddlms, ddlmm, nnms, iin)
                ii = nddle + iin + j
                vtmp(ii) = vv
                ii = ii + ndim
                vtmp(ii) = vv
                do 35 k = 1, nsingm
                    ii = ii + ndim
                    vtmp(ii) = rrm * vv
35              continue
30          continue
10      end do
    else
!
        do 60 j = 1, ndim
            do 70 i = 1, ndeple
! --- BLOCS ES,SI
                if (nconta .eq. 3 .and. ndim .eq. 3) then
                    vv = jacobi*hpg*coefff* (dlagrc-coefcr*jeu)*vectt( j)*ffe(i)
                else
                    vv = jacobi*hpg*coefff* dlagrc*vectt(j)*ffe(i)
                endif
                call indent(i, ddles, ddlem, nnes, iin)
                ii = iin + j
                vtmp(ii) = rre * vv
70          continue
60      end do
    endif
!
! --------------------- CALCUL DE [L3]----------------------------------
!
    if (nvit .eq. 1) then
        do 40 i = 1, nnc
            call xplma2(ndim, nne, nnes, ddles, i,&
                        nfhe, pli)
            do 50 j = 1, ndim-1
                ii = pli+j
                if (lpenaf) then
                    vtmp(ii) = jacobi*hpg*tt(j)*ffc(i)
                else
                    if (nconta .eq. 3 .and. ndim .eq. 3) then
                        vtmp(ii) = jacobi*hpg*tt(j)*ffc(i)/coeffr
                    else
                        vtmp(ii) = jacobi*hpg*tt(j)*ffc(i)*coefff* dlagrc/coeffr
                    endif
                endif
50          continue
40      end do
    endif
!
end subroutine
