subroutine xmmco1(ndim, nno, dsidep, pp, p,&
                  nd, nfh, ddls, jac, ffp,&
                  singu, rr, tau1, tau2, mmat)
    implicit none
    include 'jeveux.h'
    include 'asterfort/matcox.h'
    include 'asterfort/matini.h'
    integer :: ndim, nno, nfh, ddls, singu
    real(kind=8) :: mmat(216, 216), dsidep(6, 6)
    real(kind=8) :: ffp(27), jac, nd(3)
    real(kind=8) :: pp(3, 3), p(3, 3), rr
! ----------------------------------------------------------------------
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
!
! ROUTINE CONTACT (METHODE XFEM HPP - CALCUL ELEM.)
!
! --- CALCUL DES MATRICES DE COHESION
!
! ----------------------------------------------------------------------
!
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  NNO    : NOMBRE DE NOEUDS DE L'ELEMENT DE REF PARENT
! IN  DSIDEP :
! IN  PP     :
! IN  P      :
! IN  ND     : DIRECTION NORMALE
! IN  NFH    : NOMBRE DE FONCTIONS HEAVYSIDE
! IN  DDLS   : NOMBRE DE DDL (DEPL+CONTACT) À CHAQUE NOEUD SOMMET
! IN  JAC    : PRODUIT DU JACOBIEN ET DU POIDS
! IN  FFP    : FONCTIONS DE FORME DE L'ELEMENT PARENT
! IN  SINGU  : 1 SI ELEMENT SINGULIER, 0 SINON
! IN  RR     : DISTANCE AU FOND DE FISSURE
! IN  TAU1   : PREMIERE DIRECTION TANGENTE
! IN  AM     :
! I/O MMAT   : MATRICE ELEMENTAITRE DE CONTACT/FROTTEMENT
!
!
!
!
    integer :: i, j
    real(kind=8) :: ddt1(3, 3), ddt2(3, 3), ddt3(3, 3), ddt4(3, 3)
    real(kind=8) :: tau1(3), tau2(3)
!
! ----------------------------------------------------------------------
!
!     INITIALISATION
    call matini(3, 3, 0.d0, ddt1)
    call matini(3, 3, 0.d0, ddt2)
    call matini(3, 3, 0.d0, ddt3)
    call matini(3, 3, 0.d0, ddt4)
!
!           II.2.3. CALCUL DES MATRICES DE COHESION
!        ..............................
!
    do 216 i = 1, ndim
        do 217 j = 1, ndim
            ddt1(i,j)=dsidep(1,1)*nd(i)*nd(j)
            if (ndim .eq. 2) then
                ddt2(i,j)=dsidep(1,2)*nd(i)*tau1(j) +dsidep(2,1)*tau1(&
                i)*nd(j)
            else if (ndim.eq.3) then
                ddt2(i,j)=dsidep(1,2)*nd(i)*tau1(j) +dsidep(1,3)*nd(i)&
                *tau2(j) +dsidep(2,1)*tau1(i)*nd(j) +dsidep(3,1)*tau2(&
                i)*nd(j)
            endif
            ddt3(i,j)=ddt2(i,j)
            if (ndim .eq. 2) then
                ddt4(i,j)=dsidep(2,2)*tau1(i)*tau1(j)
            else if (ndim.eq.3) then
                ddt4(i,j)=dsidep(2,2)*tau1(i)*tau1(j) +dsidep(2,3)*&
                tau1(i)*tau2(j) +dsidep(3,2)*tau2(i)*tau1(j) +dsidep(&
                3,3)*tau2(i)*tau2(j)
            endif
217      continue
216  end do
!
    call matcox(ndim, pp, ddt1, ddt2, ddt3,&
                ddt4, p, nno, nfh*ndim, ddls,&
                jac, ffp, singu, rr, mmat)
!
end subroutine
