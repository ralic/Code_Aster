subroutine gdclci(fm, df, em)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    include 'blas/ddot.h'
    real(kind=8) :: fm(3, 3), df(3, 3), em(6)
! ----------------------------------------------------------------------
!       INTEGRATION DES LOIS EN GRANDES DEFORMATIONS CANO-LORENTZ
!                  CALCUL DES ELEMENTS CINEMATIQUES
! ----------------------------------------------------------------------
! IN  FM    DEFORMATION AU DEBUT DU PAS DE TEMPS
! IN  DF    INCREMENT DE DEFORMATION PENDANT LE PAS DE TEMPS
! IN  EM    DEFORMATION ELASTIQUE (-) AU DEBUT DU PAS DE TEMPS
! ----------------------------------------------------------------------
!  COMMON GRANDES DEFORMATIONS CANO-LORENTZ
!
    integer :: ind1(6), ind2(6)
    real(kind=8) :: kr(6), rac2, rc(6)
    real(kind=8) :: lambda, mu, deuxmu, unk, troisk, cother
    real(kind=8) :: jm, dj, jp, djdf(3, 3)
    real(kind=8) :: etr(6), dvetr(6), eqetr, tretr, detrdf(6, 3, 3)
    real(kind=8) :: dtaude(6, 6)
!
    common /gdclc/&
     &          ind1,ind2,kr,rac2,rc,&
     &          lambda,mu,deuxmu,unk,troisk,cother,&
     &          jm,dj,jp,djdf,&
     &          etr,dvetr,eqetr,tretr,detrdf,&
     &          dtaude
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    integer :: ij, kl, i, j, k, l
    real(kind=8) :: bem(6), pdf(6, 6), betr(6)
! ----------------------------------------------------------------------
!
!
!  CALCUL DES JACOBIENS
! ----------------------
!
    jm=fm(1,1)*(fm(2,2)*fm(3,3)-fm(2,3)*fm(3,2))&
     &  -fm(2,1)*(fm(1,2)*fm(3,3)-fm(1,3)*fm(3,2))&
     &  +fm(3,1)*(fm(1,2)*fm(2,3)-fm(1,3)*fm(2,2))
!
    dj=df(1,1)*(df(2,2)*df(3,3)-df(2,3)*df(3,2))&
     &  -df(2,1)*(df(1,2)*df(3,3)-df(1,3)*df(3,2))&
     &  +df(3,1)*(df(1,2)*df(2,3)-df(1,3)*df(2,2))
!
    jp=jm*dj
!
!
!  CALCUL DE ETR
! ---------------
!
!    CALCUL DE BE EN T-
    do 10 ij = 1, 6
        bem(ij) = kr(ij) - 2*em(ij)
10  end do
!
!
!    CALCUL PDF(AB,KL) = DF(A,K)*DF(B,L) SYMETRISE ET RACINE DE 2
    do 100 ij = 1, 6
        i = ind1(ij)
        j = ind2(ij)
        do 110 kl = 1, 6
            k = ind1(kl)
            l = ind2(kl)
            pdf(ij,kl)=rc(ij)*rc(kl)*(df(i,k)*df(j,l)+df(j,k)*df(i,l))&
            /2
110      continue
100  end do
!
!
!    CALCUL DE BE TRIAL : BETR(AB) = PDF(AB,IJ):BEM(IJ)  ET  E TRIAL
    do 200 ij = 1, 6
        betr(ij) = ddot(6, pdf(ij,1),6, bem,1)
        etr(ij) = (kr(ij)-betr(ij))/2
200  continue
!
!
!    CALCUL DES INVARIANTS DE E TRIAL
    tretr = etr(1)+etr(2)+etr(3)
    do 300 ij = 1, 6
        dvetr(ij) = etr(ij) - tretr/3.d0*kr(ij)
300  end do
    eqetr = sqrt(1.5d0 * ddot(6,dvetr,1,dvetr,1))
!
end subroutine
