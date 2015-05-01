subroutine gdsmci(fm, df, em)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "blas/dcopy.h"
#include "blas/ddot.h"
#include "blas/dscal.h"
    real(kind=8) :: fm(3, 3), df(3, 3), em(6)
!
! ----------------------------------------------------------------------
!       INTEGRATION DES LOIS EN GRANDES DEFORMATIONS SIMO-MIEHE
!                  CALCUL DES ELEMENTS CINEMATIQUES
! ----------------------------------------------------------------------
! IN  FM    : DEFORMATION AU DEBUT DU PAS DE TEMPS
! IN  DF    : INCREMENT DE DEFORMATION PENDANT LE PAS DE TEMPS
! IN  EM    : DEFORMATION ELASTIQUE AU DEBUT DU PAS DE TEMPS (VIM)
! ----------------------------------------------------------------------
! COMMON GRANDES DEFORMATIONS SIMO - MIEHE
!
    integer :: ind(3, 3), ind1(6), ind2(6)
    real(kind=8) :: kr(6), rac2, rc(6), id(6, 6)
    real(kind=8) :: bem(6), betr(6), dvbetr(6), eqbetr, trbetr
    real(kind=8) :: jp, dj, jm, dfb(3, 3)
    real(kind=8) :: djdf(3, 3), dbtrdf(6, 3, 3)
!
    common /gdsmc/&
     &            bem,betr,dvbetr,eqbetr,trbetr,&
     &            jp,dj,jm,dfb,&
     &            djdf,dbtrdf,&
     &            kr,id,rac2,rc,ind,ind1,ind2
! ----------------------------------------------------------------------
!
    integer :: ij, kl, i, j, k, l
    real(kind=8) :: pdf(6, 6)
!
    real(kind=8) :: e(3, 3), dete, fp(3, 3)
! ----------------------------------------------------------------------
!
!
! 1 - CALCUL DES JACOBIENS ET DE DF-BARRE
! ----------------------------------------
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
    call dcopy(9, df, 1, dfb, 1)
    call dscal(9, dj**(-1.d0/3.d0), dfb, 1)
!
!
!
!  CALCUL DE BETR
! ---------------
!
!    CALCUL DE BE-BARRE EN T-
    do 10 ij = 1, 6
        bem(ij) = (kr(ij) - 2*em(ij))/jm**(2.d0/3.d0)
10  end do
!
!
!    CALCUL PDF(IJ,KL) = DFB(I,K)*DFB(J,L) SYMETRISE ET RACINE DE 2
    do 100 ij = 1, 6
        i = ind1(ij)
        j = ind2(ij)
        do 110 kl = 1, 6
            k = ind1(kl)
            l = ind2(kl)
            pdf(ij,kl)=rc(ij) * rc(kl) * (dfb(i,k)*dfb(j,l)+dfb(j,k)*&
            dfb(i,l) ) / 2.d0
110      continue
100  end do
!
!
!    CALCUL DE BE TRIAL : BETR(AB) = PDF(AB,IJ):BEM(IJ)
    do 200 ij = 1, 6
        betr(ij) = ddot(6, pdf(ij,1),6, bem,1)
200  end do
!
!
    do 300 i = 1, 3
        do 320 j = 1, 3
            fp(i,j) = 0
            do 340 k = 1, 3
                fp(i,j) = fp(i,j) + df(i,k)*fm(k,j)
340          continue
320      continue
300  end do
!
    do 400 i = 1, 3
        do 420 j = 1, 3
            e(i,j) = 0
            do 440 k = 1, 3
                e(i,j) = e(i,j) + fp(i,k)*fp(j,k)
440          continue
420      continue
400  end do
!
    dete=e(1,1)*(e(2,2)*e(3,3)-e(2,3)*e(3,2))&
     &  -e(2,1)*(e(1,2)*e(3,3)-e(1,3)*e(3,2))&
     &  +e(3,1)*(e(1,2)*e(2,3)-e(1,3)*e(2,2))
!
    do 500 i = 1, 3
        do 520 j = 1, 3
            e(i,j) = e(i,j)/dete**(1.d0/3.d0)
520      continue
500  continue
!
!
! 3.4 - CALCUL DE LA PARTIE DEVIATORIQUE ET INVARIANTS DE BETR EN T+
!
    trbetr = betr(1)+betr(2)+betr(3)
    do 600 ij = 1, 6
        dvbetr(ij) = betr(ij) - trbetr/3.d0*kr(ij)
600  end do
    eqbetr = sqrt(1.5d0 * ddot(6,dvbetr,1,dvbetr,1))
!
end subroutine
