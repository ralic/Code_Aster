subroutine xdelt4(elp, nno1, ndim, ksi, ptint,&
                  pmilie, tabco, tabls, delta)
    implicit none
!
#    include "jeveux.h"
#    include "asterfort/jedema.h"
#    include "asterfort/jemarq.h"
#    include "asterfort/elrfdf.h"
#    include "asterfort/elrfvf.h"
#    include "asterfort/matini.h"
#    include "asterfort/matinv.h"
#    include "asterfort/vecini.h"
    integer :: ndim, nno1
    real(kind=8) :: ksi(ndim), delta(ndim), ptint(*), tabco(*), tabls(*)
    real(kind=8) :: pmilie(*)
    character(len=8) :: elp
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
!                 CALCUL DE LA QUANTITE A MINIMISER POUR LE CALCUL
!                    DES COORDONNEES DU PT MILIEU DE LA FISSURE
!
!     ENTREE
!       ELP     : TYPE DE L'ELEMENT
!       NDIM    : DIMENSION TOPOLOGIQUE DU MAILLAGE
!       NNO     : NOMBRE DE NOEUX DE L'ELEMENT
!       KSI     : COORDONNEES DE REFERENCE DU POINT
!       PTINT  : COORDONNÉES DES POINTS D'INTERSECTION
!       TABCO   : COORDONNEES DES NOEUDS DE L'ELEMENT
!       TABLS  : VALEUR DES LSN DES NOEUDS DE L'ELEMENT
!
!     SORTIE
!       DELTA   : QUANTITE A MINIMISER
!     ----------------------------------------------------------------
!
    integer :: nbnomx
    parameter     (nbnomx = 27)
!
    real(kind=8) :: ff1(nno1), dff1(3, nbnomx), ff2(8), dff2(ndim, 8),x(ndim)
    integer :: i, j, k, nderiv1, nderiv2, nno2
    real(kind=8) :: p(ndim), m(ndim), t1(ndim), t2(ndim), tab(8, ndim)
    real(kind=8) :: r(ndim), det, dx
    real(kind=8) :: jac(ndim, ndim), inv(ndim, ndim)
    call jemarq()
!
!
!......................................................................
!
! --- CALCUL DES FONCTIONS DE FORME ET DE LEUR DERIVEES EN UN POINT
! --- DANS LA MAILLE
!     CALCUL DES FONCTIONS DE FORME DE L'ELEMENT EN KSI
    call elrfvf(elp, ksi, nbnomx, ff1, nno1)
!
!     CALCUL DES DERIVEES FONCTIONS DE FORME DE L'ELEMENT EN KSI
    call elrfdf(elp, ksi, ndim*nbnomx, dff1, nno1,&
                nderiv1)
!
!
    call vecini(ndim, 0.d0, r)
    call matini(ndim, ndim, 0.d0, jac)
    call matini(8, ndim, 0.d0, tab)
! --- CALCUL DE R1,DERIVEES PREMIERES DE R1 EN KSI
! ---           R1 : LEVEL SET NORMALE
    do 10 j = 1, nno1
        r(1)=r(1)+ff1(j)*tabls(j)
        do 11 i = 1, ndim
            jac(1,i)=jac(1,i)+dff1(i,j)*tabls(j)
11      continue
10  end do
!
    call vecini(ndim, 0.d0, m)
    call vecini(ndim, 0.d0, t1)
    call vecini(ndim, 0.d0, t2)
    call vecini(ndim, 0.d0, p)
!
!     CALCUL DES DERIVEES PREMIERES DE R2 R3EN KSI
! ---           R2 R3 : PLAN MEDIATEUR PASSANT PAR LES 2 PTS INTER
!               SPECIFIQUE NDIM=2
!
!     CHAQUE POINT P(X,Y,Z) DU PLAN MEDIATEUR VERIFIE NOR*MP=0
!                   M=((XI+XII)/2,(YI+YII)/2,(ZI+ZII)/2)
!                   X = SUM(FF*TABCO(1))
!                   Y = SUM(FF*TABCO(1))
!                   Z = SUM(FF*TABCO(1))
!                   NOR = (XII-XI,YII-YI,ZII-ZI)
    do 30 i = 1, ndim
        tab(1,i)=ptint(ndim*(1-1)+i)
        tab(2,i)=ptint(ndim*(2-1)+i)
        tab(3,i)=ptint(ndim*(4-1)+i)
        tab(4,i)=ptint(ndim*(3-1)+i)
        tab(5,i)=pmilie(ndim*(9-1)+i)
        tab(6,i)=pmilie(ndim*(10-1)+i)
        tab(7,i)=pmilie(ndim*(11-1)+i)
        tab(8,i)=pmilie(ndim*(12-1)+i)
30  end do
!
    call vecini(ndim, 0.d0, x)
    call elrfvf('QU8', x, nbnomx, ff2, nno2)
    call vecini(ndim, 0.d0, x)
    call elrfdf('QU8', x, ndim*nbnomx, dff2, nno2,&
                nderiv2)
!
    do 20 i = 1, ndim
        do 21 j = 1, nno1
            p(i)=p(i)+ff1(j)*tabco(ndim*(j-1)+i)
21      continue
20  end do
!
    do 40 i = 1, ndim
        do 41 j = 1, nno2
            m(i)=m(i)+tab(j,i)*ff2(j)
            t1(i)=t1(i)+tab(j,i)*dff2(1,j)
            t2(i)=t2(i)+tab(j,i)*dff2(2,j)
41      continue
        r(2)=r(2)+(p(i)-m(i))*t1(i)
        r(3)=r(3)+(p(i)-m(i))*t2(i)
40  end do
!
    do 70 i = 1, ndim
        do 71 k = 1, ndim
            dx=0.d0
            do 72 j = 1, nno1
                dx=dx+dff1(i,j)*tabco(ndim*(j-1)+k)
72          continue
            jac(2,i)=jac(2,i)+dx*t1(k)
            jac(3,i)=jac(3,i)+dx*t2(k)
71      continue
70  end do
!
    det=0.d0
    call matini(ndim, ndim, 0.d0, inv)
! --- CALCUL L'INVERSE DE LA MATRICE JACOBIENNE
    call matinv('S', ndim, jac, inv, det)
!
    call vecini(ndim, 0.d0, delta)
! --- CALCUL DES QUANTITES A ANNULER
!     CALCUL DE DELTAS
    do 90 i = 1, ndim
        do 91 j = 1, ndim
            delta(i)=delta(i)+inv(i,j)*r(j)
91      continue
90  end do
!
    call jedema()
end subroutine
