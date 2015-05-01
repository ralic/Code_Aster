subroutine xdelt2(elp, n, ndime, ksi,&
                  ptint, ndim, tabco, tabls, ipp, ip,&
                  delta)
    implicit none
!
#   include "jeveux.h"
#   include "asterc/r8prem.h"
#   include "asterfort/assert.h"
#   include "asterfort/elraca.h"
#   include "asterfort/elrfdf.h"
#   include "asterfort/elrfvf.h"
#   include "asterfort/matini.h"
#   include "asterfort/matinv.h"
#   include "asterfort/provec.h"
#   include "asterfort/vecini.h"
    integer :: ndime, ndim, ipp, ip, n(3)
    real(kind=8) :: ksi(ndim), delta(ndime), ptint(*), tabco(*), tabls(*)
    character(len=8) :: elp
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
!                 CALCUL DE LA QUANTITE A MINIMISER POUR LE CALCUL
!                    DES COORDONNEES DU PT MILIEU DE LA FISSURE
!
!     ENTREE
!       ELP     : TYPE DE L'ELEMENT
!       N       : LES INDICES DES NOEUX D'UNE FACE DANS L'ELEMENT PARENT
!       NDIM    : DIMENSION TOPOLOGIQUE DU MAILLAGE
!       KSI     : COORDONNEES DE REFERENCE DU POINT
!       PTINT  : COORDONNÉES DES POINTS D'INTERSECTION
!       TABCO   : COORDONNEES DES NOEUDS DE L'ELEMENT
!       TABLS   : VALEUR DES LSN DES NOEUDS DE L'ELEMENT
!
!     SORTIE
!       DELTA   : QUANTITE A MINIMISER
!     ----------------------------------------------------------------
!
    integer :: nbnomx
    parameter     (nbnomx = 27)
    integer :: nbfamx
    parameter     (nbfamx = 20)
!
    character(len=8) :: fapg(nbfamx)
    integer :: ibid, ibid2, nnos, nbfpg, nbpg(nbfamx), nno
    real(kind=8) :: vol, refcoo(3*nbnomx)
    real(kind=8) :: ff(nbnomx), dff(3, nbnomx)
    integer :: i, j, k, nderiv
    real(kind=8) :: p(ndim), m(ndim), nor(ndim)
    real(kind=8) :: pint1(ndim), pint2(ndim)
    real(kind=8) :: r(ndime), det, dx
    real(kind=8) :: jac(ndime, ndime), inv(ndime, ndime)
    real(kind=8) :: v1(3), v2(3), nf(3)
!
!
!......................................................................
!
! --- CALCUL DES FONCTIONS DE FORME ET DE LEUR DERIVEES EN UN POINT
! --- DANS LA MAILLE
!
!
    call vecini(ndim, 0.d0, pint1)
    call vecini(ndim, 0.d0, pint2)
!
    do  i = 1, ndim
        pint1(i)=ptint(ndim*(ipp-1)+i)
        pint2(i)=ptint(ndim*(ip-1)+i)
    end do
!
!     CALCUL DES FONCTIONS DE FORME DE L'ELEMENT EN KSI
    call elrfvf(elp, ksi, nbnomx, ff, nno)
!
!     CALCUL DES DERIVEES FONCTIONS DE FORME DE L'ELEMENT EN KSI
    call elrfdf(elp, ksi, ndime*nbnomx, dff, nno,&
                nderiv)
!
    call vecini(ndime, 0.d0, r)
    call matini(ndime, ndime, 0.d0, jac)
! --- CALCUL DE R1,DERIVEES PREMIERES DE R1 EN KSI
! ---           R1 : LEVEL SET NORMALE
    do  j = 1, nno
        r(1)=r(1)+ff(j)*tabls(j)
        do 70 i = 1, ndime
            jac(1,i)=jac(1,i)+dff(i,j)*tabls(j)
70      continue
    end do
!
    call vecini(ndim, 0.d0, m)
    call vecini(ndim, 0.d0, nor)
    call vecini(ndim, 0.d0, p)
!
! --- CALCUL DE R2 EN KSI
! ---           R2 : PLAN MEDIATEUR PASSANT PAR LES 2 PTS INTER
!               SPECIFIQUE NDIM=2
!
!     CHAQUE POINT P(X,Y,Z) DU PLAN MEDIATEUR VERIFIE NOR*MP=0
!                   M=((XI+XII)/2,(YI+YII)/2,(ZI+ZII)/2)
!                   X = SUM(FF*TABCO(1))
!                   Y = SUM(FF*TABCO(1))
!                   Z = SUM(FF*TABCO(1))
!                   NOR = (XII-XI,YII-YI,ZII-ZI)
!
    do  i = 1, ndim
        do 30 j = 1, nno
            p(i)=p(i)+ff(j)*tabco(ndim*(j-1)+i)
30      continue
        m(i)=0.5d0*(pint1(i)+pint2(i))
        nor(i)=pint2(i)-pint1(i)
        r(2)=r(2)+(p(i)-m(i))*nor(i)
    end do
!
!     CALCUL DES DERIVEES PREMIERES DE R2 EN KSI
    do  i = 1, ndime
        do 45 k = 1, ndim
            dx=0.d0
            do 50 j = 1, nno
                dx=dx+dff(i,j)*tabco(ndim*(j-1)+k)
50          continue
            jac(2,i)=jac(2,i)+dx*nor(k)
45      continue
    end do
!
! --- CALCUL DE R3,DR2 EN KSI EN FONTION DE LA FACE A APPARTENIR
! ---           R3 : LA COTE DE L'ELEMENT ENFANT
!
    if (ndime .eq. 3) then
! --- RECUPERATION DES COORDONNEES DANS L'ESPACE DE REFERENCE DES
! --- NOEUDS DE L'ELEMENT PARENT
        call vecini(3*nbnomx, 0.d0, refcoo)
        call elraca(elp, ibid, ibid2, nnos, nbfpg,&
                    fapg, nbpg, refcoo, vol)
!
! ---  CALCUL DES VECTEURS N(1)N(2) ET N(1)N(3)
        call vecini(3, 0.d0, v1)
        call vecini(3, 0.d0, v2)
        do 80 i = 1, ndim
            v1(i)=refcoo(ndim*(n(2)-1)+i) - refcoo(ndim*(n(1)-1)+i)
            v2(i)=refcoo(ndim*(n(3)-1)+i) - refcoo(ndim*(n(1)-1)+i)
80      continue
!
! calcul de la NORMALE a la face
        call provec(v1, v2, nf)
!
        do 110 i = 1, ndim
            r(3)=r(3)+nf(i)*(ksi(i)-refcoo(ndim*(n(1)-1)+i))
            jac(3,i)=nf(i)
110      continue
    endif
!
    det=0.d0
    call matini(ndime, ndime, 0.d0, inv)
! --- CALCUL L'INVERSE DE LA MATRICE JACOBIENNE
    call matinv('S', ndime, jac, inv, det)
!
    call vecini(ndime, 0.d0, delta)
! --- CALCUL DES QUANTITES A ANNULER
!     CALCUL DE DELTAS
    do  i = 1, ndime
        do 100 j = 1, ndime
            delta(i)=delta(i)+inv(i,j)*r(j)
100      continue
    end do
!
end subroutine
