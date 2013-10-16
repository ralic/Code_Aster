subroutine xdelt5(elp, nno, n, ndime, ksi,&
                  tabco, ndim, tab, delta)
    implicit none
!
#    include "jeveux.h"
#    include "asterfort/jemarq.h"
#    include "asterfort/jedema.h"
#    include "asterfort/elraca.h"
#    include "asterfort/elrfdf.h"
#    include "asterfort/elrfvf.h"
#    include "asterfort/matini.h"
#    include "asterfort/matinv.h"
#    include "asterfort/provec.h"
#    include "asterfort/vecini.h"
#    include "blas/ddot.h"
    integer :: ndime, ndim, nno, n(3)
    real(kind=8) :: ksi(ndime), delta(ndime), tabco(*), tab(8, ndim)
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
!       N       : NUMERO PARENT DES TROIS NOEUDS DE FACE
!       NDIM    : DIMENSION TOPOLOGIQUE DU MAILLAGE
!       NNO     : NOMBRE DE NOEUX DE L'ELEMENT
!       KSI     : COORDONNEES DE REFERENCE DU POINT
!       TAB     : COORDONNEES DES 8 POINTS DE 'QU8'
!       TABCO   : COORDONNEES DES NOEUDS DE L'ELEMENT
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
    integer :: ibid, ibid2, nnos, nbfpg, nbpg(nbfamx)
    real(kind=8) :: vol, refcoo(3*nbnomx), centre(ndime)
    real(kind=8) :: ff1(nno), dff1(3, nbnomx)
    real(kind=8) :: ff2(8), dff2(3, 8)
    integer :: i, j, k, nderiv1, nderiv2, nno2, nno1
    real(kind=8) :: p(ndim), m(ndim)
    real(kind=8) :: t1(ndim), t2(ndim)
    real(kind=8) :: r(ndime), det, dx
    real(kind=8) :: jac(ndime, ndime), inv(ndime, ndime)
    real(kind=8) :: v1(3), v2(3), nf(3)
!
!......................................................................
!
     call jemarq()
! --- CALCUL DES FONCTIONS DE FORME ET DE LEUR DERIVEES EN UN POINT
! --- DANS LA MAILLE
!     CALCUL DES FONCTIONS DE FORME DE L'ELEMENT EN KSI
    call elrfvf(elp, ksi, nbnomx, ff1, nno1)
!     CALCUL DES DERIVEES FONCTIONS DE FORME DE L'ELEMENT EN KSI
    call elrfdf(elp, ksi, ndime*nbnomx, dff1, nno1,&
                nderiv1)
!
!
    call vecini(ndime, 0.d0, r)
    call matini(ndime, ndime, 0.d0, jac)
!
! --- CALCUL DE R1,DR1 EN KSI EN FONTION DE LA FACE A APPARTENIR
! ---           R1 : LA COTE DE L'ELEMENT ENFANT
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
        do 10 i = 1, ndim
            v1(i)=refcoo(ndim*(n(2)-1)+i) - refcoo(ndim*(n(1)-1)+i)
            v2(i)=refcoo(ndim*(n(3)-1)+i) - refcoo(ndim*(n(1)-1)+i)
10      continue
!
! calcul de la NORMALE a la face
        call provec(v1, v2, nf)
!
        do 20 i = 1, ndime
            r(3)=r(3)+nf(i)*(ksi(i)-refcoo(ndim*(n(1)-1)+i))
            jac(3,i)=nf(i)
20      continue
    endif
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
    call vecini(ndime, 0.d0, centre)
    call elrfvf('QU8', centre, nbnomx, ff2, nno2)
    call vecini(ndime, 0.d0, centre)
    call elrfdf('QU8', centre, ndime*nbnomx, dff2, nno2,&
                nderiv2)
!
    do  i = 1, ndim
        do 31 j = 1, nno
            p(i)=p(i)+ff1(j)*tabco(ndim*(j-1)+i)
31      continue
    end do
!
    do  i = 1, ndim
        do 41 j = 1, nno2
            m(i)=m(i)+tab(j,i)*ff2(j)
            t1(i)=t1(i)+tab(j,i)*dff2(1,j)
            t2(i)=t2(i)+tab(j,i)*dff2(2,j)
41      continue
        r(1)=r(1)+(p(i)-m(i))*t1(i)
        r(2)=r(2)+(p(i)-m(i))*t2(i)
    end do
!
    do  i = 1, ndime
        do 51 k = 1, ndim
            dx=0.d0
            do 52 j = 1, nno
                dx=dx+dff1(i,j)*tabco(ndim*(j-1)+k)
52          continue
            jac(1,i)=jac(1,i)+dx*t1(k)
            jac(2,i)=jac(2,i)+dx*t2(k)
51      continue
    end do
!
    call matini(ndime, ndime, 0.d0, inv)
! --- CALCUL L'INVERSE DE LA MATRICE JACOBIENNE
    call matinv('S', ndime, jac, inv, det)
!
    call vecini(ndime, 0.d0, delta)
! --- CALCUL DES QUANTITES A ANNULER
!     CALCUL DE DELTAS
    do  i = 1, ndime
        do 71 j = 1, ndime
            delta(i)=delta(i)+inv(i,j)*r(j)
71      continue
    end do
!
    call jedema()
end subroutine
