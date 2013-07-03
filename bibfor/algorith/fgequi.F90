subroutine fgequi(tz, typz, ndim, equi)
    implicit none
#include "asterfort/jacobi.h"
#include "asterfort/lchydr.h"
#include "asterfort/lciv2e.h"
#include "asterfort/lciv2s.h"
#include "asterfort/lcqeqv.h"
#include "asterfort/r8inir.h"
    integer :: ndim
    real(kind=8) :: tz(*), equi(*)
    character(len=*) :: typz
! ----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: josselin.delmas at edf.fr
!
!     BUT:
!       CALCULER LES GRANDEURS EQUIVALENTES SUIVANTES
!       . CONTRAINTES EQUIVALENTES               (= 17 VALEURS)
!          . VON MISES                               (= 1 VALEUR)
!          . TRESCA                                  (= 1 VALEUR)
!          . CONTRAINTES PRINCIPALES                 (= 3 VALEURS)
!          . VON-MISES * SIGNE (PRESSION)            (= 1 VALEUR)
!          . DIRECTIONS DES CONTRAINTES PRINCIPALES  (= 3*3 VALEURS)
!          . TRACE                                   (= 1 VALEUR)
!          . TAUX DE TRIAXIALITE                     (= 1 VALEUR)
!
!       . DEFORMATIONS EQUIVALENTES              (= 14 VALEURS)
!          . SECOND INVARIANT                        (= 1 VALEUR)
!          . DEFORMATIONS PRINCIPALES                (= 3 VALEURS)
!          . 2EME INV. * SIGNE (1ER.INV.)            (= 1 VALEUR)
!          . DIRECTIONS DES DEFORMATIONS PRINCIPALES (= 3*3 VALEURS)
!
! ----------------------------------------------------------------------
!     IN     TZ    TENSEUR CONTRAINTE OU DEFORMATION (XX YY ZZ XY XZ YZ)
!            TYPZ  TYPE DU TENSEUR 'SIGM' OU 'EPSI' SUIVI EVENTUELLEMENT
!            DES CARACTERES _DIR POUR CALCULER LES VECTEURS DIRECTEURS
!            NDIM  DIMENSION ESPACE 3 OU 2
!     OUT    EQUI  VECTEUR DES GRANDEURS EQUIVALENTES
! ----------------------------------------------------------------------
    real(kind=8) :: t(6), tn(6), tr(6), tu(6), vecp(3, 3), nul(6)
    real(kind=8) :: rac2, hyd, jacaux(3)
    real(kind=8) :: tol, toldyn
    integer :: nbvec, nperm
    integer :: type, iordre
    character(len=8) :: typ
    common /tdim/  nt,nd
!-----------------------------------------------------------------------
    integer :: i, j, nd, nitjac, nt
!-----------------------------------------------------------------------
    data   nul     /6*0.d0/
    data   nperm ,tol,toldyn    /12,1.d-10,1.d-2/
! ----------------------------------------------------------------------
    typ=typz
!
    if (ndim .eq. 3) then
        nt = 6
        nd = 3
    else if (ndim .eq. 2) then
        nt = 4
        nd = 3
    else if (ndim .eq. 1) then
        nt = 1
        nd = 1
    endif
!
    call r8inir(6, 0.d0, t, 1)
    do 30 i = 1, nt
        t(i) = tz(i)
30  end do
!
! --- TENSEUR TN = (XX YY ZZ RAC2.XY RAC2.XZ RAC2.YZ) (POUR LCIV2E)
!
    rac2 = sqrt (2.d0)
    do 10 i = 1, nd
        tn(i) = t(i)
10  end do
    do 12 i = nd+1, nt
        tn(i) = rac2 * t(i)
12  end do
!
! --- MATRICE TR = (XX XY XZ YY YZ ZZ) (POUR JACOBI)
!
    tr(1) = t(1)
    tr(2) = t(4)
    tr(3) = t(5)
    tr(4) = t(2)
    tr(5) = t(6)
    tr(6) = t(3)
!
! --- MATRICE UNITE = (1 0 0 1 0 1) (POUR JACOBI)
!
    tu(1) = 1.d0
    tu(2) = 0.d0
    tu(3) = 0.d0
    tu(4) = 1.d0
    tu(5) = 0.d0
    tu(6) = 1.d0
!
! --- VALEURS PRINCIPALES
    nbvec = 3
!
    do 20 i = 1, nbvec
        do 22 j = 1, nbvec
            vecp(j,i) = 0.0d0
22      continue
20  end do
!
! --- DEFORMATIONS
!
    if (typ(1:4) .eq. 'EPSI') then
! ------ SECOND INVARIANT
        equi(1) = lciv2e (tn)
        if (lcqeqv(tr,nul) .eq. 'OUI') then
            equi(2) = 0.d0
            equi(3) = 0.d0
            equi(4) = 0.d0
        else
            type = 0
            iordre = 0
            call jacobi(nbvec, nperm, tol, toldyn, tr,&
                        tu, vecp, equi(2), jacaux, nitjac,&
                        type, iordre)
        endif
! ------ PREMIER INVARIANT
        call lchydr(tn, hyd)
! ------ EQUIVALENT FATIGUE = SECOND INVARIANT * SIGNE(PREMIER INV)
        if (hyd .ge. 0.d0) equi(5) = equi(1)
        if (hyd .lt. 0.d0) equi(5) = - equi(1)
!
! ------ DIRECTION DES DEFORMATIONS PRINCIPALES DANS EQUI
! -      DANS L ORDRE LES COORDONNEES DU VECTEUR PUIS PASSAGE
! -      A L AUTRE VECTEUR
        if (typ(5:8) .eq. '_DIR') then
            do 100 i = 1, nbvec
                do 102 j = 1, nbvec
                    equi(5+((i-1)*nbvec)+j) = vecp(j,i)
102              continue
100          continue
        endif
!
! --- CONTRAINTES
!
    else if (typ(1:4) .eq. 'SIGM') then
! ------ VON MISES = SECOND INVARIANT
        equi(1) = lciv2s (tn)
!
        if (lcqeqv(tr,nul) .eq. 'OUI') then
            equi(3) = 0.d0
            equi(4) = 0.d0
            equi(5) = 0.d0
        else
            type = 0
            iordre = 0
            call jacobi(nbvec, nperm, tol, toldyn, tr,&
                        tu, vecp, equi(3), jacaux, nitjac,&
                        type, iordre)
        endif
! ------ TRESCA = MAX DIFF VALEURS PRINCIPALES
        equi(2) = max ( abs(equi(3)-equi(4)), abs(equi(3)-equi(5)), abs(equi(4)-equi(5)) )
!
! ------ PREMIER INVARIANT
        call lchydr(tn, hyd)
! ------ EQUIVALENT FATIGUE = SECOND INVARIANT * SIGNE(PREMIER INV)
        if (hyd .ge. 0.d0) equi(6) = equi(1)
        if (hyd .lt. 0.d0) equi(6) = - equi(1)
!
! ------ DIRECTION DES CONTRAINTES PRINCIPALES DANS EQUI
! -      DANS L ORDRE LES COORDONNEES DU VECTEUR PUIS PASSAGE
! -      A L AUTRE VECTEUR
        if (typ(5:8) .eq. '_DIR') then
            do 200 i = 1, nbvec
                do 202 j = 1, nbvec
                    equi(6+((i-1)*nbvec)+j) = vecp(j,i)
202              continue
200          continue
!
! ------    TRACE DES CONTRAINTES : TRSIG
            equi(16) = t(1)+t(2)+t(3)
!
! ------    TRIAXIALITE DES CONTRAINTES : TRIAX
!
            if (equi(1) .gt. tol*abs(equi(16))) then
                equi(17) = equi(16)/3.d0/equi(1)
            else
                equi(17) = 0.d0
            endif
        endif
    endif
!
end subroutine
