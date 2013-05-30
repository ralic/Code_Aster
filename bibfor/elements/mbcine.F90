subroutine mbcine(nno, geom, dff, alpha, beta,&
                  b, jac)
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
! ----------------------------------------------------------------------
!      CALCUL DE LA MATRICE B ET DU JACOBIEN POUR LES MEMBRANES
! ----------------------------------------------------------------------
! IN  NNO          NOMBRE DE NOEUDS
! IN  GEOM         COORDONNEES DES NOEUDS
! IN  DFF          DERIVEE DES F. DE FORME
! IN  ALPHA,BETA   ANGLES NAUTIQUES ORIENTANT LE COMPORTEMENT
!                             ORTHOTROPE DE LA MEMBRANE (EN RADIAN)
! OUT B            MATRICE DE PASSAGE DEPL. NODAL --> DEF. MEMBRANAIRES
! OUT JAC          JACOBIEN DE LA TRANSFORMATION
! ----------------------------------------------------------------------
!     LES TROIS INDICES DE B CORRESPONDENT RESPECTIVEMENT :
!               - A LA COMPOSANTE DE EPSILON MEMBRANAIRE PARMI
!                     (EPS11, EPS22, SQRT(2)EPS12)
!                     CALCULEES DANS LA BASE LOCALE
!               - A LA COMPOSANTE DU VECTEUR DEPLACEMENT
!               - AU NUMERO DU NOEUD
!
!     LA COMPOSANTE VIDE FACILITE L'UTILISATION DE NMCOMP
! ----------------------------------------------------------------------
!
    implicit none
    include 'asterc/r8prem.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/subaco.h'
    include 'asterfort/subacv.h'
    include 'asterfort/sumetr.h'
    include 'asterfort/u2mess.h'
    integer :: nno, i, n, gamma
    real(kind=8) :: geom(3, nno), dff(2, nno), vdirec(3), vortho(3)
    real(kind=8) :: cova(3, 3), metr(2, 2), jac, cnva(3, 2), a(2, 2)
    real(kind=8) :: alpha, beta, projn, b(3, 3, nno), denomi
    real(kind=8) :: factor, dicnva(2), orcnva(2)
!
! - CALCUL DES COORDONNEES COVARIANTES ET CONTRAVARIANTES DE SURFACE
!
    call subaco(nno, dff, geom, cova)
    call sumetr(cova, metr, jac)
    call subacv(cova, metr, jac, cnva, a)
!
! - CALCUL ET PROJECTION DU VECTEUR DIRECTION SUR LA SURFACE
!
    vdirec(1) = cos(beta)*cos(alpha)
    vdirec(2) = cos(beta)*sin(alpha)
    vdirec(3) = -sin(beta)
!
    projn = 0.d0
    do 10 i = 1, 3
        projn = projn + vdirec(i)*cova(i,3)
10  end do
!
    if (abs( 1.d0 - projn*projn ) .le. r8prem()) then
        call u2mess('F', 'ELEMENTS_3')
    endif
!
    denomi = sqrt(1.d0 - projn*projn)
    do 20 i = 1, 3
        vdirec(i) = (vdirec(i) - projn*cova(i,3))/denomi
20  end do
!
! - CALCUL DU VECTEUR TANGENT ORTHOGONAL AU VECTEUR DIRECTION
!
    vortho(1) = cova(2,3)*vdirec(3) - cova(3,3)*vdirec(2)
    vortho(2) = cova(3,3)*vdirec(1) - cova(1,3)*vdirec(3)
    vortho(3) = cova(1,3)*vdirec(2) - cova(2,3)*vdirec(1)
!
! - CALCUL DE LA MATRICE B
!
    call r8inir(3*nno*3, 0.d0, b, 1)
!
! - LE TERME DE CISAILLEMENT EST SYMETRISE ET MULTIPLIE PAR SQRT(2)
    factor = 1.d0/sqrt(2.d0)
!
! - ON PRECALCULE CERTAINS PRODUITS SCALAIRES
    call r8inir(2, 0.d0, dicnva, 1)
    call r8inir(2, 0.d0, orcnva, 1)
    do 30 gamma = 1, 2
        do 30 i = 1, 3
            dicnva(gamma) = dicnva(gamma) + vdirec(i)*cnva(i,gamma)
            orcnva(gamma) = orcnva(gamma) + vortho(i)*cnva(i,gamma)
30      continue
!
!
! - ON BOUCLE SUR LES DEGRES DE LIBERTE
    do 40 n = 1, nno
        do 40 i = 1, 3
            do 40 gamma = 1, 2
                b(1,i,n) = b(1,i,n) + dff(gamma,n)*dicnva(gamma)* vdirec(i)
                b(2,i,n) = b(2,i,n) + dff(gamma,n)*orcnva(gamma)* vortho(i)
                b(3,i,n) = b(3,i,n) + factor*dff(gamma,n) *(dicnva( gamma)*vortho(i)+orcnva(gamma&
                           &)*vdirec(i))
40          continue
!
end subroutine
