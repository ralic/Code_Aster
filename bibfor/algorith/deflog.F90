subroutine deflog(ndim, f, epsl, gn, lamb,&
                  logl, iret)
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
! ----------------------------------------------------------------------
    implicit none
!     CALCUL DES DEFORMATIONS LOGARITHMIQUES ET DES TERMES NECESSAIRES
!     AU POST TRAITEMENT DES CONTRAINTES ET A LA RIGIDITE TANGENTE
!     SUIVANT ARTICLE MIEHE APEL LAMBRECHT CMAME 2002
! ----------------------------------------------------------------------
!     IN    NDIM : dimension 2 ou 3
!     IN    F gradient de la transformation calcule sur config initiale
!     OUT   EPSL  deformation logarithmique + GN,LAMB,LOGL pour POSLOG
!     OUT   GN    directions propres du tenseur F
!     OUT   LAMB  valeurs propres du tenseur F
!     OUT   LOGL  log des valeurs propres du tenseur F
! ----------------------------------------------------------------------
    include 'asterc/r8miem.h'
    include 'asterfort/diago2.h'
    include 'asterfort/diagp3.h'
    include 'asterfort/lctr2m.h'
    include 'asterfort/pmat.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/tnsvec.h'
    real(kind=8) :: tr(6), gn(3, 3), epsl33(3, 3), tr2(3), ft(3, 3), gn2(2, 2)
    real(kind=8) :: f(3, 3), epsl(6), lamb(3), logl(3), f33(3, 3)
    integer :: nbvec, i, j, k, ndim, iret
! ----------------------------------------------------------------------
!
    nbvec = 3
!
!     LE CALCUL DES VALEURS PROPRES N'A PAS ENCORE ETE FAIT
    call lctr2m(3, f, ft)
    call pmat(3, ft, f, f33)
! --- VALEURS PRINCIPALES = VECTEURS PROPRES
!  VECP : DIM1=I=COMPOSANTE DIM2=J=NUM VECTEUR ASSOCIE A LAMBP(J)
    call tnsvec(3, 3, f33, tr, 1.d0)
!
    if (ndim .eq. 3) then
!
!         CALL DIAGO3(TR,GN,LAMB)
!     --------------------------------
!     pour gagner du temps
!     --------------------------------
! --- MATRICE TR = (XX XY XZ YY YZ ZZ) (POUR DIAGP3)
        tr(1) = f33(1,1)
        tr(2) = f33(1,2)
        tr(3) = f33(1,3)
        tr(4) = f33(2,2)
        tr(5) = f33(2,3)
        tr(6) = f33(3,3)
        call diagp3(tr, gn, lamb)
!
    else if (ndim.eq.2) then
!
        tr2(1)=tr(1)
        tr2(2)=tr(2)
        tr2(3)=tr(4)
        call diago2(tr2, gn2, lamb)
        lamb(3)=tr(3)
        call r8inir(9, 0.d0, gn, 1)
        do 1 i = 1, 2
            do 1 j = 1, 2
                gn(i,j)=gn2(i,j)
 1          continue
        gn(3,3)=1.d0
!
    endif
!
    do 10 i = 1, nbvec
        if (lamb(i) .le. r8miem()) then
            iret=1
            goto 9999
        endif
        logl(i)=log(lamb(i))*0.5d0
10  end do
!
!     EPSL = DEFORMATION LOGARITHMIQUE
    call r8inir(9, 0.d0, epsl33, 1)
    call r8inir(6, 0.d0, epsl, 1)
    do 11 i = 1, 3
        do 12 j = 1, 3
            do 13 k = 1, nbvec
!              Calcul de EPSL dans le repere general
                epsl33(i,j)=epsl33(i,j)+logl(k)*gn(i,k)*gn(j,k)
13          continue
12      continue
11  end do
    call tnsvec(3, 3, epsl33, epsl, sqrt(2.d0))
!
9999  continue
end subroutine
