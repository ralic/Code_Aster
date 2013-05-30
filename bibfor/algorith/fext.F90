subroutine fext(t, neq, nvect, liad, lifo,&
                f)
    implicit   none
    include 'jeveux.h'
    include 'asterfort/fointe.h'
    include 'asterfort/r8inir.h'
    include 'blas/daxpy.h'
    integer :: neq, nvect, liad(*)
    real(kind=8) :: t, f(*)
    character(len=24) :: lifo(*)
!-----------------------------------------------------------------------
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
!
!  CALCUL DU VECTEUR FEXT: FEXT = SOMME  GI(T)*FI(X)
!
!  INPUT:
!        T        : INSTANT DE CALCUL
!        NEQ      : NOMBRE D'EQUATIONS (D.D.L. ACTIFS)
!        NVECT    : NOMBRE DE VECTEURS CHARGEMENT
!        LIAD     : LISTE DES ADRESSES DES VECTEURS CHARGEMENT (NVECT)
!        LIFO     : LISTE DES NOMS DES FONCTIONS EVOLUTION (NVECT)
!
!  OUTPUT:
!        F        : VECTEUR FORCE EXTERIEURE (NEQ)
!
!-----------------------------------------------------------------------
    integer :: i, ier
    real(kind=8) :: zero, alpha
    character(len=8) :: nompar
!     ------------------------------------------------------------------
!
    nompar = 'INST'
    zero = 0.d0
    call r8inir(neq, zero, f, 1)
    do 10 i = 1, nvect
        call fointe('F ', lifo(i), 1, nompar, t,&
                    alpha, ier)
        call daxpy(neq, alpha, zr(liad(i)), 1, f,&
                   1)
10  end do
end subroutine
