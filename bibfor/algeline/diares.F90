subroutine diares(n, nevec, a, lda, tau,&
                  evec, ldevec, work)
    implicit none
    include 'asterfort/zaddrc.h'
    include 'asterfort/zmulmv.h'
    include 'asterfort/zmult.h'
    integer :: n, nevec, lda, ldevec
    complex(kind=8) :: a(lda, *), tau(*), evec(ldevec, *), work(*)
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
!-----------------------------------------------------------------------
!      REDUCTION D'UNE MATRICE HERMITIENNE EN UNE MATRICE TRIDIAGONALE
!          SYMETRIQUE (METHODE DE HOUSEHOLDER).
!-----------------------------------------------------------------------
! IN  : N    : DIMENSION DES MATRICES.
!     : NEVEC: NOMBRE DE VECTEURS PROPRES A DETERMINER
!     : A    : MATRICE COMPLEXE D'ORDRE N. SEULE LA PARTIE TIANGULAIRE
!              INFERIEURE DE LA MATRICE DE HOUSEHOLDER EST STOCKEE.
!     : LDA  : DIMENSION DE A
!     : TAU  : VECTEUR COMPLXE DE DIMENSION N, CONTENANT LA DIAGONALE
!              DE LA MATRICE UNITAIRE T.
! I/O : EVEC : MATRICE COMPLEXE D'ORDRE N.
!         IN : CONTIENT LES VECTEURS PROPRES DE LA MATRICE TRIDIAGONALE
!        OUT : CONTIENT LES VECTEURS PROPRES DE LA MATRICE INITIALE.
! IN  :LDEVEC: DIMENSION DE LA MATRICE EVEC.
! OUT : WORK : VECTEUR COMPLEXE (ZONE DE TRAVAIL).
!-----------------------------------------------------------------------
    integer :: j, nr
    real(kind=8) :: delta
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    do 10 j = 2, n
        call zmult(nevec, dconjg(tau(j)), evec(j, 1), ldevec)
10  end do
!
!  --- STOCKAGE DE LA MATRICE DE HOUSEHOLDER DANS L'ORDRE INVERSE ---
    do 20 nr = n - 1, 2, -1
        delta = dimag(a(nr,nr))*abs(a(nr,nr-1))
        if (delta .ne. 0.0d0) then
            call zmulmv('CONJUGATE', n-nr+1, nevec, (1.0d0, 0.0d0), evec(nr, 1),&
                        ldevec, a(nr, nr-1), 1, (0.0d0, 0.0d0), work,&
                        1)
            call zaddrc(n-nr+1, nevec, dcmplx(-1.0d0/delta, 0.0d0), a(nr, nr-1), 1,&
                        work, 1, evec(nr, 1), ldevec)
        endif
20  end do
end subroutine
