subroutine vp2tru(method, ty, alpha, beta, signes,&
                  a, nbvect, w, z, wk,&
                  mxiter, ier, nitqr)
    implicit none
    include 'asterfort/matini.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/vphqrp.h'
    character(len=1) :: ty
    character(len=8) :: method
    integer :: nbvect, mxiter, ier, nitqr
    integer :: vali
    real(kind=8) :: alpha(nbvect), beta(nbvect), signes(nbvect)
    real(kind=8) :: a(nbvect, *), w(*), z(*), wk(*)
    real(kind=8) :: valr(2)
!     ------------------------------------------------------------------
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
!     EXTENSION DE LA METHODE DE LANCZOS POUR TRIDIAGONALISER UN
!     FAISCEAU DE MATRICE INDEFINI
!     ------------------------------------------------------------------
! IN  METHOD : K8 : METHODE DE RESOLUTION
!         SI 'TRI_DIAG' : A EST CONSTRUITE A PARTIR DE ALPHA,BETA,SIGNES
!         SI 'ARNOLDI'  : A EST DEJA CONSTRUITE
! IN  TY : K1 : TYPE DE PROBLEME TRAITE A L'ORIGINE
!               'G' GENERALISE  ==> ELEMENTS PROPRES REELS
!               'Q' QUADRATIQUE ==> ELEMENTS PROPRES COMPLEXES
! IN  ALPHA : DIAGONALE DE LA MATRICE TRIDIAGONALE ('TRI_DIAG')
! IN  BETA  : SURDIAGONALE DE LA MATRICE TRIDIAGONALE ('TRI_DIAG')
! IN SIGNES:SIGNE POUR PASSER DE LA SUR A LA SOUS DIAGONALE('TRI_DIAG')
! OUT W : C : VALEURS PROPRES DU SYSTEME
! OUT ALPHA : R : PARTIE REELLE DES VALEURS PROPRES DU SYSTEME
! OUT BETA  : R : PARTIE IMAGINAIRE DES VALEURS PROPRES DU SYSTEME
! OUT Z : C : VECTEURS PROPRES DU SYSTEME
! OUT NITQR : NOMBRE D'ITERATIONS QR POUT ATTEIENDRE LA CONVERGENCE
!
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, ival1, j
    real(kind=8) :: ww
!-----------------------------------------------------------------------
    if (method .eq. 'TRI_DIAG') then
        call matini(nbvect, nbvect, 0.d0, a)
        a(1,1) = signes(1) * alpha(1)
        a(1,2) = signes(1) * beta(2)
        do 20 i = 2, nbvect - 1
            a(i,i-1) = signes(i) * beta(i)
            a(i,i) = signes(i) * alpha(i)
            a(i,i+1) = signes(i) * beta(i+1)
20      continue
        a(nbvect,nbvect-1) = signes(nbvect) * beta(nbvect)
        a(nbvect,nbvect) = signes(nbvect) * alpha(nbvect)
    endif
!
    ival1 = 1
    ier = 0
    call vphqrp(a, nbvect, nbvect, ival1, w,&
                z, nbvect, wk, mxiter, ier,&
                nitqr)
    if (ier .ne. 0) then
        call u2mess('F', 'ALGELINE3_57')
    endif
!
    if (ty .eq. 'G') then
        do 30 i = 1, nbvect
            alpha(i) = w(2*i-1)
            if (abs(w(2*i)) .gt. 1.d-75) then
                ww = w(2*i)/w(2*i-1)
                vali = i
                valr (1) = w(2*i-1)
                valr (2) = ww
                call u2mesg('I', 'ALGELINE4_65', 0, ' ', 1,&
                            vali, 2, valr)
            endif
            beta(i) = 0.d0
            do 30 j = 1, nbvect
                a(i,j) = z(2*nbvect* (j-1)+2*i-1)
30          continue
    else
        do 40 i = 1, nbvect
            alpha(i) = w(2*i)
            beta (i) = w(2*i-1)
40      continue
    endif
!
end subroutine
