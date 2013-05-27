subroutine genale(vec1, vec2, r, v, x,&
                  dim, long, lonv, ln)
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
!     IN  : VEC1  : VECTEUR DES VALEURS DE LA MATRICE INTERSPECTRALE
!     OUT : VEC2  : VALEURS DES FONCTIONS GENEREES POUR UN TIRAGE
!           R     : MATRICE DE TRAVAIL (MATR. INTERSP. POUR UNE FREQ.)
!           V     : VECTEUR DE TRAVAIL (VALEURS DES FONCTIONS GENEREES)
!           X     : VECTEUR DE TRAVAIL (BRUIT BLANC)
!                               V(FREQ) = R(FREQ) * X(FREQ)
!           DIM   : DIMENSION DE LA MATRICE DE TRAVAIL
!           LN    : NOMBRE DE POINTS DE LA DISCRETISATION
!           NALEA : NOMBRE ALEATOIRE POUR INITIALISER LE GENERATEUR
    include 'jeveux.h'
    include 'asterfort/genere.h'
    integer :: dim
    real(kind=8) :: vec1(long), vec2(lonv)
    complex(kind=8) :: r(dim, dim), v(dim), x(dim)
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, icomp, ix, iy, j, kf, kk
    integer :: ln, ln2, long, lonv
!-----------------------------------------------------------------------
    ln2=ln*2
    do 10 kf = 1, ln
        icomp = 0
        do 20 j = 1, dim
            do 30 i = 1, dim
                icomp = icomp+1
                ix = ln + kf + (icomp-1)*ln2
                iy = ix + ln
                r(i,j) = dcmplx(vec1(ix),vec1(iy))
30          continue
20      continue
!
        call genere(r, dim, v, x)
!
        do 40 kk = 1, dim
            ix = kf + (kk-1)*ln2
            iy = ix + ln
            vec2(ix) = dble(v(kk))
            vec2(iy) = dimag(v(kk))
40      continue
10  end do
end subroutine
