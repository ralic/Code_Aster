subroutine facint(nbpas, dim, longh, vec1, vec2,&
                  long, s, r, d, u,&
                  v, w)
    implicit none
! ----------------------------------------------------------------------
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
!    PREPARATION DES MATRICES INTERSPECTRALES POUR FACTORISATION
! ----------------------------------------------------------------------
!          NBPAS : NOMBRE DA PAS DE DISCRETISATION DE LA MATRICE INTERSP
!          DIM   : DIMENSION DE LA MATRICE
!          DIMH  : NOMBRE DE FONCTIONS DECRIVANT LA MATRICE
!    IN  : VEC1  : VECTEUR DES VALEURS DES FONCTIONS AVANT FACTORISATION
!    OUT : VEC2  : VECTEUR DES VALEURS DES FONCTIONS APRES FACTORISATION
!          LONG  : LONGUEUR DES VECTEURS VEC1 ET VEC2
!             S  : MATRICE DE TRAVAIL DE DIMENSION DIM, A FACTORISER
!             R  : MATRICE DE TRAVAIL, RESULTAT DE LA FACTORISATION
!             D  : VECTEUR DE TRAVAIL
    include 'jeveux.h'
    include 'asterfort/diaghr.h'
    include 'asterfort/u2mess.h'
    integer :: dim
    complex(kind=8) :: s(dim, dim), r(dim, dim), u(*), w(*)
    real(kind=8) :: d(dim), vec1(long), vec2(longh), v(*)
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, ico, icomp, ix, iy, j, k
    integer :: l, long, longh, nbpas, nbpt1, nbpt2
    real(kind=8) :: ai, ar, az, bz, si, sr, uu
!
!-----------------------------------------------------------------------
    nbpt1 = nbpas
    nbpt2 = nbpt1*2
    do 10 l = 1, nbpt1
        icomp = 0
        do 20 j = 1, dim
            do 30 i = 1, dim
                if (i .le. j) then
                    icomp = icomp + 1
                    ix = l + (icomp-1)*nbpt2 + nbpt1
                    iy = ix + nbpt1
                    s(i,j) = dcmplx(vec1(ix),vec1(iy))
                    if (i .ne. j) then
                        s(j,i) = dconjg (s(i,j))
                    endif
                endif
30          continue
20      continue
        sr = dble(s(1,1))
        si = dimag(s(1,1))
        if (sr .eq. 0.d0 .and. si .eq. 0.d0) then
            r(1,1) = s(1,1)
            do 40 i = 1, dim
                do 40 j = 1, dim
                    sr = dble(s(i,j))
                    si = dimag(s(i,j))
                    if (sr .ne. 0.d0 .or. si .ne. 0.d0) then
                        call u2mess('F', 'ALGORITH3_60')
                    endif
                    r(i,j) = s(i,j)
40              continue
        else
!
!     --- FACTORISATION ---
!
            call diaghr(dim, s, dim, d, r,&
                        dim, u, v, w)
!
            do 200 j = 1, dim
                uu = 0.d0
                do 210 i = 1, dim
                    ar = dble(r(i,j))
                    ai =dimag(r(i,j))
                    uu = ar*ar + ai*ai + uu
210              continue
                uu = sqrt(uu)
                do 220 k = 1, dim
                    az = dble(r(k,j))/uu
                    bz =dimag(r(k,j))/uu
                    r(k,j) = dcmplx(az,bz)
220              continue
200          continue
            do 230 i = 1, dim
                do 240 j = 1, dim
                    if (d(j) .lt. 0.d0) then
                        d(j) = 0.d0
                    endif
                    r(i,j)=r(i,j) * sqrt(d(j))
240              continue
230          continue
        endif
        ico = 0
        do 100 j = 1, dim
            do 110 i = 1, dim
                ico = ico +1
                ix = l + (ico-1)*nbpt2 + nbpt1
                iy = ix + nbpt1
                vec2(ix) = dble(r(i,j))
                vec2(iy) =dimag(r(i,j))
110          continue
100      continue
10  end do
end subroutine
