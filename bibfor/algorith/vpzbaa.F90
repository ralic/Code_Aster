subroutine vpzbaa(n, ib, a, ia, ibas,&
                  lhi, d)
!
!***********************************************************************
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!***********************************************************************
!     PROCEDURE BALANCE
!     (CF. WILKINSON & REINSCH LINEAR ALGEBRA PP.320-321)
!
!     CONDITIONNEMENT DE LA MATRICE A(N,N) EN REDUISANT LA NORME DE LA
!     MATRICE PAR DES TRANSFORMATIONS BASEES SUR UNE SIMILARITE
!     DIAGONALE EXACTE ET STOCKEES DANS LE VECTEUR D(N)
!
!
! --- DECLARATIONS
!
    implicit none
!
! ARGUMENTS
! ---------
    integer :: n, ib, ia, ibas, lhi
    real(kind=8) :: a(ia, n), d(n)
!
! VARIABLES LOCALES
! -----------------
!
    real(kind=8) :: b2, c, f, g, r, s, ff
    integer :: i, j, ij, jj, k, l
    logical :: noconv
!
!
!***********************************************************************
!                      DEBUT DU CODE EXECUTABLE
!***********************************************************************
!
!
! --- INITIALISATION
!
    b2 = dble(ib*ib)
    l = 1
    k = n
!
!======================================================================
!     ON RECHERCHE DES RANGEES ISOLANT UNE VALEUR PROPRE ET
!     ON LES POUSSE VERS LE BAS
!======================================================================
20  continue
    if (k .ge. 1) then
!
        j = k + 1
!
        do 100 jj = 1, k
            j = j - 1
            r = 0.0d0
!
            do 40 i = 1, k
                if (i .ne. j) then
                    r = r + abs(a(j,i))
                endif
40          continue
!
            if (r .eq. 0.0d0) then
!
! --- INTERVERSION DES ELEMENTS 1 A K DES COLONNES J ET K
!     ET DES ELEMENTS L A N DES RANGEES J ET K
!
                d(k)=j
                if (j .ne. k) then
                    do 60 ij = 1, k
                        ff = a(ij,j)
                        a(ij,j)=a(ij,k)
                        a(ij,k)= ff
60                  end do
                    if (l .le. n) then
                        do 80 ij = l, n
                            ff=a(j,ij)
                            a(j,ij)=a(k,ij)
                            a(k,ij)=ff
80                      continue
                    endif
                endif
                k = k - 1
                goto 20
            endif
!
100      end do
!
    endif
!
!
!======================================================================
!     ON RECHERCHE DES COLONNES ISOLANT UNE VALEUR PROPRE ET
!     ON LES POUSSE VERS LA GAUCHE
!======================================================================
!
120  continue
    if (l .le. k) then
!
        do 200 j = l, k
            c = 0.0d0
!
            do 140 i = l, k
                if (i .ne. j) then
                    c = c + abs(a(i,j))
                endif
140          continue
!
            if (c .eq. 0.0d0) then
!
! --- INTERVERSION DES ELEMENTS 1 A K DES COLONNES J ET L
!     ET DES ELEMENTS L A N DES RANGEES J ET L
!
                d(l)=j
                if (j .ne. l) then
                    do 160 ij = 1, k
                        ff = a(ij,j)
                        a(ij,j)=a(ij,l)
                        a(ij,l)= ff
160                  end do
                    if (l .le. n) then
                        do 180 ij = l, n
                            ff=a(j,ij)
                            a(j,ij)=a(l,ij)
                            a(l,ij)=ff
180                      continue
                    endif
                endif
                l = l + 1
                goto 120
            endif
!
200      end do
!
    endif
!
!
!======================================================================
!     CONDITIONNEMENT DE LA SOUS-MATRICE DANS LES RANGEES L A K
!======================================================================
!
    ibas = l
    lhi = k
!
    if (l .le. k) then
        do 220 i = l, k
            d(i) = 1.0d0
220      end do
    endif
!
240  continue
    noconv = .false.
!
    if (l .le. k) then
!
        do 400 i = l, k
!
            c = 0.0d0
            r = 0.0d0
!
            do 260 j = l, k
                if (j .ne. i) then
                    c = c + abs(a(j,i))
                    r = r + abs(a(i,j))
                endif
260          continue
!
            g = r/dble(ib)
            f = 1.0d0
            s = c + r
!
280          continue
            if (c .ge. g) goto 300
            f = f*dble(ib)
            c = c*b2
            goto 280
300          continue
            g = r*dble(ib)
320          continue
            if (c .lt. g) goto 340
            f = f/dble(ib)
            c = c/b2
            goto 320
!
340          continue
            if (((c+r)/f) .lt. (0.95d0*s)) then
                g = 1.0d0/f
                d(i) = d(i)*f
                noconv = .true.
!
                if (l .le. n) then
                    do 360 j = l, n
                        a(i,j) = a(i,j)*g
360                  continue
                endif
!
                do 380 j = 1, k
                    a(j,i) = a(j,i)*f
380              continue
!
            endif
!
400      end do
!
    endif
!
    if (noconv) then
        goto 240
    endif
!
end subroutine
