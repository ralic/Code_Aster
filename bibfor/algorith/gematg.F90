subroutine gematg(n, delta, matmoy, mat, mat1,&
                  mat2)
    implicit   none
    include 'asterc/getres.h'
    include 'asterfort/gamdev.h'
    include 'asterfort/gasdev.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/u2mess.h'
    integer :: n
    real(kind=8) :: delta, matmoy(*), mat(*), mat1(*), mat2(*)
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
!  GENERATEUR DE MATRICE ALEATOIRE DU MODELE PROBABILISTE
!  NON PARAMETRIQUE.(CF. C.SOIZE 2001).
!
!
!  N      : N X N EST LA DIMENSION DE LA MATRICE A GENERER
!  DELTA  : PARAMETRE DE DISPERSION DE LA MATRICE ALEATOIRE.
!  MATMOY : TABLEAUX DES VALEURS DE LA MATRICE MOYENNE
!  MAT    : TABLEAUX DES VALEURS DE LA MATRICE A GENERER
!  MAT1   : TABLEAU DE TRAVAIL
!  MAT2   : TABLEAU DE TRAVAIL
!
! ----------------------------------------------------------------------
    integer :: i, j, k, l, iind, jind, lind, kind
    real(kind=8) :: sum, tlii, p, sigma, alpha, v, u, borne
    character(len=8) :: k8bid
    character(len=16) :: k16bid, nomcmd
! DEB ------------------------------------------------------------------
!
    call jemarq()
!
    call getres(k8bid, k16bid, nomcmd)
!
! --- VERIFICATION BORNE SUR DELTA
    borne=sqrt(dble(n+1)/dble(n+5))
    if (delta .gt. borne) then
        call u2mess('F', 'ALGORITH3_81')
    endif
!
! --- CALCUL DES PARAMETRES P ET SIGMA ET
!     INITIALISATION DE LA MATRICE TL
!
    p = (n+1d0)/(delta**2)
    sigma = 1d0 / sqrt(p)
!
! --- CALCUL DES TERMES DIAGONAUX DE LA MATRICE ALEATOIRE TL
!
    do 80 j = 1, n
        jind = j*(j+1) / 2
        alpha = (p-j+1.d0) / 2.d0
        v = gamdev(alpha)
        mat2(jind) = sigma*sqrt(2.d0*v)
80  continue
!
! --- CALCUL DES TERMES EXTRADIAGONAUX DE LA MATRICE ALEATOIRE TL
!
    do 89 j = 2, n
        jind = j*(j-1) / 2
        do 88 i = 1, j-1
            u = gasdev()
            mat2(i+jind) = sigma*u
88      continue
89  end do
!
! --- CALCUL DE LA MATRICE ALEATOIRE G=L.TL
!
    do 100 j = 1, n
        jind = j*(j-1) / 2
        do 99 i = 1, j
            iind = i*(i-1) / 2
            sum = 0d0
            do 98 k = 1, i
                sum = sum + mat2(k + iind)*mat2(k + jind)
98          continue
            mat1(i+jind) = sum
99      continue
100  end do
!
! --- FACTORISATION DE CHOLESKY DE K
!
    do 113 i = 1, n
        iind = i*(i-1)/2
        do 112 j = i, n
            jind = j*(j-1)/2
            sum = matmoy(jind+i)
            do 111 k = 1, i-1
                sum = sum - mat2(iind +k)*mat2(jind +k)
111          continue
            if (i .eq. j) then
                if (sum .le. 0.d0) then
                    call u2mess('F', 'ALGORITH3_82')
                endif
                tlii = sqrt(sum)
                mat2(iind+i) = tlii
            else
                mat2(i+jind) = sum / tlii
            endif
112      continue
113  end do
!
! --- CALCUL DE LA MATRICE ALEATOIRE
!
    do 210 j = 1, n
        jind=j*(j-1)/2
        do 210 i = 1, j
            iind = i*(i-1) / 2
            sum = 0.d0
            do 207 l = i, j
                lind = l*(l-1) / 2
                do 207 k = 1, i
                    sum = sum + mat2(iind+k)*mat2(jind+l)*mat1(lind+k)
207              continue
            do 208 l = 1, i-1
                lind = l*(l-1) / 2
                do 208 k = 1, l
                    sum = sum + mat2(iind+k)*mat2(jind+l)*mat1(lind+k)
208              continue
            do 209 k = 1, i
                kind = k*(k-1) / 2
                do 209 l = 1, k-1
                    sum = sum + mat2(iind+k)*mat2(jind+l)*mat1(kind+l)
209              continue
            mat(jind+i) = sum
210      continue
!
    call jedema()
end subroutine
