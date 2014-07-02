subroutine vpzqrs(n, m, hh, ih, loc,&
                  valpi, valpr, zvps, iz, b,&
                  ib, u, v, acc, ifail)
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!     PROCEDURE INVIT
!     (CF. WILKINSON & REINSCH LINEAR ALGEBRA PP.427-431)
!     RECHERCHE LES M VECTEURS PROPRES D'UNE MATRICE REELLE SUPERIEURE
!     DE TYPE HESSENBERG (STOCKEE DANS LE TABLEAU HH(N,N)), CONNAISSANT
!     LES PARTIES REELLES ET IMAGINAIRES DES VALEURS PROPRES DE CETTE
!     MATRICE (STOCKEES DANS LES TABLEAUX VALPR(N) ET VALPI(N))
!     LES VECTEURS PROPRES A RECHERCHER SONT SELECTIONNES DANS LE
!     TABLEAU LOGIQUE LOC(N)
!     LES VECTEURS PROPRES RESULTATS SONT PLACES DANS LE TABLEAU
!     ZVPS(N,M). UN SEUL VECTEUR PROPRE, CORRESPONDANT A LA VALEUR
!     PROPRE DE PARTIE IMAGINAIRE POSITIVE, EST STOCKE DANS LE CAS
!     DE VALEURS PROPRES COMPLEXES CONJUGUEES. UN VECTEUR PROPRE NON
!     SELECTIONNE EST MIS A ZERO.
!     ACC REPRESENTE LA PRECISION MACHINE RELATIVE.
!
! --- DECLARATIONS
!
    implicit none
#include "asterf_types.h"
!
! ARGUMENTS
    integer :: n, m, ih, iz, ib, ifail
    real(kind=8) :: acc
    real(kind=8) :: hh(ih, n), valpi(n), valpr(n), zvps(iz, m)
    real(kind=8) :: b(ib, n), u(n), v(n)
    aster_logical :: loc(n)
!
! VARIABLES LOCALES
    integer :: i, i1, i2, ii, is, its, j, j2, k, k1
    integer :: luk, luk1, luk2, m1, n1
    real(kind=8) :: eps3, accrtl, rilam, rrlam, rnorm, rnormv
    real(kind=8) :: w, x, y
    real(kind=8) :: a, h
    real(kind=8) :: xluk, xeps3, zr, zi
    aster_logical :: conj2
!
!**********************************************************************
!                        DEBUT DU CODE EXECUTABLE
!**********************************************************************
!
!======================================================================
!     INITIALISATION
!======================================================================
    ifail = 0
    j = 0
    conj2 = .false.
!
!======================================================================
!      VERIFICATION DES DONNEES D ENTREE
!======================================================================
    do i = 1, n
        if (loc(i)) then
            j = j + 1
        endif
        if (valpi(i) .eq. 0.0d0) then
            goto 40
        endif
        if (conj2) then
            goto 20
        endif
        if (((loc(i)) .and. (.not. loc(i+1))) .or. ((.not. loc(i)) .and. (loc(i+1)))) then
            goto 60
        endif
 20     continue
        conj2 = .not. conj2
 40     continue
    end do
    if (j .le. m) then
        goto 80
    endif
!
! --- ERREUR : TAILLE DU TABLEAU LOC(TRUE) SUPERIEURE A M
!
    ifail = 1
    goto 999
!
! --- ERREUR : VALEURS PROPRES COMPLEXES MAL SELECTIONNEES
!
 60 continue
    ifail = 2
    goto 999
!
!=======================================================================
!     BOUCLE SUR LES N VALEURS PROPRES DE LA MATRICE HH
!=======================================================================
 80 continue
    luk = 0
    is = 1
!
    do k = 1, n
!
! === TEST PUIS CALCUL SUR LES MODES SELECTIONNES
        if (loc(k)) then
!
!======================================================================
! -1- NORMALISATION DE LA SOUS-MATRICE DE DIMENSION LUK*LUK
!======================================================================
            if (luk .lt. k) then
                n1 = n - 1
                if (k .le. n1) then
                    do luk = k, n1
                        luk1 = luk + 1
                        if (hh(luk1,luk) .eq. 0.0d0) then
                            goto 120
                        endif
                    end do
                endif
                luk = n
120             continue
                rnorm = 0.0d0
                m1 = 1
                do i = 1, luk
                    x = 0.0d0
                    if (m1 .le. luk) then
                        do j = m1, luk
                            x = x + abs(hh(i,j))
                        end do
                    endif
                    if (x .gt. rnorm) then
                        rnorm = x
                    endif
                    m1 = i
                end do
!
!======================================================================
! -2- REMPLACEMENT PAR EPS3 DU PIVOT NUL DANS LA DECOMPOSITION
!     LES RACINES PROCHES SONT MODIFIEES PAR EPS3
!     (ACCRTL EST LE CRITERE D ACCROISSEMENT)
!======================================================================
                eps3 = acc*rnorm
                accrtl = (1.0d0/sqrt(dble(luk)))/100.0d0
            endif
!
!======================================================================
! -3- TEST SUR LA NORME DE LA SOUS-MATRICE DE DIMENSION LUK*LUK
!======================================================================
            if (rnorm .eq. 0.0d0) then
                do i = 1, n
                    zvps(i,is) = 0.0d0
                end do
                zvps(k,is) = 1.0d0
                is = is + 1
                goto 9000
            endif
            rrlam = valpr(k)
            rilam = valpi(k)
!
!======================================================================
! -4- PERTURBATION DE LA VALEUR PROPRE SI ELLE EST PROCHE
!     D UNE AUTRE VALEUR PROPRE
!======================================================================
400         continue
            i = k
            k1 = k - 1
            if (k1 .ge. 1) then
                do ii = 1, k1
                    i = i - 1
                    if ((loc(i)) .and. (abs(valpr(i)-rrlam).lt.eps3) .and.&
                        (abs(valpi(i)-rilam).lt.eps3)) then
                        rrlam = rrlam +eps3
                        goto 400
                    endif
                end do
            endif
            valpr(k) = rrlam
!
!======================================================================
! -5- FORMATION DE LA MATRICE DE HESSENBERG SUPERIEURE B = HH - RRLAM*I
!     ET DU VECTEUR REEL INITIAL U
!======================================================================
            m1 = 1
            do i = 1, luk
                if (m1 .le. luk) then
                    do j = m1, luk
                        b(i,j) = hh(i,j)
                    end do
                endif
                b(i,i) = b(i,i) - rrlam
                m1 = i
                u(i) = eps3
            end do
            its = 0
!
!======================================================================
! -6- TEST SUR LA PARTIE IMAGINAIRE DE LA VALEUR PROPRE
!======================================================================
            if (rilam .eq. 0.0d0) then
!
!***********************************************************************
!    TRAITEMENT DANS LE CAS D UNE VALEUR PROPRE REELLE
!***********************************************************************
                if (luk .ge. 2) then
!======================================================================
! -7- TRANSFORMATIONS ET DECOMPOSITION
!======================================================================
                    do i = 2, luk
                        m1 = i - 1
                        if (abs(b(i,m1)) .le. abs(b(m1,m1))) then
                            goto 720
                        endif
                        if (m1 .le. luk) then
                            do j = m1, luk
                                y = b(i,j)
                                b(i,j) = b(m1,j)
                                b(m1,j) = y
                            end do
                        endif
720                     continue
                        if (b(m1,m1) .eq. 0.0d0) then
                            b(m1,m1) = eps3
                        endif
                        x = b(i,m1)/b(m1,m1)
                        if (x .ne. 0.0d0) then
                            do j = i, luk
                                b(i,j) = b(i,j) - x*b(m1,j)
                            end do
                        endif
                    end do
                endif
!
!======================================================================
! -8- REMPLACEMENT DES PIVOTS NULS PAR EPS3
!======================================================================
                if (b(luk,luk) .eq. 0.0d0) then
                    b(luk,luk) = eps3
                endif
!
!======================================================================
! -9- VECTEUR REEL INITIAL U
!======================================================================
900             continue
                i = luk + 1
                do ii = 1, luk
                    i = i - 1
                    y = u(i)
                    i1 = i + 1
                    if (i1 .le. luk) then
                        do j = i1, luk
                            y = y - b(i,j)*u(j)
                        end do
                    endif
                    u(i) = y/b(i,i)
                end do
!
!======================================================================
! -10- CALCUL DE LA NORME
!======================================================================
                its = its + 1
                rnorm = 0.0d0
                rnormv = 0.0d0
                do i = 1, luk
                    x = abs(u(i))
                    if (rnormv .lt. x) then
                        rnormv = x
                        j = i
                    endif
                    rnorm = rnorm + x
                end do
!
!======================================================================
! -11- VECTEUR PROPRE REEL
!======================================================================
                if (rnorm .ge. accrtl) then
! --- ON CONSERVE LE VECTEUR CALCULE SI LA NORME EST SUPERIEURE A ACCRTL
                    x = 1.0d0/u(j)
                    if (luk .ge. 1) then
                        do i = 1, luk
                            zvps(i,is) = u(i)*x
                        end do
                    endif
                    j = luk + 1
                    goto 1140
                endif
                if (its .lt. luk) then
                    xluk = sqrt(dble(luk))
                    xeps3 = eps3/(xluk + 1.0d0)
                    u(1) = eps3
                    if (n .ge. 2) then
                        do i = 2, n
                            u(i)=xeps3
                        end do
                    endif
                    u(luk-its+1)=u(luk-its+1) - eps3*xluk
                    goto 900
                endif
! --- MISE A ZERO DU VECTEUR PROPRE
                j = 1
1140             continue
                if (j .le. n) then
                    do i = j, n
                        zvps(i,is) = 0.0d0
                    end do
                endif
                is = is + 1
                goto 9000
!
!======================================================================
! -6- TEST SUR LE SIGNE DE LA PARTIE IMAGINAIRE DE LA VALEUR PROPRE
!======================================================================
!
!***********************************************************************
!    TRAITEMENT DANS LE CAS D UNE VALEUR PROPRE COMPLEXE
!    AVEC PARTIE IMAGINAIRE POSITIVE
!***********************************************************************
            else if (rilam.gt.0.0d0) then
                do i = 1, luk
                    v(i) = 0.0d0
                end do
!
!======================================================================
! -7-  DECOMPOSITION TRIANGULAIRE
!======================================================================
                b(3,1) = -rilam
                luk2 = luk + 2
                i = luk + 3
                if (luk2 .ge. 4) then
                    do ii = 4, luk2
                        i = i - 1
                        b(i,1) = 0.0d0
                    end do
                endif
                if (luk .ge. 2) then
                    do i = 2, luk
                        m1 = i - 1
                        w = b(i,m1)
                        i1 = i + 1
                        x = b(m1,m1)**2 + b(i1,m1)**2
                        if ((w**2) .gt. x) then
                            x = b(m1,m1)/w
                            y = b(i1,m1)/w
                            b(m1,m1) = w
                            b(i1,m1) = 0.0d0
                            do j = i, luk
                                w = b(i,j)
                                b(i,j) = b(m1,j) - x*w
                                b(m1,j) = w
                                j2 = j + 2
                                b(j2,i) = b(j2,m1) - y*w
                                b(j2,m1) = 0.0d0
                            end do
                            i2 = i + 2
                            b(i2,m1) = -rilam
                            b(i,i) = b(i,i) - y*rilam
                            b(i2,i) = b(i2,i) + x*rilam
                            goto 770
                        endif
                        if (x .eq. 0.0d0) then
                            b(m1,m1) = eps3
                            b(i1,m1) = 0.0d0
                            x = eps3**2
                        endif
                        w = w/x
                        x = b(m1,m1)*w
                        y = -b(i1,m1)*w
                        do j = i, luk
                            j2 = j + 2
                            b(i,j) = b(i,j) - x*b(m1,j) + y*b(j2,m1)
                            b(j2,i) = -x*b(j2,m1) - y*b(m1,j)
                        end do
                        i2 = i + 2
                        b(i2,i) = b(i2,i) - rilam
770                     continue
                    end do
                endif
!
!======================================================================
! -8- REMPLACEMENT DES PIVOTS NULS PAR EPS3
!======================================================================
                if ((b(luk,luk).eq.0.0d0) .and. (b(luk+2,luk) .eq.0.0d0)) then
                    b(luk,luk) = eps3
                endif
!
!======================================================================
! -9- VECTEURS INITIAUX U ET V
!======================================================================
910             continue
                if (luk .ge. 1) then
                    i = luk + 1
                    do ii = 1, luk
                        i = i - 1
                        x = u(i)
                        y = v(i)
                        i1 = i + 1
                        if (i1 .le. luk) then
                            do j = i1, luk
                                x = x - b(i,j)*u(j) + b(j+2,i)*v(j)
                                y = y - b(i,j)*v(j) - b(j+2,i)*u(j)
                            end do
                        endif
                        if (abs(b(i,i)) .gt. abs(b(i+2,i))) then
                            h = b(i+2,i)/b(i,i)
                            a = 1.0d0/(h*b(i+2,i) + b(i,i))
                            u(i) = (x + h*y)*a
                            v(i) = (y - h*x)*a
                        else
                            h = b(i,i)/b(i+2,i)
                            a = 1.0d0/(h*b(i,i) + b(i+2,i))
                            u(i) = (y + h*x)*a
                            v(i) = (h*y - x)*a
                        endif
                    end do
                endif
!
!======================================================================
! -10- CALCUL DE LA NORME
!======================================================================
                its = its + 1
                rnorm = 0.0d0
                rnormv = 0.0d0
                if (luk .ge. 1) then
                    do i = 1, luk
                        zr = abs(u(i))
                        zi = abs(v(i))
                        if (zi .gt. zr) then
                            h = zr
                            zr = zi
                            zi = h
                        endif
                        if (zi .eq. 0.0d0) then
                            x = zr
                        else
                            h = zr * sqrt(1.0d0 + (zi/zr)**2.0d0)
                            x = h
                        endif
                        if (rnormv .lt. x) then
                            rnormv = x
                            j = i
                        endif
                        rnorm = rnorm + x
                    end do
                endif
                m1 = is + 1
!
!======================================================================
! -11- VECTEUR PROPRE COMPLEXE
!======================================================================
                if (rnorm .ge. accrtl) then
! --- ON CONSERVE LE VECTEUR CALCULE SI LA NORME EST SUPERIEURE A ACCRTL
                    x = u(j)
                    y = v(j)
                    if (luk .ge. 1) then
                        do i = 1, luk
                            if (abs(x) .gt. abs(y)) then
                                h = y/x
                                a = 1.0d0/(h*y + x)
                                zvps(i,is) = (u(i) + h*v(i))*a
                                zvps(i,m) = (v(i) - h*u(i))*a
                            else
                                h = x/y
                                a = 1.0d0/(h*x + y)
                                zvps(i,is) = (v(i) + h*u(i))*a
                                zvps(i,m) = (h*v(i) - u(i))*a
                            endif
                        end do
                    endif
                    j = luk + 1
                    goto 1170
                endif
                if (its .lt. luk) then
                    xluk = sqrt(dble(luk))
                    xeps3 = eps3/(xluk + 1.0d0)
                    u(1) = eps3
                    if (n .ge. 2) then
                        do i = 2, n
                            u(i)=xeps3
                        end do
                    endif
                    u(luk-its+1)=u(luk-its+1) - eps3*xluk
                    if (luk .ge. 1) then
                        do i = 1, luk
                            v(i) = 0.0d0
                        end do
                    endif
                    goto 910
                endif
! --- MISE A ZERO DU VECTEUR PROPRE
                j = 1
1170             continue
                if (j .le. n) then
                    do i = j, n
                        zvps(i,is) = 0.0d0
                        zvps(i,m1) = 0.0d0
                    end do
                endif
                is = is + 2
            endif
        endif
!
9000     continue
    end do
!
999 continue
end subroutine
