subroutine mtcrog(a, b, nmax, n, nbsc,&
                  c, wks, ier)
    implicit none
!
#include "jeveux.h"
#include "asterfort/mtcro1.h"
#include "asterfort/mtcro2.h"
#include "asterfort/mtcro3.h"
#include "asterfort/utmess.h"
    integer :: nmax, n, nbsc, ier
    real(kind=8) :: a(nmax, n), b(nmax, nbsc), c(nmax, nbsc), wks(nmax)
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     RESOLUTION APPROCHEE D UN SYSTEME LINEAIRE AX = B PAR LA METHODE
!     DE CROUT, POUR UNE MATRICE A QUELCONQUE DE DIMENSION N*N
!     SI B EST DE DIMENSION N*1, IL S AGIT D UN SIMPLE SYSTEME
!     LINEAIRE. SI B EST DE DIMENSION N*N ET VAUT L IDENTITE, IL S AGIT
!     DE L INVERSION D UNE MATRICE
! ----------------------------------------------------------------------
!     OPERATEUR APPELANT: OP0144, FLUST3, MEFIST, MEFEIG, MEFREC, MEFCIR
! ----------------------------------------------------------------------
! IN  : NMAX   : DIMENSIONS DES TABLEAUX
! IN  : N      : DIMENSIONS EFFECTIVE DES MATRICES
! IN  : NBSC   : DIMENSIONS DU SYSTEM A RESOUDRE
! IN  : A      : MATRICE A DU SYSTEME LINEAIRE (MODIFIEE LORS DE LA
!                RESOLUTION)
! IN  : B      : MATRICE B DU SYSTEME LINEAIRE
! OUT : C      : SOLUTION DU SYSTEME LINEAIRE (SI IER = 0)
!                SI B EST L IDENTITE ET NBSC = N, C EST L INVERSE DE A
! --  : WKS    : TABLEAU DE TRAVAIL
! OUT : IER    : INDICE D ERREUR (0: RESOLUTION CORRECTE
!                                 1: PAS DE CONVERGENCE OU ERREUR)
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    integer :: id, i, j, k, l, iapro
    real(kind=8) :: det, prec, x, y
! ----------------------------------------------------------------------
!
!
! --- PREC EST LA PRECISION MACHINE.
! --- PREC   : LE PLUS PETIT REEL POSITIF, TEL QUE: 1.0 + PREC > 1.0
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    prec = .1110223024625156651d-15
!
    ier = 0
!
! --- TEST DE VERIFICATION DES DIMENSIONS
!
    if (nmax .lt. n) then
        call utmess('F', 'ALGELINE2_14')
    endif
!
!
! --- SAUVEGARDE DE LA MATRICE B
!
    do 10 i = 1, n
        do 10 j = 1, nbsc
            c(i,j) = b(i,j)
10      continue
!
! --- A EST DECOMPOSEE EN A = LU, OU L EST UNE MATRICE TRIANGULAIRE
! --- INFERIEURE, ET U, UNE MATRICE TRIANGULAIRE SUPERIEURE UNITAIRE.
! --- L ET U SONT ECRITES A LA PLACE DE A
! --- WKS EST LA TABLE DES PERMUTATIONS EFFECTUEES
! --- SI LA MATRICE A EST SINGULIERE OU PRESQUE SINGULIERE, IER = 1.
! --- IER = 0 SINON, ET DET EST LE DETERMINANT
!
!
    do 20 i = 1, n
        wks(i) = 0.0d0
20  end do
!
    do 60 j = 1, n
        do 40 i = 1, n
            wks(i) = wks(i) + a(i,j)**2
40      continue
60  end do
!
    do 80 i = 1, n
        if (wks(i) .le. 0.0d0) goto 240
        wks(i) = 1.0d0/sqrt(wks(i))
80  end do
!
    det = 1.0d0
    id = 0
    do 220 k = 1, n
        l = k
        x = 0.0d0
        do 100 i = k, n
            y = abs(a(i,k)*wks(i))
            if (y .le. x) goto 100
            x = y
            l = i
100      continue
        if (l .ne. k) then
            det = -det
            do 120 j = 1, n
                y = a(k,j)
                a(k,j) = a(l,j)
                a(l,j) = y
120          continue
            wks(l) = wks(k)
        endif
        wks(k) = l
        det = det*a(k,k)
        if (x .lt. 8.0d0*prec) then
            ier = 1
            goto 240
        endif
160      continue
        if (abs(det) .lt. 1.0d0) goto 180
        det = det*0.0625d0
        id = id + 4
        goto 160
180      continue
        if (abs(det) .ge. 0.0625d0) goto 200
        det = det*16.0d0
        id = id - 4
        goto 180
200      continue
        if (k .lt. n) then
            call mtcro1(k, a, nmax, a(1, k+1))
            call mtcro3(n-k, k, a(k+1, 1), nmax, a(1, k+1),&
                        a(k+1, k+1))
        endif
220  end do
240  continue
!
!
! --- RESOLUTION D UN SYSTEME LINEAIRE AX = B
! --- B UNE EST UNE MATRICE DE DIMENSION NMAX*NBSC
! --- A EST SOUS LA FORME A = LU, OU L EST UNE MATRICE TRIANGULAIRE
! --- INFERIEURE, ET U, UNE MATRICE TRIANGULAIRE SUPERIEURE UNITAIRE.
! --- L ET U SONT ECRITES A LA PLACE DE A
! --- P EST LA TABLE DES PERMUTATIONS EFFECTUEES AUPARAVANT.
! --- AX = B EST RESOLU EN TROIS ETAPES, PERMUTATION DES ELEMENTS DE B,
! --- CALCUL DE LY = B, ET CALCUL DE  UX = Y. LES MATRICES Y ET X SONT
! --- ECRITES A LA PLACE DE B
!
    if (ier .eq. 0) then
! ---    PERMUTATION DES ELEMENTS DE B
!
        do 340 i = 1, n
            iapro = int(wks(i) + 0.5d0)
            if (iapro .eq. i) goto 340
            do 320 k = 1, nbsc
                x = b(i,k)
                b(i,k) = b(iapro,k)
                b(iapro,k) = x
320          continue
340      continue
        do 360 k = 1, nbsc
! ---       RECHERCHE DE LA SOLUTION DE LY = B
            call mtcro1(n, a, nmax, b(1, k))
! ---       RECHERCHE DE LA SOLUTION DE UX = Y
            call mtcro2(n, a, nmax, b(1, k))
360      continue
!
! --- RESTAURATION DE LA MATRICE B, ET DE LA SOLUTION
!
        do 380 i = 1, n
            do 380 j = 1, nbsc
                x = b(i,j)
                b(i,j) = c(i,j)
                c(i,j) = x
380          continue
!
    else
        ier = 1
    endif
!
!
end subroutine
