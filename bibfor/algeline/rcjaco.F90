subroutine rcjaco(ar, br, valpro)
    implicit none
#include "asterf_types.h"
#include "asterfort/utmess.h"
    real(kind=8) :: ar(*), br(*), valpro(3)
! ======================================================================
! COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ------------------------------------------------------------------
! BUT : ROUTINE SIMPLIFIEE DE JACOBI POUR PERFORMANCE
!
! ----------------------------------------------------------------------
    integer :: nperm, i, ii, niter, j, jp1, jm1, ljk, jj, kp1, km1, jk, kk, im1
    integer :: ij, ik, lji, lki, ji, ki, k
    real(kind=8) :: tol, toldyn, valaux(3), eps, akk, ajj, ab, verif
    real(kind=8) :: eptola, epcoma, eptolb, epcomb, raci, d1, d2, den, ca, cg
    real(kind=8) :: aj, bj, ak, bk, rtol, dif, epsa, compa, epsb, compb
    aster_logical :: iconv
    data   nperm, tol, toldyn / 12, 1.d-10, 1.d-2 /
! ----------------------------------------------------------------------
!
!     ---       INITIALISATION DES VALEURS PROPRES      ---
!     --- TERME DIAGONAL RAIDEUR / TERME DIAGONAL MASSE ---
!
    ii = 1
    do 10 i = 1, 3
        if (br(ii) .eq. 0.0d0) then
            call utmess('F', 'ALGELINE4_19')
        endif
        valaux(i) = ar(ii) / br(ii)
        valpro(i) = valaux(i)
        ii = ii + 3 + 1 - i
 10 end do
!
!     ------------------------------------------------------------------
!     ------------------- ALGORITHME DE JACOBI -------------------------
!     ------------------------------------------------------------------
!
    niter = 0
!
 30 continue
!
    niter = niter + 1
    eps = (toldyn**niter)**2
!
!     --- BOUCLE SUR LES LIGNES ---
    do 40 j = 1, 3 - 1
        jp1 = j + 1
        jm1 = j - 1
        ljk = jm1 * 3 - jm1 * j / 2
        jj = ljk + j
!        ---- BOUCLE SUR LES COLONNES ---
        do 41 k = jp1, 3
            kp1 = k + 1
            km1 = k - 1
            jk = ljk + k
            kk = km1 * 3 - km1 * k / 2 + k
!           --- CALCUL DES COEFFICIENTS DE LA ROTATION DE GIVENS ---
            eptola = abs( (ar(jk)*ar(jk)) )
            epcoma = abs(ar(jj)*ar(kk))*eps
            eptolb = abs( (br(jk)*br(jk)) )
            epcomb = abs(br(jj)*br(kk))*eps
            if ((eptola.eq.0.d0) .and. (eptolb.eq.0.d0)) goto 41
            if ((eptola.le.epcoma) .and. (eptolb.le.epcomb)) goto 41
            akk = ar(kk)*br(jk) - br(kk)*ar(jk)
            ajj = ar(jj)*br(jk) - br(jj)*ar(jk)
            ab = ar(jj)*br(kk) - ar(kk)*br(jj)
            verif = (ab * ab + 4.0d0 * akk * ajj)/4.0d0
            if (verif .ge. 0.0d0) then
                raci = sqrt(verif)
                d1 = ab*0.5d0 + raci
                d2 = ab*0.5d0 - raci
            else
                goto 41
            endif
            den = d1
            if (abs(d2) .gt. abs(d1)) den = d2
            if (den .eq. 0.0d0) then
                ca = 0.d0
                cg = - ar(jk)/ar(kk)
            else
                ca = akk / den
                cg = -ajj/den
            endif
!           --- TRANSFORMATION DES MATRICES DE RAIDEUR ET DE MASSE ---
            if (jm1-1 .ge. 0) then
                do 51 i = 1, jm1
                    im1 = i - 1
                    ij = im1 * 3 - im1 * i / 2 + j
                    ik = im1 * 3 - im1 * i / 2 + k
                    aj = ar(ij)
                    bj = br(ij)
                    ak = ar(ik)
                    bk = br(ik)
                    ar(ij) = aj + cg * ak
                    br(ij) = bj + cg * bk
                    ar(ik) = ak + ca * aj
                    br(ik) = bk + ca * bj
 51             continue
            endif
            if (kp1-3 .le. 0) then
                lji = jm1 * 3 - jm1 * j / 2
                lki = km1 * 3 - km1 * k / 2
                do 52 i = kp1, 3
                    ji = lji + i
                    ki = lki + i
                    aj = ar(ji)
                    bj = br(ji)
                    ak = ar(ki)
                    bk = br(ki)
                    ar(ji) = aj + cg * ak
                    br(ji) = bj + cg * bk
                    ar(ki) = ak + ca * aj
                    br(ki) = bk + ca * bj
 52             continue
            endif
            if (jp1-km1 .le. 0) then
                lji = jm1 * 3 - jm1 * j /2
                do 53 i = jp1, km1
                    ji = lji + i
                    im1 = i - 1
                    ik = im1 * 3 - im1 * i / 2 + k
                    aj = ar(ji)
                    bj = br(ji)
                    ak = ar(ik)
                    bk = br(ik)
                    ar(ji) = aj + cg * ak
                    br(ji) = bj + cg * bk
                    ar(ik) = ak + ca * aj
                    br(ik) = bk + ca * bj
 53             continue
            endif
            ak = ar(kk)
            bk = br(kk)
            ar(kk) = ak + 2.0d0 * ca * ar(jk) + ca * ca * ar(jj)
            br(kk) = bk + 2.0d0 * ca * br(jk) + ca * ca * br(jj)
            ar(jj) = ar(jj) + 2.0d0 * cg * ar(jk) + cg * cg * ak
            br(jj) = br(jj) + 2.0d0 * cg * br(jk) + cg * cg * bk
            ar(jk) = 0.0d0
            br(jk) = 0.0d0
!
 41     continue
 40 end do
!
!     --- CALCUL DES NOUVELLES VALEURS PROPRES ---
!
    ii = 1
    do 60 i = 1, 3
        if (br(ii) .eq. 0.0d0) then
            call utmess('F', 'ALGELINE4_19')
        endif
        valpro(i) = ar(ii) / br(ii)
        ii = ii + 3 + 1 - i
 60 end do
!
!     --- TEST DE CONVERGENCE SUR LES VALEURS PROPRES ---
!
    iconv = .true.
    do 70 i = 1, 3
        rtol = tol * valaux(i)
        dif = abs(valpro(i) - valaux(i))
        if (dif .gt. abs(rtol)) then
            iconv = .false.
            goto 9998
        endif
 70 end do
!
!     ---    CALCUL DES FACTEURS DE COUPLAGE   ---
!     --- TEST DE CONVERGENCE SUR CES FACTEURS ---
!
    eps = tol**2
    do 80 j = 1, 3 - 1
        jm1 = j - 1
        jp1 = j + 1
        ljk = jm1 * 3 - jm1 * j /2
        jj = ljk + j
        do 81 k = jp1, 3
            km1 = k - 1
            jk = ljk + k
            kk = km1 * 3 - km1 * k /2 + k
            epsa = abs(ar(jk) * ar(jk))
            compa = eps * abs(ar(jj) * ar(kk))
            epsb = abs(br(jk) * br(jk))
            compb = eps * abs(br(jj) * br(kk))
            if (epsa .ge. compa .or. epsb .ge. compb) then
                iconv = .false.
                goto 9998
            endif
 81     continue
 80 end do
!
9998 continue
!
!     ---  SI ON N'A PAS CONVERGE ---
!
    if (.not.iconv) then
!
!        --- TRANSLATION DES VALEURS PROPRES ---
!
        do 82 i = 1, 3
            valaux(i) = valpro(i)
 82     continue
!
!        --- TEST SUR LE NOMBRE D'ITERATIONS ---
!
        if (niter .lt. nperm) goto 30
!
    endif
!
end subroutine
