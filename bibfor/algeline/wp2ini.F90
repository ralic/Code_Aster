subroutine wp2ini(appr, lmasse, lamor, lraide, lmatra,&
                  lmtpsc, sigma, xh, xb, optiof,&
                  prorto, nborto, nbvect, neq, lbloq,&
                  lddl, alpha, beta, signe, yh,&
                  yb, solveu)
! aslint: disable=W1504
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/codent.h"
#include "asterfort/ggubs.h"
#include "asterfort/jecreo.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveut.h"
#include "asterfort/mrmult.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/wp2ayl.h"
#include "asterfort/wp2biy.h"
#include "asterfort/wp2bry.h"
    character(len=1) :: appr
    integer :: lmasse, lamor, lraide, lmatra, lmtpsc
    complex(kind=8) :: sigma
    real(kind=8) :: xh(*), xb(*)
    character(len=*) :: optiof
    integer :: nborto, nbvect, neq, lbloq(*), lddl(*)
    real(kind=8) :: prorto
    real(kind=8) :: alpha(*), beta(*), signe(*), yh(neq, *), yb(neq, *)
    character(len=19) :: solveu
!     ------------------------------------------------------------------
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
!     GENERATION DES VECTEURS DE LANCZOS ET DE LA TRIADIAGONALE
!     ASSOCIEE POUR LE PROBLEME QUADRATIQUE AUX VALEURS PROPRES
!     ------------------------------------------------------------------
!     1. REDUCTION A UN PROBLEME GENERALISE
!
!         !K   0! !P!          !-C   -M! !P!
!         !     ! ! ! = LAMBDA !       ! ! ! <=> K.G*Z = LAMBDA*M.G*Z
!         !0  -M! !Q!          !-M    0! !Q!
!
!     2. DECALAGE SPECTRAL K.G_S = K.G - SIGMA*M.G
!     3. DEDUCTION D' UN OPERATEUR REEL A
!
!              A = RE(K.G_S**-1 * M.G)
!          OU  A = IM(K.G_S**-1 * M.G)
!
!     4. CHOIX D' UN PSEUDO PRODUIT SCALAIRE SUR R
!
!              B = RE(K.G_S**-1)**-1
!          OU  B = IM(K.G_S**-1)**-1
!
!     ------------------------------------------------------------------
! IN  APPR   : K : INDICATEUR DE L' APPROCHE POUR A ( 'R' OU 'I')
! IN  LMASSE : I : MATRICE DE MASSE
! IN  LAMOR  : I : MATRICE D' AMORTISSEMENT
! IN  LRAIDE : I : MATRICE DE RAIDEUR
! IN  LMATRA : I : MATRICE DYNAMIQUE(S) FACTORISEE LDLT
! IN  LMTPSC : I : MATRICE DYNAMIQUE(RE(S)) FACTORISEE LDLT
! IN  SIGMA  : C : VALEUR DU PARAMETRE DE DECALAGE SPECTRAL
! IN  XH     : R : PARTIE SUPERIEURE DU VECTEURS INITIAL
! IN  XB     : R : PARTIE INFERIEURE DU VECTEURS INITIAL
! IN  NBORTO : I : NOMBRE MAXIMAL DE REORTHOGONALISATION AUTORISEE
! IN  PRORTO : R : PRECISION DE LA REORTHOGONALISATION
! IN  NBVECT : I : NOMBRE DE VECTEURS A GENERER
! IN  NEQ    : I : DIMENSION DE L' ESPACE DE DEPART
! IN  LBLOQ  : I : TYPE DES DDL (LBOLOQ(I) = 0 <=> DDL(I) = BLOQUE)
! IN  LDDL   : I : TYPE DES DDL (LDDL(I) = 0 <=> DDL(I) = LAGRANGE)
! OUT ALPHA  : R : DIAGONALE DE LA TRIDIAGONALE
! OUT BETA   : R : SUR-DIAGONALE DE LA TRIDIAGONALE
! OUT SIGNE  : R : SIGNE DES TERMES DE LA SOUS-DIAGONALE
! OUT YH     : R : PARTIE SUPERIEURE DES VECTEURS DE LANCZOS (P)
! OUT YB     : R : PARTIE INFERIEURE DES VECTEURS DE LANCZOS (Q)
! IN  SOLVEU : K19: SD SOLVEUR POUR PARAMETRER LE SOLVEUR LINEAIRE
!     ------------------------------------------------------------------
!
!
!     ------------------------------------------------------------------
    character(len=12) :: strg
    character(len=24) :: valk
    integer :: au1, au2, au3, au4, av, abayh, abayb, aptbyh, aptbyb
    integer :: vali(4)
    integer :: i, j, k, abyh, abyb, io
    real(kind=8) :: a, b, c, sr, si, deuxsr, mods2, invsi, si2, d1, d2
    aster_logical :: oc, ro
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: ii, ips
    real(kind=8) :: dseed
!-----------------------------------------------------------------------
    call jemarq()
    si = dimag(sigma)
    si2 = si*si
    sr = dble(sigma)
    deuxsr = 2.0d0*sr
    mods2 = si2 + sr*sr
!
    if (optiof .eq. 'CENTRE') then
        invsi = 1.d0/si
    else
        invsi = 0.d0
    endif
!
!     ---- ALLOCATION DES ZONES DE TRAVAIL ---
    call wkvect('&&WP2INI.VECTEUR.AUX.U1R', 'V V R', neq, au1)
    call wkvect('&&WP2INI.VECTEUR.AUX.U2R', 'V V R', neq, au2)
    call wkvect('&&WP2INI.VECTEUR.AUX.U3R', 'V V R', neq, au3)
    call wkvect('&&WP2INI.VECTEUR.AUX.U4R', 'V V R', neq, au4)
    if (si .ne. 0.d0) then
        call wkvect('&&WP2INI.VECTEUR.AUX.VC ', 'V V C', neq, av)
    else
        av = 0
    endif
    call wkvect('&&WP2INI.B_A.VECT.LANC.H', 'V V R', neq, abayh)
    call wkvect('&&WP2INI.B_A.VECT.LANC.B', 'V V R', neq, abayb)
!
    call wkvect('&&WP2INI.PT.B.LANCZO.H', 'V V I', nbvect, aptbyh)
    call wkvect('&&WP2INI.PT.B.LANCZO.B', 'V V I', nbvect, aptbyb)
!
    do 10 i = 1, nbvect, 1
        call codent(i, 'G', strg)
        call jecreo('&&WP2INI.BYH'//strg, 'V V R')
        call jeecra('&&WP2INI.BYH'//strg, 'LONMAX', neq)
        call jeecra('&&WP2INI.BYH'//strg, 'LONUTI', neq)
        call jeveut('&&WP2INI.BYH'//strg, 'E', zi(aptbyh + i-1))
        call jecreo('&&WP2INI.BYB'//strg, 'V V R')
        call jeecra('&&WP2INI.BYB'//strg, 'LONMAX', neq)
        call jeecra('&&WP2INI.BYB'//strg, 'LONUTI', neq)
        call jeveut('&&WP2INI.BYB'//strg, 'E', zi(aptbyb + i-1))
 10 end do
!
    dseed = 773218.d0
    call ggubs(dseed, neq, xb)
    do 15 ii = 1, neq
        xh(ii) = 0.d0
        xb(ii) = lbloq(ii)*lddl(ii)*xb(ii)
 15 end do
!
!     1 - GENERATION DU PREMIER VECTEUR
!
!     --- 1.1. DIRECTION
    call wp2ayl(appr, lmatra, lmasse, lamor, sigma,&
                lbloq, xh, xb, yh(1, 1), yb(1, 1),&
                zr(au1), zr(au2), zr(au3), zr(au4), zc(av),&
                neq, solveu)
!
    do 100 i = 1, neq
        zr(abayh + i-1) = -zr(au1 + i-1) - zr(au2 + i-1)
        zr(abayb + i-1) = -zr(au3 + i-1)
100 end do
!
!     --- 1.2. B_NORMALISATION
    c = 0.d0
    do 110 ips = 1, neq
        c = c + zr(abayh+ips-1)*yh(ips,1) + zr(abayb+ips-1)*yb(ips,1)
110 end do
    a = 1.d0/sqrt(abs(c))
    if (c .gt. 0.d0) then
        signe(1) = 1.d0
    else
        signe(1) = -1.d0
        a = -a
    endif
!
    abyh = zi(aptbyh + 1-1)
    abyb = zi(aptbyb + 1-1)
    do 120 i = 1, neq
        yh(i,1) = a*yh(i,1)
        yb(i,1) = a*yb(i,1)
        zr(abyh + i-1) = a*zr(abayh + i-1)
        zr(abyb + i-1) = a*zr(abayb + i-1)
120 end do
!
!     --- 1.3. COEFFICIENT DE LA TRIDIAGONALE
    call mrmult('ZERO', lamor, yh(1, 1), zr(au1), 1,&
                .false._1)
    call mrmult('ZERO', lmasse, yb(1, 1), zr(au2), 1,&
                .false._1)
    call mrmult('ZERO', lmasse, yh(1, 1), zr(au3), 1,&
                .false._1)
!
    a = 0.d0
    do 130 i = 1, neq
        a = a - yh(i,1)*(zr(au1 + i-1) + zr(au2 + i-1)) - yb(i,1)* zr( au3 + i-1)
130 end do
    alpha(1) = a
    beta(1) = 0.d0
!
!     2  -  GENERATION DES VECTEURS 2, 3, .. , NBVECT
    do 200 j = 2, nbvect
!
!        --- 2.1. DIRECTION
        call wp2ayl(appr, lmatra, lmasse, lamor, sigma,&
                    lbloq, yh(1, j-1), yb(1, j-1), yh(1, j), yb(1, j),&
                    zr(au1), zr(au2), zr(au3), zr(au4), zc(av),&
                    neq, solveu)
!
        do 210 i = 1, neq
            zr(abayh + i-1) = -zr(au1 + i-1) - zr(au2 + i-1)
            zr(abayb + i-1) = -zr(au3 + i-1)
210     continue
!
        a = 0.d0
        do 215 ips = 1, neq
            a = a + zr(abayh+ips-1)*yh(ips,j-1) + zr(abayb+ips-1)*yb( ips,j-1)
215     continue
!
        d1 = signe(j-1)
        a = d1*a
!
        if (j .eq. 2) then
            do 220 i = 1, neq
                yh(i,j) = yh(i,j) - a*yh(i,j-1)
                yb(i,j) = yb(i,j) - a*yb(i,j-1)
220         continue
        else
            b = 0.d0
            do 225 ips = 1, neq
                b = b + zr(abayh+ips-1)*yh(ips,j-2) + zr(abayb+ips-1)* yb(ips,j-2)
225         continue
            d2 = signe(j-2)
            b = d2*b
            do 230 i = 1, neq
                yh(i,j) = yh(i,j) - a*yh(i,j-1) - b*yh(i,j-2)
                yb(i,j) = yb(i,j) - a*yb(i,j-1) - b*yb(i,j-2)
230         continue
        endif
!
!        --- 2.2. NORMALISATION
        abyh = zi(aptbyh + j-1)
        abyb = zi(aptbyb + j-1)
        if (appr .eq. 'R') then
            call wp2bry(lmtpsc, lmasse, lamor, lraide, sr,&
                        si2, yh(1, j), yb( 1, j), zr(abyh), zr(abyb),&
                        zr(au1), zr(au2), zr(au3), zr(au4), neq,&
                        solveu)
        else
            call wp2biy(lmasse, lamor, lraide, mods2, deuxsr,&
                        invsi, yh(1, j), yb(1, j), zr(abyh), zr(abyb),&
                        lbloq, zr(au1), zr(au2), zr(au3), zr(au4),&
                        neq)
        endif
        c = 0.d0
        do 235 ips = 1, neq
            c = c + zr(abyh+ips-1)*yh(ips,j) + zr(abyb+ips-1)*yb(ips, j)
235     continue
!
        a = 1.d0/sqrt(abs(c))
        if (c .gt. 0.d0) then
            signe(j) = 1.d0
        else
            signe(j) = -1.d0
            a = -a
        endif
!
        do 240 i = 1, neq
            zr(abyh + i-1) = a*zr(abyh + i-1)
            zr(abyb + i-1) = a*zr(abyb + i-1)
            yh(i,j) = a*yh(i,j)
            yb(i,j) = a*yb(i,j)
240     continue
!
!        --- 2.3. REORTHOGONALISTION
        ro = .false.
        do 300 i = 1, j-1
            abyh = zi(aptbyh + i-1)
            abyb = zi(aptbyb + i-1)
            a = 0.d0
            do 310 ips = 1, neq
                a = a + zr(abyh+ips-1)*yh(ips,j) + zr(abyb+ips-1)*yb( ips,j)
310         continue
            oc = ( abs(a) .lt. prorto )
            ro = (.not. oc) .or. ro
!
            io = 1
600         continue
            if ((.not. oc) .and. (io .le. nborto)) then
                a = a*signe(i)
                do 315 k = 1, neq
                    yh(k,j) = yh(k,j) - a*yh(k,i)
                    yb(k,j) = yb(k,j) - a*yb(k,i)
315             continue
                b = 0.d0
                do 320 ips = 1, neq
                    b = b + zr(abyh+ips-1)*yh(ips,j) + zr(abyb+ips-1)* yb(ips,j)
320             continue
                if (abs(b) .gt. abs(a)) then
                    vali (1) = io
                    vali (2) = io
                    vali (3) = j
                    vali (4) = i
                    valk = '"ENNUI" POSSIBLE'
                    call utmess('I', 'ALGELINE4_86', sk=valk, ni=4, vali=vali)
                    oc = .true.
                else
                    a = b
                    io = io + 1
                    oc = ( abs(b) .lt. prorto )
                endif
                goto 600
            endif
300     continue
!
!        --- 2.4. REACTUALISATION
        if (ro) then
            abyh = zi(aptbyh + j-1)
            abyb = zi(aptbyb + j-1)
            if (appr .eq. 'R') then
                call wp2bry(lmtpsc, lmasse, lamor, lraide, sr,&
                            si2, yh(1, j), yb(1, j), zr(abyh), zr(abyb),&
                            zr(au1), zr(au2), zr( au3), zr(au4), neq,&
                            solveu)
            else
                call wp2biy(lmasse, lamor, lraide, mods2, deuxsr,&
                            invsi, yh(1, j), yb(1, j), zr(abyh), zr(abyb),&
                            lbloq, zr(au1), zr( au2), zr(au3), zr(au4),&
                            neq)
            endif
            c = 0.d0
            do 350 ips = 1, neq
                c = c + zr(abyh+ips-1)*yh(ips,j) + zr(abyb+ips-1)*yb( ips,j)
350         continue
            a = 1.d0/sqrt(abs(c))
            if (c .gt. 0.d0) then
                signe(j) = 1.d0
            else
                signe(j) = -1.d0
                a = -a
            endif
!
            do 400 i = 1, neq
                zr(abyh + i-1) = a*zr(abyh + i-1)
                zr(abyb + i-1) = a*zr(abyb + i-1)
                yh(i,j) = a*yh(i,j)
                yb(i,j) = a*yb(i,j)
400         continue
        endif
!
!        --- 2.5. COEFFICIENTS DE LA TRIDIAGONALE
        call mrmult('ZERO', lamor, yh(1, j), zr(au1), 1,&
                    .false._1)
        call mrmult('ZERO', lmasse, yb(1, j), zr(au2), 1,&
                    .false._1)
        call mrmult('ZERO', lmasse, yh(1, j), zr(au3), 1,&
                    .false._1)
!
        a = 0.d0
        b = 0.d0
        do 500 i = 1, neq, 1
            a = a - yh(i,j)*(zr(au1+ i-1) + zr(au2 + i-1)) - yb(i,j)* zr(au3 + i-1)
            b = b - yh(i,j-1)*(zr(au1 + i-1) + zr(au2 + i-1)) - yb(i, j-1)* zr(au3 + i-1)
500     continue
        alpha(j) = a
        beta (j) = b
200 end do
!
!     --- DESTRUCTION DES OJB TEMPORAIRES
    call jedetr('&&WP2INI.VECTEUR.AUX.U1R')
    call jedetr('&&WP2INI.VECTEUR.AUX.U2R')
    call jedetr('&&WP2INI.VECTEUR.AUX.U3R')
    call jedetr('&&WP2INI.VECTEUR.AUX.U4R')
    call jedetr('&&WP2INI.VECTEUR.AUX.VC ')
    call jedetr('&&WP2INI.B_A.VECT.LANC.H')
    call jedetr('&&WP2INI.B_A.VECT.LANC.B')
    call jedetr('&&WP2INI.PT.B.LANCZO.H')
    call jedetr('&&WP2INI.PT.B.LANCZO.B')
    do 700 i = 1, nbvect, 1
        call codent(i, 'G', strg)
        call jedetr('&&WP2INI.BYH'//strg)
        call jedetr('&&WP2INI.BYB'//strg)
700 end do
!
    call jedema()
end subroutine
