subroutine calck1(norev, nomdb, sigmrv, sigmdb, tbscrv,&
                  tbscmb, prodef, londef, deklag, lrev,&
                  k1a, k1b)
!
    implicit     none
#include "jeveux.h"
#include "asterc/r8pi.h"
#include "asterc/r8prem.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mess.h"
    integer :: norev, nomdb
    real(kind=8) :: prodef, londef, deklag, lrev, k1a, k1b
    character(len=19) :: sigmrv, sigmdb, tbscrv, tbscmb
! ======================================================================
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
!
!
! ======================================================================
! ======================================================================
! --- BUT : CALCUL DES FACTEURS D'INTENSITE DE CONTRAINTES ELASTIQUES
! ======================================================================
! IN  : NOREV  : NOMBRE DE NOEUDS COTE REVETEMENT ----------------------
! --- : NOMDB  : NOMBRE DE NOEUDS COTE METAL DE BASE -------------------
! --- : SIGMRV : CONTRAINTES COTE REVETEMENT ---------------------------
! --- : SIGMDB : CONTRAINTES COTE METAL DE BASE ------------------------
! --- : TBSCRV : ABSCISSES CURVILIGNES COTE REVETEMENT -----------------
! --- : TBSCMB : ABSCISSES CURVILIGNES COTE METAL DE BASE --------------
! --- : PRODEF : PROFONDEUR DU DEFAUT ----------------------------------
! --- : LONDEF : LONGUEUR DU DEFAUT ------------------------------------
! --- : LREV   : LONGUEUR DU REVETEMENT --------------------------------
! --- : DEKLAG : DECALAGE DU DEFAUT COTE REVETEMENT (TOUJOURS NEGATIF) -
! OUT : K1A    : FACTEUR D'INTENSITE DE CONTRAINTES POINTE A -----------
! --- : K1B    : FACTEUR D'INTENSITE DE CONTRAINTES POINTE B -----------
! ======================================================================
! ======================================================================
    integer :: jsigmr, jsigmb, jabsrv, jabsmb, ific
    real(kind=8) :: zero, un, deux, rappo, gamma1, gamma2, fa, fb, fab
    real(kind=8) :: a, b, pi, alpha, beta, z, z2, z3, z4, z5
    real(kind=8) :: gamx, gamy, ldefo, rtole
! ======================================================================
! --- INITIALISATION DE PARAMETRES -------------------------------------
! ======================================================================
    parameter       ( zero   =  0.0d0 )
    parameter       ( un     =  1.0d0 )
    parameter       ( deux   =  2.0d0 )
! ======================================================================
    call jemarq()
! ======================================================================
! --- INITIALISATIONS DES VECTEURS -------------------------------------
! ======================================================================
    call jeveuo(tbscrv, 'L', jabsrv)
    call jeveuo(tbscmb, 'L', jabsmb)
    call jeveuo(sigmrv, 'L', jsigmr)
    call jeveuo(sigmdb, 'L', jsigmb)
! ======================================================================
! --- INITIALISATIONS DES VARIABLES REPRESENTANT LES FACTEURS ----------
! --- D'INTENSITE ------------------------------------------------------
! ======================================================================
    k1a = zero
    k1b = zero
    rtole = 1.0d-10
! ======================================================================
! --- INITIALISATIONS DES VARIABLES NECESSAIRE AU CALCUL ---------------
! ======================================================================
    ldefo = zero
    a = prodef/deux
    b = londef/deux
    pi = r8pi()
! ======================================================================
! --- VERIFICATION DE LA COHERENCE DE LA PROFONDEUR DU DEFAUT ET -------
! --- DES ABSCISSES CURVILIGNES COTE REVETEMENT ET COTE METAL DE BASE --
! ======================================================================
    ldefo = zr(jabsrv+norev-1) + zr(jabsmb+nomdb-1)
    if (abs(ldefo - prodef) .gt. rtole) then
        call u2mess('F', 'PREPOST_5')
    endif
! ======================================================================
! --- CALCULS DES FACTEURS D'INTENSITE DE CONTRAINTES COTE REVETEMENT --
! ======================================================================
    do 10 ific = 1, norev-1
        alpha = (zr(jsigmr+ific) - zr(jsigmr+ific-1) ) / ( zr(jabsrv+ ific) - zr(jabsrv+ific-1))
        beta = zr(jsigmr+ific-1) - alpha * ( zr(jabsrv+ific-1) - a )
        gamx = ( zr(jabsrv+ific-1) - a )
        gamy = sqrt( abs(a*a - gamx*gamx) )
        if (gamy .le. r8prem()) then
            if (gamx .lt. zero) then
                gamma1 = - pi / deux
            else
                gamma1 = pi / deux
            endif
        else
            gamma1 = atan2( gamx/gamy , un )
        endif
        gamx = ( zr(jabsrv+ific ) - a )
        gamy = sqrt( abs(a*a - gamx*gamx) )
        if (gamy .le. r8prem()) then
            if (gamx .lt. zero) then
                gamma2 = - pi / deux
            else
                gamma2 = pi / deux
            endif
        else
            gamma2 = atan2( gamx/gamy , un )
        endif
        k1a = k1a + (beta-alpha*a/2) * (gamma2-gamma1) + (beta-alpha* a) * (cos(gamma2)-cos(gamma&
              &1)) + alpha*a*(sin(2*gamma2)-sin(2* gamma1))/4
        k1b = k1b + (beta+alpha*a/2) * (gamma2-gamma1) - (beta+alpha* a) * (cos(gamma2)-cos(gamma&
              &1)) - alpha*a*(sin(2*gamma2)-sin(2* gamma1))/4
10  end do
! ======================================================================
! --- CALCULS DES FACTEURS D'INTENSITE DE CONTRAINTES COTE METAL DE BASE
! ======================================================================
    do 20 ific = 1, nomdb-1
        alpha = (zr(jsigmb+ific) - zr(jsigmb+ific-1) ) / ( zr(jabsmb+ ific) - zr(jabsmb+ific-1))
        beta = zr(jsigmb+ific-1) - alpha * ( zr(jabsmb+ific-1) - deklag - a )
        gamx = ( zr(jabsmb+ific-1) - deklag - a )
        gamy = sqrt( abs(a*a - gamx*gamx) )
        if (gamy .le. r8prem()) then
            if (gamx .lt. zero) then
                gamma1 = - pi / deux
            else
                gamma1 = pi / deux
            endif
        else
            gamma1 = atan2( gamx/gamy , un )
        endif
        gamx = ( zr(jabsmb+ific ) - deklag - a )
        gamy = sqrt( abs(a*a - gamx*gamx) )
        if (gamy .le. r8prem()) then
            if (gamx .lt. zero) then
                gamma2 = - pi / deux
            else
                gamma2 = pi / deux
            endif
        else
            gamma2 = atan2( gamx/gamy , un )
        endif
        k1a = k1a + (beta-alpha*a/2) * (gamma2-gamma1) + (beta-alpha* a) * (cos(gamma2)-cos(gamma&
              &1)) + alpha*a*(sin(2*gamma2)-sin(2* gamma1))/4
        k1b = k1b + (beta+alpha*a/2) * (gamma2-gamma1) - (beta+alpha* a) * (cos(gamma2)-cos(gamma&
              &1)) - alpha*a*(sin(2*gamma2)-sin(2* gamma1))/4
20  end do
    k1a = k1a * sqrt(a/pi)
    k1b = k1b * sqrt(a/pi)
! ======================================================================
! --- CORRECTION PAR LES FACTEURS DE BORDS -----------------------------
! ======================================================================
    z = a / (a + lrev + deklag)
    z2 = z * z
    z3 = z2 * z
    z4 = z3 * z
    z5 = z4 * z
    fa = 0.998742d0 + 0.142801d0*z - 1.133379d0*z2 + 5.491256d0*z3 - 8.981896d0*z4 + 5.765252d0*z&
         &5
    if (z .le. (0.92d0)) then
        fb = 1.0d0 - 0.012328d0*z+ 0.395205d0*z2 - 0.527964d0*z3 + 0.432714d0*z4
    else
        fb = - 414.20286d0 + 1336.75998d0*z - 1436.1197d0*z2 + 515.14949d0*z3
    endif
! ======================================================================
! --- CORRECTION PAR LES FACTEURS D'ELLIPTICITE ------------------------
! ======================================================================
    rappo = a/b
    if (a .le. b) then
        fab = 1.0d0 / sqrt(1.0d0+1.464d0*(rappo**1.65d0))
    else
        fab = 1.0d0 / ( rappo * sqrt(1.0d0+1.464d0*((1.0d0/rappo)** 1.65d0)))
    endif
    k1a = k1a * fa * fab
    k1b = k1b * fb * fab
! ======================================================================
    call jedema()
! ======================================================================
end subroutine
