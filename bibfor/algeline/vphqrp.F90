subroutine vphqrp(mat, neq, mxeq, icode, w,&
                  z, iz, wk, mxiter, ier,&
                  nitqr)
    implicit none
#include "asterfort/vpzbal.h"
#include "asterfort/vpzech.h"
#include "asterfort/vpzhes.h"
#include "asterfort/vpzqrh.h"
#include "asterfort/vpzrbk.h"
    integer :: neq, mxeq, icode, iz, ier, nitqr
    real(kind=8) :: mat(mxeq, 1), wk(neq, 1), w(1), z(1)
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
!     CALCUL DE TOUTES LES VALEURS PROPRES D'UNE MATRICE COMPLETE REELLE
!     MISE SOUS FORME DE HESSENBERG PUIS RESOLUTION PAR LA METHODE QR
!     ------------------------------------------------------------------
! VAR MAT   : R8 : MATRICE REEL D'ORDRE NEQ DONT ON CALCULE LES VALEURS
!                  TE LES VECTEURS PROPRES
!              *** EN SORTIE MAT EST DETRUITE ***
! IN  NEQ   : IS : ORDRE DE LA MATRICE
! IN  MXEQ  : IS : DIMENSION EXACT DE LA MATRICE ( IE  MAT(MXEQ,1) )
! IN  ICODE : IS : CODE DE CALCUL
!          = 0, CALCUL DES VALEURS PROPRES SEULEMENT
!          = 1, CALCUL DES VALEURS ET VECTEURS PROPRES
! OUT W     : C8 : VECTEUR (COMPLEXE) DES VALEURS PROPRES DE LA MATRICE
! OUT Z     : C8 : MATRICE (COMPLEXE) DES VECTEURS PROPRES DE LA MATRICE
!                  LA J-IEME COLONNE CONTIENT LE VECTEUR ASSOCIE A LA
!                  LA J-IEME VALEUR PROPRE DE W
!                  IF ICODE = 0, Z N'EST PAS UTILISE
! IN  IZ    : IS : 1-ERE DIMENSION (EXACTE) DE LA MATRICE Z
! LOC WK    : R8 : ZONE DE TRAVAIL DE LONGUEUR 2*NEQ
! IN  MXITER: IS : NOMBRE MAX D'ITERARION POUR LE QR
!                    (30 EST UN BON NOMBRE)
! OUT IER   : IS : PARAMETRE  D'ERREUR
!             IER = 0 OK
!             IER = J >0 , NON CONVERGENCE POUR LA J-IEME VALEUR PROPRE
!                LES J PREMIERES VALEURS PROPRES NE SONT PAS CALCULEES
! OUT NITQR : NOMBRE D'ITERATIONS QR POUR ATTEINDRE LA CONVERGENCE
!     ------------------------------------------------------------------
!     REFERENCE: F.L. BAUER - J.H. WILKINSON - C. REINSCH
!        HANDBOOK FOR AUTOMATIC COMPUTATION - LINEAR ALGEBRA - VOL.2
!        PAGE XXX
!     ------------------------------------------------------------------
    integer :: jer, iz2, k, l, i, n2, iiz, npi, jw, j, is, ig, igz
    real(kind=8) :: z11
!
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: mxiter
!-----------------------------------------------------------------------
    ier = 0
    jer = 0
    iz2 = iz+iz
    if (icode .lt. 0 .or. icode .gt. 1) then
        icode = 0
    endif
    if (icode .ne. 0 .and. iz .lt. neq) then
        icode = 0
    endif
!
    k = neq
    l = neq
    n2 = 2
    if (icode .eq. 0) n2 = 1
!
!     --- EQUILIBRAGE DE LA MATRICE INITIALE ---
!
    call vpzbal(mat, neq, mxeq, wk, k,&
                l)
!
!     --- MISE SOUS FORME DE HESSENBERG ---
!
    if (icode .eq. 0) then
        iiz = 1
    else
        iiz = neq
    endif
    if (l .ne. 0) then
!        IF L <> 0, MAT EST DEJA SOUS FORME DE HESSENBERG
        call vpzhes(mat, k, l, neq, neq,&
                    wk(1, n2))
    endif
!
!     --- TRANSFORMATION INVERSE HESSENBERG - EQUILIBRAGE
!     --- POUR LE CALCUL DES VECTEURS PROPRES
!
    if (icode .eq. 1) then
!        --- FORMATION DE LA MATRICE IDENTITE DANS Z
        do 25 i = 1, neq*neq
            z(i) = 0.d0
25      continue
        do 30 i = 1, neq*neq, neq+1
            z(i) = 1.d0
30      continue
        call vpzrbk(z, mat, wk(1, n2), neq, neq,&
                    k, l)
    endif
!
!     --- CALCUL DES VALEURS PROPRES (ET DES VECTEURS PROPRES)
!
    if (icode .eq. 0 .and. neq .eq. 1) then
        z11 = z(1)
    endif
    nitqr = 0
    call vpzqrh(mat, neq, neq, k, l,&
                w(1), w(neq+1), z, iiz, mxiter,&
                jer, nitqr)
    if (icode .eq. 0 .and. neq .eq. 1) then
        z(1) = z11
    endif
!
!     --- TRANSFORMATION INVERSE EQUILIBRAGE - MATRICE INITIALE
!     --- POUR LE CALCUL DES VECTEURS PROPRES
!
    if (jer .eq. 0 .and. icode .eq. 1) then
        call vpzech(wk, z, k, l, neq,&
                    neq, neq)
    endif
!
!     --- CONVERSION DES VALEURS PROPRES (W) EN FORMAT COMPLEXE
!
    do 45 i = 1, neq
        npi = neq+i
        wk(i,1) = w(npi)
45  end do
    jw = neq+neq
    j = neq
    do 50 i = 1, neq
        w(jw-1) = w(j)
        w(jw) = wk(j,1)
        jw = jw-2
        j = j-1
50  end do
!
!        TRAITEMENT DES VECTEURS PROPRES
!        IE. : LES CONVERTIR  EN FORMAT COMPLEXE DANS Z(IZ,NEQ)
!
    if (icode .eq. 1) then
!
        j = neq
60      continue
        if (j .lt. 1) goto 9999
        if (w(j+j) .ne. 0.d0) then
!           TRANSLATER LA PAIRE DE VECTEURS COMPLEXES CONJUGUES
            is = iz2*(j-1)+1
            ig = neq*(j-2)+1
            igz = ig+neq
!           TRANSLATER LE VECTEUR COMPLEXE CONJUGE
            do 65 i = 1, neq
                z(is) = z(ig)
                z(is+1) = -z(igz)
                is = is+2
                ig = ig+1
                igz = igz+1
65          continue
!           TRANSLATER LE VECTEUR COMPLEXE
            is = iz2*(j-2)+1
            ig = is+iz2
            do 70 i = 1, neq
                z(is) = z(ig)
                z(is+1) = -z(ig+1)
                is = is+2
                ig = ig+2
70          continue
            j = j-2
            goto 60
        endif
!        TRANSLATER LE VECTEUR REEL
        is = iz2*(j-1)+neq+neq
        ig = neq*j
        do 80 i = 1, neq
            z(is-1) = z(ig)
            z(is) = 0.d0
            is = is-2
            ig = ig-1
80      continue
        j = j-1
        goto 60
    endif
!
9999  continue
    ier = jer
end subroutine
