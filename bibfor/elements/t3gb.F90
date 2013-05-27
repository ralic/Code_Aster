subroutine t3gb(carat3, xyzl, bmat)
    implicit  none
    include 'jeveux.h'
    include 'asterfort/bcoqaf.h'
    include 'asterfort/dstbfb.h'
    include 'asterfort/dxtbm.h'
    include 'asterfort/elref5.h'
    include 'asterfort/t3gbc.h'
    real(kind=8) :: xyzl(3, 1), carat3(*), bmat(6, 1)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     ------------------------------------------------------------------
! --- CALCUL DE LA MATRICE (B) RELIANT LES DEFORMATIONS DU PREMIER
! --- ORDRE AUX DEPLACEMENTS AU POINT D'INTEGRATION D'INDICE IGAU
! --- POUR UN ELEMENT DE TYPE T3G
! --- (I.E. (EPS_1) = (B)*(UN))
! --- D'AUTRE_PART, ON CALCULE LE PRODUIT NOTE JACGAU = JACOBIEN*POIDS
!     ------------------------------------------------------------------
!     IN  XYZL(3,NNO)   : COORDONNEES DES CONNECTIVITES DE L'ELEMENT
!                         DANS LE REPERE LOCAL DE L'ELEMENT
!     IN  IGAU          : INDICE DU POINT D'INTEGRATION
!     OUT BMAT(6,1)     : MATRICE (B) AU POINT D'INTEGRATION COURANT
    integer :: ndim, nno, nnos, npg, ipoids, icoopg, ivf, idfdx, idfd2, jgano
    real(kind=8) :: bm(3, 6), bf(3, 9), bc(2, 9), qsi, eta
! ------------------------------------------------------------------
!
    call elref5(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, icoopg, ivf, idfdx,&
                idfd2, jgano)
!
! --- CALCUL DE LA MATRICE B_MEMBRANE NOTEE, ICI, (BM)
!     ------------------------------------------------
    call dxtbm(carat3(9), bm)
!
!     ------- CALCUL DE LA MATRICE BFB -------------------------------
    call dstbfb(carat3(9), bf)
!
!        ---- CALCUL DE LA MATRICE BC ----------------------------------
    qsi = 1.d0/3.d0
    eta = qsi
    call t3gbc(xyzl, qsi, eta, bc)
!
! --- AFFECTATION DE LA MATRICE B COMPLETE, NOTEE (BMAT)
! --- AVEC LES MATRICES (BM), (BF) ET (BC)
!     ------------------------------------
    call bcoqaf(bm, bf, bc, nno, bmat)
!
end subroutine
