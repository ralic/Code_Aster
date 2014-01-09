subroutine q4grig(nomte, xyzl, option, pgl, rig, ener)
    implicit none
#include "jeveux.h"
#include "asterfort/bsthpl.h"
#include "asterfort/dsqbfb.h"
#include "asterfort/dxmate.h"
#include "asterfort/dxqbm.h"
#include "asterfort/dxqloc.h"
#include "asterfort/dxqloe.h"
#include "asterfort/elref5.h"
#include "asterfort/gquad4.h"
#include "asterfort/jevech.h"
#include "asterfort/jquad4.h"
#include "asterfort/q4gbc.h"
#include "asterfort/r8inir.h"
#include "asterfort/utbtab.h"
#include "asterfort/utctab.h"
#include "asterfort/utdtab.h"
#include "asterfort/utmess.h"
#include "asterfort/utpvgl.h"
    real(kind=8) :: xyzl(3, *), pgl(*), rig(*), ener(*)
    character(len=16) :: option, nomte
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
!     ------------------------------------------------------------------
!
!     MATRICE DE RIGIDITE DE L'ELEMENT Q4GAMMA (AVEC CISAILLEMENT)
!     ------------------------------------------------------------------
!     IN  XYZL   : COORDONNEES LOCALES DES QUATRE NOEUDS
!     IN  OPTION : OPTION RIGI_MECA OU EPOT_ELEM
!     IN  PGL    : MATRICE DE PASSAGE GLOBAL/LOCAL
!     OUT RIG    : MATRICE DE RIGIDITE
!     OUT ENER   : TERMES POUR ENER_POT (EPOT_ELEM)
!     ------------------------------------------------------------------
    integer :: multic
    real(kind=8) :: wgt, depl(24)
    real(kind=8) :: df(3, 3), dm(3, 3), dmf(3, 3), dc(2, 2), dci(2, 2)
    real(kind=8) :: dmc(3, 2), dfc(3, 2)
    real(kind=8) :: bf(3, 12)
    real(kind=8) :: bc(2, 12)
    real(kind=8) :: bm(3, 8)
    real(kind=8) :: xab1(3, 12), xab2(2, 12), xab3(3, 8)
    real(kind=8) :: xab4(3, 12)
!                   ---(12,12)---
    real(kind=8) :: kf(144)
    real(kind=8) :: kc(144)
!                   -----(12,12) ----(12,12)
    real(kind=8) :: flexi(144), flex(144)
!                   -----(8,8)   -----(8,8)
    real(kind=8) :: membi(64), memb(64)
!                   -----(8,12)  -----(8,12)
    real(kind=8) :: mefli(96), mefl(96), kmc(96), kfc(144)
    real(kind=8) :: bsigth(24), enerth, caraq4(25)
    real(kind=8) :: t2iu(4), t2ui(4), t1ve(9), jacob(5), qsi, eta
    logical :: coupmf, indith
    integer :: i, jcoqu, jdepg, k
    real(kind=8) :: ctor, excent, zero
    integer :: ndim, nno, nnos, npg, ipoids, icoopg, ivf, idfdx, idfd2, jgano
!     ------------------------------------------------------------------
!
    call elref5(' ', 'RIGI', ndim, nno, nnos, npg, ipoids, icoopg, ivf, idfdx, idfd2, jgano)
!
    zero = 0.0d0
    enerth = zero
!
    call jevech('PCACOQU', 'L', jcoqu)
    ctor = zr(jcoqu+3)
    excent = zr(jcoqu+4)
!
! --- ON NE CALCULE PAS ENCORE LA MATRICE DE RIGIDITE D'UN ELEMENT
! --- Q4G EXCENTRE, ON S'ARRETE EN ERREUR FATALE :
!     ------------------------------------------
    if (excent .ne. zero) then
        call utmess('F', 'ELEMENTS2_57')
    endif
!
    call r8inir(96, zero, kmc, 1)
    call r8inir(144, zero, kfc, 1)
!
!     ----- CALCUL DES MATRICES DE RIGIDITE DU MATERIAU EN FLEXION,
!           MEMBRANE ET CISAILLEMENT INVERSEE --------------------------
    call dxmate('RIGI', df, dm, dmf, dc, dci, dmc, dfc, nno, pgl, multic, coupmf, t2iu, t2ui, t1ve)
!     ----- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE QUADRANGLE --------
    call gquad4(xyzl, caraq4)
!
    call r8inir(144, zero, flex, 1)
    call r8inir(64, zero, memb, 1)
    call r8inir(96, zero, mefl, 1)
!
    do i = 1, npg
        qsi = zr(icoopg-1+ndim*(i-1)+1)
        eta = zr(icoopg-1+ndim*(i-1)+2)
!        ---------------------------------------------------------------
!        CALCUL DE LA MATRICE DE RIGIDITE DE L'ELEMENT EN FLEXION
!        ---------------------------------------------------------------
!        ----- CALCUL DU JACOBIEN SUR LE QUADRANGLE --------------------
        call jquad4(xyzl, qsi, eta, jacob)
!        ---- CALCUL DE LA MATRICE BF ----------------------------------
        call dsqbfb(qsi, eta, jacob(2), bf)
!        ---- CALCUL DU PRODUIT BFT.DF.BF ------------------------------
        call utbtab('ZERO', 3, 12, df, bf, xab1, kf)
!        ---- CALCUL DE LA MATRICE BC ----------------------------------
        call q4gbc(qsi, eta, jacob(2), caraq4, bc)
!        ---- CALCUL DU PRODUIT BCT.DC.BC -----------------------------
        call utbtab('ZERO', 2, 12, dc, bc, xab2, kc)
!        ----- CALCUL DU PRODUIT BFT.DFC.BC ----------------------
        call utdtab('ZERO', 3, 2, 12, 12, dfc, bc, bf, xab4, kfc)
!        ----- CALCUL DE LA SOMME KF + KC = FLEXI ----------------------
        do k = 1, 144
            flexi(k) = kf(k) + kc(k) + kfc(k)
        end do
        wgt = zr(ipoids+i-1)*jacob(1)
        do k = 1, 144
            flex(k) = flex(k) + flexi(k)*wgt
        end do
!        ---------------------------------------------------------------
!        CALCUL DE LA MATRICE DE RIGIDITE DE L'ELEMENT EN MEMBRANE
!        ---------------------------------------------------------------
!        ----- CALCUL DE LA MATRICE BM ---------------------------------
        call dxqbm(qsi, eta, jacob(2), bm)
!        ----- CALCUL DU PRODUIT BMT.DM.BM -----------------------------
        call utbtab('ZERO', 3, 8, dm, bm, xab3, membi)
!        ----- CALCUL DE LA MATRICE DE RIGIDITE EN MEMBRANE ------------
        do k = 1, 64
            memb(k) = memb(k) + membi(k)*wgt
        end do
!        ----- CALCUL DU PRODUIT BMT.DMC.BC ----------------------
        call utdtab('ZERO', 3, 2, 12, 8, dmc, bc, bm, xab4, kmc)
!
        if (coupmf) then
!           ------------------------------------------------------------
!           CALCUL DES MATRICES DE COUPLAGE MEMBRANE/FLEXION
!           ------------------------------------------------------------
!           ----- CALCUL DU PRODUIT BMT.DMF.BF -------------------------
            call utctab('ZERO', 3, 12, 8, dmf, bf, bm, xab1, mefli)
            do k = 1, 96
                mefl(k) = mefl(k) + (mefli(k)+kmc(k))*wgt
            end do
        endif
    end do
!
    if (option .eq. 'RIGI_MECA') then
        call dxqloc(flex, memb, mefl, ctor, rig)
!
    else if (option.eq.'EPOT_ELEM') then
        call jevech('PDEPLAR', 'L', jdepg)
        call utpvgl(4, 6, pgl, zr(jdepg), depl)
        call dxqloe(flex, memb, mefl, ctor, coupmf, depl, ener)
        call bsthpl(nomte, bsigth, indith)
        if (indith) then
            do i = 1, 24
                enerth = enerth + depl(i)*bsigth(i)
            end do
            ener(1) = ener(1) - enerth
        endif
    endif
!
end subroutine
