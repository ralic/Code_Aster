subroutine dsqrig(nomte, xyzl, option, pgl, rig,&
                  ener)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8gaem.h"
#include "asterfort/bsthpl.h"
#include "asterfort/dsqbfa.h"
#include "asterfort/dsqbfb.h"
#include "asterfort/dsqcis.h"
#include "asterfort/dsqdi2.h"
#include "asterfort/dsqdis.h"
#include "asterfort/dsxhft.h"
#include "asterfort/dxhmft.h"
#include "asterfort/dxmate.h"
#include "asterfort/dxqbm.h"
#include "asterfort/dxqloc.h"
#include "asterfort/dxqloe.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/gquad4.h"
#include "asterfort/jevech.h"
#include "asterfort/jquad4.h"
#include "asterfort/promat.h"
#include "asterfort/r8inir.h"
#include "asterfort/utbtab.h"
#include "asterfort/utctab.h"
#include "asterfort/utdtab.h"
#include "asterfort/utpvgl.h"
    real(kind=8) :: xyzl(3, *), pgl(*), rig(*), ener(*)
    character(len=16) :: option, nomte
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
!
!     MATRICE DE RIGIDITE DE L'ELEMENT DE PLAQUE DSQ (AVEC CISAILLEMENT)
!     ------------------------------------------------------------------
!     IN  XYZL   : COORDONNEES LOCALES DES QUATRE NOEUDS
!     IN  OPTION : OPTION RIGI_MECA OU EPOT_ELEM
!     IN  PGL    : MATRICE DE PASSAGE GLOBAL/LOCAL
!     OUT RIG    : MATRICE DE RIGIDITE
!     OUT ENER   : TERMES POUR ENER_POT (EPOT_ELEM)
!     ------------------------------------------------------------------
    integer :: i, int, j, jcoqu, jdepg, k, multic
    real(kind=8) :: wgt, depl(24)
    real(kind=8) :: df(3, 3), dm(3, 3), dmf(3, 3), dc(2, 2), dci(2, 2)
    real(kind=8) :: dmc(3, 2), dfc(3, 2)
    real(kind=8) :: bfb(3, 12), bfa(3, 4), hft2(2, 6), hmft2(2, 6)
    real(kind=8) :: bcb(2, 12), bca(2, 4), bcm(2, 8), pb(4, 12), pm(4, 8)
    real(kind=8) :: bm(3, 8)
    real(kind=8) :: xab1(3, 12), xab2(3, 4), xab3(2, 12), xab4(2, 4)
    real(kind=8) :: xab5(4, 12), xab6(3, 8), xab7(3, 2)
    real(kind=8) :: xab8(12, 2), xab9(4, 2), xab10(8, 2), xab11(4, 8)
    real(kind=8) :: xab12(2, 8)
    real(kind=8) :: kba(12, 4), kf12(12, 4), kfc12(12, 4)
    real(kind=8) :: kmf11(8, 12), kmf12(8, 4), kmf(8, 12)
!                   -----(12,12) -----(4,4)
    real(kind=8) :: kf11(12, 12), kf22(4, 4)
    real(kind=8) :: kbb(12, 12), kaa(4, 4)
    real(kind=8) :: kfc11(12, 12), kfc22(4, 4)
!                   -----(12,12) -----(12,12)
    real(kind=8) :: kfc(12, 12), kfb(12, 12)
!                   -----(12,12) ----(12,12)
    real(kind=8) :: flexi(12, 12), flex(12, 12)
!                   -----(8,8)   -----(8,8)
    real(kind=8) :: membi(8, 8), memb(8, 8), memexc(8, 8)
!                   -----(8,12)  -----(8,12)
    real(kind=8) :: mefli(8, 12), mefl(8, 12)
    real(kind=8) :: kfcg11(12, 4), kfc21(4, 4), kmc(8, 4), kmapb(8, 12)
    real(kind=8) :: bcmbcb(8, 12), kma(8, 4), kmb(8, 12)
    real(kind=8) :: kmpmt(8, 8), kmpm(8, 8), membcf(8, 8), bcapm(2, 8)
    real(kind=8) :: bsigth(24), enerth, ctor, un, zero, eta, excent, qsi
    real(kind=8) :: jacob(5), caraq4(25), t2iu(4), t2ui(4), t1ve(9)
    aster_logical :: coupmf, exce, indith
    integer :: ndim, nno, nnos, npg, ipoids, icoopg, ivf, idfdx, idfd2, jgano
!     ------------------------------------------------------------------
!
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnos, npg=npg,&
                     jpoids=ipoids, jcoopg=icoopg, jvf=ivf, jdfde=idfdx, jdfd2=idfd2,&
                     jgano=jgano)
!
    zero = 0.0d0
    un = 1.0d0
    enerth = zero
!
    call r8inir(48, zero, kfcg11, 1)
    call r8inir(16, zero, kfc21, 1)
    call r8inir(32, zero, kmc, 1)
    call r8inir(32, zero, pm, 1)
    call r8inir(64, zero, memexc, 1)
    call r8inir(32, zero, kmf12, 1)
    call r8inir(16, zero, bcm, 1)
    call r8inir(32, zero, kma, 1)
    call r8inir(64, zero, membcf, 1)
    call r8inir(96, zero, bcmbcb, 1)
    call r8inir(96, zero, kmf11, 1)
    call r8inir(64, zero, memb, 1)
    call r8inir(144, zero, flex, 1)
    call r8inir(96, zero, kmb, 1)
    call r8inir(96, zero, mefl, 1)
    call r8inir(64, zero, kmpmt, 1)
    call r8inir(64, zero, kmpm, 1)
    call r8inir(96, zero, kmapb, 1)
    call r8inir(16, zero, bcapm, 1)
!
    call jevech('PCACOQU', 'L', jcoqu)
    ctor = zr(jcoqu+3)
    excent = zr(jcoqu+4)
!
    exce = .false.
    if (abs(excent) .gt. un/r8gaem()) exce = .true.
!
!     ----- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE QUADRANGLE --------
    call gquad4(xyzl, caraq4)
!
!     ----- CALCUL DES MATRICES DE RIGIDITE DU MATERIAU EN FLEXION,
!           MEMBRANE ET CISAILLEMENT INVERSEE --------------------------
    call dxmate('RIGI', df, dm, dmf, dc,&
                dci, dmc, dfc, nno, pgl,&
                multic, coupmf, t2iu, t2ui, t1ve)
!
!     ---- CALCUL DE LA MATRICE PB -------------------------------------
    if (exce) then
        call dsqdi2(xyzl, df, dci, dmf, dfc,&
                    dmc, pb, pm)
    else
        call dsqdis(xyzl, caraq4, df, dci, pb)
    endif
!
! --- BOUCLE SUR LES POINTS D'INTEGRATION :
!     -----------------------------------
    do 10 int = 1, npg
!
!============================================================
! --- CALCUL DE LA MATRICE DE RIGIDITE DE L'ELEMENT POUR    =
! --- LA FLEXION ET LE CISAILLEMENT                         =
!============================================================
!
! ---   COORDONNEES DU POINT D'INTEGRATION COURANT :
!       ------------------------------------------
        qsi = zr(icoopg-1+ndim*(int-1)+1)
        eta = zr(icoopg-1+ndim*(int-1)+2)
!
! ---   CALCUL DU JACOBIEN SUR LE QUADRANGLE :
!       ------------------------------------
        call jquad4(xyzl, qsi, eta, jacob)
!
! ---   CALCUL DE LA MATRICE BM :
!       -----------------------
        call dxqbm(qsi, eta, jacob(2), bm)
!
! ---   CALCUL DE LA MATRICE BFB  :
!       ------------------------
        call dsqbfb(qsi, eta, jacob(2), bfb)
!
! ---   CALCUL DE LA MATRICE BFA  :
!       ------------------------
        call dsqbfa(qsi, eta, jacob(2), caraq4, bfa)
!
! ---   CALCUL DU PRODUIT BFBT.DF.BFB :
!       -----------------------------
        call utbtab('ZERO', 3, 12, df, bfb,&
                    xab1, kf11)
!
! ---   CALCUL DU PRODUIT BFAT.DF.BFA :
!       -----------------------------
        call utbtab('ZERO', 3, 4, df, bfa,&
                    xab2, kf22)
!
! ---   CALCUL DU PRODUIT BFBT.DF.BFA :
!       -----------------------------
        call utctab('ZERO', 3, 4, 12, df,&
                    bfa, bfb, xab2, kf12)
!
! ---   CALCUL DU PRODUIT HF.T2 :
!       -----------------------
        call dsxhft(df, jacob(2), hft2)
!
! ---   CALCUL DU PRODUIT HMF.T2 :
!       ------------------------
        call dxhmft(dmf, jacob(2), hmft2)
!
! ---   CALCUL DES MATRICES BCB, BCA ET BCM:
!       -----------------------------------
        call dsqcis(qsi, eta, caraq4, hmft2, hft2,&
                    bcm, bcb, bca)
!
! ---   CALCUL DES MATRICES BCBT.DCI.BCB  :
!       --------------------------------
        call utbtab('ZERO', 2, 12, dci, bcb,&
                    xab3, kbb)
!
! ---   CALCUL DU PRODUIT BCAT.DCI.BCA :
!       -----------------------------
        call utbtab('ZERO', 2, 4, dci, bca,&
                    xab4, kaa)
!
! ---   CALCUL DU PRODUIT BCBT.DCI.BCA :
!       ------------------------------
        call utctab('ZERO', 2, 4, 12, dci,&
                    bca, bcb, xab4, kba)
!
! ---   CALCUL DU MATERIAU ELAS_COQUE :
!       =============================
!
! ---   CALCUL DU PRODUIT BFBT.DFC.DCI.BCA :
!       ----------------------------------
        call utdtab('ZERO', 3, 2, 2, 12,&
                    dfc, dci, bfb, xab7, xab8)
        call promat(xab8, 12, 12, 2, bca,&
                    2, 2, 4, kfcg11)
!
! ---   CALCUL DU PRODUIT BFAT.DFC.DCI.BCA :
!       ----------------------------------
        call utdtab('ZERO', 3, 2, 2, 4,&
                    dfc, dci, bfa, xab7, xab9)
        call promat(xab9, 4, 4, 2, bca,&
                    2, 2, 4, kfc21)
!
! ---   CALCUL DU PRODUIT BMT.DMC.DCI.BCA :
!       ----------------------------------
        call utdtab('ZERO', 3, 2, 2, 8,&
                    dmc, dci, bm, xab7, xab10)
        call promat(xab10, 8, 8, 2, bca,&
                    2, 2, 4, kmc)
!
! ---   CALCUL DES SOMMES KF + KC = KFC :
!       -------------------------------
        do 20 i = 1, 12
            do 30 j = 1, 12
                kfc11(i,j) = kf11(i,j) + kbb(i,j)
 30         continue
 20     continue
        do 40 i = 1, 12
            do 50 j = 1, 4
                kfc12(i,j) = kf12(i,j) + kba(i,j) + kfcg11(i,j)
 50         continue
 40     continue
        do 60 i = 1, 4
            do 70 j = 1, 4
                kfc22(i,j) = kf22(i,j) + kaa(i,j) + kfc21(i,j) + kfc21(j,i)
 70         continue
 60     continue
!
        if (coupmf .or. exce) then
!
! ---     CALCUL DU PRODUIT BMT.DMF.BFB :
!         -----------------------------
            call utctab('ZERO', 3, 12, 8, dmf,&
                        bfb, bm, xab1, kmf11)
!
! ---     CALCUL DU PRODUIT BMT.DMF.BFA :
!         -----------------------------
            call utctab('ZERO', 3, 4, 8, dmf,&
                        bfa, bm, xab2, kmf12)
!
        endif
!
!===============================================
! ---   PREPARATION DU CAS DE L'EXCENTREMENT   =
!===============================================
        if (exce) then
!
            call r8inir(64, zero, kmpmt, 1)
            call r8inir(64, zero, kmpm, 1)
            call r8inir(96, zero, kmapb, 1)
!
! ---     AFFECTATION DE LA MATRICE [MEMBCF] EGALE A [BCM]T*[DCI]*[BCM]:
!         -------------------------------------------------------------
            call utbtab('ZERO', 2, 8, dci, bcm,&
                        xab12, membcf)
!
!
! ---     AFFECTATION DE LA MATRICE [KMA] EGALE A [BCM]T*[DCI]*[BCA] :
!         ---------------------------------------------------------
            call utctab('ZERO', 2, 4, 8, dci,&
                        bca, bcm, xab4, kma)
!
! ---     AFFECTATION DE LA MATRICE [KMB] EGALE A [BCM]T*[DCI]*[BCB] :
!         ---------------------------------------------------------
            call utctab('ZERO', 2, 12, 8, dci,&
                        bcb, bcm, xab3, kmb)
!
! ---     DETERMINATION DU TERME [PM]T*([KMA]*T+[KMF12]T) :
!         -----------------------------------------------
            do 80 i = 1, 8
                do 90 j = 1, 8
                    do 100 k = 1, 4
                        kmpmt(i,j) = kmpmt(i,j) + pm(k,i)*(kma(j,k)+ kmf12(j,k))
100                 continue
 90             continue
 80         continue
!
! ---     DETERMINATION DU TERME ([KMA]+[KMF12])*[PM] :
!         --------------------------------------------
            do 110 i = 1, 8
                do 120 j = 1, 8
                    do 130 k = 1, 4
                        kmpm(i,j) = kmpm(i,j) + (kma(i,k)+kmf12(i,k))* pm(k,j)
130                 continue
120             continue
110         continue
!
! ---     DETERMINATION DU TERME [KMA]*[PB] :
!         ---------------------------------
            do 140 i = 1, 8
                do 150 j = 1, 12
                    do 160 k = 1, 4
                        kmapb(i,j) = kmapb(i,j) + kma(i,k)*pb(k,j)
160                 continue
150             continue
140         continue
!
        endif
!=======================================================================
! --- CALCUL DE LA MATRICE DE RIGIDITE EN FLEXION                      =
! ---   FLEXI = KF11 + KBB + KFC12*PB + PB_T*KFC12_T + PB_T*KFC22*PB   =
!=======================================================================
!
! ---   FLEXI = KFC11 + KFC12*PB + PB_T*KFC12_T + PB_T*KFC22*PB  :
!       =======================================================
!
! ---   CALCUL DU PRODUIT PBT.KFC22.PB :
!       ------------------------------
        call utbtab('ZERO', 4, 12, kfc22, pb,&
                    xab5, flexi)
!
        do 170 i = 1, 12
            do 170 j = 1, 12
                kfb(i,j) = zero
170         continue
!
        do 180 i = 1, 12
            do 190 j = 1, 12
                do 200 k = 1, 4
                    kfb(i,j) = kfb(i,j) + kfc12(i,k)*pb(k,j)
200             continue
                kfc(j,i) = kfb(i,j)
190         continue
180     continue
!
        do 210 i = 1, 12
            do 220 j = 1, 12
                flexi(i,j) = flexi(i,j) + kfc11(i,j) + kfb(i,j) + kfc( i,j)
220         continue
210     continue
!
        wgt = zr(ipoids+int-1)*jacob(1)
        do 230 i = 1, 12
            do 230 j = 1, 12
                flex(i,j) = flex(i,j) + flexi(i,j)*wgt
230         continue
!
!============================================================
! --- CALCUL DE LA MATRICE DE RIGIDITE EN MEMBRANE          =
! --- K_MEMBRANE =   MEMBI + KMF12*PM + PM_T*KMF12_T        =
! ---              + PM_T*(KF22+KAA)*PM + BCM_T*DCI*BCM     =
! ---              + KMA*PM + PM_T*KMA_T                    =
!============================================================
!
! ---   CALCUL DU PRODUIT BMT.DM.BM :
!       ---------------------------
        call utbtab('ZERO', 3, 8, dm, bm,&
                    xab6, membi)
!
! ---    CALCUL DE [PM]T*([KF22] + [KAA])*[PM]
!       --------------------------------------
        if (exce) then
            call utbtab('ZERO', 4, 8, kfc22, pm,&
                        xab11, memexc)
        endif
!
! ---   CALCUL DE LA MATRICE DE RIGIDITE EN MEMBRANE :
!*****************************************************************
! ATTENTION POUR LE MOMENT ON DESACTIVE LES TERMES SUPPLEMENTAIRES
! DUS A L'EXCENTREMENT ET INTERVENANT EN MEMBRANE :
!    MEMEXC -> PM_T*(KF22+KAA)*PM
!    KMPM   -> (KMF12+KMA)*PM
!    KMPMT  -> PM_T*(KMF12_T+KMA_T)
!    MEMBCF -> BCM_T*DCI*BCM
! FINALEMENT ON DECIDE DE REACTIVER CES TERMES EN ATTENDANT
! DES ANOMALIES TROP IMPORTANTES
! ---------------------------------------------------------------
        do 240 i = 1, 8
            do 250 j = 1, 8
                memb(i,j) = memb(i,j) + (memexc(i,j)+membi(i,j)+kmpm( i,j)+kmpmt(i,j) +membcf(i,j&
                            &))*wgt
!     +                            MEMBI(I,J)*WGT
!*****************************************************************
250         continue
240     continue
!
!====================================================================
! --- CALCUL DE LA MATRICE DE RIGIDITE DE COUPLAGE MEMBRANE-FLEXION =
! --- K_MEMBRANE-FLEXION =   KMF11 + KMB + PM_T*(KF22+KAA)*PB       =
! ---                      +  PM_T*(KF12_T+KBA_T) + KMA*PB          =
! ---                      +  KMF12*PB                              =
!====================================================================
!
        if (coupmf .or. exce) then
!
            do 260 i = 1, 8
                do 270 j = 1, 12
                    kmf(i,j) = zero
270             continue
260         continue
!
! ---     CALCUL DU TERME  [PM]T*([KF22] + [KAA])*[PB]
!         --------------------------------------------
            call utctab('ZERO', 4, 12, 8, kfc22,&
                        pb, pm, xab5, kmf)
!
            do 280 i = 1, 8
                do 290 j = 1, 12
                    do 300 k = 1, 4
                        kmf(i,j) = kmf(i,j) + (kmf12(i,k)+kmc(i,k))* pb(k,j) + pm(k,i)*kfc12(j,k)
300                 continue
                    mefli(i,j) = kmf11(i,j) + kmf(i,j) + kmb(i,j) + kmapb(i,j)
290             continue
280         continue
!
            do 310 i = 1, 8
                do 320 j = 1, 12
                    mefl(i,j) = mefl(i,j) + mefli(i,j)*wgt
320             continue
310         continue
!
        endif
!
 10 end do
!
    if (option .eq. 'RIGI_MECA') then
        call dxqloc(flex, memb, mefl, ctor, rig)
!
    else if (option.eq.'EPOT_ELEM') then
        call jevech('PDEPLAR', 'L', jdepg)
        call utpvgl(4, 6, pgl, zr(jdepg), depl)
        call dxqloe(flex, memb, mefl, ctor, coupmf,&
                    depl, ener)
        call bsthpl(nomte, bsigth, indith)
        if (indith) then
            do 330 i = 1, 24
                enerth = enerth + depl(i)*bsigth(i)
330         continue
            ener(1) = ener(1) - enerth
        endif
    endif
!
end subroutine
