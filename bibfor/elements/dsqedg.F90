subroutine dsqedg(xyzl, option, pgl, depl, edgl)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8miem.h"
#include "asterfort/dsqbfa.h"
#include "asterfort/dsqbfb.h"
#include "asterfort/dsqcis.h"
#include "asterfort/dsqdi2.h"
#include "asterfort/dsxhft.h"
#include "asterfort/dxhmft.h"
#include "asterfort/dxmate.h"
#include "asterfort/dxqbm.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/gquad4.h"
#include "asterfort/jevech.h"
#include "asterfort/jquad4.h"
    real(kind=8) :: xyzl(3, *), pgl(3, *), depl(*), edgl(*)
    character(len=16) :: option
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
!     EFFORTS ET DEFORMATIONS GENERALISES DE L'ELEMENT DE PLAQUE DSQ
!     ------------------------------------------------------------------
!     IN  XYZL   : COORDONNEES LOCALES DES QUATRE NOEUDS
!     IN  OPTION : NOM DE L'OPTION DE CALCUL
!     IN  PGL    : MATRICE DE PASSAGE GLOBAL - LOCAL
!     IN  DEPL   : DEPLACEMENTS
!     OUT EDGL   : EFFORTS OU DEFORMATIONS GENERALISES AUX NOEUDS DANS
!                  LE REPERE INTRINSEQUE A L'ELEMENT
    integer :: ndim, nno, nnos, npg, ipoids, icoopg, ivf, idfdx, idfd2, jgano
    integer :: multic, ne, k, j, i, ie, jcara
    real(kind=8) :: depf(12), depm(8)
    real(kind=8) :: df(3, 3), dm(3, 3), dmf(3, 3), dc(2, 2), dci(2, 2)
    real(kind=8) :: dmc(3, 2)
    real(kind=8) :: hft2(2, 6), an(4, 12), hmft2(2, 6), dfc(3, 2)
    real(kind=8) :: bfb(3, 12), bfa(3, 4), bfn(3, 12), bf(3, 12)
    real(kind=8) :: bcb(2, 12), bca(2, 4), bcn(2, 12), bc(2, 12), bcm(2, 8)
    real(kind=8) :: bm(3, 8), am(4, 8), bdf(3), bdm(3), dcis(2), btm(2, 8), bdf2(3)
    real(kind=8) :: bdfm(3), bdm1(3)
    real(kind=8) :: vf(3), vm(3), vt(2), excent, bfam(3, 8), bcam(2, 8)
    real(kind=8) :: vfm(3), vmf(3), vmc(3), vfc(3), vcm(2), vcf(2)
    real(kind=8) :: qsi, eta, jacob(5), caraq4(25), t2iu(4), t2ui(4), t1ve(9)
    aster_logical :: coupmf, excen
    character(len=4) :: fami
!     ------------------------------------------------------------------
!
    if (option(6:9) .eq. 'ELGA') then
        call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnos, npg=npg,&
                         jpoids=ipoids, jcoopg=icoopg, jvf=ivf, jdfde=idfdx, jdfd2=idfd2,&
                         jgano=jgano)
        ne = npg
        fami='RIGI'
    else if (option(6:9).eq.'ELNO') then
        call elrefe_info(fami='NOEU', ndim=ndim, nno=nno, nnos=nnos, npg=npg,&
                         jpoids=ipoids, jcoopg=icoopg, jvf=ivf, jdfde=idfdx, jdfd2=idfd2,&
                         jgano=jgano)
        ne = nno
        fami='NOEU'
    endif
!
    call jevech('PCACOQU', 'L', jcara)
    excent = zr(jcara+4)
    excen = .false.
    if (excent .gt. r8miem()) then
        excen=.true.
    endif
!     ----- CALCUL DES MATRICES DE RIGIDITE DU MATERIAU EN FLEXION,
!           MEMBRANE ET CISAILLEMENT INVERSEES -------------------------
!
!     ----- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE QUADRANGLE --------
    call gquad4(xyzl, caraq4)
!     ----- CARACTERISTIQUES DES MATERIAUX --------
    call dxmate(fami, df, dm, dmf, dc,&
                dci, dmc, dfc, nno, pgl,&
                multic, coupmf, t2iu, t2ui, t1ve)
!     ----- COMPOSANTES DEPLACEMENT MEMBRANE ET FLEXION ----------------
    do 20 j = 1, nno
        do 10 i = 1, 2
            depm(i+2*(j-1)) = depl(i+6*(j-1))
 10     continue
        depf(1+3* (j-1)) = depl(1+2+6*(j-1))
        depf(2+3* (j-1)) = depl(3+2+6*(j-1))
        depf(3+3* (j-1)) = -depl(2+2+6*(j-1))
 20 continue
!     ---- CALCUL DE LA MATRICE AN -------------------------------------
    call dsqdi2(xyzl, df, dci, dmf, dfc,&
                dmc, an, am)
!
    if (option(1:4) .eq. 'DEGE') then
!         ---------------------
        do 180 ie = 1, ne
!
! ---     COORDONNEES DU POINT D'INTEGRATION COURANT :
            qsi = zr(icoopg-1+ndim*(ie-1)+1)
            eta = zr(icoopg-1+ndim*(ie-1)+2)
!
!           ----- CALCUL DU JACOBIEN SUR LE QUADRANGLE -----------------
            call jquad4(xyzl, qsi, eta, jacob)
!
!           ----- CALCUL DE LA MATRICE BM ------------------------------
            call dxqbm(qsi, eta, jacob(2), bm)
!
! ---     CALCUL DU PRODUIT HF.T2 :
            call dsxhft(df, jacob(2), hft2)
!
! ---     CALCUL DU PRODUIT HMF.T2 :
            call dxhmft(dmf, jacob(2), hmft2)
!
! ---     CALCUL DES MATRICES BCB, BCA ET BCM :
            call dsqcis(qsi, eta, caraq4, hmft2, hft2,&
                        bcm, bcb, bca)
!
!           ------ BC = BCB + BCA.AN -----------------------------------
            do 30 k = 1, 24
                bcn(k,1) = 0.d0
 30         continue
            do 60 i = 1, 2
                do 50 j = 1, 12
                    do 40 k = 1, 4
                        bcn(i,j) = bcn(i,j) + bca(i,k)*an(k,j)
 40                 continue
                    bc(i,j) = bcb(i,j) + bcn(i,j)
 50             continue
 60         continue
!           ------ VT = BC.DEPF ---------------------------------------
            vt(1) = 0.d0
            vt(2) = 0.d0
            do 80 i = 1, 2
                do 70 j = 1, 12
                    vt(i) = vt(i) + bc(i,j)*depf(j)
 70             continue
 80         continue
!           ----- CALCUL DE LA MATRICE BFB AU POINT QSI ETA -----------
            call dsqbfb(qsi, eta, jacob(2), bfb)
!           ----- CALCUL DE LA MATRICE BFA AU POINT QSI ETA -----------
            call dsqbfa(qsi, eta, jacob(2), caraq4, bfa)
!           ------ BF = BFB + BFA.AN ----------------------------------
            do 90 k = 1, 36
                bfn(k,1) = 0.d0
 90         continue
            do 120 i = 1, 3
                do 110 j = 1, 12
                    do 100 k = 1, 4
                        bfn(i,j) = bfn(i,j) + bfa(i,k)*an(k,j)
100                 continue
                    bf(i,j) = bfb(i,j) + bfn(i,j)
110             continue
120         continue
            do 130 k = 1, 3
                bdf(k) = 0.d0
                bdm(k) = 0.d0
130         continue
            do 160 i = 1, 3
                do 140 j = 1, 12
                    bdf(i) = bdf(i) + bf(i,j)*depf(j)
140             continue
                do 150 j = 1, 8
                    bdm(i) = bdm(i) + bm(i,j)*depm(j)
150             continue
160         continue
!           ------ DCIS = DCI.VT --------------------------------------
            dcis(1) = dci(1,1)*vt(1) + dci(1,2)*vt(2)
            dcis(2) = dci(2,1)*vt(1) + dci(2,2)*vt(2)
            do 170 i = 1, 3
                edgl(i+8* (ie-1)) = bdm(i)
                edgl(i+3+8* (ie-1)) = bdf(i)
170         continue
!           --- PASSAGE DE LA DISTORSION A LA DEFORMATION DE CIS. ------
            edgl(3+8* (ie-1)) = edgl(3+8* (ie-1))/2.d0
            edgl(6+8* (ie-1)) = edgl(6+8* (ie-1))/2.d0
            edgl(7+8* (ie-1)) = dcis(1)/2.d0
            edgl(8+8* (ie-1)) = dcis(2)/2.d0
180     continue
!
    else
        do 360 ie = 1, ne
!
! ---     COORDONNEES DU POINT D'INTEGRATION COURANT :
            qsi = zr(icoopg-1+ndim*(ie-1)+1)
            eta = zr(icoopg-1+ndim*(ie-1)+2)
!
!           ----- CALCUL DU JACOBIEN SUR LE QUADRANGLE -----------------
            call jquad4(xyzl, qsi, eta, jacob)
!
!           ----- CALCUL DE LA MATRICE BM ------------------------------
            call dxqbm(qsi, eta, jacob(2), bm)
!
! ---     CALCUL DU PRODUIT HF.T2 :
            call dsxhft(df, jacob(2), hft2)
!
! ---     CALCUL DU PRODUIT HMF.T2 :
            call dxhmft(dmf, jacob(2), hmft2)
!
! ---     CALCUL DES MATRICES BCB, BCA ET BCM :
            call dsqcis(qsi, eta, caraq4, hmft2, hft2,&
                        bcm, bcb, bca)
!
!           ------ BC = BCB + BCA.AN -----------------------------------
            do 190 k = 1, 24
                bcn(k,1) = 0.d0
                bfam(k,1) = 0.d0
190         continue
            do 191 k = 1, 16
                btm(k,1) = 0.d0
                bcam(k,1) = 0.d0
191         continue
            do 220 i = 1, 2
                do 210 j = 1, 12
                    do 200 k = 1, 4
                        bcn(i,j) = bcn(i,j) + bca(i,k)*an(k,j)
200                 continue
                    bc(i,j) = bcb(i,j) + bcn(i,j)
210             continue
220         continue
!           ------ VT = BC.DEPF ---------------------------------------
            vt(1) = 0.d0
            vt(2) = 0.d0
            do 240 i = 1, 2
                do 230 j = 1, 12
                    vt(i) = vt(i) + bc(i,j)*depf(j)
230             continue
240         continue
!
            if (excen) then
                do 221 i = 1, 2
                    do 211 j = 1, 8
                        do 201 k = 1, 4
                            bcam(i,j) = bcam(i,j) + bca(i,k)*am(k,j)
201                     continue
                        btm(i,j) = bcm(i,j) + bcam(i,j)
211                 continue
221             continue
                do 241 i = 1, 2
                    do 231 j = 1, 8
                        vt(i) = vt(i) + btm(i,j)*depm(j)
231                 continue
241             continue
            endif
!
!           ----- CALCUL DE LA MATRICE BFB AU POINT QSI ETA -----------
            call dsqbfb(qsi, eta, jacob(2), bfb)
!           ----- CALCUL DE LA MATRICE BFA AU POINT QSI ETA -----------
            call dsqbfa(qsi, eta, jacob(2), caraq4, bfa)
!           ------ BF = BFB + BFA.AN ----------------------------------
            do 250 k = 1, 36
                bfn(k,1) = 0.d0
250         continue
            do 280 i = 1, 3
                do 270 j = 1, 12
                    do 260 k = 1, 4
                        bfn(i,j) = bfn(i,j) + bfa(i,k)*an(k,j)
260                 continue
                    bf(i,j) = bfb(i,j) + bfn(i,j)
270             continue
280         continue
            do 281 i = 1, 3
                do 271 j = 1, 8
                    do 261 k = 1, 4
                        bfam(i,j) = bfam(i,j) + bfa(i,k)*am(k,j)
261                 continue
271             continue
281         continue
            do 290 k = 1, 3
                bdf(k) = 0.d0
                bdf2(k)= 0.0d0
                bdfm(k) = 0.d0
                bdm(k) = 0.d0
                bdm1(k) = 0.d0
                vf(k) = 0.d0
                vm(k) = 0.d0
                vfm(k) = 0.d0
                vmf(k) = 0.d0
                vmc(k) = 0.0d0
                vfc(k) = 0.0d0
290         continue
            vcm(1) = 0.0d0
            vcm(2) = 0.0d0
            vcf(1) = 0.0d0
            vcf(2) = 0.0d0
!           ------ VF = DF.BF.DEPF , VFM = DMF.BM.DEPM ----------------
!           ------ VM = DM.BM.DEPM , VMF = DMF.BF.DEPF ----------------
            do 320 i = 1, 3
                do 300 j = 1, 12
                    bdf(i) = bdf(i) + bf(i,j)*depf(j)
                    bdf2(i) = bdf2(i) +bfb(i,j)*depf(j)
300             continue
                do 310 j = 1, 8
                    bdfm(i) = bdfm(i) + bfam(i,j)*depm(j)
                    bdm(i) = bdm(i) + bm(i,j)*depm(j)
                    bdm1(i) = bdm1(i) + (bm(i,j)+excent*bfam(i,j))* depm(j)
310             continue
320         continue
            do 340 i = 1, 3
                do 330 j = 1, 3
                    vf(i) = vf(i) + df(i,j)*bdf(j)
                    vfm(i) = vfm(i) + dmf(i,j)*bdm(j)
                    vm(i) = vm(i) + dm(i,j)*bdm1(j)
                    vmf(i) = vmf(i) + dmf(i,j)*bdf2(j)
330             continue
340         continue
            if (excen) then
                do 341 i = 1, 3
                    do 331 j = 1, 3
                        vfm(i) = vfm(i) + df(i,j)*bdfm(j)
331                 continue
341             continue
            endif
!
            dcis(1) = dci(1,1)*vt(1) + dci(1,2)*vt(2)
            dcis(2) = dci(2,1)*vt(1) + dci(2,2)*vt(2)
!
            vmc(1) = dmc(1,1)*dcis(1) + dmc(1,2)*dcis(2)
            vmc(2) = dmc(2,1)*dcis(1) + dmc(2,2)*dcis(2)
            vmc(3) = dmc(3,1)*dcis(1) + dmc(3,2)*dcis(2)
!
            vcm(1) = dmc(1,1)*vm(1) + dmc(2,1)*vm(2) + dmc(3,1)*vm(3)
            vcm(2) = dmc(1,2)*vm(1) + dmc(2,2)*vm(2) + dmc(3,2)*vm(3)
!
            vfc(1) = dfc(1,1)*dcis(1) + dfc(1,2)*dcis(2)
            vfc(2) = dfc(2,1)*dcis(1) + dfc(2,2)*dcis(2)
            vfc(3) = dfc(3,1)*dcis(1) + dfc(3,2)*dcis(2)
!
            vcf(1) = dfc(1,1)*vf(1) + dfc(2,1)*vf(2) + dfc(3,1)*vf(3)
            vcf(2) = dfc(1,2)*vf(1) + dfc(2,2)*vf(2) + dfc(3,2)*vf(3)
!
            do 350 i = 1, 3
                edgl(i+8* (ie-1)) = vm(i) + vmf(i) + vmc(i)
                edgl(i+3+8* (ie-1)) = vf(i) + vfm(i) + vfc(i)
350         continue
            edgl(7+8* (ie-1)) = vt(1) + vcm(1) + vcf(1)
            edgl(8+8* (ie-1)) = vt(2) + vcm(2) + vcf(2)
360     continue
    endif
!
end subroutine
