subroutine dstedg(xyzl, option, pgl, depl, edgl)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/dstbfa.h"
#include "asterfort/dstbfb.h"
#include "asterfort/dstcis.h"
#include "asterfort/dsxhft.h"
#include "asterfort/dxmate.h"
#include "asterfort/dxtbm.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/gtria3.h"
    real(kind=8) :: xyzl(3, *), pgl(3, *), depl(*), edgl(*)
    character(len=16) :: option
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
!     ------------------------------------------------------------------
!     EFFORTS ET DEFORMATIONS GENERALISES DE L'ELEMENT DE PLAQUE DST
!     ------------------------------------------------------------------
!     IN  XYZL   : COORDONNEES LOCALES DES TROIS NOEUDS
!     IN  OPTION : NOM DE L'OPTION DE CALCUL
!     IN  PGL    : MATRICE DE PASSAGE GLOBAL - LOCAL
!     IN  DEPL   : DEPLACEMENTS
!     OUT EDGL   : EFFORTS OU DEFORMATIONS GENERALISES AUX NOEUDS DANS
!                  LE REPERE INTRINSEQUE A L'ELEMENT
    integer :: ndim, nno, nnos, npg, ipoids, icoopg, ivf, idfdx, idfd2, jgano
    integer :: multic, ne, k, j, i, ie
    real(kind=8) :: depf(9), depm(6)
    real(kind=8) :: df(3, 3), dm(3, 3), dmf(3, 3), dc(2, 2), dci(2, 2)
    real(kind=8) :: dmc(3, 2)
    real(kind=8) :: bfb(3, 9), bfa(3, 3), bfn(3, 9), bf(3, 9), dfc(3, 2)
    real(kind=8) :: bca(2, 3), bcn(2, 9), hft2(2, 6), an(3, 9)
    real(kind=8) :: bm(3, 6), bdm(3), bdf(3), dcis(2), vf(3), vm(3), vt(2)
    real(kind=8) :: vfm(3), vmf(3), vmc(3), vfc(3), vcm(2), vcf(2)
    real(kind=8) :: qsi, eta, carat3(21), t2iu(4), t2ui(4), t1ve(9)
    aster_logical :: coupmf
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
!     ----- CALCUL DES MATRICES DE RIGIDITE DU MATERIAU EN FLEXION,
!           MEMBRANE ET CISAILLEMENT INVERSEES -------------------------
!
!     ----- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE TRIANGLE ----------
    call gtria3(xyzl, carat3)
!     ----- CARACTERISTIQUES DES MATERIAUX --------
    call dxmate(fami, df, dm, dmf, dc,&
                dci, dmc, dfc, nno, pgl,&
                multic, coupmf, t2iu, t2ui, t1ve)
!     ----- COMPOSANTES DEPLACEMENT MEMBRANE ET FLEXION ----------------
    do 20 j = 1, nno
        do 10 i = 1, 2
            depm(i+2* (j-1)) = depl(i+6* (j-1))
 10     continue
        depf(1+3* (j-1)) = depl(1+2+6* (j-1))
        depf(2+3* (j-1)) = depl(3+2+6* (j-1))
        depf(3+3* (j-1)) = -depl(2+2+6* (j-1))
 20 end do
!     ------ CALCUL DE LA MATRICE BM -----------------------------------
    call dxtbm(carat3(9), bm)
    do 30 k = 1, 3
        bdm(k) = 0.d0
 30 end do
    do 50 i = 1, 3
        do 40 j = 1, 6
            bdm(i) = bdm(i) + bm(i,j)*depm(j)
 40     continue
 50 end do
!     ------- CALCUL DU PRODUIT HF.T2 ----------------------------------
    call dsxhft(df, carat3(9), hft2)
!     ------- CALCUL DES MATRICES BCA ET AN ----------------------------
    call dstcis(dci, carat3, hft2, bca, an)
!     ------ VT = BCA.AN.DEPF ------------------------------------------
    do 60 k = 1, 18
        bcn(k,1) = 0.d0
 60 end do
    vt(1) = 0.d0
    vt(2) = 0.d0
    do 90 i = 1, 2
        do 80 j = 1, 9
            do 70 k = 1, 3
                bcn(i,j) = bcn(i,j) + bca(i,k)*an(k,j)
 70         continue
            vt(i) = vt(i) + bcn(i,j)*depf(j)
 80     continue
 90 end do
!     ------- CALCUL DE LA MATRICE BFB ---------------------------------
    call dstbfb(carat3(9), bfb)
!
    if (option(1:4) .eq. 'DEGE') then
        do 180 ie = 1, ne
! ---     COORDONNEES DU POINT D'INTEGRATION COURANT :
!         ------------------------------------------
            qsi = zr(icoopg-1+ndim*(ie-1)+1)
            eta = zr(icoopg-1+ndim*(ie-1)+2)
!           ----- CALCUL DE LA MATRICE BFA AU POINT QSI ETA -----------
            call dstbfa(qsi, eta, carat3, bfa)
!           ------ BF = BFB + BFA.AN -----------------------------------
            do 100 k = 1, 27
                bfn(k,1) = 0.d0
100         continue
            do 130 i = 1, 3
                do 120 j = 1, 9
                    do 110 k = 1, 3
                        bfn(i,j) = bfn(i,j) + bfa(i,k)*an(k,j)
110                 continue
                    bf(i,j) = bfb(i,j) + bfn(i,j)
120             continue
130         continue
            do 140 k = 1, 3
                bdf(k) = 0.d0
140         continue
            do 160 i = 1, 3
                do 150 j = 1, 9
                    bdf(i) = bdf(i) + bf(i,j)*depf(j)
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
    else
        do 190 k = 1, 3
            vm(k) = 0.d0
            vfm(k) = 0.d0
            vmc(k) = 0.0d0
190     continue
        vcm(1) = 0.0d0
        vcm(2) = 0.0d0
        do 210 i = 1, 3
            do 200 j = 1, 3
                vm(i) = vm(i) + dm(i,j)*bdm(j)
                vfm(i) = vfm(i) + dmf(i,j)*bdm(j)
200         continue
210     continue
        do 320 ie = 1, ne
! ---     COORDONNEES DU POINT D'INTEGRATION COURANT :
!         ------------------------------------------
            qsi = zr(icoopg-1+ndim*(ie-1)+1)
            eta = zr(icoopg-1+ndim*(ie-1)+2)
!           ----- CALCUL DE LA MATRICE BFA AU POINT QSI ETA ------------
            call dstbfa(qsi, eta, carat3, bfa)
!           ------ BF = BFB + BFA.AN -----------------------------------
            do 220 k = 1, 27
                bfn(k,1) = 0.d0
220         continue
            do 250 i = 1, 3
                do 240 j = 1, 9
                    do 230 k = 1, 3
                        bfn(i,j) = bfn(i,j) + bfa(i,k)*an(k,j)
230                 continue
                    bf(i,j) = bfb(i,j) + bfn(i,j)
240             continue
250         continue
            do 260 k = 1, 3
                bdf(k) = 0.d0
                vf(k) = 0.d0
                vmf(k) = 0.d0
                vfc(k) = 0.0d0
260         continue
            vcf(1) = 0.0d0
            vcf(2) = 0.0d0
!           ------ VF = DF.BF.DEPF , VMF = DMF.BF.DEPF -------------
            do 280 i = 1, 3
                do 270 j = 1, 9
                    bdf(i) = bdf(i) + bf(i,j)*depf(j)
270             continue
280         continue
            do 300 i = 1, 3
                do 290 j = 1, 3
                    vf(i) = vf(i) + df(i,j)*bdf(j)
                    vmf(i) = vmf(i) + dmf(i,j)*bdf(j)
290             continue
300         continue
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
            do 310 i = 1, 3
                edgl(i+8* (ie-1)) = vm(i) + vmf(i) + vmc(i)
                edgl(i+3+8* (ie-1)) = vf(i) + vfm(i) + vfc(i)
310         continue
            edgl(7+8* (ie-1)) = vt(1) + vcm(1) + vcf(1)
            edgl(8+8* (ie-1)) = vt(2) + vcm(2) + vcf(2)
320     continue
    endif
!
end subroutine
