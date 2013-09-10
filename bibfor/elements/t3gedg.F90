subroutine t3gedg(xyzl, option, pgl, depl, edgl)
    implicit  none
#include "jeveux.h"
#include "asterfort/dstbfb.h"
#include "asterfort/dxmate.h"
#include "asterfort/dxtbm.h"
#include "asterfort/elref5.h"
#include "asterfort/gtria3.h"
#include "asterfort/t3gbc.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!     EFFORTS ET DEFORMATIONS GENERALISES DE L'ELEMENT DE PLAQUE T3GAMMA
!     OPTION DEGE_ELNO
!     ------------------------------------------------------------------
!     IN  XYZL   : COORDONNEES LOCALES DES QUATRE NOEUDS
!     IN  OPTION : NOM DE L'OPTION DE CALCUL
!     IN  PGL    : MATRICE DE PASSAGE GLOBAL - LOCAL
!     IN  DEPL   : DEPLACEMENTS
!     OUT EDGL   : EFFORTS OU DEFORMATIONS GENERALISES AUX NOEUDS DANS
!                  LE REPERE INTRINSEQUE A L'ELEMENT
    integer :: ndim, nno, nnos, npg, ipoids, icoopg, ivf, idfdx, idfd2, jgano
    integer :: multic, ne, k, j, i, ie
    real(kind=8) :: depf(9), depm(6)
    real(kind=8) :: df(3, 3), dm(3, 3), dmf(3, 3), dmc(3, 2), dfc(3, 2)
    real(kind=8) :: dci(2, 2), dc(2, 2)
    real(kind=8) :: bf(3, 9), bm(3, 6), bc(2, 9)
    real(kind=8) :: bdm(3), bdf(3), bcdf(2), dcis(2)
    real(kind=8) :: vf(3), vm(3), vt(2)
    real(kind=8) :: vfm(3), vmf(3), vmc(3), vfc(3), carat3(21)
    real(kind=8) :: t2iu(4), t2ui(4), t1ve(9)
    real(kind=8) :: qsi, eta
    logical :: coupmf
    character(len=4) :: fami
!     ------------------------------------------------------------------
!
    fami = 'RIGI'
    if (option(6:9) .eq. 'ELGA') then
        call elref5(' ', 'RIGI', ndim, nno, nnos,&
                    npg, ipoids, icoopg, ivf, idfdx,&
                    idfd2, jgano)
        ne = npg
        fami='RIGI'
    else if (option(6:9).eq.'ELNO') then
        call elref5(' ', 'NOEU', ndim, nno, nnos,&
                    npg, ipoids, icoopg, ivf, idfdx,&
                    idfd2, jgano)
        ne = nno
        fami='NOEU'
    endif
!     ----- CALCUL DES MATRICES DE RIGIDITE DU MATERIAU EN FLEXION,
!           MEMBRANE ET CISAILLEMENT INVERSEES -------------------------
!
!     ----- CARACTERISTIQUES DES MATERIAUX --------
    call dxmate(fami, df, dm, dmf, dc,&
                dci, dmc, dfc, nno, pgl,&
                multic, coupmf, t2iu, t2ui, t1ve)
!     ----- COMPOSANTES DEPLACEMENT MEMBRANE ET FLEXION ----------------
    do 20 j = 1, nno
        do 10 i = 1, 2
            depm(i+2* (j-1)) = depl(i+6* (j-1))
10      continue
        depf(1+3* (j-1)) = depl(1+2+6* (j-1))
        depf(2+3* (j-1)) = depl(3+2+6* (j-1))
        depf(3+3* (j-1)) = -depl(2+2+6* (j-1))
20  end do
!
!     ----- CALCUL DU JACOBIEN SUR LE TRIANGLE -----------------
    call gtria3(xyzl, carat3)
!     ------ CALCUL DE LA MATRICE BM -----------------------------------
    call dxtbm(carat3(9), bm)
!
!     ------- CALCUL DE LA MATRICE BFB -------------------------------
    call dstbfb(carat3(9), bf)
!
!     ---- CALCUL DE LA MATRICE BC ----------------------------------
!
    if (option(1:4) .eq. 'DEGE') then
        qsi = 1.d0/3.d0
        eta = qsi
        call t3gbc(xyzl, qsi, eta, bc)
        do 90 ie = 1, ne
            bcdf(1) = 0.d0
            bcdf(2) = 0.d0
            do 30 j = 1, 9
                bcdf(1) = bcdf(1) + bc(1,j)*depf(j)
                bcdf(2) = bcdf(2) + bc(2,j)*depf(j)
30          continue
            do 40 k = 1, 3
                bdf(k) = 0.d0
                bdm(k) = 0.d0
40          continue
            do 70 i = 1, 3
                do 50 j = 1, 9
                    bdf(i) = bdf(i) + bf(i,j)*depf(j)
50              continue
                do 60 j = 1, 6
                    bdm(i) = bdm(i) + bm(i,j)*depm(j)
60              continue
70          continue
            do 80 i = 1, 3
                edgl(i+8* (ie-1)) = bdm(i)
                edgl(i+3+8* (ie-1)) = bdf(i)
80          continue
            edgl(7+8* (ie-1)) = bcdf(1)
            edgl(8+8* (ie-1)) = bcdf(2)
!           --- PASSAGE DE LA DISTORSION A LA DEFORMATION DE CIS. ------
            edgl(3+8* (ie-1)) = edgl(3+8* (ie-1))/2.d0
            edgl(6+8* (ie-1)) = edgl(6+8* (ie-1))/2.d0
            edgl(7+8* (ie-1)) = bcdf(1)/2.d0
            edgl(8+8* (ie-1)) = bcdf(2)/2.d0
90      continue
!
    else
        do 180 ie = 1, ne
!           ------ VT = DC.BC.DEPF -------------------------------------
!     ---- CALCUL DE LA MATRICE BC ----------------------------------
            qsi = 1.d0/3.d0
            eta = qsi
!
            call t3gbc(xyzl, qsi, eta, bc)
!
            vt(1) = 0.d0
            vt(2) = 0.d0
            bcdf(1) = 0.d0
            bcdf(2) = 0.d0
            do 100 j = 1, 9
                bcdf(1) = bcdf(1) + bc(1,j)*depf(j)
                bcdf(2) = bcdf(2) + bc(2,j)*depf(j)
100          continue
            vt(1) = dc(1,1)*bcdf(1) + dc(1,2)*bcdf(2)
            vt(2) = dc(2,1)*bcdf(1) + dc(2,2)*bcdf(2)
            do 110 k = 1, 3
                bdf(k) = 0.d0
                bdm(k) = 0.d0
                vf(k) = 0.d0
                vm(k) = 0.d0
                vfm(k) = 0.d0
                vmf(k) = 0.d0
                vmc(k) = 0.0d0
                vfc(k) = 0.0d0
110          continue
!           ------ VF = DF.BF.DEPF , VFM = DMF.BM.DEPM ----------------
!           ------ VM = DM.BM.DEPM , VMF = DMF.BF.DEPF ----------------
            do 140 i = 1, 3
                do 120 j = 1, 9
                    bdf(i) = bdf(i) + bf(i,j)*depf(j)
120              continue
                do 130 j = 1, 6
                    bdm(i) = bdm(i) + bm(i,j)*depm(j)
130              continue
140          continue
            do 160 i = 1, 3
                do 150 j = 1, 3
                    vf(i) = vf(i) + df(i,j)*bdf(j)
                    vfm(i) = vfm(i) + dmf(i,j)*bdm(j)
                    vm(i) = vm(i) + dm(i,j)*bdm(j)
                    vmf(i) = vmf(i) + dmf(i,j)*bdf(j)
150              continue
160          continue
!
            dcis(1) = dci(1,1)*vt(1) + dci(1,2)*vt(2)
            dcis(2) = dci(2,1)*vt(1) + dci(2,2)*vt(2)
!
            vmc(1) = dmc(1,1)*dcis(1) + dmc(1,2)*dcis(2)
            vmc(2) = dmc(2,1)*dcis(1) + dmc(2,2)*dcis(2)
            vmc(3) = dmc(3,1)*dcis(1) + dmc(3,2)*dcis(2)
!
            vfc(1) = dfc(1,1)*dcis(1) + dfc(1,2)*dcis(2)
            vfc(2) = dfc(2,1)*dcis(1) + dfc(2,2)*dcis(2)
            vfc(3) = dfc(3,1)*dcis(1) + dfc(3,2)*dcis(2)
!
!
            do 170 i = 1, 3
                edgl(i+8* (ie-1)) = vm(i) + vmf(i) + vmc(i)
                edgl(i+3+8* (ie-1)) = vf(i) + vfm(i) + vfc(i)
170          continue
            edgl(7+8* (ie-1)) = vt(1)
            edgl(8+8* (ie-1)) = vt(2)
180      continue
    endif
end subroutine
