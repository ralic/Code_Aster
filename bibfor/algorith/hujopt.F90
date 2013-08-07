subroutine hujopt(mod, angmas, imat, nmat, mater,&
                  nvi, vinf, nr, drdy, sigf,&
                  dsde, iret)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: alexandre.foucault at edf.fr
!     ----------------------------------------------------------------
!     CALCUL DU JACOBIEN DU SYSTEME NL A RESOUDRE = DRDY(DY)
!     POUR LE MODELE HUJEUX
!     IN  MOD    :  TYPE DE MODELISATIONS
!         ANGMAS :  ANGLE NAUTIQUE (AFFE_CARA_ELEM)
!         NVI    :  NOMBRE DE VARIABLES INTERNES
!         NMAT   :  DIMENSION TABLEAU DES DONNEES MATERIAU
!         MATER  :  COEFFICIENTS MATERIAU
!         VINF   :  VARIABLES INTERNES A T+DT
!         NR     :  DIMENSION MATRICE JACOBIENNE
!         DRDY   :  MATRICE JACOBIENNE
!     OUT DSDE   :  MATRICE TANGENTE EN VITESSE
!     ----------------------------------------------------------------
! aslint: disable=W1306
    implicit none
!     ----------------------------------------------------------------
#include "asterc/r8prem.h"
#include "asterc/r8vide.h"
#include "asterfort/hujori.h"
#include "asterfort/hujtel.h"
#include "asterfort/hujtid.h"
#include "asterfort/lceqma.h"
#include "asterfort/lcinma.h"
#include "asterfort/lcprmm.h"
#include "asterfort/mgauss.h"
#include "asterfort/promat.h"
#include "asterfort/r8inir.h"
#include "asterfort/trace.h"
#include "asterfort/u2mess.h"
    integer :: nmat, nr, nvi, iret, imat
    real(kind=8) :: drdy(nr, nr), dsde(6, 6), mater(nmat, 2), vinf(nvi)
    real(kind=8) :: angmas(3), sigf(6)
    character(len=8) :: mod
!
    integer :: nbmeca, norm, i, j, ndt, ndi, nz
    real(kind=8) :: hook(6, 6), un, zero, deux, trois, matert(22, 2)
    real(kind=8) :: e, nu, al, demu, la, e1, e2, e3, nu12, nu13, nu23, g1, g2
    real(kind=8) :: g3
    real(kind=8) :: nu21, nu31, nu32, denom, i1f, hooknl(6, 6), pref, ne
    real(kind=8) :: coef0
    real(kind=8) :: y0(6, 6), y1(6, 9), y2(9, 6), y3(9, 9)
    real(kind=8) :: y4(6, 6), y5(6, 6), det, maxi, mini
    real(kind=8) :: dsdeb(6, 6), bid16(6), bid66(6, 6)
    real(kind=8) :: ccond
    character(len=4) :: cargau
    logical :: reorie
!
    parameter (ndt   = 6   )
    parameter (ndi   = 3   )
    parameter (zero  = 0.d0)
    parameter (un    = 1.d0)
    parameter (deux  = 2.d0)
    parameter (trois = 3.d0)
! === =================================================================
! --- RECHERCHE DU MAXIMUM DE DRDY
! === =================================================================
!
    norm=0
    if (norm .eq. 0) goto 5
!
    maxi = 0.d0
    do 1 i = 1, nr
        do 2 j = 1, nr
            if(abs(drdy(i,j)).gt.maxi)maxi = abs(drdy(i,j))
 2      end do
 1  end do
!
! === =================================================================
! --- DIMENSIONNEMENT A R8PREM
! === =================================================================
    mini = r8prem()*maxi
    do 3 i = 1, nr
        do 4 j = 1, nr
            if(abs(drdy(i,j)).lt.mini)drdy(i,j) = 0.d0
 4      end do
 3  end do
!
 5  continue
!
! === =================================================================
! --- SEPARATION DES TERMES DU JACOBIEN
! === =================================================================
! --- DETERMINATION DU NOMBRE DE MECANISMES ACTIFS - NBMECA
    nbmeca = 0
    do 10 i = 1, 8
        if (vinf(23+i) .eq. un) nbmeca = nbmeca + 1
10  continue
!
    nz = 1+2*nbmeca
!
! === ==============================================================
! --- REDIMENSIONNEMENT DU JACOBIEN
! === ==============================================================
    ccond = mater(1,1)
    pref = mater(8,2)
!
! --- DLEDR
    do 20 i = 1, nbmeca
        do 30 j = 1, ndt
            drdy(j,ndt+1+i) = drdy(j,ndt+1+i)*abs(pref)
30      continue
20  end do
!
! --- DLEDLA
    do 40 i = 1, nbmeca
        do 50 j = 1, ndt
            drdy(j,ndt+1+nbmeca+i) = drdy(j,ndt+1+nbmeca+i)*ccond
50      continue
40  end do
!
! --- DLRDLA
    do 60 i = 1, nbmeca
        drdy(ndt+1+i,ndt+1+nbmeca+i) = drdy(ndt+1+i,ndt+1+nbmeca+i) *ccond/abs(pref)
60  end do
!
! --- DLRDEVP
    do 70 i = 1, nbmeca
        drdy(ndt+1+i,ndt+1) = drdy(ndt+1+i,ndt+1)*ccond/abs(pref)
70  end do
!
! --- DLEVPDS
    do 80 i = 1, ndt
        drdy(ndt+1,i) = drdy(ndt+1,i)*ccond
80  end do
!
! --- DLEVPDR
    do 90 i = 1, nbmeca
        drdy(ndt+1,ndt+1+i) = drdy(ndt+1,ndt+1+i)/ccond*abs(pref)
90  end do
!
! --- DLFDR
    do 100 i = 1, nbmeca
        drdy(ndt+1+nbmeca+i,ndt+1+i) = drdy(ndt+1+nbmeca+i,ndt+1+i) *abs(pref)
100  end do
!
! --- DLFDEVP
    do 110 i = 1, nbmeca
        drdy(ndt+1+nbmeca+i,ndt+1) = drdy(ndt+1+nbmeca+i,ndt+1) *ccond
110  end do
!
! ----------------------------------------------
! --- CONSTRUCTION DE L'OPERATEUR CONSISTANT ---
! ----------------------------------------------
    call lcinma(zero, y0)
    do 310 i = 1, 9
        do 320 j = 1, ndt
            y1(j,i) = zero
            y2(i,j) = zero
320      continue
310  end do
!
    do 330 i = 1, 9
        do 340 j = 1, 9
            y3(i,j) = zero
340      continue
330  end do
!
    do 120 i = 1, ndt
        do 130 j = 1, ndt
            y0(i,j) = drdy(i,j)
130      continue
120  end do
    do 140 i = 1, ndt
        do 150 j = 1, nz
            y1(i,j) = drdy(i,j+ndt)
150      continue
140  continue
    do 160 i = 1, nz
        do 170 j = 1, ndt
            y2(i,j) = drdy(i+ndt,j)
170      continue
160  continue
    do 180 i = 1, nz
        do 190 j = 1, nz
            y3(i,j) = drdy(i+ndt,j+ndt)
190      continue
180  continue
!
! === =================================================================
! --- CONSTRUCTION TENSEUR RIGIDITE ELASTIQUE A T+DT
! === =================================================================
! ====================================================================
! --- OPERATEURS ELASTICITE LINEAIRES---------------------------------
! ====================================================================
    call lcinma(zero, hook)
!
    if ((mod(1:2) .eq. '3D') .or. (mod(1:6) .eq. 'D_PLAN')) then
!
        if (mater(17,1) .eq. un) then
!
            e = mater(1,1)
            nu = mater(2,1)
            al = e*(un-nu) /(un+nu) /(un-deux*nu)
            demu = e /(un+nu)
            la = e*nu/(un+nu)/(un-deux*nu)
!
            do 200 i = 1, ndi
                do 200 j = 1, ndi
                    if (i .eq. j) hook(i,j) = al
                    if (i .ne. j) hook(i,j) = la
200              continue
            do 210 i = ndi+1, ndt
                hook(i,i) = demu
210          continue
!
        else if (mater(17,1).eq.deux) then
!
            e1 = mater(1,1)
            e2 = mater(2,1)
            e3 = mater(3,1)
            nu12 = mater(4,1)
            nu13 = mater(5,1)
            nu23 = mater(6,1)
            g1 = mater(7,1)
            g2 = mater(8,1)
            g3 = mater(9,1)
            nu21 = mater(13,1)
            nu31 = mater(14,1)
            nu32 = mater(15,1)
            denom= mater(16,1)
!
            hook(1,1) = (un - nu23*nu32)*e1/denom
            hook(1,2) = (nu21 + nu31*nu23)*e1/denom
            hook(1,3) = (nu31 + nu21*nu32)*e1/denom
            hook(2,2) = (un - nu13*nu31)*e2/denom
            hook(2,3) = (nu32 + nu31*nu12)*e2/denom
            hook(3,3) = (un - nu21*nu12)*e3/denom
            hook(2,1) = hook(1,2)
            hook(3,1) = hook(1,3)
            hook(3,2) = hook(2,3)
            hook(4,4) = g1
            hook(5,5) = g2
            hook(6,6) = g3
!
        else
            call u2mess('F', 'COMPOR1_38')
        endif
    else if (mod(1:6) .eq. 'C_PLAN' .or. mod(1:2) .eq. '1D') then
        call u2mess('F', 'COMPOR1_4')
    endif
! ====================================================================
! --- OPERATEUR ELASTICITE NON LINEAIRE ------------------------------
! ====================================================================
    i1f = trace(ndi,sigf)/trois
    ne = mater(1,2)
    if ((i1f/pref) .lt. 1.d-6) i1f = 1.d-6*pref
!
    coef0 = (i1f/pref) ** ne
    do 220 i = 1, ndt
        do 220 j = 1, ndt
            hooknl(i,j) = coef0*hook(i,j)
220      continue
!
!     CHOIX DES PARAMETRES DE LANCEMENT DE MGAUSS
!     METHODE 'S' : SURE
    cargau = 'NCSP'
!     METHODE 'W' : RATEAU
!      CARGAU = 'NCWP'
! === =================================================================
! --- CONSTRUCTION TENSEUR CONSTITUTIF TANGENT DSDE
! === =================================================================
!     Y2=INVERSE(Y3)*Y2
    call mgauss(cargau, y3, y2, 9, nz,&
                ndt, det, iret)
    if (iret .gt. 1) then
        call lceqma(hook, dsde)
        goto 9999
    endif
!
! --- PRODUIT DU TERME Y1 * (Y3)^-1 * Y2 = Y4
    call promat(y1, ndt, ndt, 9, y2,&
                9, 9, ndt, y4)
!
! --- DIFFERENCE DE MATRICE (DR1DY1 - Y4) = Y5
    do 230 i = 1, ndt
        do 240 j = 1, ndt
            y5(i,j)=y0(i,j)-y4(i,j)
240      continue
230  end do
!
! --- INVERSION DU TERME Y5
    call r8inir(ndt*ndt, 0.d0, dsdeb, 1)
    do 250 i = 1, ndt
        dsdeb(i,i) = un
250  end do
    call mgauss(cargau, y5, dsdeb, ndt, ndt,&
                ndt, det, iret)
!
    if (iret .gt. 1) then
        call lceqma(hook, dsde)
    else
        call lcinma(zero, dsde)
        call lcprmm(dsdeb, hooknl, dsde)
    endif
!
9999  continue
    if (angmas(1) .eq. r8vide()) call u2mess('F', 'ALGORITH8_20')
    reorie =(angmas(1).ne.zero) .or. (angmas(2).ne.zero)&
     &         .or. (angmas(3).ne.zero)
    if (iret .ne. 0) then
        iret = 0
        call hujori('LOCAL', 1, reorie, angmas, sigf,&
                    bid66)
        call hujtid(mod, imat, sigf, vinf, dsde,&
                    iret)
        call hujori('GLOBA', 1, reorie, angmas, sigf,&
                    bid66)
        if (iret .ne. 0) then
            iret = 0
            do 260 i = 1, 22
                matert(i,1) = mater(i,1)
                matert(i,2) = mater(i,2)
260          continue
            call hujtel(mod, matert, sigf, dsde)
        endif
    endif
    call hujori('GLOBA', 2, reorie, angmas, bid16,&
                dsde)
!
end subroutine
