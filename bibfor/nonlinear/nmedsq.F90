subroutine nmedsq(sg, qg, dsdug, d, npg,&
                  typmod, imate, bum, bdu, sign,&
                  vim, option, geom, nno, lgpg,&
                  kpg, def)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
!
#include "asterf_types.h"
#include "asterfort/nmedel.h"
#include "asterfort/r8inir.h"
    integer :: nno, npg, kpg, imate, lgpg
    real(kind=8) :: sg(2), qg(2, *), dsdug(2, 8), d(4, *), sign(*)
    real(kind=8) :: geom(2, nno)
    real(kind=8) :: vim(lgpg, npg)
    real(kind=8) :: bum(6), bdu(6), def(4, nno, 2)
    character(len=8) :: typmod(*)
    character(len=16) :: option
!
!------------------------------------------------------------------
!  CALCUL DU VECTEUR S, DE SA DERIVEE PAR RAPPORT AU DEPL (DSDUG)
!  ET DE LA MATRICE Q AU POINT DE GAUSS COURANT POUR L'ELEMENT A
!  DISCONTINUITE INTERNE AFIN DE RESOUDRE LE SYSTEME NON LINEAIRE
!  SUIVANT   :   S+Q.ALPHA = VECT_SIG(ALPHA)
!------------------------------------------------------------------
!
!  IN : D,NPG,TYPMOD,IMATE,BUM,BDU,SIGN,VIM,OPTION,GEOM,NNO,LGPG
!       KPG,DEF
! OUT : SG , QG , DSDUG
!
!------------------------------------------------------------------
!
    integer :: i, j, k, kl, n
    real(kind=8) :: epsm(6), deps(6), sig(6)
    real(kind=8) :: long, da, dda, alphap(2), alpham(2)
    real(kind=8) :: mtemp(4, 8), dsidep(6, 6)
    real(kind=8) :: xa, xb, ya, yb
    aster_logical :: axi
!
    axi = typmod(1) .eq. 'AXIS'
!
    call r8inir(2, 0.d0, sg, 1)
    call r8inir(4, 0.d0, qg, 1)
    call r8inir(6, 0.d0, epsm, 1)
    call r8inir(16, 0.d0, dsdug, 1)
    call r8inir(32, 0.d0, mtemp, 1)
    call r8inir(36, 0.d0, dsidep, 1)
!
!
    alpham(1) = vim(1,kpg)
    alpham(2) = vim(2,kpg)
!
! SOIT A ET B LES MILIEUX DES COTES [14] ET [23]
! t TANGENT AU COTE [AB]
!
    xa = ( geom(1,1) + geom(1,4) ) / 2
    ya = ( geom(2,1) + geom(2,4) ) / 2
!
    xb = ( geom(1,2) + geom(1,3) ) / 2
    yb = ( geom(2,2) + geom(2,3) ) / 2
!
!     LONGUEUR DE L'ELEMENT : NORME DU COTE [AB]
    long = sqrt( (xa-xb)*(xa-xb) + (ya-yb)*(ya-yb) )
    if (axi) long = long * (xa + xb)/2.d0
!
!
! CALCUL DE SG :
! -------------
!
    alphap(1) = 0.d0
    alphap(2) = 0.d0
    do 80 i = 1, 4
        da = 0.d0
        dda = 0.d0
        do 70 j = 1, 2
            da = da + d(i,j)*alpham(j)
            dda = dda + d(i,j)*(alphap(j)-alpham(j))
 70     continue
        epsm(i) = bum(i) + da
        deps(i) = bdu(i) + dda
 80 end do
!
    call r8inir(6, 0.d0, sig, 1)
    call nmedel(2, typmod, imate, deps, sign,&
                option, sig, dsidep)
!
    do 40 i = 1, 2
        do 50 kl = 1, 4
            sg(i) = sg(i) - d(kl,i)*sig(kl)/long
 50     continue
 40 end do
!
!
! CALCUL DE DSDUG :
! ---------------
! DERIVEE DE SG PAR RAPPORT AU DEPL, NECESSAIRE
! POUR LE CALCUL DE LA MATRICE TANGENTE  (DSDUG = Dt DSIDEP DEF / LONG)
!
!
    do 41 k = 1, 4
!
        do 51 n = 1, nno
            do 52 i = 1, 2
!
                kl=2*(n-1)+i
                do 53 j = 1, 4
                    mtemp(k,kl) = mtemp(k,kl) + dsidep(k,j)*def(j,n,i)
 53             continue
!
 52         continue
 51     continue
!
 41 end do
!
!
    do 42 i = 1, 2
        do 43 j = 1, 8
            do 44 kl = 1, 4
                dsdug(i,j) = dsdug(i,j) - d(kl,i)*mtemp(kl,j)/long
 44         continue
 43     continue
 42 end do
!
!
! CALCUL DE QG :
! -------------
!
!     PREMIERE COLONNE :
!
    alphap(1) = 1.d0
    alphap(2) = 0.d0
    do 81 i = 1, 4
        da = 0.d0
        dda = 0.d0
        do 71 j = 1, 2
            da = da + d(i,j)*alpham(j)
            dda = dda + d(i,j)*(alphap(j)-alpham(j))
 71     continue
        epsm(i) = bum(i) + da
        deps(i) = bdu(i) + dda
 81 end do
!
    call r8inir(6, 0.d0, sig, 1)
    call nmedel(2, typmod, imate, deps, sign,&
                option, sig, dsidep)
!
    do 100 i = 1, 2
        do 110 kl = 1, 4
            qg(i,1) = qg(i,1) - d(kl,i)*sig(kl)/long
110     continue
        qg(i,1) = qg(i,1) - sg(i)
100 end do
!
!
!     DEUXIEME COLONNE :
!
    alphap(1) = 0.d0
    alphap(2) = 1.d0
    do 82 i = 1, 4
        da = 0.d0
        dda = 0.d0
        do 72 j = 1, 2
            da = da + d(i,j)*alpham(j)
            dda = dda + d(i,j)*(alphap(j)-alpham(j))
 72     continue
        epsm(i) = bum(i) + da
        deps(i) = bdu(i) + dda
 82 end do
!
    call r8inir(6, 0.d0, sig, 1)
    call nmedel(2, typmod, imate, deps, sign,&
                option, sig, dsidep)
!
    do 160 i = 1, 2
        do 170 kl = 1, 4
            qg(i,2) = qg(i,2) - d(kl,i)*sig(kl)/long
170     continue
        qg(i,2) = qg(i,2) - sg(i)
160 end do
!
!
! ON IMPOSE SG=0 SI RIGI_MECA_TANG, QG LUI N'EST PAS
! NUL POUR RIGI_MECA_TANG CAR INDEP DE SIG
!
    if (option .eq. 'RIGI_MECA_TANG') then
        call r8inir(2, 0.d0, sg, 1)
    endif
!
end subroutine
