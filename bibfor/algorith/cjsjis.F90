subroutine cjsjis(mod, mater, deps, yd, yf,&
                  r, drdy)
    implicit none
!       ================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     INTEGRATION PLASTIQUE (MECANISME ISOTROPE SEUL) DE LA LOI CJS
!
!     RESOLUTION PAR METHODE DE NEWTON       DRDY(DYI) DDYI = - R(DYI)
!
!     CALCUL DU SECOND MEMBRE : - R(DYI)
!     CALCUL DU JACOBIEN      : DRDY(DYI)
!                               Y =  ( SIG    ,  VIN    ,  LAMBI   )
!                               R = -( LE     ,  LQ     ,  FI      )
!                           DRDY  =  ( DLEDS  ,  DLEDQ  ,  DLEDL   )
!                                    ( DLQDS  ,  DLQDQ  ,  DLQDL   )
!                                    ( DFIDS  ,  DFIDQ  ,  DFIDL   )
!     ------------------------------------------------------------------
!     IN   MOD      :  MODELISATION
!          MATER    :  COEFFICIENTS MATERIAU A T+DT
!          DEPS     :  INCREMENT DE DEFORMATION
!          YD       :  VARIABLES A T = (SIGD, VIND, LAMBID)
!          YF       :  VARIABLES A T+DT = (SIGF, VINF, LAMBIF)
!     OUT  R        :  SECOND MEMBRE
!          DRDY     :  JACOBIEN
!     ------------------------------------------------------------------
!
    include 'asterfort/lcicma.h'
    include 'asterfort/lcinma.h'
    include 'asterfort/lcinve.h'
    include 'asterfort/lcprmv.h'
    include 'asterfort/u2mess.h'
    integer :: ndt, ndi, nmod
    parameter     (nmod = 8 )
!
    real(kind=8) :: deps(6)
    real(kind=8) :: yd(nmod), yf(nmod), r(nmod), drdy(nmod, nmod)
    real(kind=8) :: mater(14, 2), n, kop, pa
    real(kind=8) :: hooknl(6, 6), hook(6, 6)
    real(kind=8) :: e, nu, al, la, mu, i1f
    real(kind=8) :: dlambi, coef1, coef2
    real(kind=8) :: le(6), lq, fi
    real(kind=8) :: dleds(6, 6), dledq(6), dledl(6)
    real(kind=8) :: dlqds(6), dlqdq, dlqdl
    real(kind=8) :: dfids(6), dfidq, dfidl
    real(kind=8) :: dsignl(6), dsigl(6), depse(6), qinit
    integer :: i, j
!
    real(kind=8) :: zero, un, d12, deux, trois, kron(6), iden6(6, 6)
!
    parameter     ( d12  = .5d0   )
    parameter     ( un   = 1.d0   )
    parameter     ( zero = 0.d0   )
    parameter     ( deux = 2.d0   )
    parameter     ( trois= 3.d0   )
!
!
    character(len=8) :: mod
!
    common /tdim/   ndt, ndi
!
    data    iden6  /un     , zero  , zero  , zero  ,zero  ,zero,&
     &                   zero   , un    , zero  , zero  ,zero  ,zero,&
     &                   zero   , zero  , un    , zero  ,zero  ,zero,&
     &                   zero   , zero  , zero  , un    ,zero  ,zero,&
     &                   zero   , zero  , zero  , zero  ,un    ,zero,&
     &                   zero   , zero  , zero  , zero  ,zero  ,un/
!
    data          kron /un , un , un , zero ,zero ,zero/
!
!
!-----------------------------------------------------------------------
!->     PROPRIETES CJS MATERIAU
!------------------------------
!
    n = mater(3,2)
    kop = mater(4,2)
    pa = mater(12,2)
    qinit = mater(13,2)
!
!-----------------------------------------------------------------------
!->     OPERATEURS DE RIGIDITE
!-----------------------------------------------------------------------
!
!
!- OPERATEUR LINEAIRE
!++++++++++++++++++++
!
    call lcinma(zero, hook)
!
    e = mater(1,1)
    nu = mater(2,1)
    al = e * (un-nu) / (un+nu) / (un-deux*nu)
    la = nu * e / (un+nu) / (un-deux*nu)
    mu = e * d12 / (un+nu)
!
!
! - 3D/DP/AX
    if (mod(1:2) .eq. '3D' .or. mod(1:6) .eq. 'D_PLAN' .or. mod(1:4) .eq. 'AXIS') then
        do 10 i = 1, ndi
            do 10 j = 1, ndi
                if (i .eq. j) hook(i,j) = al
                if (i .ne. j) hook(i,j) = la
10          continue
        do 15 i = ndi+1, ndt
            do 15 j = ndi+1, ndt
                if (i .eq. j) hook(i,j) = deux* mu
15          continue
!
! - CP/1D
    else if (mod(1:6) .eq. 'C_PLAN' .or. mod(1:2) .eq. '1D') then
        call u2mess('F', 'ALGORITH2_15')
    endif
!
!
!- OPERATEUR NON LINEAIRE
!++++++++++++++++++++++++
!
    i1f = zero
    do 20 i = 1, ndi
        i1f = i1f + yf(i)
20  continue
    if ((i1f +qinit) .eq. 0.d0) then
        i1f = -qinit+1.d-12 * pa
    endif
!
    coef1 = ((i1f +qinit)/trois/pa)**n
!
    do 25 i = 1, ndt
        do 25 j = 1, ndt
            hooknl(i,j) = coef1*hook(i,j)
25      continue
!
!
!
!
!
!
!--------------------------------------------------------
!->     LOI D ETAT : LE
!->  ET DERIVEE DE LA LOI D ETAT : DLEDS, DLEDQ, DLEDL
!--------------------------------------------------------
!
!- LOI D ETAT
!++++++++++++
!
    dlambi = yf(ndt+2) - yd(ndt+2)
!
    do 40 i = 1, ndt
        depse(i) = deps(i) + dlambi*kron(i)/trois
40  continue
!
    call lcprmv(hooknl, depse, dsignl)
!
    do 45 i = 1, ndt
        le(i) = yf(i) - yd(i) - dsignl(i)
45  continue
!
!
!- DERIVEE DE LA LOI D ETAT
!++++++++++++++++++++++++++
!
    coef2 = n/trois/pa * (trois*pa/(i1f +qinit))**(un-n)
    call lcprmv(hook, depse, dsigl)
    call lcinma(zero, dleds)
!
    do 50 i = 1, ndt
!
        do 60 j = 1, ndt
            dleds(i,j) = iden6(i,j) - coef2 * dsigl(i) * kron(j)
60      continue
!
        dledq(i) = zero
!
        dledl(i) = zero
        do 70 j = 1, ndt
            dledl(i) = dledl(i) - hooknl(i,j)*kron(j)/trois
70      continue
!
50  continue
!
!
!
!
!
!-----------------------------------------------------------------------
!->     LOI D ECROUISSAGE DE QISO : LQ
!->  ET DERIVEE DE LA LOI D ECROUISSAGE DE QISO : DLQDS, DLQDQ, DLQDL
!-----------------------------------------------------------------------
!
!
!- LOI D ECROUISSAGE
!+++++++++++++++++++
    lq = yf(ndt+1) - yd(ndt+1) + dlambi * kop * (yf(ndt+1)/pa)**n
!
!
!- DERIVEE DE LA LOI D ECROUISSAGE
!+++++++++++++++++++++++++++++++++
!
    call lcinve(zero, dlqds)
    dlqdq = un + dlambi * kop * n / pa * (pa/yf(ndt+1))**(un-n)
    dlqdl = kop * (yf(ndt+1)/pa)**n
!------------------------------------------------------------------
!->     SEUIL ISOTROPE : FI
!->  ET DERIVEE DE LA FONCTION SEUIL ISOTROPE : DFIDS, DFIDQ, DFIDL
!------------------------------------------------------------------
!
!
!- SEUIL ISOTROPE
!++++++++++++++++
!
    fi = -(i1f +qinit)/trois + yf(ndt+1)
!
!
!- DERIVEE DU SEUIL ISOTROPE
!+++++++++++++++++++++++++++
!
    do 80 i = 1, ndt
        dfids(i) = -kron(i)/trois
80  continue
!
    dfidq = un
    dfidl = zero
!
!
!
!
!-------------------------------------------
!->     ASSEMBLAGE DE R = - ( LE, LQ, FI )
!->  ET ASSEMBLAGE DE DRDY
!
!       DRDY  =   DLEDS   DLEDQ   DLEDL
!                 DLQDS   DLQDQ   DLQDL
!                 DFIDS   DFIDQ   DFIDL
!
!-------------------------------------------
!
!
!- ASSEMBLAGE DE R
!+++++++++++++++++
!
    do 90 i = 1, ndt
        r(i) = -le(i)
90  continue
!
    r(ndt+1) = - lq
    r(ndt+2) = - fi
!
!
!- ASSEMBLAGE DE DRDY
!++++++++++++++++++++
!
    call lcicma(dleds, 6, 6, ndt, ndt,&
                1, 1, drdy, nmod, nmod,&
                1, 1)
    call lcicma(dledq, 6, 1, ndt, 1,&
                1, 1, drdy, nmod, nmod,&
                1, ndt+1)
    call lcicma(dledl, 6, 1, ndt, 1,&
                1, 1, drdy, nmod, nmod,&
                1, ndt+2)
!
    call lcicma(dlqds, 1, 6, 1, ndt,&
                1, 1, drdy, nmod, nmod,&
                ndt+1, 1)
    drdy(ndt+1, ndt+1) = dlqdq
    drdy(ndt+1, ndt+2) = dlqdl
!
    call lcicma(dfids, 1, 6, 1, ndt,&
                1, 1, drdy, nmod, nmod,&
                ndt+2, 1)
    drdy(ndt+2, ndt+1) = dfidq
    drdy(ndt+2, ndt+2) = dfidl
!
!
end subroutine
