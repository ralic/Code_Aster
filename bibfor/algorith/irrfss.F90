subroutine irrfss(sig, ddfdds)
    implicit none
    include 'asterfort/lcdevi.h'
    include 'asterfort/lcdima.h'
    include 'asterfort/lcinma.h'
    include 'asterfort/lcnrts.h'
    include 'asterfort/lcprsm.h'
    include 'asterfort/lcprsv.h'
    include 'asterfort/lcprte.h'
    real(kind=8) :: sig(6), ddfdds(6, 6)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
!       3D / DP / CP
!       DERIVEE / S / S DE LA FONCTION SEUIL A (SIG ) DONNES
!
!       IN  SIG    :  TENSEUR CONTRAINTE
!                                                     T
!       OUT D2FD2S :  DDFDDS = 1/S ( 3/2 ID - DFDS DFDS )
!                     DFDS   = 3 D / 2 S
!                                            T
!                     ID     = I4 - 1/3 I2 I2
!                                           T           1/2
!                     S      = (3/2(D-X1-X2) (D-X1-X2))
!                     D      = SIG - 1/3 TR(SIG) I
!       ----------------------------------------------------------------
    real(kind=8) :: id(6, 6), d23, d13, zero, un, s
    parameter       ( d23  =  .66666666666666D0 )
    parameter       ( d13  = -.33333333333333D0 )
    parameter       ( zero =  0.d0              )
    parameter       ( un   =  1.d0              )
    real(kind=8) :: dev(6), dfds(6), dfds2(6, 6)
    data id         / d23   , d13   , d13   , zero , zero , zero ,&
     &                  d13   , d23   , d13   , zero , zero , zero ,&
     &                  d13   , d13   , d23   , zero , zero , zero ,&
     &                  zero  , zero  , zero  , un   , zero , zero ,&
     &                  zero  , zero  , zero  , zero , un   , zero ,&
     &                  zero  , zero  , zero  , zero , zero , un /
!
!
!
    call lcdevi(sig, dev)
    s = lcnrts ( dev )
    if (s .eq. 0.d0) then
        call lcinma(0.d0, ddfdds)
    else
        call lcprsv(1.5d0 / s, dev, dfds)
        call lcprte(dfds, dfds, dfds2)
        call lcprsm(1.5d0, id, ddfdds)
        call lcdima(ddfdds, dfds2, ddfdds)
        call lcprsm(1.d0/ s, ddfdds, ddfdds)
    endif
!
end subroutine
