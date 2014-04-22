subroutine nm1das(fami, kpg, ksp, e, syc,&
                  syt, etc, ett, cr, tmoins,&
                  tplus, icodma, sigm, deps, vim,&
                  sig, vip, dsdem, dsdep)
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ----------------------------------------------------------------------
!
    implicit none
! ----------------------------------------------------------------------
!          PLASTICITE VON MISES ISOTROPE BILINEAIRE MONODIM
!    ECROUISSAGE ISOTROPE ASYMETRIQUE LINEAIRE - VON MISES-
!
!
! IN  FAMI     : FAMILLE DES POINTS DE GAUSS
! IN  KPG      : NUMERO DU POINT DE GAUSS
! IN  KSP      : NUMERO DU SOUS-POINT DE GAUSS
! IN  E        : MODULE D YOUNG
!       ETT    : ET EN TRACTION
!       ETC    : ET EN COMPRESSION
!       SYC    : LIMITE ELASTIQUE EN COMPRESSION
!       SYT    : LIMITE ELASTIQUE EN TRACTION
!       CR     : COEFFICIENT DE RESTAURATION. =0 POUR LE MOMENT
! IN  SIGM     : CONTRAINTE AU TEMPS MOINS
! IN  DEPS     : DEFORMATION  TOTALE PLUS - DEFORMATION TOTALE MOINS
! IN  VIM      : DEFORMATION  PLASTIQUE CUMULEE  AU TEMPS MOINS
!
! OUT SIG     : CONTRAINTES AU TEMPS PLUS
! OUT VIP    : DEFORMATION  PLASTIQUE CUMULEE TRACTION AU TEMPS PLUS
! OUT DSDEM   : DSIG/DEPS TEMPS MOINS
! OUT DSDEP   : DSIG/DEPS TEMPS PLUS
!     ------------------------------------------------------------------
!     ARGUMENTS
!     ------------------------------------------------------------------
#include "asterfort/verift.h"
    real(kind=8) :: e, epsthe, syc, syt, etc, ett, cr, tmoins, tplus
    real(kind=8) :: sigm, deps, pmt, pmc, xmt, xmc, xpt, xpc, vim(4), vip(4)
    real(kind=8) :: sig, ppt, ppc, dsdem, dsdep
    integer :: kpg, ksp
    character(len=*) :: fami
!     ------------------------------------------------------------------
!     VARIABLES LOCALES
!     ------------------------------------------------------------------
    real(kind=8) :: rmc, rmt, sige, ht, hc, depmec, dpt, rpt, dpc, rpc, sigd
    integer :: icodma
!
!     ------------------------------------------------------------------
!     VARIABLES INTERMEDIAIRES
!     ------------------------------------------------------------------
!
    ht = e*ett/(e-ett)
    hc = e*etc/(e-etc)
    pmt = vim(1)
    xmt = vim(2)
    pmc = vim(3)
    xmc = vim(4)
!     ------------------------------------------------------------------
!     DELTA DEFORMATION MECANIQUE
!     ------------------------------------------------------------------
!
    call verift(fami, kpg, ksp, 'T', icodma,&
                elas_keyword = 'ELAS', epsth=epsthe)
!
    depmec = deps-epsthe
!     ------------------------------------------------------------------
!     FONCTIONS D'ECROUISSAGE AU TEMPS MOINS
!     ------------------------------------------------------------------
!
    rmt = ht*pmt+syt
    rmc = hc*pmc+syc
!
!     RIGI_MECA_TANG = MATRICE ELASTIQUE
!
    dsdem = e
!     ------------------------------------------------------------------
!     ESTIMATION ELASTIQUE
!     ------------------------------------------------------------------
    sigd=sigm+e*depmec
!     ------------------------------------------------------------------
!     CALCUL EPSP, P , SIG
!     ------------------------------------------------------------------
!
    if (depmec .gt. 0.d0) then
!
!        CAS DE LA "TRACTION"
!
        sige = sigd - xmt
!
        if (sige .lt. rmt) then
!
!           ON RESTE ELASTIQUE
!
            ppt = pmt
            xpt = xmt
            ppc = pmc
            sig = sigd
!CC         XPC = CR * SIG
!JMP        XPC = XMC + CR * (SIG-XMC)
            xpc = sig + (xmc-sig)*exp(-cr*(tplus-tmoins))
            dsdep = e
        else
!
!           ON PLASTIFIE EN TRACTION
!
            dpt = (sige - rmt)/(e+ht)
            ppt = pmt + dpt
            ppc = pmc
            rpt = syt + ht*ppt
            sig = sige/(1.d0+e*dpt/rpt)+xmt
!CC         XPC = CR * SIG
!JMP        XPC = XMC + CR * (SIG-XMC)
            xpc = sig + (xmc-sig)*exp(-cr*(tplus-tmoins))
            xpt = xmt
            dsdep = ett
!
        endif
!
    else if (depmec.lt.0.d0) then
!
!        CAS DE LA "COMPRESSION"
!
        sige = sigd - xmc
!
        if (sige .gt. (-rmc)) then
!
!           ON RESTE ELASTIQUE
!
            ppt = pmt
            xpc = xmc
            ppc = pmc
            sig = sigd
!CC         XPT = CR * SIG
!JMP        XPT = XMT + CR * (SIG-XMT)
            xpt = sig + (xmt-sig)*exp(-cr*(tplus-tmoins))
            dsdep = e
        else
!
!           ON PLASTIFIE EN COMPRESSION
!
            dpc = (abs(sige) - rmc)/(e+hc)
            ppt = pmt
            ppc = pmc + dpc
            rpc = syc + hc*ppc
            sig = sige/(1.d0+e*dpc/rpc)+xmc
!CC         XPT = CR * SIG
!JMP        XPT = XMT + CR * (SIG-XMT)
            xpt = sig + (xmt-sig)*exp(-cr*(tplus-tmoins))
            xpc = xmc
            dsdep = etc
!
        endif
!
    else
!
!           ON RESTE ELASTIQUE ET DEPS = 0
!
        ppt = pmt
        xpc = xmc
        ppc = pmc
        xpt = xmt
        sig = sigm
        dsdep = e
!
    endif
    vip(1) = ppt
    vip(2) = xpt
    vip(3) = ppc
    vip(4) = xpc
!
end subroutine
