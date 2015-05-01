subroutine lcmfga(mode, eps, gameps, dgamde, itemax, precvg, iret)
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
    implicit none
#include "asterfort/lcmfdr.h"
#include "asterfort/lcmfra.h"
#include "asterfort/lcvalp.h"
    integer,intent(in) :: mode, itemax
    real(kind=8),intent(in) :: eps(6), precvg
    integer,intent(out):: iret
    real(kind=8),intent(out):: gameps, dgamde(6)
! --------------------------------------------------------------------------------------------------
!  CALCUL DE GAMMA(EPS) POUR LA LOI ENDO_SCALAIRE AVEC GRAD_VARI
! --------------------------------------------------------------------------------------------------
!  MODE    FONCTION RECHERCHEE (0=VALEUR, 1=VAL ET DER)
!  EPS     VALEUR DE L'ARGUMENT EPS(1:NDIMSI)
!  GAMEPS  FONCTION GAMMA(EPS) - SI MODE=1 OU MODE=0
!  DGAMDE  DERIVEE DE GAMMA    - SI MODE=1
!  PRECVG  PRECISION POUR CALCUL DE GAMMA: DGAM < PRECVG
!  IRET    0 SI OK, 1 SI PB POUR CALCULER LE RAYON
! --------------------------------------------------------------------------------------------------
    real(kind=8),parameter,dimension(6):: kr=(/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/)
! --------------------------------------------------------------------------------------------------
    real(kind=8) :: treps, sig(6), sigvp(3), chi, dchids(6), coef, trchid, precx1
! --------------------------------------------------------------------------------------------------
    real(kind=8) :: lambda, deuxmu, troisk, rigmin, pc, pr, epsth
    common /lcee/ lambda,deuxmu,troisk,rigmin,pc,pr,epsth
! --------------------------------------------------------------------------------------------------
    real(kind=8) :: pk, pm, pp
    common /lces/ pk,pm,pp
! --------------------------------------------------------------------------------------------------
    real(kind=8) :: pct, pch, pcs
    common /lcmqu/ pch,pct,pcs
! --------------------------------------------------------------------------------------------------
!
!  INITIALISATION: PRECISION RECHERCHEE SUR CHI * DCHI
    precx1 = 0.5d0*pm/pk*precvg         
!
!
!  CALCUL DE LA VALEUR
!
    treps = eps(1)+eps(2)+eps(3)
    sig = lambda*treps*kr + deuxmu*eps
    call lcvalp(sig, sigvp)
    call lcmfra(sigvp, itemax, precx1, chi, iret)
    if (iret .eq. 1) goto 999
    gameps = pk/pm*chi**2
!
!
!  CALCUL DE LA DERIVEE
!
    if (mode .eq. 1) then
!
        if (chi .ne. 0) then
            call lcmfdr(sig, sigvp, chi, precvg, dchids)
            coef = 2*pk/pm*chi
            trchid = dchids(1)+dchids(2)+dchids(3)
            dgamde = coef*(lambda*trchid*kr+deuxmu*dchids)
        else
            dgamde = 0
        endif
    endif
!
!
999 continue
end subroutine
