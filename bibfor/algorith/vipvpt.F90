subroutine vipvpt(nbvari, vintm, vintp, advico, vicpvp,&
                  dimcon, p2, congem, adcp11, adcp12,&
                  ndim, pvp0, dp1, dp2, t,&
                  dt, mamolv, r, rho11, kh,&
                  signe, cp11, cp12, yate, pvp,&
                  pvpm, retcom)
! aslint: disable=W1504
    implicit      none
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/iunifi.h"
#include "asterfort/tecael.h"
    integer :: nbvari, advico, vicpvp, adcp11, adcp12, ndim, dimcon
    integer :: yate, retcom
    real(kind=8) :: vintm(nbvari), vintp(nbvari), congem(dimcon), pvp0, dp1
    real(kind=8) :: dp2, t, dt, mamolv, r, rho11, cp11, cp12, pvp, pvpm, p2
    real(kind=8) :: signe
    real(kind=8) :: kh
! ======================================================================
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
! ======================================================================
! --- BUT : CALCUL ET STOCKAGE DES PRESSIONS DE VAPEUR INTER PTILDE-----
! -------   DANS LES CAS AVEC AIR DISSOUS ------------------------------
! ======================================================================
    integer :: iadzi, iazk24, umess
    real(kind=8) :: varpv, epxmax
    parameter    (epxmax = 5.d0)
    character(len=8) :: nomail
! ======================================================================
! ======================================================================
! --- CALCUL DES ARGUMENTS EN EXPONENTIELS -----------------------------
! --- ET VERIFICATION DES COHERENCES -----------------------------------
! ======================================================================
!      VARPV = MAMOLV*(1/R/T-1/KH)* DP2/RHO11-MAMOLV/R/T*DP1/RHO11
    varpv = mamolv/r/t*(dp2-signe*dp1)/rho11-mamolv/rho11/kh*dp2
    if (yate .eq. 1) then
        varpv = varpv+(congem(adcp12+ndim+1)-congem(adcp11+ndim+1))* (1.0d0/(t-dt)-1.0d0/t&
                )*mamolv/r
        varpv = varpv+(cp12-cp11)*(log(t/(t-dt))-(dt/t))*mamolv/r
    endif
    if (varpv .gt. epxmax) then
        umess = iunifi('MESSAGE')
        call tecael(iadzi, iazk24)
        nomail = zk24(iazk24-1+3) (1:8)
        write (umess,9001) 'VIPVPT','VARPV > EXPMAX A LA MAILLE: ',&
        nomail
        retcom = 1
        goto 30
    endif
    vintp(advico+vicpvp) = - pvp0 + (vintm(advico+vicpvp)+pvp0)*exp(varpv)
    pvp = vintp(advico+vicpvp) + pvp0
    pvpm = vintm(advico+vicpvp) + pvp0
    if ((p2-pvp) .lt. 0.0d0) then
        umess = iunifi('MESSAGE')
        call tecael(iadzi, iazk24)
        nomail = zk24(iazk24-1+3) (1:8)
        write (umess,9001) 'VIPVPT','PGAZ-PVAP <=0 A LA MAILLE: ',&
        nomail
        retcom = 1
        goto 30
    endif
    if ((pvp) .lt. r8prem()) then
        umess = iunifi('MESSAGE')
        call tecael(iadzi, iazk24)
        nomail = zk24(iazk24-1+3) (1:8)
        write (umess,9001) 'VIPVPT','PVAP =0 A LA MAILLE: ',nomail
        retcom = 1
        goto 30
    endif
! ======================================================================
30  continue
! ======================================================================
    9001 format (a8,2x,a30,2x,a8)
! ======================================================================
end subroutine
