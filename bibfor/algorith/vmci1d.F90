subroutine vmci1d(fami, kpg, ksp, imate, em,&
                  ep, sigm, deps, vim, option,&
                  materi, sigp, vip, dsde)
!
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
! person_in_charge: jean-luc.flejou at edf.fr
    implicit none
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
    integer :: kpg, ksp, imate
    real(kind=8) :: ep, em, sigm, deps, sigp, dsde
    real(kind=8) :: vim(*), vip(*)
    character(len=16) :: option
    character(len=*) :: fami, materi
!
! --------------------------------------------------------------------------------------------------
!
!           PLASTICITE VON MISES CINEMATIQUE LINEAIRE EN 1D
!              FORTEMENT INSPIRE DE NM1DCI
!  IN
!        FAMI   : FAMILLE DU POINT DE GAUSS
!        KPG    : NUMERO DU POINT DE GAUSS
!        KSP    : NUMERO DU SOUS-POINT DE GAUSS / FIBRE
!        IMATE  : POINTEUR MATERIAU CODE
!        EM     : MODULE D YOUNG MOINS
!        EP     : MODULE D YOUNG PLUS
!        SIGM   : CONTRAINTE AU TEMPS MOINS
!        DEPS   : DEFORMATION TOTALE PLUS - DEFORMATION MOINS
!                       - INCREMENT DEFORMATION THERMIQUE
!        VIM    : VARIABLE INTERNES MOINS
!        OPTION : OPTION DE CALCUL
!  OUT
!        SIGP   : CONTRAINTES PLUS
!        VIP    : VARIABLE INTERNES PLUS
!        DSDE   : DSIG/DEPS
! --------------------------------------------------------------------------------------------------
!     VARIABLES INTERNES
!        1  -> ICELS : CRITERE SIGMA
!        2  -> ICELU : CRITERE EPSI
!        3  -> IXM   : ECROUISSAGE CINEMATIQUE
!        4  -> IPLAS : INDICATEUR PLASTIQUE
!        5  -> IDISS : DISSIPATION PLASTIQUE
!        6  -> IWTHE : DISSIPATION THERMODYNAMIQUE
! --------------------------------------------------------------------------------------------------
!   index des variables internes
    integer :: icels,  icelu,  ixm,  iplas,  idiss,  iwthe
    parameter (icels=1,icelu=2,ixm=3,iplas=4,idiss=5,iwthe=6)
! --------------------------------------------------------------------------------------------------
    real(kind=8) :: sigy, sieleq, sige, dp, etm, etp, xp, xm, hm, hp, sgels, epelu
    character(len=16) :: valkm(3)
    integer ::          icodre(4)
    real(kind=8) ::     valres(4)
    character(len=8) :: nomecl(4)
!
    data nomecl/'D_SIGM_E','SY','SIGM_LIM','EPSI_LIM'/
! --------------------------------------------------------------------------------------------------
!   instant -
    call rcvalb(fami, kpg, ksp, '-', imate,&
                materi, 'ECRO_LINE', 0, ' ', [0.d0],&
                1, nomecl, valres, icodre, 1)
    etm = valres(1)
    hm = em*etm/(em-etm)
!   instant +
    call rcvalb(fami, kpg, ksp, '+', imate,&
                materi, 'ECRO_LINE', 0, ' ', [0.d0],&
                4, nomecl, valres, icodre, 1)
!   vérification que SIGM_LIM, EPSI_LIM sont présents
    if (icodre(3)+icodre(4) .ne. 0) then
        valkm(1)='VMIS_CINE_GC'
        valkm(2)=nomecl(3)
        valkm(3)=nomecl(4)
        call utmess('F', 'COMPOR1_76', nk=3, valk=valkm)
    endif
    etp = valres(1)
    sigy = valres(2)
    sgels = valres(3)
    epelu = valres(4)
!
    hp = ep*etp/(ep-etp)
    xm = vim(ixm)
!
    sige = ep*(sigm/em+deps) - hp*xm/hm
    sieleq = abs(sige)
! --------------------------------------------------------------------------------------------------
!   calcul : EPSP, P , SIG
    if ((option(1:9).eq.'FULL_MECA') .or. (option.eq.'RAPH_MECA')) then
        if (sieleq .le. sigy) then
            vip(iplas) = 0.d0
            dsde = ep
            dp = 0.d0
            xp = hp*xm/hm
            sigp = ep*(sigm/em+deps)
            vip(ixm) = xp
            vip(icelu) = (sigm/em+deps)/epelu
        else
            vip(iplas) = 1.d0
            dp = (sieleq-sigy)/(ep+hp)
            if (option .eq. 'FULL_MECA_ELAS') then
                dsde = ep
            else
                dsde = etp
            endif
            xp = hp*xm/hm + hp*dp*sige/sieleq
            sigp = xp + sigy*sige/sieleq
            vip(ixm) = xp
            vip(icelu) = ((sigp-sigy)/etp + sigy/ep)/epelu
        endif
        vip(icels) = sigp/sgels
!       dissipation thermodynamique
        vip(iwthe) = vim(iwthe) + sigy*dp
!       dissipation irréversible
        vip(idiss) = vim(idiss) + (dsde*deps-(sigp-sigm))*deps/2.0d0
    endif
    if (option(1:10) .eq. 'RIGI_MECA_') then
        if ((vim(iplas).lt.0.5d0) .or. (option.eq.'RIGI_MECA_ELAS')) then
            dsde = ep
        else
            dsde = etp
        endif
    endif
end subroutine
