subroutine epstmc(fami     , ndim  , instan, poum   , kpg   ,&
                  ksp      , xyzgau, repere, j_mater, option,&
                  epsi_varc)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/matrot.h"
#include "asterfort/get_elas_type.h"
#include "asterfort/get_elas_para.h"
#include "asterfort/calc_epth_elga.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
#include "asterfort/utpslg.h"
#include "asterfort/utrcyl.h"
#include "asterfort/verift.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=*), intent(in) :: fami
    integer, intent(in) :: ndim
    character(len=*), intent(in) :: poum
    integer, intent(in) :: kpg
    integer, intent(in) :: ksp
    integer, intent(in) :: j_mater
    real(kind=8), intent(in) :: xyzgau(3)
    real(kind=8), intent(in) :: repere(7)
    character(len=16), intent(in) :: option
    real(kind=8), intent(in) :: instan
    real(kind=8), intent(out) :: epsi_varc(6)
!
! --------------------------------------------------------------------------------------------------
!
! Compute variable commands strains (themrics, drying, etc.)
!
! For isoparametric elements
!
! --------------------------------------------------------------------------------------------------
!
! In  fami         : Gauss family for integration point rule
! In  ndim         : dimension of space
! In  poum         : parameters evaluation 
!                     '-' for previous temperature
!                     '+' for current temperature
!                     'T' for current and previous temperature
! In  kpg          : current point gauss
! In  ksp          : current "sous-point" gauss
! In  j_mater      : coded material address
! In  xyzgau       : coordinates of current Gauss point
! In  repere       : definition of basis (for non-isotropic materials)
! In  instan       : current time
! In  option       : name of option to compute
! Out epsi_varc    : command variables strains
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: nbres = 3
    integer :: icodre(nbres)
    character(len=16) :: nomres(nbres)
    real(kind=8) :: valres(nbres)
!
    integer :: nbv, elas_type
    real(kind=8) :: biot, e
    character(len=8) :: nompar
    character(len=32) :: phenom
    real(kind=8) :: valpar, bendog, kdessi
    real(kind=8) :: troisk, nu
    real(kind=8) :: hydr, sech, sref, ptot
    integer :: k, iret
    character(len=6), parameter :: epsa(6)=(/'EPSAXX','EPSAYY','EPSAZZ','EPSAXY','EPSAXZ','EPSAYZ'/)
!
! --------------------------------------------------------------------------------------------------
!
    nompar         = 'INST'
    valpar         = instan
    epsi_varc(1:6) = 0.d0
    biot           = 0.d0
    bendog         = 0.d0
    kdessi         = 0.d0
!
! - Get command variables
!
    call rcvarc(' ', 'HYDR', poum, fami, kpg,&
                ksp, hydr, iret)
    if (iret .eq. 1) hydr=0.d0
    call rcvarc(' ', 'SECH', poum, fami, kpg,&
                ksp, sech, iret)
    if (iret .eq. 1) sech=0.d0
    call rcvarc(' ', 'PTOT', poum, fami, kpg,&
                ksp, ptot, iret)
    if (iret .eq. 1) ptot=0.d0
    call rcvarc(' ', 'SECH', 'REF', fami, 1,&
                1, sref, iret)
    if (iret .eq. 1) sref=0.d0
!
    if (option(11:14) .eq. 'HYDR') then
!
! ----- Hydric strains
!
        if (hydr .ne. 0.d0) then
            phenom = 'ELAS'
            nomres(1) = 'B_ENDOGE'
            nbv = 1
            call rcvalb(fami, kpg, ksp, poum, j_mater,&
                        ' ', phenom, 1, nompar, [valpar],&
                        nbv, nomres, valres, icodre, 0)
            if (icodre(1) .eq. 0) then
                bendog = valres(1)
                epsi_varc(1) = - bendog*hydr
                epsi_varc(2) = - bendog*hydr
                epsi_varc(3) = - bendog*hydr
            else
                call utmess('I', 'COMPOR5_12')
            endif
        endif
    else if (option(11:14).eq.'PTOT') then
!
! ----- Fluid pressure strain
!
        if (ptot .ne. 0.d0) then
            phenom = 'THM_DIFFU'
            nomres(1) = 'BIOT_COEF'
            nbv = 1
            call rcvalb(fami, kpg, ksp, poum, j_mater,&
                        ' ', phenom, 1, nompar, [valpar],&
                        nbv, nomres, valres, icodre, 0)
            if (icodre(1) .eq. 0) then
                biot = valres(1)
            else
                biot = 0.d0
                call utmess('I', 'COMPOR5_13')
            endif
            call get_elas_para(fami     , j_mater, poum, kpg, ksp, &
                               elas_type,&
                               time     = instan,&
                               e   = e, nu = nu )
            troisk = e/(1.d0-2.d0*nu)
            epsi_varc(1) = biot/troisk*ptot
            epsi_varc(2) = epsi_varc(1)
            epsi_varc(3) = epsi_varc(1)
        endif
    else if (option(11:14).eq.'SECH') then
!
! ----- Drying strains
!
        phenom = 'ELAS'
        nomres(1) = 'K_DESSIC'
        nbv = 1
        call rcvalb(fami, kpg, ksp, poum, j_mater,&
                    ' ', phenom, 1, nompar, [valpar],&
                    nbv, nomres, valres, icodre, 0)
        if (icodre(1) .eq. 0) then
            kdessi = valres(1)
        else
            kdessi = 0.d0
        endif
        epsi_varc(1) = - kdessi*(sref-sech)
        epsi_varc(2) = - kdessi*(sref-sech)
        epsi_varc(3) = - kdessi*(sref-sech)
    else if (option(11:14).eq.'EPSA') then
!
! ----- User strains for command variables
!
        do k = 1, 6
            call rcvarc(' ', epsa(k), poum, fami, kpg,&
                        ksp, epsi_varc(k), iret)
            if (iret .eq. 1) epsi_varc(k)=0.d0
        end do
    else
!
! ----- Thermic strains
!
        call calc_epth_elga(fami   , ndim  , poum  , kpg  , ksp,&
                            j_mater, xyzgau, repere, epsi_varc)
    endif
!
end subroutine
