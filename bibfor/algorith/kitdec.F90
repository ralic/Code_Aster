subroutine kitdec(kpi, yachai, yamec, yate, yap1,&
                  yap2, meca, thmc, ther, hydr,&
                  imate, defgem, defgep, addeme, addep1,&
                  addep2, addete, ndim, t0, p10,&
                  p20, phi0, pvp0, depsv, epsv,&
                  deps, t, p1, p2, dt,&
                  dp1, dp2, grat, grap1, grap2,&
                  retcom, rinstp)
! ======================================================================
! ======================================================================
! person_in_charge: sylvie.granet at edf.fr
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
! aslint: disable=W1504
    implicit none
#include "asterfort/calcva.h"
#include "asterfort/tecael.h"
#include "asterfort/thmrcp.h"
    integer :: kpi, yamec, yate, yap1, yap2, imate
    logical(kind=1) :: yachai
    integer :: addeme, addep1, addep2, addete, ndim, retcom
    real(kind=8) :: t0, p10, p20, phi0, pvp0, depsv, epsv, deps(6), t
    real(kind=8) :: p1, p2, grat(3), grap1(3), grap2(3), dp1, dp2, dt
    real(kind=8) :: defgem(*), defgep(*), rinstp
    character(len=16) :: meca, thmc, ther, hydr
! ======================================================================
! --- RECUPERATION DES DONNEES INITIALES -------------------------------
! ======================================================================
    integer :: rbid54, rndim
    real(kind=8) :: rbid1, rbid2, rbid3, rbid4, rbid5, rbid6
    real(kind=8) :: rbid8, rbid9, rbid10, rbid11, rbid12(6), rbid13, rbid14
    real(kind=8) :: rbid15(3), rbid16(3, 3), rbid17, rbid18, rbid19, rbid20
    real(kind=8) :: rbid21, rbid22, rbid23, rbid24, rbid25, rbid26
    real(kind=8) :: rbid27, rbid28, rbid29, rbid30, rbid31, rbid32
    real(kind=8) :: rbid33, rbid34, rbid35, rbid36, rbid37(3, 3)
    real(kind=8) :: rbid39, rbid40, rbid41, rbid42, rbid43, rbid44
    real(kind=8) :: rbid45, rbid46, rbid47, rbid48, rbid49, rbid50
    real(kind=8) :: rbid52, rbid53, rbid38(3, 3), rbid51(3, 3)
    real(kind=8) :: r7bid(3)
! ======================================================================
! --- INITIALISATION DE VARIABLES --------------------------------------
! ======================================================================
    call thmrcp('INITIALI', imate, thmc, meca, hydr,&
                ther, t0, p10, p20, phi0,&
                pvp0, rbid1, rbid2, rbid3, rbid4,&
                rbid5, rbid6, rbid8, rbid9, rbid10,&
                rbid11, rbid12, rbid13, rbid53, rbid14,&
                rbid15, rbid16, rbid17, rbid18, rbid19,&
                rbid20, rbid21, rbid22, rbid23, rbid24,&
                rbid25, rbid26, rbid27, rbid28, rbid29,&
                rbid30, rbid31, rbid32, rbid33, rbid34,&
                rbid35, rbid36, rbid37, rbid38, rbid39,&
                rbid40, rbid41, rbid42, rbid43, rbid44,&
                rbid45, rbid46, rbid47, rbid48, rbid49,&
                rbid50, rbid51, rbid52, rinstp, retcom,&
                r7bid, rbid54, rndim)
! ======================================================================
! --- CALCUL DES VARIABLES ---------------------------------------------
! ======================================================================
    call calcva(kpi, yachai, yamec, yate, yap1,&
                yap2, defgem, defgep, addeme, addep1,&
                addep2, addete, ndim, t0, p10,&
                p20, depsv, epsv, deps, t,&
                p1, p2, grat, grap1, grap2,&
                dp1, dp2, dt, retcom)
! ======================================================================
end subroutine
