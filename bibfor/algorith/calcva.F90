subroutine calcva(kpi, yachai, yamec, yate, yap1,&
                  yap2, defgem, defgep, addeme, addep1,&
                  addep2, addete, ndim, t0, p10,&
                  p20, depsv, epsv, deps, t,&
                  p1, p2, grat, grap1, grap2,&
                  dp1, dp2, dt, retcom)
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
! person_in_charge: sylvie.granet at edf.fr
! ======================================================================
! ======================================================================
! --- CALCUL DE VARIABLES (MECANIQUES, HYDRAULIQUES, THERMIQUES) -------
! ======================================================================
! aslint: disable=W1504
    implicit      none
#include "jeveux.h"
#include "asterfort/rcvarc.h"
#include "asterfort/tecael.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
    integer :: kpi, yamec, yate, yap1, yap2
    logical :: yachai
    integer :: addeme, addep1, addep2, addete, ndim, retcom
    real(kind=8) :: defgem(*), defgep(*), t0, p10, p20
    real(kind=8) :: depsv, epsv, deps(6), t, p1, p2, dt, dp1, dp2
    real(kind=8) :: grat(ndim), grap1(ndim), grap2(ndim)
! ======================================================================
    integer :: i, iadzi, iazk24, iret1, iret2
    character(len=8) :: nomail
    real(kind=8) :: epsvp, epsvm
! ======================================================================
! ======================================================================
! --- CALCUL DES DIFFERENTES VARIABLES QUELLE QUE SOIT L'OPTION --------
! ======================================================================
! --- VARIABLES MECANIQUES ---------------------------------------------
! ======================================================================
    depsv = 0.0d0
    epsv = 0.0d0
    call rcvarc(' ', 'DIVU', '-', 'RIGI', kpi,&
                1, epsvm, iret1)
    call rcvarc(' ', 'DIVU', '+', 'RIGI', kpi,&
                1, epsvp, iret2)
!
    yachai = (iret1.eq.0).and.(iret2.eq.0)
!
! REMARQUE : YAMEC PEUT ETRE EGAL A 2
!            (ELEMENTS DE JOINTS HM)
    if ((yamec.ne.0) .and. yachai) then
        call u2mess('F', 'CHAINAGE_1')
    else if (iret1.ne.iret2) then
        call u2mess('F', 'CHAINAGE_2')
    endif
!
! 1ER CAS : ON A DE LA MECANIQUE ET ON EST EN TOTALEMENT COUPLE
!
    if (yamec .eq. 1) then
        do 100 i = 1, 6
            deps(i)=defgep(addeme+ndim-1+i)-defgem(addeme+ndim-1+i)
100      continue
        do 101 i = 1, 3
            depsv=depsv+defgep(addeme+ndim-1+i)-defgem(addeme+ndim-1+&
            i)
101      continue
        do 102 i = 1, 3
            epsv=epsv+defgep(addeme+ndim-1+i)
102      continue
    endif
!
    if (yachai) then
        depsv = epsvp - epsvm
        epsv = epsvp
    endif
! ======================================================================
! --- VARIABLES HYDRAULIQUES -------------------------------------------
! ======================================================================
    p1 = p10
    dp1 = 0.0d0
    p2 = p20
    dp2 = 0.0d0
    if (yap1 .eq. 1) then
        p1=defgep(addep1)+p10
        dp1=defgep(addep1)-defgem(addep1)
        do 103 i = 1, ndim
            grap1(i)=defgep(addep1+i)
103      continue
        if (yap2 .eq. 1) then
            p2=defgep(addep2)+p20
            dp2=defgep(addep2)-defgem(addep2)
            do 104 i = 1, ndim
                grap2(i)=defgep(addep2+i)
104          continue
        endif
    endif
! ======================================================================
! --- VARIABLES THERMIQUES ---------------------------------------------
! ======================================================================
    t = t0
    dt = 0.0d0
    if (yate .eq. 1) then
        dt=defgep(addete)-defgem(addete)
        t=defgep(addete)+t0
        do 105 i = 1, ndim
            grat(i)=defgep(addete+i)
105      continue
        if (t .le. 0.d0) then
            call tecael(iadzi, iazk24)
            nomail = zk24(iazk24-1+3) (1:8)
            call u2mesk('A', 'ELEMENTS5_41', 1, nomail)
            retcom = 1
        endif
    endif
!
end subroutine
