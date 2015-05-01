subroutine capaca(rho0, rho11, rho12, rho21, rho22,&
                  sat, phi, csigm, cp11, cp12,&
                  cp21, cp22, dalal, t, coeps,&
                  retcom)
    implicit none
#include "jeveux.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
    integer :: retcom
    real(kind=8) :: rho0, rho11, rho12, rho21, rho22, sat, phi, csigm, dalal, t
    real(kind=8) :: cp11, cp12, cp21, cp22, coeps
! ======================================================================
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
! ======================================================================
! --- CALCUL DE LA CAPACITE CALORIFIQUE --------------------------------
! ======================================================================
    integer :: iadzi, iazk24
    real(kind=8) :: umprhs
    character(len=8) :: nomail
! ======================================================================
! ======================================================================
! --- CALCUL DES ARGUMENTS EN EXPONENTIELS -----------------------------
! --- ET VERIFICATION DES COHERENCES -----------------------------------
! ======================================================================
    umprhs = rho0-(rho11+rho22)*sat*phi-(rho12+rho21)*(1.d0-sat)*phi
    if (umprhs .le. 0.d0) then
        call tecael(iadzi, iazk24)
        nomail = zk24(iazk24-1+3) (1:8)
        call utmess('A', 'ALGORITH17_39', sk=nomail)
        retcom = 1
        goto 30
    endif
    coeps = umprhs*csigm + phi*sat*(rho11*cp11+rho22*cp22) + phi* (1.d0-sat)* (rho12*cp12+rho21*c&
            &p21)
! ======================================================================
! --- CALCUL DE COEPS SI MECANIQUE SINON ALPHA0=0 ----------------------
! ======================================================================
    coeps = coeps - t*dalal
    if (coeps .le. 0.d0) then
        call tecael(iadzi, iazk24)
        nomail = zk24(iazk24-1+3) (1:8)
        call utmess('A', 'ALGORITH17_40', sk=nomail)
        retcom = 1
        goto 30
    endif
! ======================================================================
30  continue
! =====================================================================
! ======================================================================
end subroutine
