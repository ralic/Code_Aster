subroutine capaca(rho0, rho11, rho12, rho21, rho22,&
                  sat, phi, csigm, cp11, cp12,&
                  cp21, cp22, k0, alpha0, t,&
                  coeps, retcom)
    implicit      none
#include "jeveux.h"
#include "asterfort/iunifi.h"
#include "asterfort/tecael.h"
    integer :: retcom
    real(kind=8) :: rho0, rho11, rho12, rho21, rho22, sat, phi, csigm, alpha0, t
    real(kind=8) :: k0, cp11, cp12, cp21, cp22, coeps
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
! --- CALCUL DE LA CAPACITE CALORIFIQUE --------------------------------
! ======================================================================
    integer :: iadzi, iazk24, umess
    real(kind=8) :: umprhs
    character(len=8) :: nomail
! ======================================================================
! ======================================================================
! --- CALCUL DES ARGUMENTS EN EXPONENTIELS -----------------------------
! --- ET VERIFICATION DES COHERENCES -----------------------------------
! ======================================================================
    umprhs = rho0-(rho11+rho22)*sat*phi-(rho12+rho21)*(1.d0-sat)*phi
    if (umprhs .le. 0.d0) then
        umess = iunifi('MESSAGE')
        call tecael(iadzi, iazk24)
        nomail = zk24(iazk24-1+3) (1:8)
        write (umess,9001) 'CAPACA',' RHOS(1-PHI) <=0 A LA MAILLE: ',&
        nomail
        retcom = 1
        goto 30
    endif
    coeps = umprhs*csigm + phi*sat*(rho11*cp11+rho22*cp22) + phi* (1.d0-sat)* (rho12*cp12+rho21*c&
            &p21)
! ======================================================================
! --- CALCUL DE COEPS SI MECANIQUE SINON ALPHA0=0 ----------------------
! ======================================================================
    coeps = coeps - 9.d0*t*k0*alpha0*alpha0
    if (coeps .le. 0.d0) then
        umess = iunifi('MESSAGE')
        call tecael(iadzi, iazk24)
        nomail = zk24(iazk24-1+3) (1:8)
        write (umess,9001) 'CAPACA',' COEPS <=0 A LA MAILLE: ',&
        nomail
        retcom = 1
        goto 30
    endif
! ======================================================================
30  continue
! =====================================================================
    9001 format (a8,2x,a30,2x,a8)
! ======================================================================
end subroutine
