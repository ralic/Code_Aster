subroutine piesfg(lcesga, eta, g, dg)
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/utmess.h"

    interface
    subroutine lcesga(mode, eps, gameps, dgamde, itemax, precvg, iret)
        integer,intent(in) :: mode, itemax
        real(kind=8),intent(in) :: eps(6), precvg
        integer,intent(out):: iret
        real(kind=8),intent(out):: gameps, dgamde(6)
    end subroutine lcesga
    end interface

    real(kind=8),intent(in) :: eta
    real(kind=8),intent(out):: g, dg
! --------------------------------------------------------------------------------------------------
! CALCUL DE LA FONCTION SEUIL ET DE SA DERIVEE
! --------------------------------------------------------------------------------------------------
! IN  LCESGA FONCTION DE CALCUL DE GAMMA(EPSILON)
! IN  ETA    PARAMETRE DE PILOTAGE
! OUT G      VALEUR DE LA FONCTION SEUIL
! OUT DG     VALEUR DE LA DERIVEE DE LA FONCTION SEUIL
! --------------------------------------------------------------------------------------------------
    integer, parameter:: itemax=100
! --------------------------------------------------------------------------------------------------
    integer :: iret
    real(kind=8) :: eps(6), phi, gameps, dgadep(6)
! --------------------------------------------------------------------------------------------------
    real(kind=8) :: lambda, deuxmu, troisk, gamma, rigmin, pc, pr, epsth
    common /lcee/ lambda,deuxmu,troisk,gamma,rigmin,pc,pr,epsth
! --------------------------------------------------------------------------------------------------
    real(kind=8) :: pk, pm, pp, pq
    common /lces/ pk,pm,pp,pq
! --------------------------------------------------------------------------------------------------
    real(kind=8) :: ep0(6), ep1(6), phi0, phi1, a, drda, precga
    common /pies/ ep0,ep1,phi0,phi1,a,drda,precga
! --------------------------------------------------------------------------------------------------
!
!
    eps = ep0 + eta*ep1
    phi = phi0+ eta*phi1
    call lcesga(1, eps, gameps, dgadep, itemax, precga, iret)
    if (iret .ne. 0) call utmess('F', 'PILOTAGE_83')
    g  = -drda*gameps-pk-pr*a+phi
    dg = -drda*dot_product(dgadep,ep1)+phi1
!
end subroutine
