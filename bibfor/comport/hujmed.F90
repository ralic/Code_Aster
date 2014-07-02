subroutine hujmed(k, mater, vin, sig)
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   --------------------------------------------------------------------
!   ENREGISTREMENT VARIABLE MEMOIRE POUR MECANISME DEVIATOIRE DU PLAN KP
!   IN  K      :  PLAN DE PROJECTION (1 A 3) OU (5 A 7)
!       MATER  :  COEFFICIENTS MATERIAU
!       VIN    :  VARIABLES INTERNES A T
!       SIG    :  CHAMPS DE CONTRAINTE A T
!
!   OUT VIN    :  VARIABLES INTERNES MODIFIEES
!         (VARIABLES MEMOIRES + COMPOSANTES DE LA NORMALE A LA SURFACE)
!   --------------------------------------------------------------------
#include "asterf_types.h"
#include "asterc/r8prem.h"
    integer :: ndt, ndi, k, i, j, kp
    real(kind=8) :: dd, beta, b, m, pcr, rc
    real(kind=8) :: sig(6), phi, pcref, ptrac
    real(kind=8) :: vin(*), tou(3), p, d12
    real(kind=8) :: mater(22, 2), degr, epsvp, un, zero
    real(kind=8) :: xk(2), th(2), sc(2), deux, qsc
    aster_logical :: debug
!
! ----------------------------------------------------------------------
    common /tdim/   ndt, ndi
    common /meshuj/ debug
! ----------------------------------------------------------------------
    parameter     (degr = 0.0174532925199d0)
    parameter     (d12  = 0.5d0  )
    parameter     (zero = 0.0d0  )
    parameter     (un   = 1.0d0  )
    parameter     (deux = 2.0d0  )
!
!
! ==================================================================
! --- VARIABLES INTERNES -------------------------------------------
! ==================================================================
    epsvp = vin(23)
!
    if (k .gt. 4) then
        rc = vin(k)
    else
        rc = vin(k+4)
    endif
!
!
! ==================================================================
! --- CARACTERISTIQUES MATERIAU ------------------------------------
! ==================================================================
    beta = mater(2, 2)
    b = mater(4, 2)
    phi = mater(5, 2)
    pcref = mater(7, 2)
    pcr = pcref*exp(-beta*epsvp)
    ptrac = mater(21,2)
    m = sin(degr*phi)
!
!
! ==================================================================
! --- ON TRAVAILLE AVEC DES INDICES COMPRIS ENTRE 1 ET 3 -----------
! ==================================================================
!
    if (k .gt. 4) then
        kp = k-4
    else
        kp=k
    endif
!
!
! ==================================================================
! --------------- VARIABLES MEMOIRES -------------------------------
! ==================================================================
    xk(1) = vin(4*kp+5)
    xk(2) = vin(4*kp+6)
    th(1) = vin(4*kp+7)
    th(2) = vin(4*kp+8)
!
!
! ==================================================================
! --- PROJECTION DANS LE PLAN DEVIATEUR K ------------------------
! ==================================================================
    j = 1
    do 10 i = 1, ndi
        if (i .ne. kp) then
            tou(j) = sig(i)
            j = j+1
        endif
 10 continue
    tou(3) = sig(ndt+1-kp)
!
    dd= d12*( tou(1)-tou(2) )
    p = d12*( tou(1)+tou(2) )
    p = p -ptrac
!
!
! ==================================================================
! --- MISE A JOUR DES VARIABLES INTERNES DE MEMOIRE ----------------
! ==================================================================
!
! --- ENREGISTREMENT DE LA SURFACE DE CHARGE CYCLIQUE PRECEDENTE
    vin(5*kp+31) = vin(4*kp+5)
    vin(5*kp+32) = vin(4*kp+6)
    vin(5*kp+33) = vin(4*kp+7)
    vin(5*kp+34) = vin(4*kp+8)
    vin(5*kp+35) = rc
!
! --- ENREGISTREMENT DU TENSEUR DIRECTION DE MEMOIRE
    sc(1) = dd-m*p*(un-b*log(p/pcr))*(xk(1)-th(1)*rc)
    sc(2) = tou(3)-m*p*(un-b*log(p/pcr))*(xk(2)-th(2)*rc)
    qsc = sqrt(sc(1)**deux+d12*sc(2)**deux)
!
    if (abs(qsc) .gt. r8prem()) then
        vin(4*kp+7)=-sc(1)/qsc
        vin(4*kp+8)=-sc(2)/qsc
    else
        vin(4*kp+7)=zero
        vin(4*kp+8)=zero
    endif
! --- ENREGISTREMENT DU TENSEUR DEVIATOIRE MEMOIRE
    vin(4*kp+5)=dd/(m*p*(un-b*log(p/pcr)))
    vin(4*kp+6)=tou(3)/(m*p*(un-b*log(p/pcr)))
!
end subroutine
