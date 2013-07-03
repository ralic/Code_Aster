subroutine hujpxd(k, mater, sig, vin, prox,&
                  proxc)
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    ---------------------------------------------------------------
!    HUJEUX:  CRITERE DE PROXIMITE POUR LES SEUILS DEVIATOIRES
!                        MONOTONES ET CYCLIQUES
!             FD(K) = QII(K) + M*PK*RK*( 1 - B*LOG(PK/PC) )
!    ---------------------------------------------------------------
!    IN  K      :  PLAN DE PROJECTION CONSIDERE (K = 1 A 3)
!        SIG    :  TENSEUR DES CONTRAINTES
!        MATER  :  PARAMETRES MATERIAU
!        VIN    :  VARIABLES INTERNES = ( R, X )
!    OUT PROX   :  CRITERE DE PROXIMITE
!                  .TRUE. MECANISMES ASSEZ PROCHES POUR ACTIVER
!                  LE MECANISME MONOTONE
!    ---------------------------------------------------------------
#include "asterfort/hujprj.h"
#include "asterfort/infniv.h"
    integer :: k, ndt, ndi
    integer :: ifm, niv
    real(kind=8) :: mater(22, 2), sig(6), vin(*)
    real(kind=8) :: un, r, epsvp, pcr, pa, tole1, tole2
    real(kind=8) :: degr, beta, b, m, phi, pcref, ptrac
    real(kind=8) :: sigd(3), p, q, dist, rh
    logical :: debug, prox, proxc
    parameter    (un = 1.d0)
    parameter    (tole1 = 1.d-6)
    parameter    (tole2 = 1.d-7)
    parameter    (degr = 0.0174532925199d0)
!
    common /tdim/   ndt, ndi
    common /meshuj/ debug
!
    call infniv(ifm, niv)
!
!
! ==================================================================
! --- VARIABLES INTERNES -------------------------------------------
! ==================================================================
    epsvp = vin(23)
    rh = vin(k-4)
!
!
! ==================================================================
! --- CARACTERISTIQUES MATERIAU ------------------------------------
! ==================================================================
    beta = mater(2, 2)
    b = mater(4, 2)
    phi = mater(5, 2)
    pcref = mater(7, 2)
    pa = mater(8, 2)
    ptrac = mater(21,2)
    pcr = pcref*exp(-beta*epsvp)
    m = sin(degr*phi)
!
!
! ==================================================================
! --- PROJECTION DANS LE PLAN DEVIATEUR K ------------------------
! ==================================================================
    call hujprj(k-4, sig, sigd, p, q)
    if (((p -ptrac)/pa) .le. tole2) then
        if (debug) write (ifm,'(A)') 'HUJPXD :: LOG(P/PA) NON DEFINI'
        prox = .false.
        goto 999
    endif
!
!
! ==================================================================
! --- CALCUL DU SEUIL DU MECANISME DEVIATOIRE K ------------------
! ==================================================================
    r = -q /( m*(p -ptrac)*(un-b*log((p -ptrac)/pcr)) )
    dist = abs(r-rh)/rh
!
    if (dist .lt. 1.d-5) then
        prox = .true.
    else
        prox = .false.
    endif
!
! ==================================================================
! --- SEUIL CYCLIQUE ELASTIQUE  + TANGENT AU SEUIL MONOTONE --------
! ==================================================================
    rh = sqrt(vin(4*k-11)**2+(vin(4*k-10)**2)/2.d0)
    if (rh .gt. tole1) then
        dist = abs(r-rh)/rh
    else
        dist = abs(r-rh)
    endif
!
    if ((dist .lt. tole1) .and. (vin(k).eq.mater(18,2))) then
        proxc = .true.
    endif
!
999  continue
end subroutine
