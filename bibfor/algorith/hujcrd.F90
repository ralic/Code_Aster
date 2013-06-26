subroutine hujcrd(k, mater, sig, vin, seuild)
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
!    ---------------------------------------------------------------
!    HUJEUX:  SEUIL DU MECANISME DEVIATOIRE K(=1 A 3)
!             FD(K) = QII(K) + M*PK*RK*( 1 - B*LOG(PK/PC) )
!    ---------------------------------------------------------------
!    IN  K      : PLAN DE PROJECTION (K = 1 A 3)
!        SIG    :  CONTRAINTE
!        VIN    :  VARIABLES INTERNES = ( Q, R, X )
!    OUT SEUILD :  SEUIL DU MECANISME DEVIATOIRE K
!    ---------------------------------------------------------------
    include 'asterfort/hujprj.h'
    include 'asterfort/infniv.h'
    integer :: k, ndt, ndi
    integer :: ifm, niv
    real(kind=8) :: mater(22, 2), sig(6), vin(*), seuild
    real(kind=8) :: un, r, epsvp, pcr, pa, tole
    real(kind=8) :: degr, beta, b, m, phi, pcref, ptrac
    real(kind=8) :: sigd(3), p, q
    logical :: debug
    parameter    (un = 1.d0)
    parameter    (tole = 1.d-7)
    parameter    (degr = 0.0174532925199d0)
!
!       ------------------------------------------------------------
    common /tdim/   ndt, ndi
    common /meshuj/ debug
!
    call infniv(ifm, niv)
!
! ==================================================================
! --- VARIABLES INTERNES -------------------------------------------
! ==================================================================
    epsvp = vin(23)
    r = vin(k)
!
! ==================================================================
! --- CARACTERISTIQUES MATERIAU ------------------------------------
! ==================================================================
    beta = mater(2, 2)
    b = mater(4, 2)
    phi = mater(5, 2)
    pcref = mater(7, 2)
    pa = mater(8, 2)
    pcr = pcref*exp(-beta*epsvp)
    ptrac = mater(21,2)
    m = sin(degr*phi)
!
!
! ==================================================================
! --- PROJECTION DANS LE PLAN DEVIATEUR K ------------------------
! ==================================================================
    call hujprj(k, sig, sigd, p, q)
!
    p =p -ptrac
!
    if ((p/pa) .le. tole) then
        if (debug) write (ifm,'(A)') 'HUJCRD :: LOG(P/PA) NON DEFINI'
        seuild =-1.d0
        goto 999
    endif
!
!        IF(K.EQ.1)THEN
!         WRITE(6,*)'QK =',QK,' --- FR =',RK*(UN-B*LOG(PK/PCR))*M*PK
!        ENDIF
! ==================================================================
! --- CALCUL DU SEUIL DU MECANISME DEVIATOIRE K ------------------
! ==================================================================
    seuild = -q /m/p - r*(un-b*log(p/pcr))
!
999  continue
end subroutine
