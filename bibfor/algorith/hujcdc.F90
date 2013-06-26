subroutine hujcdc(k, mater, sig, vin, seuil)
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
!    HUJEUX:  SEUIL DU MECANISME DEVIATOIRE CYCLIQUE K(=1 A 3)
!             FD(K) = QIIC(K) + M*PK*RK*( 1 - B*LOG(PK/PC) )
!             QIIC(K)=QII(TOU(K)+(X(K)-TH*RK)*PK*(1-B*LOG(PK/PC)*M))
!    ---------------------------------------------------------------
!    IN  K      :  PLAN DE PROJECTION K = 1, 2 OU 3
!        MATER  :  COEFFICIENTS MATERIAU
!        SIG    :  CHAMPS DE CONTRAINTES A T
!        VIN    :  VARIABLES INTERNES A T
!    OUT SEUIL  :  SEUIL DU MECANISME DEVIATOIRE
!   ------------------------------------------------------------------
    include 'asterfort/infniv.h'
    integer :: k, ndt, ndi
    integer :: ifm, niv
    integer :: i, j
    real(kind=8) :: mater(22, 2), sig(6), vin(*), seuil
    real(kind=8) :: un, r, epsvp, pcr, pa, tole
    real(kind=8) :: degr, beta, b, m, phi, pcref, ptrac
    real(kind=8) :: p, q, x(2)
    real(kind=8) :: tou(3), th(2), touc(2)
    real(kind=8) :: d12, dd, deux
    logical :: debug
    parameter    (un = 1.d0)
    parameter    (tole = 1.d-7)
    parameter    (degr = 0.0174532925199d0)
!       ------------------------------------------------------------
    common /tdim/   ndt, ndi
    common /meshuj/ debug
!
    data   d12, deux /0.5d0, 2.d0/
!
    call infniv(ifm, niv)
!
!
! ==================================================================
! --- VARIABLES INTERNES -------------------------------------------
! ==================================================================
    epsvp = vin(23)
    r = vin(4+k)
    x(1) = vin(5+4*k)
    x(2) = vin(6+4*k)
    th(1) = vin(7+4*k)
    th(2) = vin(8+4*k)
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
    pcr = pcref*exp(-beta*epsvp)
    ptrac = mater(21,2)
    m = sin(degr*phi)
!
!
! ==================================================================
! --- PROJECTION DANS LE PLAN DEVIATEUR K --------------------------
! ==================================================================
    j = 1
    do 10 i = 1, ndi
        if (i .ne. k) then
            tou(j) = sig(i)
            j = j+1
        endif
10  continue
!
    tou(3) = sig(ndt+1-k)
    dd = d12*( tou(1)-tou(2) )
!
!
! ==================================================================
! --- CALCUL DE PK, QCK --------------------------------------------
! ==================================================================
    p = d12*( tou(1)+tou(2) )
    p = p -ptrac
    tou(1) = dd
    tou(2) = -dd
!
    if ((p/pa) .le. tole) then
        if (debug) write (ifm,'(A)') 'HUJCDC :: LOG(P/PA) NON DEFINI'
        seuil=-1.d0
        goto 999
    endif
!
    touc(2) = tou(3)-(x(2)-r*th(2))*p*(un-b*log(p/pcr))*m
    touc(1) = tou(1)-(x(1)-r*th(1))*p*(un-b*log(p/pcr))*m
!
    q = touc(1)**deux + (touc(2)**deux)/deux
    q = sqrt(q)
!
! ==================================================================
! --- CALCUL DU SEUIL DU MECANISME CYCLIQUE DEVIATOIRE K -----------
! ==================================================================
    seuil = -q /m/p - r*(un-b*log(p/pcr))
!
999  continue
end subroutine
