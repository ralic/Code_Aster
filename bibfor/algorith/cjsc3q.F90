subroutine cjsc3q(sig, x, pa, qinit, q,&
                  qii, cos3tq, devnul, trac)
    implicit none
!       ================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ------------------------------------------------------------------
!     CALCUL DE COS(3*( ANGLE DE LODES POUR Q))
!     ------------------------------------------------------------------
!     IN
!          SIG      :  CONTRAINTES
!          X        :  VARIABLES ECROUI CINE
!          PA       :  PRESS ATMOSPHERIQUE ( DONNEE AMTERIAU)
!          QINIT    :   DONNEE AMTERIAU
!
!     OUT
!          Q        : DEV(SIG)-TRACE(SIG)*X
!          QII      : SQRT(QIJ*QIJ)
!          COS3TQ   : SQRT(54)*DET(Q)/(QII**3)
!          DEVNUL   : VRAI SI DEVIATEUR DE Q NUL
!          TRAC     : VRAI SI I1  NUL
!
!     ------------------------------------------------------------------
!
    include 'asterfort/cjsqij.h'
    include 'asterfort/lcdete.h'
    include 'asterfort/lcdevi.h'
    include 'asterfort/lcprsc.h'
    integer :: ndt, ndi
!
!
    common /tdim/   ndt, ndi
!
    real(kind=8) :: sig(6), x(6), pa, qinit, q(6), qii, cos3tq
    logical :: devnul, trac
    real(kind=8) :: i1, s(6), qiirel
    real(kind=8) :: detq, pref
    integer :: i
    real(kind=8) :: zero, un, trois, epssig
!
    parameter     ( zero  = 0.d0   )
    parameter     ( un    = 1.d0   )
    parameter     ( trois = 3.d0   )
    parameter     ( epssig = 1.d-8   )
!
!
!
!
!
    i1 = zero
    do 10 i = 1, ndi
        i1 = i1 + sig(i)
10  continue
!
!
    if ((i1+qinit) .eq. 0.d0) then
        i1 = -qinit+1.d-12 * pa
        pref = abs(pa)
    else
        pref = abs(i1+qinit)
    endif
!
!
!
    if ((i1+qinit) .gt. 0.d0) then
        trac = .true.
    else
        trac = .false.
    endif
!
!
    call lcdevi(sig, s)
    call cjsqij(s, i1, x, q)
    call lcprsc(q, q, qii)
    qii = sqrt(qii)
    qiirel = qii/pref
    call lcdete(q, detq)
    if (qiirel .gt. epssig) then
        devnul = .false.
        cos3tq = sqrt(54.d0)*detq/qii**trois
    else
        cos3tq = un
        devnul = .true.
    endif
!
    if (cos3tq .ge. 1.d0) cos3tq = 0.999999999999999d0
    if (cos3tq .le. -1.d0) cos3tq = -0.999999999999999d0
end subroutine
