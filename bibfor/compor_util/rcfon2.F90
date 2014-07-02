subroutine rcfon2(quest, jprol, jvale, nbvale, sigy,&
                  e, nu, p, rp, rprim,&
                  c, sieleq, dp)
!
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/utmess.h"
!
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
!
    character(len=1), intent(in) :: quest
    integer, intent(in) :: jprol
    integer, intent(in) :: jvale
    integer, intent(in) :: nbvale
    real(kind=8), optional, intent(in) :: sieleq
    real(kind=8), optional, intent(in) :: e
    real(kind=8), optional, intent(in) :: nu
    real(kind=8), optional, intent(in) :: p
    real(kind=8), optional, intent(out) :: sigy
    real(kind=8), optional, intent(out) :: rp
    real(kind=8), optional, intent(out) :: rprim
    real(kind=8), optional, intent(in) :: c
    real(kind=8), optional, intent(out) :: dp
!
! --------------------------------------------------------------------------------------------------
!
! Comportment - Utility
!
! Get information by interpolation on traction curve R(p) - Prager version
!
! --------------------------------------------------------------------------------------------------
!
! In  Quest   : type of information to get
!               'S' - Elastic yield
!               'V' - R(p), dR(p), A[R(p),p]
!               'E' - R(p), dR(p)/dp, dp, A[R(p),p]
! In  jprol   : adress of .PROL object for R(p) function
! In  jvale   : adress of .VALE object for R(p) function
! In  nbvale  : number of values in R(p) function
! Out sigy    : elastic yield
! In  e       : Young modulus
! In  nu      : Poisson coefficient
! In  sieleq  : elastic stress
! In  p       : internal variable (plastic strain)
! In  c       : Prager constant
! Out rp      : value of R(p)
! Out rprim   : value of dR(p)/dp at p
! Out dp      : cumulutated plastic strain
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: tessup
    character(len=1) :: type_prol
    character(len=24) :: func_name
    integer :: jp, jr, i, i0
    real(kind=8) :: p0, rp0, pp, equ, deuxmu, rpm
!
! --------------------------------------------------------------------------------------------------
!
    jp = jvale
    jr = jvale + nbvale
    type_prol = zk24(jprol+4)(2:2)
    func_name = zk24(jprol+5)
!
! - Elastic yield
!
    if (quest .eq. 'S') then
        sigy = zr(jr)
        goto 999
    endif
!
! - Check
!
    if (p .lt. 0) then
        call utmess('F', 'COMPOR5_59')
    endif
    tessup = .false.
!
! - PARCOURS JUSQU'A P
    do i = 1, nbvale-1
        if (p .lt. zr(jp+i)) then
            i0 = i-1
            goto 20
        endif
    end do
    tessup = .true.
    if (type_prol .eq. 'E') then
        call utmess('F', 'COMPOR5_60', sk=func_name, sr=p)
    endif
    i0=nbvale-1
 20 continue
!
! - CALCUL DES VALEURS DE R(P), R'(P) ET AIRE(P)
!
    if (quest .eq. 'V') then
        if (tessup) then
            if (type_prol .eq. 'L') then
                rprim = (zr(jr+i0)-zr(jr+i0-1))/ (zr(jp+i0)-zr(jp+i0- 1))
            else
                rprim = 0.d0
            endif
        else
            rprim = (zr(jr+i0+1)-zr(jr+i0))/ (zr(jp+i0+1)-zr(jp+i0))
        endif
!
        p0 = zr(jp+i0)
        rp0 = zr(jr+i0)
        rp = rp0 + rprim*(p-p0) - 1.5d0*c*p
        rprim = rprim - 1.5d0*c
        goto 999
    endif
!
! - RESOLUTION DE L'EQUATION R(P+DP) + 3/2*(2MU+C) DP = SIELEQ
!
    deuxmu = e/(1+nu)
    do i = i0+1, nbvale-1
        equ = zr(jr+i) + 1.5d0*(deuxmu+c)*(zr(jp+i)-p) - sieleq
        if (equ .gt. 0) then
            i0 = i-1
            goto 40
        endif
    end do
    tessup = .true.
    if (type_prol .eq. 'E') then
        call utmess('F', 'COMPOR5_60', sk=func_name, sr=p)
    endif
    i0 = nbvale-1
 40 continue
!
! - CALCUL DES VALEURS DE DP, R(P+DP), R'(P+DP)
!
    if (tessup) then
        if (type_prol .eq. 'L') then
            rprim = (zr(jr+i0)-zr(jr+i0-1))/ (zr(jp+i0)-zr(jp+i0-1))
        else
            rprim = 0.d0
        endif
    else
        rprim = (zr(jr+i0+1)-zr(jr+i0))/ (zr(jp+i0+1)-zr(jp+i0))
    endif
!
    p0 = zr(jp+i0)
    rp0 = zr(jr+i0)
    rpm = rp0+rprim*(p-p0) -1.5d0*c*p
    dp = (sieleq-rpm)/(1.5d0*deuxmu+rprim)
    pp = p+dp
    rp = rp0 + rprim*(pp-p0)-1.5d0*c*pp
    rprim = rprim - 1.5d0*c
!
999 continue
end subroutine
