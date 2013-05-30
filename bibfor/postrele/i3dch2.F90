subroutine i3dch2(epsi, seuil, maxitr, fk, m,&
                  r, s, iret)
    implicit none
!
    include 'asterfort/i3efk2.h'
    include 'asterfort/rvegal.h'
    integer :: maxitr, iret
    real(kind=8) :: epsi, seuil, fk(4, *), m(*), r(*), s(*)
!
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     INITIALISATION DU CALCUL DES COORDONEES DE REFERENCE DANS UNE
!     FACE QUAD PLANE PAR DICHOTOMIE-RELAXATION
!     ------------------------------------------------------------------
! IN  EPSI   : R : PRECISION
! IN  SEUIL  : R : CRITERE D' ARRET
! IN  MAXITR : I : NOMBRE MAX D' ITERATION AUTORISE
! IN  FK     : R : TABLE(1..4,1..2) DES COEF DE LA TRANSFORMATION
! IN  M      : R : TABLE(1..2)      DES COORDONNEES DU POINT
! OUT R      : R : TABLE(1..2)      INTERVALE DE LA COORDO REF 1
! OUT S      : R : TABLE(1..2)      INTERVALE DE LA COORDO REF 2
! OUT IRET   : I : CODE RETOUR :  0 --> SOLUTION TROUVER
!                                 I --> NBR ITER UTILISE
!                                -1 --> DEGENERESCENCE
!     ------------------------------------------------------------------
!
    integer :: i
    real(kind=8) :: valfk(3, 4), valr(4), vals(4), un, zero
    real(kind=8) :: x0, y0, a, b, c, d, rsup, rinf, ssup, sinf, t1, t2
    logical :: fini, egalr, egals
!
!======================================================================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    fini = .false.
    x0 = m(1)
    y0 = m(2)
    i = 1
    iret = 1
    un = 1.0d0
    zero = 0.0d0
    rsup = un
    rinf = -un
    ssup = un
    sinf = -un
10  continue
    if (.not. fini) then
        valr(1) = rinf
        valr(2) = rsup
        valr(3) = rsup
        valr(4) = rinf
        vals(1) = sinf
        vals(2) = sinf
        vals(3) = ssup
        vals(4) = ssup
        call i3efk2(fk, 4, valr, vals, valfk)
        d = un/(rinf - rsup)
        a = valfk(1,2)
        b = valfk(1,1)
        c = a
        a = (b - a)*d
        b = (c*rinf - b*rsup)*d
        t1 = (x0 - b)/a
        a = valfk(1,3)
        b = valfk(1,4)
        c = a
        a = (b - a)*d
        b = (c*rinf - b*rsup)*d
        t2 = (x0 - b)/a
        rsup = max(-un,min( un,max(t1,t2)))
        rinf = min( un,max(-un,min(t1,t2)))
        d = un/(sinf - ssup)
        a = valfk(2,3)
        b = valfk(2,2)
        c = a
        a = (b - a)*d
        b = (c*sinf - b*ssup)*d
        t1 = (y0 - b)/a
        a = valfk(2,4)
        b = valfk(2,1)
        c = a
        a = (b - a)*d
        b = (c*sinf - b*ssup)*d
        t2 = (y0 - b)/a
        ssup = max(-un,min( un,max(t1,t2)))
        sinf = min( un,max(-un,min(t1,t2)))
        call rvegal(epsi, 'R', rsup, rinf, egalr,&
                    a)
        call rvegal(epsi, 'R', ssup, sinf, egals,&
                    a)
        if (egalr) then
            iret = 0
            rsup = 0.5d0*(rsup + rinf)
            rinf = rsup
            if (.not. egals) then
                b = fk(1,2)+fk(2,2)*rsup
                a = fk(3,2)+fk(4,2)*rsup
                if (a .eq. zero) then
                    iret = -1
                    fini = .true.
                else
                    ssup = (y0 - b)/a
                    sinf = ssup
                endif
            endif
        endif
        if (egals) then
            iret = 0
            ssup = 0.5d0*(ssup + sinf)
            sinf = ssup
            if (.not. egalr) then
                b = fk(1,1)+fk(3,1)*ssup
                a = fk(2,1)+fk(4,1)*ssup
                if (a .eq. zero) then
                    iret = -1
                    fini = .true.
                else
                    rsup = (x0 - b)/a
                    rinf = rsup
                endif
            endif
        endif
        d = max(abs(rinf-rsup),abs(sinf-ssup))
        if (d .gt. seuil) then
            i = i + 1
        else
            fini = .true.
        endif
        fini = (egalr .or. egals .or. fini .or. (i .gt. maxitr) )
        goto 10
    endif
    r(1) = rinf
    r(2) = rsup
    s(1) = sinf
    s(2) = ssup
    if ((iret .ne. 0) .and. (iret .ne. -1)) then
        iret = i
    endif
end subroutine
