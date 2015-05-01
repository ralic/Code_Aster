subroutine i3qpsp(epsi, k, f, sgt, coorsm,&
                  res, nbpt)
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/i3crad.h"
#include "asterfort/i3crqp.h"
#include "asterfort/i3pdm2.h"
#include "asterfort/utmess.h"
    integer :: k, f, nbpt
    real(kind=8) :: epsi, sgt(*), coorsm(3, *), res(3, *)
!
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!     INTERSECTION PROJ DE FACE QUAD GAUCHE DANS PLAN AVEC SGT PROJ
!     ------------------------------------------------------------------
! IN  EPSI   : R : PRECISION
! IN  K      : I : -
! IN  F      : I : NUMERO LOCALE DE LA FACE TRAITEE
! IN  SGT    : R : TABLE(1..6)      : COORDO DU  SGT DANS REPERE FACE
! IN  COORSM : R : TABLE(1..3,1..4) : COORDO DES SOMMETS PROJ
! OUT RES    : R : TABLE(1..3,1..3) : COLONE = (COORDO_REF,ABSC_SGT)
! OUT NBPT   : I : NOMBRE DE POINT TROUVE
!     ------------------------------------------------------------------
!
!
!
    integer :: arete, nd, nf, nba, i, iret
    integer :: vali(3)
    real(kind=8) :: t, ndf, zero, un, x(3), nab
    real(kind=8) :: a11, a12, a21, a22, b1, b2, r1, r2, r3, t1, t2
    real(kind=8) :: xa, ya, xb, yb, xd, yd, xf, yf, td, tf
    aster_logical :: finf, atrv, btrv, pb
!
!======================================================================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    arete = 0
    nba = 4
    zero = 0.0d0
    un = 1.0d0
    finf = .false.
    pb = .false.
    atrv = .false.
    btrv = .false.
    nbpt = 0
    x(1) = zero
    x(2) = zero
    x(3) = un
    xa = sgt(1)
    xb = sgt(4)
    ya = sgt(2)
    yb = sgt(5)
    nab = un/((xa-xb)*(xa-xb)+(ya-yb)*(ya-yb))
100 continue
    if (.not. finf) then
        arete = arete + 1
        nd = arete
        nf = max(mod(nd+1,nba+1),1)
        xd = coorsm(1,nd)
        xf = coorsm(1,nf)
        yd = coorsm(2,nd)
        yf = coorsm(2,nf)
        t = max(abs(xf),abs(xd),abs(yd),abs(yf))
        ndf = sqrt((yf-yd)*(yf-yd) +(xf-xd)*(xf-xd))
        a11 = xf - xd
        a21 = yf - yd
        a12 = xa - xb
        a22 = ya - yb
        b1 = xa -xd
        b2 = ya -yd
        if (ndf .le. epsi*t) then
            vali (1) = k
            vali (2) = f
            vali (3) = arete
            call utmess('F', 'INTEMAIL_26', ni=3, vali=vali)
        else
            r1 = max(abs(a11),abs(a12))
            if (abs(r1) .gt. epsi) then
                r1 = un/r1
                a11 = a11*r1
                a12 = a12*r1
                b1 = b1 *r1
            endif
            r1 = max(abs(a21),abs(a22))
            if (abs(r1) .gt. epsi) then
                r1 = un/r1
                a21 = a21*r1
                a22 = a22*r1
                b2 = b2 *r1
            endif
            r1 = a11*a22-a21*a12
            r2 = a11*b2 -a21*b1
            if (abs(r1) .le. epsi) then
                if (abs(r2) .le. epsi) then
                    finf = .true.
                    ndf = un/ndf
                    td = ((xd-xa)*(xb-xa)+(yd-ya)*(yb-ya))*nab
                    tf = ((xf-xa)*(xb-xa)+(yf-ya)*(yb-ya))*nab
                    t1 = max(min(td,tf),zero)
                    t2 = min(max(td,tf),un)
                    if (abs(t1-t2) .le. epsi) then
                        nbpt = 1
                        t = (t1 + t2)*0.5d0
                        r3 = (t-td)/(tf-td)
                        call i3crad(k, f, arete, nba, r3,&
                                    r1, r2)
                        res(1,1) = r1
                        res(2,1) = r2
                        res(3,1) = t
                    else if (t1 .lt. t2) then
                        nbpt = 2
                        t = t1
                        r3 = (t-td)/(tf-td)
                        call i3crad(k, f, arete, nba, r3,&
                                    r1, r2)
                        res(1,1) = r1
                        res(2,1) = r2
                        res(3,1) = t
                        t = t2
                        r3 = (t-td)/(tf-td)
                        call i3crad(k, f, arete, nba, r3,&
                                    r1, r2)
                        res(1,2) = r1
                        res(2,2) = r2
                        res(3,2) = t
                    else
                        nbpt = 0
                    endif
                endif
            else
                r1 = un/r1
                r2 = r2*r1
                r1 = (a22*b1-a12*b2)*r1
                if (abs(r1) .lt. epsi) then
                    r1 = zero
                endif
                if (abs(r2) .lt. epsi) then
                    r2 = zero
                endif
                if (abs(r2-un) .lt. epsi) then
                    r2 = un
                endif
                if (abs(r1-un) .lt. epsi) then
                    r1 = un
                endif
                if ((r1 .ge. zero) .and. ((r1-un) .le. epsi) .and. (r2 .ge. zero) .and.&
                    ((r2-un) .le. epsi)) then
                    if (nbpt .eq. 0) then
                        nbpt = nbpt + 1
                        res(3,nbpt) = r2
                        call i3crad(k, f, arete, nba, r1,&
                                    r2, r3)
                        res(1,nbpt) = r2
                        res(2,nbpt) = r3
                    else if (abs(r2 - res(3,1)) .gt. epsi) then
                        nbpt = nbpt + 1
                        res(3,nbpt) = r2
                        call i3crad(k, f, arete, nba, r1,&
                                    r2, r3)
                        res(1,nbpt) = r2
                        res(2,nbpt) = r3
                    else
                    endif
                endif
            endif
            finf = ( finf .or. (arete .eq. nba) )
        endif
        goto 100
    endif
    if (nbpt .eq. 0) then
        call i3pdm2(epsi, x, coorsm, nba, sgt,&
                    atrv)
        call i3pdm2(epsi, x, coorsm, nba, sgt(4),&
                    btrv)
        if (atrv .and. btrv) then
            r1 = sgt(1)
            r2 = sgt(2)
            call i3crqp(epsi, epsi, coorsm, r1, r2,&
                        x, iret)
            if (iret .ne. -1) then
                r1 = x(1)
                r2 = x(2)
                res(1,1) = r1
                res(2,1) = r2
                res(3,1) = zero
                r1 = sgt(4)
                r2 = sgt(5)
                call i3crqp(epsi, epsi, coorsm, r1, r2,&
                            x, iret)
                if (iret .ne. -1) then
                    r1 = x(1)
                    r2 = x(2)
                    res(1,2) = r1
                    res(2,2) = r2
                    res(3,2) = un
                else
                    pb = .true.
                endif
            else
                pb = .true.
            endif
            nbpt = 2
        endif
    else if (nbpt .eq. 1) then
        t = res(3,1)
        if ((abs(t-un) .lt. epsi) .or. (abs(t) .lt. epsi)) then
            if (abs(t-un) .lt. epsi) then
                i = 1
                t = zero
            else
                i = 4
                t = un
            endif
            call i3pdm2(epsi, x, coorsm, nba, sgt(i),&
                        atrv)
            if (atrv) then
                r1 = sgt(i)
                r2 = sgt(i+1)
                call i3crqp(epsi, epsi, coorsm, r1, r2,&
                            x, iret)
                if (iret .ne. -1) then
                    r1 = x(1)
                    r2 = x(2)
                    res(1,2) = r1
                    res(2,2) = r2
                    res(3,2) = t
                    nbpt = 2
                else
                    pb = .true.
                    nbpt = 0
                endif
            endif
        else
            call i3pdm2(epsi, x, coorsm, nba, sgt,&
                        atrv)
            call i3pdm2(epsi, x, coorsm, nba, sgt(4),&
                        btrv)
            if (atrv) then
                r1 = sgt(1)
                r2 = sgt(2)
                call i3crqp(epsi, epsi, coorsm, r1, r2,&
                            x, iret)
                if (iret .ne. -1) then
                    r1 = x(1)
                    r2 = x(2)
                    res(1,2) = r1
                    res(2,2) = r2
                    res(3,2) = zero
                    nbpt = 2
                else
                    pb = .true.
                endif
            else if (btrv) then
                r1 = sgt(4)
                r2 = sgt(5)
                call i3crqp(epsi, epsi, coorsm, r1, r2,&
                            x, iret)
                if (iret .ne. -1) then
                    r1 = x(1)
                    r2 = x(2)
                    res(1,2) = r1
                    res(2,2) = r2
                    res(3,2) = un
                    nbpt = 2
                else
                    pb = .true.
                endif
            else
                nbpt = 0
            endif
        endif
    else
    endif
    if (nbpt .eq. 2) then
        r1 = res(3,1)
        r2 = res(3,2)
        if (r2 .lt. r1) then
            res(3,1) = r2
            res(3,2) = r1
            r1 = res(1,1)
            res(1,1) = res(1,2)
            res(1,2) = r1
            r1 = res(2,1)
            res(2,1) = res(2,2)
            res(2,2) = r1
        endif
    endif
    if (pb) then
        vali (1) = k
        vali (2) = f
        call utmess('F', 'INTEMAIL_24', ni=2, vali=vali)
    endif
end subroutine
