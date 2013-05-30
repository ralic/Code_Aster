subroutine i3icfp(epsi, fglo, sgt, floc1, floc2,&
                  floc3, typf, nbpt, iret)
    implicit none
!
    include 'asterfort/i3icfs.h'
    integer :: nbpt, typf, iret
    real(kind=8) :: epsi, fglo(3, *), sgt(*), floc1(*), floc2(*), floc3(*)
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
!     INTERSECTION SGT COUPR DE FACE GAUCHE
!     ------------------------------------------------------------------
! IN  EPSI   : R : PRECISION
! IN  FGLO   : R : TABLE(1..3,1..3) : COORDO DE LA COUPE DANS (S1,EI)
! IN  SGT    : R : TABLE(1..6)      : COORDO DU SEGMENT  DANS (S1,EI)
! VAR FLOC1  : R : TABLE(1..3)
!            :   :   IN  : COORDO DEBUT COUPE DANS REPERE COUPE
!            :   :   OUT : REPERAGE POINT 1 CALCULE (PARAB,SGT)
! VAR FLOC2  : R : TABLE(1..3)
!            :   :   IN  : COORDO FIN COUPE DANS REPERE COUPE
!            :   :   OUT : REPERAGE POINT 2 CALCULE (PARAB,SGT)
! IN  FLOC3  : R : TABLE(1..3) : COORDO MILIEU COUPE DANS REPERE COUPE
! OUT TYPF   : I : CODE DE CONVEXITE DE LA FACE PAR RAPPORT A LA COUPE
! OUT NBPT   : I : NBR DE POINT CALCULES (-2 = INFINI)
! OUT IRET   : I : CODE RETOUR -1 = DEGENERESCE
!     ------------------------------------------------------------------
!     PARAMETRAGE COUPE = (-1,1)
!     PARAMETRAGE SGT   = ( 0,1)
!     ------------------------------------------------------------------
!
    real(kind=8) :: zero, unsur2, un, deux
    real(kind=8) :: x1, x2, y1, y2, x3, y3, xa, ya, xb, yb, dx, dy, a, b, c, d
    real(kind=8) :: r1, r2, t1, t2
    logical :: okt1, okt2, okr1, okr2
!
!======================================================================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    zero = 0.0d0
    unsur2 = 0.5d0
    un = 1.0d0
    deux = 2.0d0
    iret = 0
    nbpt = 0
    typf = 0
    xa = sgt(1)
    ya = sgt(2)
    xb = sgt(4)
    yb = sgt(5)
    x1 = fglo(1,1)
    y1 = fglo(2,1)
    x2 = fglo(1,2)
    y2 = fglo(2,2)
    x3 = fglo(1,3)
    y3 = fglo(2,3)
    dx = un/(floc2(1)-floc1(1))
    xa = ((xa-x1)*(x2-x1)+(ya-y1)*(y2-y1))*dx
    xb = ((xb-x1)*(x2-x1)+(yb-y1)*(y2-y1))*dx
    ya = sgt(3)
    yb = sgt(6)
    x1 = floc1(1)
    y1 = floc1(2)
    x2 = floc2(1)
    y2 = floc2(2)
    x3 = floc3(1)
    y3 = floc3(2)
    d = (x3-x1)*(y2-y1) - (x2-x1)*(y3-y1)
    r1 = max(abs(x1),abs(x2),abs(x3),abs(y1),abs(y2),abs(y3))
    r2 = max(abs(xa),abs(xb),abs(ya),abs(yb))
    if (abs(d) .lt. epsi*r1) then
        call i3icfs(epsi, fglo, sgt, floc1, floc2,&
                    nbpt, iret)
        typf = 0
    else
        if (d .ge. zero) then
            typf = -1
        else
            typf = 1
        endif
        dx = xb - xa
        dy = yb - ya
        a = (unsur2*(x2+x1)-x3)*dy - (unsur2*(y2+y1)-y3)*dx
        b = unsur2*(dy*(x2-x1)-dx*(y2-y1))
        c = dy*(x3-xa)-dx*(y3-ya)
        d = max(abs(a),abs(b),abs(c))
        if (abs(d) .le. epsi*r1*r2) then
            iret = -1
            nbpt = 0
        else
            d = un/d
            a = a*d
            b = b*d
            c = c*d
            d = b*b - (deux+deux)*a*c
            if (abs(a) .gt. epsi) then
                if (d .gt. epsi) then
                    a = unsur2/a
                    r1 = a*(-b-sqrt(d))
                    r2 = a*(-b+sqrt(d))
                    okr1 = (((-un-epsi).le.r1).and.(r1.le.(un+epsi)))
                    okr2 = (((-un-epsi).le.r2).and.(r2.le.(un+epsi)))
                    if (okr1 .or. okr2) then
                        b = (x2-x1)*unsur2
                        c = (unsur2*(x1+x2)-x3)
                        x1 = x3 + r1*(b + r1*c)
                        x2 = x3 + r2*(b + r2*c)
                        b = (y2-y1)*unsur2
                        c = (unsur2*(y1+y2)-y3)
                        y1 = y3 + r1*(b + r1*c)
                        y2 = y3 + r2*(b + r2*c)
                        d = (xa-xb)*(xa-xb)+(ya-yb)*(ya-yb)
                        if (abs(d) .gt. epsi) then
                            d = un/d
                            t1 = ((x1-xa)*dx+(y1-ya)*dy)*d
                            t2 = ((x2-xa)*dx+(y2-ya)*dy)*d
                            okt1 = ((-epsi.le.t1).and.(t1.le.(un+epsi) ))
                            okt2 = ((-epsi.le.t2).and.(t2.le.(un+epsi) ))
                            if (okt1 .and. okr1) then
                                if (okt2 .and. okr2) then
                                    nbpt = 2
                                    if (t1 .gt. t2) then
                                        d = t1
                                        t1 = t2
                                        t2 = d
                                        d = r1
                                        r1 = r2
                                        r2 = d
                                    endif
                                else
                                    nbpt = 1
                                endif
                            else if (okt2 .and. okr2) then
                                nbpt = 1
                                t1 = t2
                                r1 = r2
                            else
                                nbpt = 0
                            endif
                        else
                            iret = -1
                            nbpt = 0
                        endif
                    else
                        nbpt = 0
                    endif
                else
                    nbpt = 0
                endif
            else
                if (abs(b) .le. epsi*r1*r2) then
                    nbpt = 0
                else
                    r1 = -c/b
                    okr1 = ( (-epsi .le. r1) .and. (r1 .le. (un+epsi)) )
                    if (okr1) then
                        d = (xa-xb)*(xa-xb)+(ya-yb)*(ya-yb)
                        if (abs(d) .gt. epsi) then
                            d = un/d
                            t1 = ((x1-xa)*dx+(y1-ya)*dy)*d
                            t2 = ((x2-xa)*dx+(y2-ya)*dy)*d
                            okt1 = ((-epsi.le.t1).and.(t1.le.(un+epsi) ))
                            if (okt1) then
                                nbpt = 1
                            else
                                nbpt = 0
                            endif
                        else
                            nbpt = 0
                            iret = -1
                        endif
                    else
                        nbpt = 0
                    endif
                endif
            endif
        endif
    endif
    if (typf .ne. 0) then
        if (nbpt .ge. 1) then
            floc1(1) = r1
            floc1(2) = t1
        endif
        if (nbpt .ge. 2) then
            floc2(1) = r2
            floc2(2) = t2
        endif
    endif
end subroutine
