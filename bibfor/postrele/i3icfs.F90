subroutine i3icfs(epsi, fglo, sgt, floc1, floc2,&
                  nbpt, iret)
    implicit none
!
    integer :: nbpt
    real(kind=8) :: epsi, fglo(3, *), sgt(*), floc1(*), floc2(*)
!
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!     INTERSECTION SGT COUPE DE FACE GAUCHE
!     ------------------------------------------------------------------
! IN  EPSI   : R : PRECISION
! IN  FGLO   : R : TABLE(1..3,1..3) : COORDO DE LA COUPE DANS (S1,EI)
! IN  SGT    : R : TABLE(1..6)      : COORDO DU SEGMENT  DANS (S1,EI)
! VAR FLOC1  : R : TABLE(1..3)
!            :   :   IN  : COORDO DEBUT COUPE DANS REPERE COUPE
!            :   :   OUT : REPERAGE POINT 1 CALCULE (ABSC_CURV)
! VAR FLOC2  : R : TABLE(1..3)
!            :   :   IN  : COORDO FIN COUPE DANS REPERE COUPE
!            :   :   OUT : REPERAGE POINT 2 CALCULE (ABSC_CURV)
! OUT NBPT   : I : NBR DE POINT CALCULES (-2 = INFINI)
! OUT IRET   : I : CODE RETOUR -1 = DEGENERESCENCE
!     ------------------------------------------------------------------
!     PARAMETRAGE COUPE = (-1,1)
!     PARAMETRAGE SGT   = ( 0,1)
!     ------------------------------------------------------------------
!
    real(kind=8) :: zero, unsur2, un, deux
    real(kind=8) :: x1, x2, y1, y2, xa, ya, xb, yb, d, a11, a12, a21, a22, b1
    real(kind=8) :: b2
!
!======================================================================
!
!-----------------------------------------------------------------------
    integer :: iret
!-----------------------------------------------------------------------
    zero = 0.0d0
    unsur2 = 0.5d0
    un = 1.0d0
    deux = 2.0d0
    iret = 0
    nbpt = 0
    xa = sgt(1)
    ya = sgt(2)
    xb = sgt(4)
    yb = sgt(5)
    x1 = fglo(1,1)
    y1 = fglo(2,1)
    x2 = fglo(1,2)
    y2 = fglo(2,2)
    d = un/(floc2(1)-floc1(1))
    xa = ((xa-x1)*(x2-x1)+(ya-y1)*(y2-y1))*d
    xb = ((xb-x1)*(x2-x1)+(yb-y1)*(y2-y1))*d
    ya = sgt(3)
    yb = sgt(6)
    x1 = floc1(1)
    y1 = floc1(2)
    x2 = floc2(1)
    y2 = floc2(2)
    a11 = xb-xa
    a21 = yb-ya
    a12 = (x1-x2)*unsur2
    a22 = (y1-y2)*unsur2
    b1 = (x1+x2)*unsur2 - xa
    b2 = (y1+y2)*unsur2 - ya
    d = max(abs(a11),abs(a12))
    if (abs(d) .gt. epsi) then
        d = un/d
        a11 = a11*d
        a12 = a12*d
        b1 = b1 *d
    endif
    d = max(abs(a21),abs(a22))
    if (abs(d) .gt. epsi) then
        d = un/d
        a21 = a21*d
        a22 = a22*d
        b2 = b2 *d
    endif
    if (iret .ne. -1) then
        d = a11*a22 - a12*a21
        if (abs(d) .le. epsi) then
            d = a11*b2 - a21*b1
            if (abs(d) .le. epsi) then
                d = un/(x2-x1)
                b1 = deux*(xa - x1)*d - un
                b2 = deux*(xb - x1)*d - un
                d = b1
                b1 = min(b1,b2)
                b2 = max(d ,b2)
                b1 = max(-un,b1)
                b2 = min( un,b2)
                if (b1 .ge. b2) then
                    nbpt = 0
                else
                    d = un/(xb-xa)
                    y1 = (x1-xa)*d
                    y2 = (x2-xa)*d
                    y1 = max(y1,zero)
                    y2 = min(y2,un)
                    if (y1 .ge. y2) then
                        nbpt = 0
                    else
                        nbpt = -2
                        floc1(1) = b1
                        floc1(2) = y1
                        floc2(1) = b2
                        floc2(2) = y2
                    endif
                endif
            else
                nbpt = 0
            endif
        else
            d = un/d
            x1 = (b1*a22 - b2*a12)*d
            x2 = (b2*a11 - b1*a21)*d
            if (( -epsi .le. x1) .and. (x1 .le. (un+epsi)) .and. (-(un+epsi) .le. x2) .and.&
                (x2 .le. (un+epsi))) then
                nbpt = 1
                floc1(1) = x2
                floc2(2) = x1
            else
                nbpt = 0
            endif
        endif
    endif
end subroutine
