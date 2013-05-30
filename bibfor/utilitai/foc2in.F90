subroutine foc2in(method, nbpts, var, fon, cste,&
                  res)
    implicit none
    character(len=*) :: method
    integer :: nbpts
    real(kind=8) :: var(*), fon(*), cste, res(*)
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
!     INTEGRATION D'UNE FONCTION PAR LA METHODE DE SIMPSON.
!     ------------------------------------------------------------------
! IN  METHOD : K  : NOM DE LA METHODE D'INTEGRATION
!                       TRAPEZES   : DISPONIBLE
!                       SIMPSON    : DISPONIBLE
! IN  NBPTS  : IS : NOMBRE DE PAS DE TEMPS
! IN  VAR    : R8 : TABLEAU DE LA VARIABLE (LES INSTANTS)
! IN  FON    : R8 : TABLEAU DE LA FONCTION A INTEGRER
! IN  CSTE   : R8 : CONSTANTE D'INTEGRATION
! OUT RES    : R8 : TABLEAU DE LA FONCTION INTEGREE
!     ------------------------------------------------------------------
!
    real(kind=8) :: fa, fm, fb
    real(kind=8) :: h1, h2, bma, deltah, epsi
    real(kind=8) :: ct1, ct2, ct3
    real(kind=8) :: zero, un, deux, quatre, six, eps
!
!     COEF(1) POUR LES IMPAIRS, COEF(2) POUR LES PAIRS
    real(kind=8) :: coef(2)
    integer :: ip(2)
!-----------------------------------------------------------------------
    integer :: i, iperm
!-----------------------------------------------------------------------
    data      ip/2,1/
!     ------------------------------------------------------------------
    zero = 0.0d0
    un = 1.0d0
    deux = 2.0d0
    quatre = 4.0d0
    six = 6.0d0
    eps = 1.0d-04
!
!
!
    if (method .eq. 'TRAPEZE') then
!
        res(1) = cste
        do 100 i = 2, nbpts
            res(i) = res(i-1) + (var(i)-var(i-1)) * (fon(i)+fon(i-1)) * 0.5d0
100      end do
!
    else if (method.eq.'SIMPSON') then
!
        fm = fon(1)
        fb = fon(2)
        h2 = var(2) - var(1)
        coef(1) = cste
        coef(2) = cste +(fb+fm)*h2/deux
        res(1) = coef(1)
        res(2) = coef(2)
        iperm = 1
        do 200 i = 3, nbpts
            h1 = h2
            h2 = var(i) - var(i-1)
            bma = h1 + h2
            fa = fm
            fm = fb
            fb = fon(i)
            if (h1 .eq. zero .or. h2 .eq. zero) then
                ct1 = un
                ct2 = quatre
                ct3 = un
            else
                deltah = h2 - h1
                if (abs( deltah / h1 ) .le. eps) then
                    ct1 = un
                    ct2 = quatre
                    ct3 = un
                else
!              EXPRESSION "SIMPLE" DES COEFFICIENTS
!              CT1  = DEUX - H2/H1
!              CT2  = (H1+H2)*(H1+H2)/(H1*H2)
!              CT3  = DEUX - H1/H2
!
!              EXPRESSION "INFORMATIQUE" DES COEFFICIENTS
                    epsi = deltah / (h1*h2)
                    ct1 = un - epsi * h2
                    ct2 = quatre + epsi * deltah
                    ct3 = un + epsi * h1
                endif
            endif
            coef(iperm) = coef(iperm) + (bma/six)*(ct1*fa+ct2*fm+ct3* fb)
            res(i) = coef(iperm)
            iperm = ip(iperm)
200      end do
!
    endif
!
end subroutine
