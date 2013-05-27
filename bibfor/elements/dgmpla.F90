subroutine dgmpla(eb, nub, ea, sya, num,&
                  nuf, h, a, b1, b,&
                  nnap, rx, ry, mp, drp,&
                  w)
!
    implicit   none
!
! - PARAMETRES ENTRANTS
!
    real(kind=8) :: eb, nub, ea(*), sya(*), num, nuf
    real(kind=8) :: h, a, b1, b, mp, rx(*), ry(*)
    integer :: nnap, ilit1
!
! - PARAMETRES SORTANTS
    real(kind=8) :: drp, d3, w
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! person_in_charge: sebastien.fayolle at edf.fr
! ----------------------------------------------------------------------
!
! BUT : CALCUL DES INFORMATIONS NECESSAIRES A LA DETERMINATION DES
! PENTES POST ELASTIQUE EN FLEXION DE GLRC_DM OPTION PLAS
!
! ----------------------------------------------------------------------
!
! - PARAMETRES INTERMEDIAIRES
    real(kind=8) :: delta, x1, x2, m, f, p
    real(kind=8) :: d1, d2, d4, d5, a1, z, c
!
    m=eb/(1.d0-nub**2)*(1.d0-nub*num)
    f=eb/(1.d0-nub**2)*(1.d0-nub*nuf)
    a1=(f+m)*h**2
    z=-(b*h+m*h**2/2.d0)
    c=-f*h**2/4.d0-h*b1
!
    delta=z*z-4.d0*a1*c
    x1=(z-sqrt(delta))/(2.d0*a1)
    x2=(z+sqrt(delta))/(2.d0*a1)
!
    if ((x1 .gt. 0.d0) .and. (x1 .lt. 1.d0)) then
        w=x1
    else if ((x2 .gt. 0.d0) .and. (x2 .lt. 1.d0)) then
        w=x2
    endif
!
    d1=eb/(1.d0-nub**2)*(1.d0-nub*num)*h/2.d0*(1.d0-2.d0*w)+b
    d2=-eb/(1.d0-nub**2)*(1.d0-nub*nuf)*h**2/8.d0*&
     &   (1.d0-4.d0*w**2)-h*b1
    d3=-d2/d1
    p=(rx(1)+ry(1))/2.d0
    drp=abs(sya(1)/(ea(1)*(d3+p*h)))
!
    do 10, ilit1 = 1,nnap
    p=(rx(ilit1)+ry(ilit1))/2.d0
    if (abs(sya(ilit1)/(ea(ilit1)*(d3-p*h))) .lt. drp) then
        drp=abs(sya(ilit1)/(ea(ilit1)*(d3-p*h)))
    endif
    10 end do
!
    ilit1 = nnap
    d4=eb/(1.d0-nub**2)*(-(1.d0-nub*num)*d3*h**2/8.d0*&
     &   (1.d0-4.d0*w**2)+(1.d0-nub*nuf)*h**3/24.d0*(1.d0-8.d0*w**3))
    d5=h*(-b1*d3+a*h)
    c=d4+d5
    mp=c*drp
!
end subroutine
