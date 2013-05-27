subroutine dgmmax(eb, nub, num, nuf, h,&
                  a, b1, b, mp, drp,&
                  w, c)
!
    implicit none
!
! PARAMETRES ENTRANTS
    real(kind=8) :: eb, nub, num, nuf
    real(kind=8) :: h, a, b1, b, drp
!
! PARAMETRES SORTANTS
    real(kind=8) :: d3, w, mp, c
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
! PENTES POST ELASTIQUE EN FLEXION DE GLRC_DM OPTION EPSI_MAX
! OU PENTE_LIM
!
! ----------------------------------------------------------------------
!
! PARAMETRES INTERMEDIAIRES
    real(kind=8) :: delta, x1, x2, m, f
    real(kind=8) :: d1, d2, d4, d5, a1, z
!
    m=eb/(1.d0-nub**2)*(1.d0-nub*num)
    f=eb/(1.d0-nub**2)/2.d0*(1.d0-nub*nuf)
    a1=f-m
    z=(b+m*h/2.d0)
    c=-f*h**2/4.d0-h*b1
!
    delta=z*z-4.d0*a1*c
    x1=(z-sqrt(delta))/(2.d0*a1)
    x2=(z+sqrt(delta))/(2.d0*a1)
!
    if (abs(x1) .ge. abs(x2)) then
        w=abs(x2)
    else
        w=abs(x1)
    endif
!
    d1=eb/(1.d0-nub**2)*(1.d0-nub*num)*(h/2.d0-w)+b
    d2=-eb/(1.d0-nub**2)/2.d0*(1.d0-nub*nuf)*(h**2/4.d0-w**2)-h*b1
    d3=-d2/d1
    d4=eb/(1.d0-nub**2)*(-(1.d0-nub*num)*d3/2.d0*(h**2/4.d0-w**2)+&
     &   (1.d0-nub*nuf)/3.d0*(h**3/8.d0-w**3))
    d5=h*(-b1*d3+a*h)
    c=d4+d5
    mp=c*drp
end subroutine
