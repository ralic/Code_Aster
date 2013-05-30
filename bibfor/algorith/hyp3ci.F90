subroutine hyp3ci(c11, c22, c33, c12, c13,&
                  c23, c10, c01, c20, siso,&
                  codret)
!
! ======================================================================
! COPYRIGHT (C) 2005 UCBL LYON1 - T. BARANGER     WWW.CODE-ASTER.ORG
! COPYRIGHT (C) 2007 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    real(kind=8) :: c11, c22, c33
    real(kind=8) :: c12, c13, c23
    real(kind=8) :: c10, c01, c20
    real(kind=8) :: siso(6)
    integer :: codret
!
! ----------------------------------------------------------------------
!
! LOI DE COMPORTEMENT HYPERELASTIQUE DE SIGNORINI
!
! 3D - CALCUL DES CONTRAINTES - PARTIE ISOTROPIQUE
!
! ----------------------------------------------------------------------
!
!
! IN  C11,C22,C33,C12,C13,C23 : ELONGATIONS
! IN  C10,C01,C20             : CARACTERISTIQUES MATERIAUX
! OUT SISO   : CONTRAINTES ISOTROPIQUES
! OUT CODRET : CODE RETOUR ERREUR INTEGRATION (1 SI PROBLEME, 0 SINON)
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: grd(6)
    real(kind=8) :: t1, t3, t5, t7
    real(kind=8) :: t10, t12, t13, t14, t15, t17, t18, t19
    real(kind=8) :: t20, t23, t26, t27, t29
    real(kind=8) :: t33, t40, t43, t46
    real(kind=8) :: t56, t59, t69, t73, t89, t104
!
! ----------------------------------------------------------------------
!
    t1 = c11*c22
    t3 = c23**2
    t5 = c12**2
    t7 = c12*c13
    t10 = c13**2
    t12 = t1*c33-c11*t3-t5*c33+2*t7*c23-t10*c22
    t13 = t12**(1.d0/3.d0)
!
    if ((t12.eq.0.d0) .or. (t13.eq.0.d0)) then
        codret = 1
        goto 99
    endif
!
    t14 = 1.d0/t13
    t15 = c11+c22+c33
    t17 = 1.d0/t13/t12
    t18 = t15*t17
    t19 = c22*c33
    t20 = t19-t3
    t23 = t14-t18*t20/3.d0
    t26 = t13**2
!
    if ((t15.eq.0.d0) .or. (t26.eq.0.d0)) then
        codret=1
        goto 99
    endif
!
    t27 = 1.d0/t26
    t29 = c11*c33
    t33 = (t1+t29+t19-t5-t10-t3)/t26/t12
    t40 = c20*(t15*t14-3.d0)
    t43 = t29-t10
    t46 = t14-t18*t43/3.d0
    t56 = t1-t5
    t59 = t14-t18*t56/3.d0
    t69 = c10*t15
    t73 = -2.d0*c12*c33+2*c13*c23
    t89 = 2.d0*c12*c23-2*c13*c22
    t104 = -2.d0*c11*c23+2*t7
!
    grd(1) = c10*t23+ c01*((c22+c33)*t27-2.d0/3.d0*t33*t20)+ 2.d0*t40*t23
    grd(2) = c10*t46+ c01*((c11+c33)*t27-2.d0/3.d0*t33*t43)+ 2.d0*t40*t46
    grd(3) = c10*t59+ c01*((c11+c22)*t27-2.d0/3.d0*t33*t56)+ 2.d0*t40*t59
    grd(4) = -t69*t17*t73/3.d0+ c01*(-2.d0*c12*t27-2.d0/3.d0*t33*t73 )- 2.d0/3.d0*t40*t18*t73
    grd(5) = -t69*t17*t89/3.d0+ c01*(-2.d0*c13*t27-2.d0/3.d0*t33*t89 )- 2.d0/3.d0*t40*t18*t89
    grd(6) = -t69*t17*t104/3.d0+ c01*(-2.d0*c23*t27-2.d0/3.d0*t33*t104 )- 2.d0/3.d0*t40*t18*t104
    siso(1) = 2.d0*grd(1)
    siso(2) = 2.d0*grd(2)
    siso(3) = 2.d0*grd(3)
    siso(4) = 2.d0*grd(4)
    siso(5) = 2.d0*grd(5)
    siso(6) = 2.d0*grd(6)
99  continue
end subroutine
