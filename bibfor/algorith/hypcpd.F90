subroutine hypcpd(c11, c22, c33, c12, k,&
                  c10, c01, c20, dsidep, codret)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 2005 UCBL LYON1 - T. BARANGER     WWW.CODE-ASTER.ORG
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
! TOLE CRP_20
!
    implicit none
    real(kind=8) :: c11, c22, c33, c12
    real(kind=8) :: k
    real(kind=8) :: c10, c01, c20
    real(kind=8) :: dsidep(6, 6)
    integer :: codret
!
! ----------------------------------------------------------------------
!
! LOI DE COMPORTEMENT HYPERELASTIQUE DE SIGNORINI
!
! C_PLAN - CALCUL DE LA MATRICE TANGENTE
!
! ----------------------------------------------------------------------
!
! IN  C11,C22,C33,C12: ELONGATIONS
! IN  C10,C01,C20:     CARACTERISTIQUES MATERIAUX
! IN  K      : MODULE DE COMPRESSIBILITE
! OUT DSIDEP : MATRICE TANGENTE
! OUT CODRET : CODE RETOUR ERREUR INTEGRATION (1 SI PROBLEME, 0 SINON)
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: t1, t2, t3, t5, t6, t8, t10, t12, t13, t28, t45
    real(kind=8) :: t17, t31, t36, t20, t15, t24, t42, t16, t18, t34, t35
    real(kind=8) :: t14, t22, t47, t57, t48, t52, t33
    real(kind=8) :: t60, t62, t55, t56, t59, t53, t54, t21, t25
    real(kind=8) :: t19, t27, t38, t29, t30, t7, t37, t26, t43, t75, t77
    real(kind=8) :: t78, t49, t66, t69, t58, t71, t74, t32, t72, t9, t68
    real(kind=8) :: t41, t46, t76, t44, t23
    real(kind=8) :: t79, t81, t82, t73
!
! ----------------------------------------------------------------------
!
!
    t1=c11*c22
    t3=c12**2
    t5=t1*c33-t3*c33
    t6=t5**(1.d0/3.d0)
    if ((t5.le.0.d0)) then
        codret=1
        goto 10
!
    endif
    t8=1.d0/t6/t5
    t12=c11+c22+c33
    t13=t5**2
    t17=c22**2
    t18=c33**2
    t19=t17*t18
    t22=-2.d0/3.d0*t8*c22*c33+4.d0/9.d0*t12/t6/t13*t19
    t26=t6**2
    t30=c22*c33
    t43=1.d0/t6
    t48=(t43-t12*t8*t30/3.d0)**2.d0
    t59=sqrt(t5)
    t62=t59**2
    dsidep(1,1)=4.d0*c10*t22+4.d0*c01*&
     &            (-4.d0/3.d0*(c22+c33)/t26/t5*t30+&
     &            10.d0/9.d0*(t1+c11*c33+t30-t3)/t26/t13*t19)+&
     &            8.d0*c20*t48+8.d0*c20*(t12*t43-3.d0)*t22+k/t5*t19-&
     &            k*(t59-1.d0)/t62/t59*t17*t18
!
    t1=c11*c22
    t3=c12**2
    t5=t1*c33-t3*c33
    t6=t5**(1.d0/3.d0)
    if ((t5.le.0.d0)) then
        codret=1
        goto 10
!
    endif
    t8=1.d0/t6/t5
    t15=c11+c22+c33
    t16=t5**2
    t20=c33**2
    t22=c22*t20*c11
    t25=t15*t8
    t28=-t8*c11*c33/3.d0-t8*c22*c33/3.d0+4.d0/9.d0*t15/t6/t16*t22-&
     &    t25*c33/3.d0
    t31=t6**2
    t35=1.d0/t31/t5
    t37=c11*c33
    t42=c22*c33
    t45=t1+t37+t42-t3
    t57=1.d0/t6
    t75=sqrt(t5)
    t77=k*(t75-1.d0)
    t78=t75**2
    dsidep(1,2)=4.d0*c10*t28+4.d0*c01*&
     &            (1.d0/t31-2.d0/3.d0*(c22+c33)*t35*t37-&
     &            2.d0/3.d0*(c11+c33)*t35*t42+&
     &            10.d0/9.d0*t45/t31/t16*t22-2.d0/3.d0*t45*t35*c33)+&
     &            8.d0*c20*(t57-t25*t37/3.d0)*(t57-t25*t42/3.d0)+&
     &            8.d0*c20*(t15*t57-3.d0)*t28+k/t5*t22-t77/t78/t75*t22+&
     &            2.d0*t77/t75*c33
!
    t1=c11*c22
    t3=c12**2
    t5=t1*c33-t3*c33
    t6=t5**(1.d0/3.d0)
    if ((t5.le.0.d0)) then
        codret=1
        goto 10
!
    endif
    t8=1.d0/t6/t5
    t9=t1-t3
    t15=c11+c22+c33
    t16=t5**2
    t20=c22*c33
    t21=t20*t9
    t24=t15*t8
    t27=-t8*t9/3.d0-t8*c22*c33/3.d0+4.d0/9.d0*t15/t6/t16*t21-&
     &    t24*c22/3.d0
    t30=t6**2
    t34=1.d0/t30/t5
    t43=t1+c11*c33+t20-t3
    t55=1.d0/t6
    t73=sqrt(t5)
    t75=k*(t73-1.d0)
    t76=t73**2
    dsidep(1,3)=4.d0*c10*t27+4.d0*c01*&
     &            (1.d0/t30-2.d0/3.d0*(c22+c33)*t34*t9-&
     &            2.d0/3.d0*(c11+c22)*t34*t20+&
     &            10.d0/9.d0*t43/t30/t16*t21-2.d0/3.d0*t43*t34*c22)+&
     &            8.d0*c20*(t55-t24*t9/3.d0)*(t55-t24*t20/3.d0)+&
     &            8.d0*c20*(t15*t55-3.d0)*t27+k/t5*t21-t75/t73/t76*t21+&
     &            2.d0*t75/t73*c22
!
!
!
    t1=c11*c22
    t3=c12**2
    t5=t1*c33-t3*c33
    t6=t5**(1.d0/3.d0)
    if ((t5.le.0.d0)) then
        codret=1
        goto 10
!
    endif
    t8=1.d0/t6/t5
    t12=c11+c22+c33
    t13=t5**2
    t17=c33**2
    t19=c22*t17*c12
    t22=2.d0/3.d0*t8*c12*c33-8.d0/9.d0*t12/t6/t13*t19
    t26=t6**2
    t28=1.d0/t26/t5
    t30=c12*c33
    t34=c22*c33
    t49=1.d0/t6
    t66=sqrt(t5)
    t69=t66**2
    dsidep(1,4)=4.d0*c10*t22+4.d0*c01*&
     &            (4.d0/3.d0*(c22+c33)*t28*t30+4.d0/3.d0*c12*t28*t34-&
     &            20.d0/9.d0*(t1+c11*c33+t34-t3)/t26/t13*t19)+&
     &            16.d0/3.d0*c20*t12*t8*t30*(t49-t12*t8*t34/3.d0)+&
     &            8.d0*c20*(t12*t49-3.d0)*t22-2.d0*k/t5*t19+&
     &            2.d0*k*(t66-1.d0)/t69/t66*t19
!
    t1=c11*c22
    t3=c12**2
    t5=t1*c33-t3*c33
    t6=t5**(1.d0/3.d0)
    if ((t5.le.0.d0)) then
        codret=1
        goto 10
!
    endif
    t8=1.d0/t6/t5
    t15=c11+c22+c33
    t16=t5**2
    t20=c33**2
    t22=c22*t20*c11
    t25=t15*t8
    t28=-t8*c11*c33/3.d0-t8*c22*c33/3.d0+4.d0/9.d0*t15/t6/t16*t22-&
     &    t25*c33/3.d0
    t31=t6**2
    t35=1.d0/t31/t5
    t37=c11*c33
    t42=c22*c33
    t45=t1+t37+t42-t3
    t57=1.d0/t6
    t75=sqrt(t5)
    t77=k*(t75-1.d0)
    t78=t75**2
    dsidep(2,1)=4.d0*c10*t28+4.d0*c01*&
     &            (1.d0/t31-2.d0/3.d0*(c22+c33)*t35*t37-&
     &            2.d0/3.d0*(c11+c33)*t35*t42+&
     &            10.d0/9.d0*t45/t31/t16*t22-2.d0/3.d0*t45*t35*c33)+&
     &            8.d0*c20*(t57-t25*t37/3.d0)*(t57-t25*t42/3.d0)+&
     &            8.d0*c20*(t15*t57-3.d0)*t28+k/t5*t22-t77/t78/t75*t22+&
     &            2.d0*t77/t75*c33
!
    t1=c11*c22
    t3=c12**2
    t5=t1*c33-t3*c33
    t6=t5**(1.d0/3.d0)
    if ((t5.le.0.d0)) then
        codret=1
        goto 10
!
    endif
    t8=1.d0/t6/t5
    t12=c11+c22+c33
    t13=t5**2
    t17=c11**2
    t18=c33**2
    t19=t17*t18
    t22=-2.d0/3.d0*t8*c11*c33+4.d0/9.d0*t12/t6/t13*t19
    t26=t6**2
    t30=c11*c33
    t43=1.d0/t6
    t48=(t43-t12*t8*t30/3.d0)**2.d0
    t59=sqrt(t5)
    t62=t59**2
    dsidep(2,2)=4.d0*c10*t22+4.d0*c01*&
     &            (-4.d0/3.d0*(c11+c33)/t26/t5*t30+&
     &            10.d0/9.d0*(t1+t30+c22*c33-t3)/t26/t13*t19)+&
     &            8.d0*c20*t48+8.d0*c20*(t12*t43-3.d0)*t22+k/t5*t19-&
     &            k*(t59-1.d0)/t62/t59*t17*t18
!
    t1=c11*c22
    t3=c12**2
    t5=t1*c33-t3*c33
    t6=t5**(1.d0/3.d0)
    if ((t5.le.0.d0)) then
        codret=1
        goto 10
!
    endif
    t8=1.d0/t6/t5
    t9=t1-t3
    t15=c11+c22+c33
    t16=t5**2
    t20=c11*c33
    t21=t20*t9
    t24=t15*t8
    t27=-t8*t9/3.d0-t8*c11*c33/3.d0+4.d0/9.d0*t15/t6/t16*t21-&
     &    t24*c11/3.d0
    t30=t6**2
    t34=1.d0/t30/t5
    t43=t1+c22*c33+t20-t3
    t55=1.d0/t6
    t73=sqrt(t5)
    t75=k*(t73-1.d0)
    t76=t73**2
    dsidep(2,3)=4.d0*c10*t27+4.d0*c01*&
     &            (1.d0/t30-2.d0/3.d0*(c11+c33)*t34*t9-&
     &            2.d0/3.d0*(c11+c22)*t34*t20+&
     &            10.d0/9.d0*t43/t30/t16*t21-2.d0/3.d0*t43*t34*c11)+&
     &            8.d0*c20*(t55-t24*t9/3.d0)*(t55-t24*t20/3.d0)+&
     &            8.d0*c20*(t15*t55-3.d0)*t27+k/t5*t21-t75/t73/t76*t21+&
     &            2.d0*t75/t73*c11
!
!
!
    t1=c11*c22
    t3=c12**2
    t5=t1*c33-t3*c33
    t6=t5**(1.d0/3.d0)
    if ((t5.le.0.d0)) then
        codret=1
        goto 10
!
    endif
    t8=1.d0/t6/t5
    t12=c11+c22+c33
    t13=t5**2
    t17=c33**2
    t19=c11*t17*c12
    t22=2.d0/3.d0*t8*c12*c33-8.d0/9.d0*t12/t6/t13*t19
    t26=t6**2
    t28=1.d0/t26/t5
    t30=c12*c33
    t34=c11*c33
    t49=1.d0/t6
    t66=sqrt(t5)
    t69=t66**2
    dsidep(2,4)=4.d0*c10*t22+4.d0*c01*&
     &            (4.d0/3.d0*(c11+c33)*t28*t30+4.d0/3.d0*c12*t28*t34-&
     &            20.d0/9.d0*(t1+t34+c22*c33-t3)/t26/t13*t19)+&
     &            16.d0/3.d0*c20*t12*t8*t30*(t49-t12*t8*t34/3.d0)+&
     &            8.d0*c20*(t12*t49-3.d0)*t22-2.d0*k/t5*t19+&
     &            2.d0*k*(t66-1.d0)/t69/t66*t19
    t1=c11*c22
    t3=c12**2
    t5=t1*c33-t3*c33
    t6=t5**(1.d0/3.d0)
    if ((t5.le.0.d0)) then
        codret=1
        goto 10
!
    endif
    t8=1.d0/t6/t5
    t12=t1-t3
    t15=c11+c22+c33
    t16=t5**2
    t21=t12*c22*c33
    t24=t15*t8
    t27=-t8*c22*c33/3.d0-t8*t12/3.d0+4.d0/9.d0*t15/t6/t16*t21-&
     &    t24*c22/3.d0
    t30=t6**2
    t34=1.d0/t30/t5
    t36=c22*c33
    t44=t1+c11*c33+t36-t3
    t56=1.d0/t6
    t74=sqrt(t5)
    t76=k*(t74-1.d0)
    t77=t74**2
    dsidep(3,1)=4.d0*c10*t27+4.d0*c01*&
     &            (1.d0/t30-2.d0/3.d0*(c11+c22)*t34*t36-&
     &            2.d0/3.d0*(c22+c33)*t34*t12+&
     &            10.d0/9.d0*t44/t30/t16*t21-2.d0/3.d0*t44*t34*c22)+&
     &            8.d0*c20*(t56-t24*t36/3.d0)*(t56-t24*t12/3.d0)+&
     &            8.d0*c20*(t15*t56-3.d0)*t27+k/t5*t21-t76/t77/t74*t21+&
     &            2.d0*t76/t74*c22
!
    t1=c11*c22
    t3=c12**2
    t5=t1*c33-t3*c33
    t6=t5**(1.d0/3.d0)
    if ((t5.le.0.d0)) then
        codret=1
        goto 10
!
    endif
    t8=1.d0/t6/t5
    t12=t1-t3
    t15=c11+c22+c33
    t16=t5**2
    t21=t12*c11*c33
    t24=t15*t8
    t27=-t8*c11*c33/3.d0-t8*t12/3.d0+4.d0/9.d0*t15/t6/t16*t21-&
     &    t24*c11/3.d0
    t30=t6**2
    t34=1.d0/t30/t5
    t36=c11*c33
    t44=t1+t36+c22*c33-t3
    t56=1.d0/t6
    t74=sqrt(t5)
    t76=k*(t74-1.d0)
    t77=t74**2
    dsidep(3,2)=4.d0*c10*t27+4.d0*c01*&
     &            (1.d0/t30-2.d0/3.d0*(c11+c22)*t34*t36-&
     &            2.d0/3.d0*(c11+c33)*t34*t12+&
     &            10.d0/9.d0*t44/t30/t16*t21-2.d0/3.d0*t44*t34*c11)+&
     &            8.d0*c20*(t56-t24*t36/3.d0)*(t56-t24*t12/3.d0)+&
     &            8.d0*c20*(t15*t56-3.d0)*t27+k/t5*t21-t76/t77/t74*t21+&
     &            2.d0*t76/t74*c11
!
    t1=c11*c22
    t3=c12**2
    t5=t1*c33-t3*c33
    t6=t5**(1.d0/3.d0)
    if ((t5.le.0.d0)) then
        codret=1
        goto 10
!
    endif
    t8=1.d0/t6/t5
    t9=t1-t3
    t12=c11+c22+c33
    t13=t5**2
    t17=t9**2
    t20=-2.d0/3.d0*t8*t9+4.d0/9.d0*t12/t6/t13*t17
    t24=t6**2
    t41=1.d0/t6
    t46=(t41-t8*t12*t9/3.d0)**2.d0
    t57=sqrt(t5)
    t60=t57**2
    dsidep(3,3)=4.d0*c10*t20+4.d0*c01*&
     &            (-4.d0/3.d0*(c11+c22)/t24/t5*t9+10.d0/9.d0*&
     &            (t1+c11*c33+c22*c33-t3)/t24/t13*t17)+8.d0*c20*t46+&
     &            8.d0*c20*(t12*t41-3.d0)*t20+k/t5*t17-&
     &            k*(t57-1.d0)/t60/t57*t17
!
    t1=c11*c22
    t3=c12**2
    t5=t1*c33-t3*c33
    t6=t5**(1.d0/3.d0)
    if ((t5.le.0.d0)) then
        codret=1
        goto 10
!
    endif
    t8=1.d0/t6/t5
    t12=c11+c22+c33
    t13=t5**2
    t17=t1-t3
    t19=t17*c12*c33
    t22=t8*t12
    t25=2.d0/3.d0*t8*c12*c33-8.d0/9.d0*t12/t6/t13*t19+&
     &    2.d0/3.d0*t22*c12
    t29=t6**2
    t31=1.d0/t29/t5
    t33=c12*c33
    t41=t1+c11*c33+c22*c33-t3
    t55=1.d0/t6
    t71=sqrt(t5)
    t73=k*(t71-1.d0)
    t74=t71**2
    dsidep(3,4)=4.d0*c10*t25+4.d0*c01*&
     &            (4.d0/3.d0*(c11+c22)*t31*t33+4.d0/3.d0*c12*t31*t17-&
     &            20.d0/9.d0*t41/t29/t13*t19+4.d0/3.d0*t41*t31*c12)+&
     &            16.d0/3.d0*c20*t12*t8*t33*(t55-t22*t17/3.d0)+&
     &            8.d0*c20*(t12*t55-3.d0)*t25-2.d0*k/t5*t19+&
     &            2.d0*t73/t74/t71*t19-4.d0*t73/t71*c12
!
    t1=c11*c22
    t3=c12**2
    t5=t1*c33-t3*c33
    t6=t5**(1.d0/3.d0)
    if ((t5.le.0.d0)) then
        codret=1
        goto 10
!
    endif
    t8=1.d0/t6/t5
    t10=c12*c33
    t13=c11+c22+c33
    t15=t5**2
    t17=1.d0/t6/t15
    t19=c33**2
    t21=c12*t19*c22
    t24=t6**2
    t26=1.d0/t24/t5
    t28=c22*c33
    t45=1.d0/t6
    t53=t8*c12*c33
    t58=c20*(t13*t45-3.d0)
    t71=sqrt(t5)
    t74=t71**2
    dsidep(4,1)=8.d0/3.d0*c10*t8*t10-32.d0/9.d0*c10*t13*t17*t21+&
     &            4.d0*c01*(4.d0/3.d0*c12*t26*t28+&
     &            4.d0/3.d0*(c22+c33)*t26*t10-&
     &            20.d0/9.d0*(t1+c11*c33+t28-t3)/t24/t15*t21)+&
     &            16.d0/3.d0*c20*(t45-t13*t8*t28/3.d0)*t13*t53+&
     &            16.d0/3.d0*t58*t53-64.d0/9.d0*t58*t13*t17*c12*t19*c22-&
     &            2.d0*k/t5*t21+2.d0*k*(t71-1.d0)/t74/t71*t21
!
    t1=c11*c22
    t3=c12**2
    t5=t1*c33-t3*c33
    t6=t5**(1.d0/3.d0)
    if ((t5.le.0.d0)) then
        codret=1
        goto 10
!
    endif
    t8=1.d0/t6/t5
    t10=c12*c33
    t13=c11+c22+c33
    t15=t5**2
    t17=1.d0/t6/t15
    t19=c33**2
    t21=c12*t19*c11
    t25=t6**2
    t27=1.d0/t25/t5
    t32=c11*c33
    t47=1.d0/t6
    t57=c20*(t13*t47-3.d0)
    t72=sqrt(t5)
    t75=t72**2
    dsidep(4,2)=8.d0/3.d0*c10*t8*t10-32.d0/9.d0*c10*t13*t17*t21+&
     &            4.d0*c01*(4.d0/3.d0*(c11+c33)*t27*t10+&
     &            4.d0/3.d0*c12*t27*t32-20.d0/9.d0*(t1+t32+c22*c33-t3)/&
     &            t25/t15*t21)+16.d0/3.d0*c20*t13*t8*t10*&
     &            (t47-t13*t8*t32/3.d0)+16.d0/3.d0*t57*t8*c12*c33-&
     &            64.d0/9.d0*t57*t13*t17*c12*t19*c11-2.d0*k/t5*t21+&
     &            2.d0*k*(t72-1.d0)/t75/t72*t21
!
    t1=c11*c22
    t3=c12**2
    t5=t1*c33-t3*c33
    t6=t5**(1.d0/3.d0)
    if ((t5.le.0.d0)) then
        codret=1
        goto 10
!
    endif
    t8=1.d0/t6/t5
    t10=c12*c33
    t13=c11+c22+c33
    t14=c10*t13
    t15=t5**2
    t17=1.d0/t6/t15
    t19=t1-t3
    t20=t10*t19
    t23=t8*c12
    t27=t6**2
    t29=1.d0/t27/t5
    t38=t1+c11*c33+c22*c33-t3
    t52=1.d0/t6
    t53=t13*t8
    t62=c20*(t13*t52-3.d0)
    t79=sqrt(t5)
    t81=k*(t79-1.d0)
    t82=t79**2
    dsidep(4,3)=8.d0/3.d0*c10*t8*t10-32.d0/9.d0*t14*t17*t20+&
     &            8.d0/3.d0*t14*t23+4.d0*c01*&
     &            (4.d0/3.d0*(c11+c22)*t29*t10+4.d0/3.d0*c12*t29*t19-&
     &            20.d0/9.d0*t38/t27/t15*t20+4.d0/3.d0*t38*t29*c12)+&
     &            16.d0/3.d0*c20*t13*t8*t10*(t52-t53*t19/3.d0)+&
     &            16.d0/3.d0*t62*t23*c33-64.d0/9.d0*t62*t13*t17*c12*c33*&
     &            t19+16.d0/3.d0*t62*t53*c12-2.d0*k/t5*t20+&
     &            2.d0*t81/t82/t79*t20-4.d0*t81/t79*c12
!
    t1=c11+c22+c33
    t2=c10*t1
    t3=c11*c22
    t5=c12**2
    t7=t3*c33-t5*c33
    if ((t7.le.0.d0)) then
        codret=1
        goto 10
!
    endif
    t8=t7**2
    t9=t7**(1.d0/3.d0)
    t13=c33**2
    t14=1.d0/t9/t8*t5*t13
    t18=1.d0/t9/t7
    t22=t9**2
    t26=1.d0/t22/t7
    t32=t3+c11*c33+c22*c33-t5
    t34=1.d0/t22/t8
    t36=t5*t13
    t45=t1**2
    t54=c20*(t1/t9-3.d0)
    t66=sqrt(t7)
    t68=k*(t66-1.d0)
    t69=t66**2
    dsidep(4,4)=64.d0/9.d0*t2*t14+8.d0/3.d0*t2*t18*c33+&
     &            4.d0*c01*(-2.d0/t22-16.d0/3.d0*t5*t26*c33+&
     &            40.d0/9.d0*t32*t34*t36+4.d0/3.d0*t32*t26*c33)+&
     &            32.d0/9.d0*c20*t45*t34*t5*t13+128.d0/9.d0*t54*t1*t14+&
     &            16.d0/3.d0*t54*t1*t18*c33+4.d0*k/t7*t36-&
     &            4.d0*t68/t69/t66*t5*t13-4.d0*t68/t66*c33
10  continue
end subroutine
