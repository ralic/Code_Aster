subroutine bre1ec(k0, k1, k2, eta1, eta2,&
                  e1i, e2i, a, t, b,&
                  e2p, pw, e1f)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     CALCUL DE E2F AVEC HYPOTHESE D ECOULEMENT ETAGE 2
!
    implicit none
!
!-----------------------------------------------------------------------
    real(kind=8) :: k0, k1, k2
    real(kind=8) :: a, b, e1f, e1i, e2i, e2p, eta1
    real(kind=8) :: eta2, pw, t, t100, t101, t102, t103
    real(kind=8) :: t104, t106, t107, t108, t109, t110, t111
    real(kind=8) :: t112, t115, t116, t117, t119, t121, t122
    real(kind=8) :: t124, t125, t126, t127, t128, t129, t130
    real(kind=8) :: t14, t140, t143, t147, t148, t152, t154
    real(kind=8) :: t155, t157, t160, t163, t164, t17, t177
    real(kind=8) :: t18, t187, t194, t197, t198, t199, t2
    real(kind=8) :: t200, t203, t205, t208, t211, t214, t217
    real(kind=8) :: t218, t220, t222, t223, t228, t231, t232
    real(kind=8) :: t234, t235, t237, t238, t239, t24, t241
    real(kind=8) :: t242, t244, t246, t247, t249, t250, t251
    real(kind=8) :: t252, t254, t257, t259, t262, t264, t266
    real(kind=8) :: t267, t268, t269, t271, t273, t275, t277
    real(kind=8) :: t278, t28, t289, t296, t297, t298, t3
    real(kind=8) :: t300, t303, t305, t306, t31, t314, t33
    real(kind=8) :: t331, t36, t39, t4, t40, t41, t44
    real(kind=8) :: t45, t46, t48, t5, t50, t51, t52
    real(kind=8) :: t53, t54, t56, t59, t6, t61, t62
    real(kind=8) :: t63, t64, t68, t69, t7, t70, t71
    real(kind=8) :: t72, t73, t74, t75, t76, t77, t78
    real(kind=8) :: t79, t8, t80, t82, t84, t85, t86
    real(kind=8) :: t87, t89, t9, t90, t91, t92, t94
    real(kind=8) :: t96, t97, t98, t99
!-----------------------------------------------------------------------
    t2=eta1*k1
    t3=k1*eta2
    t4=k0*eta2
    t5=k2*eta1
    t6=eta1**2
    t7=k1**2
    t8=t7*t6
    t9=t7*eta1
    t14=k1*t6
    t17=eta2**2
    t18=t17*t7
    t24=k0**2
    t28=k2**2
    t31=sqrt(t8+0.2d1*eta2*t9-0.2d1*t4*t2+0.2d1*k2*t14+t18+&
     &    0.2d1*k0*t17*k1-0.2d1*t5*t3+t17*t24-0.2d1*t5*t4+t6*t28)
    t33=0.1d1/eta1
    t36=0.1d1/eta2*t
    t39=exp(-t36*t33*(t2+t3+t4+t5-t31)/0.2d1)
    t40=eta2*t24
    t41=t7*e1i
    t44=0.4d1*k2*t41*t40
    t45=t28*k2
    t46=e2p*t45
    t48=eta1*k0*k1
    t50=0.3d1*t48*t46
    t51=k1*t31
    t52=k2*t24
    t53=e2p*t52
    t54=t53*t51
    t56=eta1*a
    t59=0.2d1*k1*t56*k2*t40
    t61=k0*k2*pw
    t62=t61*t9
    t63=t24*k0
    t64=eta2*t63
    t68=0.2d1*e1i*k1*k2*t64
    t69=t7*k1
    t70=t69*eta1
    t71=t28*e1i
    t72=t71*t70
    t73=t71*t64
    t74=t24*e1i
    t75=t74*t70
    t76=b*t28
    t77=t76*t64
    t78=pw*t28
    t79=t78*t40
    t80=-t44-t50+t54-t59-t62-t68-t72-t73-t75+t77+t79
    t82=t24*t7
    t84=0.2d1*t82*pw*eta2
    t85=eta1*t45
    t86=t41*t85
    t87=t74*t85
    t89=a*k0
    t90=t89*t6*t45
    t91=k1*pw
    t92=t91*t85
    t94=b*t24*t85
    t96=k0*pw*t85
    t97=eta2*t69
    t98=t71*t97
    t99=t74*t97
    t100=eta2*t7
    t101=t78*t100
    t102=e1i*t31
    t103=t82*t102
    t104=t84+t86+t87+t90-t92-t94-t96-t98-t99+t101+t103
    t106=t78*t51
    t107=t24*t31
    t108=t76*t107
    t109=e2p*t28
    t110=t109*t107
    t111=t41*t64
    t112=k0*e1i
    t115=0.2d1*k1*t112*t85
    t116=t45*e2i
    t117=t24*eta1
    t119=0.2d1*t117*t116
    t121=0.2d1*t9*t116
    t122=t63*e2i
    t124=0.2d1*t100*t122
    t125=t28*t7
    t126=t125*t102
    t127=t28*t24
    t128=t127*t102
    t129=t117*t46
    t130=-t106-t108+t110-t111+t115+t119+t121-t124+t126+t128-t129
    t140=t28*eta2
    t143=k1*k2
    t147=k0*t28
    t148=b*t147
    t152=e2p*t147
    t154=k2*t100
    t155=eta1*t89
    t157=-0.2d1*t9*t46+0.2d1*t100*b*t63-a*t24*t18-t78*t9+t109*t64-&
     &     0.2d1*t140*t122+0.3d1*pw*t143*t40+t148*t100-t53*t9+t53*t100+&
     &     t152*t100+t155*t154
    t160=t24*e2i
    t163=e2i*eta1
    t164=t24*k1
    t177=a*t28
    t187=t28*k1
    t194=-0.4d1*t154*t160+0.4d1*t28*t164*t163-0.4d1*k2*t3*t122+&
     &     0.4d1*t48*t116-t148*t9-k1*a*t17*t63+0.2d1*k0*t177*t14+&
     &     k0*a*k2*t8+0.3d1*b*t143*t64+0.2d1*e2p*t187*t40-&
     &     0.2d1*pw*t147*t2
    t197=0.2d1*b*t127*t2
    t198=b*t52
    t199=t198*t51
    t200=t148*t51
    t203=t48*a*k2*t31
    t205=t28*t74*t2
    t208=0.2d1*k2*t82*t163
    t211=0.4d1*k0*t125*t163
    t214=0.3d1*k1*t71*t40
    t217=0.3d1*t28*t41*t4
    t218=k1*t140
    t220=0.4d1*t218*t160
    t222=k2*t74*t9
    t223=-t197-t199-t200+t203+t205+t208+t211-t214-t217-t220-t222
    t228=0.2d1*t28*t100*e2i*k0
    t231=0.2d1*pw*t187*t4
    t232=k2*t164
    t234=0.2d1*t232*t102
    t235=k1*t147
    t237=0.2d1*t235*t102
    t238=a*t40
    t239=t238*t51
    t241=t78*k0*t31
    t242=t238*t9
    t244=t155*t28*t31
    t246=e2p*t143*t64
    t247=t198*t9
    t249=0.3d1*t152*t9
    t250=-t228+t231+t234+t237-t239-t241-t242+t244+t246-t247-t249
    t251=t155*t218
    t252=k2*t112
    t254=0.2d1*t252*t97
    t257=0.2d1*b*t187*t40
    t259=eta1*t177*t40
    t262=0.2d1*e2p*t127*t2
    t264=k2*k0*t7
    t266=0.2d1*t264*t102
    t267=t61*t51
    t268=t152*t51
    t269=k1*k0
    t271=b*t269*t85
    t273=0.2d1*t252*t70
    t275=0.3d1*t61*t100
    t277=0.3d1*t198*t100
    t278=-t251-t254+t257-t259-t262+t266-t267+t268-t271-t273+t275+t277
    t289=0.1d1/(t82+t125+0.2d1*t264+0.2d1*t232+t127+0.2d1*t235)/t31
    t296=exp(-t36*t33*(t2+t3+t4+t5+t31)/0.2d1)
    t297=-t44-t50-t54-t59-t62-t68-t72-t73-t75+t77+t79
    t298=t84+t86+t87+t90-t92-t94-t96-t98-t99+t101-t103
    t300=t106+t108-t110-t111+t115+t119+t121-t124-t126-t128-t129
    t303=-t197+t199+t200-t203+t205+t208+t211-t214-t217-t220-t222
    t305=-t228+t231-t234-t237+t239+t241-t242-t244+t246-t247-t249
    t306=-t251-t254+t257-t259-t262-t266+t267-t268-t271-t273+t275+t277
    t314=a*t+b-e2p
    t331=((k1+k0)*k2+t269)**2
    e1f=t289*(t80+t104+t130+t157+t194+t223+t250+t278)*t39/0.2d1-&
     &    t289*(t297+t298+t300+t157+t194+t303+t305+t306)*t296/0.2d1+&
     &    0.1d1/t331*(t28*(t24*t314+k0*(k1*t314-t56+pw)+t91)+&
     &    t143*(k0*t314-t56+pw)*k0+eta2*a*t164)
!
end subroutine
