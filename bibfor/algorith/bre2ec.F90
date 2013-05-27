subroutine bre2ec(k0, k1, k2, eta1, eta2,&
                  e1i, e2i, a, t, b,&
                  e2p, pw, e2f)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     CALCUL DE E2F AVEC HYPOTHESE D ECOULEMENT ETAGE 2
!
    implicit none
!
!
!-----------------------------------------------------------------------
    real(kind=8) :: k0, k1, k2
    real(kind=8) :: a, b, e1i, e2f, e2i, e2p, eta1
    real(kind=8) :: eta2, pw, t, t10, t101, t106, t108
    real(kind=8) :: t109, t11, t110, t111, t112, t113, t115
    real(kind=8) :: t116, t117, t118, t119, t12, t120, t121
    real(kind=8) :: t122, t123, t124, t125, t126, t128, t129
    real(kind=8) :: t131, t132, t133, t141, t144, t149, t154
    real(kind=8) :: t158, t162, t17, t172, t175, t191, t193
    real(kind=8) :: t2, t20, t200, t21, t211, t214, t219
    real(kind=8) :: t228, t229, t230, t232, t234, t235, t237
    real(kind=8) :: t238, t239, t240, t242, t244, t246, t247
    real(kind=8) :: t249, t250, t252, t253, t254, t255, t256
    real(kind=8) :: t257, t260, t261, t263, t265, t267, t268
    real(kind=8) :: t27, t270, t273, t276, t279, t280, t291
    real(kind=8) :: t292, t295, t298, t299, t301, t304, t305
    real(kind=8) :: t31, t310, t313, t330, t331, t333, t336
    real(kind=8) :: t34, t36, t37, t38, t39, t42, t43
    real(kind=8) :: t44, t45, t48, t49, t5, t50, t51
    real(kind=8) :: t52, t53, t54, t55, t56, t57, t58
    real(kind=8) :: t6, t60, t61, t62, t63, t64, t65
    real(kind=8) :: t67, t7, t71, t72, t73, t75, t76
    real(kind=8) :: t78, t8, t81, t82, t83, t84, t85
    real(kind=8) :: t87, t9, t90, t92, t95, t96
!-----------------------------------------------------------------------
    t2 = a * k0
    t5 = eta1 * k1
    t6 = k1 * eta2
    t7 = k0 * eta2
    t8 = k2 * eta1
    t9 = eta1 ** 2
    t10 = k1 ** 2
    t11 = t10 * t9
    t12 = t10 * eta1
    t17 = k1 * t9
    t20 = eta2 ** 2
    t21 = t20 * t10
    t27 = k0 ** 2
    t31 = k2 ** 2
    t34 = sqrt(&
          t11 + 0.2d1 * eta2 * t12 - 0.2d1 * t7 * t5 + 0.2d1 * k2 * t17 + t21 + 0.2d1 * k0 * t20 &
          &* k1 - 0.2d1 * t8 * t6 + t20 * t27 - 0.2d1 * t8 * t7 + t9 * t31&
          )
    t36 = 0.1d1 / eta1
    t37 = t36 * (t5 + t6 + t7 + t8 - t34)
    t38 = 0.1d1 / eta2
    t39 = t * t38
    t42 = exp(-t39 * t37 / 0.2d1)
    t43 = t27 * k0
    t44 = eta2 * t43
    t45 = k1 * k2
    t48 = 0.3d1 * b * t45 * t44
    t49 = eta2 * t10
    t50 = k0 * t31
    t51 = e2p * t50
    t52 = t51 * t49
    t53 = t31 * t6
    t54 = eta1 * t2
    t55 = t54 * t53
    t56 = k1 * t34
    t57 = t51 * t56
    t58 = a * k2
    t60 = k0 * t58 * t11
    t61 = k2 * t27
    t62 = b * t61
    t63 = t62 * t56
    t64 = b * t50
    t65 = t64 * t56
    t67 = t54 * t31 * t34
    t71 = k0 * eta1 * k1
    t72 = t71 * a * k2 * t34
    t73 = t27 * e2i
    t75 = 0.4d1 * t53 * t73
    t76 = eta2 * t27
    t78 = eta1 * a
    t81 = 0.2d1 * k1 * t78 * k2 * t76
    t82 = t48 + t52 - t55 + t57 + t60 - t63 - t65 + t67 + t72 - t75 -t81
    t83 = t10 * k1
    t84 = t83 * eta1
    t85 = t27 * e1i
    t87 = t31 * e1i
    t90 = pw * t31
    t92 = eta2 * t83
    t95 = t31 * k2
    t96 = eta1 * t95
    t101 = k1 * pw
    t106 = -t85 * t84 - t87 * t44 - t87 * t84 + t90 * t49 - t85 * t92- t87 * t92 - k0 * pw * t96 &
           &- b * t27 * t96 - t101 * t96 + t2 * t9 * t95 + t85 * t96
    t108 = t10 * e1i
    t109 = t108 * t96
    t110 = t108 * t44
    t111 = t27 * t34
    t112 = e2p * t31
    t113 = t112 * t111
    t115 = t90 * k0 * t34
    t116 = b * t31
    t117 = t116 * t111
    t118 = t90 * t56
    t119 = e1i * t34
    t120 = t27 * t10
    t121 = t120 * t119
    t122 = t31 * t27
    t123 = t122 * t119
    t124 = t31 * t10
    t125 = t124 * t119
    t126 = t43 * e2i
    t128 = 0.2d1 * t49 * t126
    t129 = t95 * e2i
    t131 = 0.2d1 * t12 * t129
    t132 = t109 - t110 + t113 - t115 - t117 - t118 + t121 + t123 + t125 - t128 + t131
    t133 = t27 * eta1
    t141 = k1 * a
    t144 = t27 * a
    t149 = e2p * t95
    t154 = a * t31
    t158 = e2i * eta1
    t162 = 0.2d1 * t133 * t129 - 0.2d1 * t31 * eta2 * t126 + t112 * t44 - t141 * t20 * t43 - t90 &
           &* t12 - t144 * t21 + 0.2d1 * t49 * b *t43 - 0.2d1 * t12 * t149 - t133 * t149 - t64 * &
           &t12 + 0.2d1 * k0 *t154 * t17 + 0.2d1 * k2 * t120 * t158
    t172 = k2 * t49
    t175 = k1 * k0
    t191 = e2p * t61
    t193 = t31 * k1
    t200 = -0.2d1 * t31 * t49 * e2i * k0 + 0.4d1 * k0 * t124 * t158 -0.4d1 * t172 * t73 - b * t17&
           &5 * t96 - 0.4d1 * k2 * t108 * t76 - 0.2d1 * e1i * k1 * k2 * t44 - 0.3d1 * k1 * t87 * &
           &t76 - 0.3d1 * t31 * t108 * t7 - t191 * t12 + 0.2d1 * pw * t193 * t7 + 0.2d1 * b * t19&
           &3 * t76
    t211 = a * t76
    t214 = k0 * k2 * pw
    t219 = t27 * k1
    t228 = -eta1 * t154 * t76 + e2p * t45 * t44 + 0.3d1 * pw * t45 * t76 - 0.2d1 * b * t122 * t5 &
           &- t211 * t12 - t214 * t12 - t62 * t12 - 0.3d1 * t51 * t12 + 0.4d1 * t31 * t219 * t158&
           & - 0.4d1 * k2 * t6 * t126 + 0.4d1 * t71 * t129
    t229 = t200 + t228
    t230 = t191 * t49
    t232 = 0.3d1 * t214 * t49
    t234 = 0.3d1 * t62 * t49
    t235 = pw * eta2
    t237 = 0.2d1 * t120 * t235
    t238 = t90 * t76
    t239 = t116 * t44
    t240 = t54 * t172
    t242 = 0.3d1 * t71 * t149
    t244 = k2 * k0 * t10
    t246 = 0.2d1 * t244 * t119
    t247 = k2 * t219
    t249 = 0.2d1 * t247 * t119
    t250 = k1 * t50
    t252 = 0.2d1 * t250 * t119
    t253 = t230 + t232 + t234 + t237 + t238 + t239 + t240 - t242 + t246 + t249 + t252
    t254 = t211 * t56
    t255 = t191 * t56
    t256 = t214 * t56
    t257 = k0 * e1i
    t260 = 0.2d1 * k1 * t257 * t96
    t261 = k2 * t257
    t263 = 0.2d1 * t261 * t84
    t265 = k2 * t85 * t12
    t267 = t31 * t85 * t5
    t268 = t64 * t49
    t270 = 0.2d1 * t261 * t92
    t273 = 0.2d1 * e2p * t193 * t76
    t276 = 0.2d1 * e2p * t122 * t5
    t279 = 0.2d1 * pw * t50 * t5
    t280 = -t254 + t255 - t256 + t260 - t263 - t265 + t267 + t268 - t270 + t273 - t276 - t279
    t291 = 0.1d1 / ( t120 + t124 + 0.2d1 * t244 + 0.2d1 * t247 + t122 + 0.2d1 * t250 ) / t34
    t292 = t291 * (t82 + t106 + t132 + t162 + t229 + t253 + t280) * t42
    t295 = t36 * (t5 + t6 + t7 + t8 + t34)
    t298 = exp(-t39 * t295 / 0.2d1)
    t299 = t48 + t52 - t55 - t57 + t60 + t63 + t65 - t67 - t72 - t75 - t81
    t301 = t109 - t110 - t113 + t115 + t117 + t118 - t121 - t123 - t125 - t128 + t131
    t304 = t230 + t232 + t234 + t237 + t238 + t239 + t240 - t242 - t246 - t249 - t252
    t305 = t254 - t255 + t256 + t260 - t263 - t265 + t267 + t268 - t270 + t273 - t276 - t279
    t310 = t291 * (t299 + t106 + t301 + t162 + t229 + t304 + t305) * t298
    t313 = a * t + b - e2p
    t330 = ((k1 + k0) * k2 + t175) ** 2
    t331 = 0.1d1 / t330
    t333 = t292 / 0.2d1 - t310 / 0.2d1 + t331 * (&
           t31 * (t27 * t313 + k0 * (k1 * t313 - t78 + pw) + t101 ) + t45 * (k0 * t313 - t78 + pw&
           )* k0 + eta2 * a * t219&
           )
    t336 = t333 * k1
    e2f = 0.1d1 / (t8 - t7) * (-t * eta2 * t2 + eta2 *k0 * t333 + eta1 * t336 + eta2 * t336 - t23&
          &5 + eta2 * eta1 * (-t292 * t38 * t37 / 0.4d1 + t310 * t38 * t295 / 0.4d1 + t331 * (t31&
          & *(t144 + k0 * t141) + t58 * t219)) - k0 * b * eta2 + k2 * e2p * eta1)
!
end subroutine
