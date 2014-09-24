subroutine flu_inc3d(e0i, e1i, e2i, ve1i, ve2i,&
                     k0, k1, h1, h2, vk0,&
                     vk1, vh1, vh2, depst, delta,&
                     e0f, e1f, e2f, dsigma, ve1f,&
                     ve2f, dissip)
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
! person_in_charge: etienne.grimal at edf.fr
!=====================================================================
!    calcul des increments de deformation visqueuses
!    basee sur une interpolatin parabolique en temps des etages de KV et
!    mineaire sur l etage elastique, lineaire sur la deformation totale
!    e0(t):=e0i+a0*t; etage elastique
!    e1(t):=e1i+ve1i*t+a1*t^2; etage de KV
!    e2(t):=e2i+ve2i*t+a2*t^2;
!    les 5 inconnues locales sont trouvees en resolvant les 3 equadiff d
!    la compatibilité en deformation totale et vitesse de deformation e
!    l increment de temps est delta, l increment de deformation totale d
!    les proprietes materielles evolue pendant l increment de temps
!    suivant les vitesses vk (rigidites), vh (viscosite)
!    la dissipation (dissip) est calculee suivant ces hypothese en suppo
!    variation parabilique de e2 et lineaire de h2 sur le pas de temps
!=====================================================================
    implicit none
        real(kind=8) :: e0i
        real(kind=8) :: e1i
        real(kind=8) :: e2i
        real(kind=8) :: ve1i
        real(kind=8) :: ve2i
        real(kind=8) :: k0
        real(kind=8) :: k1
        real(kind=8) :: h1
        real(kind=8) :: h2
        real(kind=8) :: vk0
        real(kind=8) :: vk1
        real(kind=8) :: vh1
        real(kind=8) :: vh2
        real(kind=8) :: depst
        real(kind=8) :: delta
        real(kind=8) :: e0f
        real(kind=8) :: e1f
        real(kind=8) :: e2f
        real(kind=8) :: dsigma
        real(kind=8) :: ve1f
        real(kind=8) :: ve2f
        real(kind=8) :: dissip
        real(kind=8) ::t1,t2,t3,t4,t5,t7,t8,t9,t10,t12,t13,t14,t16
        real(kind=8) ::t17,t18,t20,t21,t23,t24,t26,t27,t28,t30,t31
        real(kind=8) ::t32,t34,t36,t37,t39,t40,t42,t44,t45,t47,t48
        real(kind=8) ::t50,t51,t52,t54,t56,t58,t61,t64,t65,t67,t69
        real(kind=8) :: t70,t71,t74,t77,t78,t79
        real(kind=8) ::t82,t83,t84,t86,t87,t90,t105,t108,t113,t134
        real(kind=8) ::t135,t137,t141,t139,t143,t144,t146,t148
        real(kind=8) ::t150,t152,t153,t155,t157,t164,t160,t162,t166
        real(kind=8) ::t168,t171,t173,t176,t179,t180,t181,t182,t183
        real(kind=8) ::t184,t186,t188,t190,t191,t192,t195,t197
        real(kind=8) ::t200,t203,t205,t212,t214,t215
        real(kind=8) ::t218,t219,t220,t231,t249
        real(kind=8) ::t169,t89,t88,t81,t46
    t1 = ve1i * delta
    t2 = delta ** 2
    t3 = vh2 * t2
    t4 = vk0 * depst
    t5 = t3 * t4
    t7 = t2 * delta
    t8 = vh2 * t7
    t9 = vk0 * ve1i
    t10 = t8 * t9
    t12 = h2 * vk0
    t13 = e0i * delta
    t14 = t12 * t13
    t16 = h2 * delta
    t17 = vh1 * ve1i
    t18 = t16 * t17
    t20 = k1 * ve1i
    t21 = t16 * t20
    t23 = vk1 * e1i
    t24 = t16 * t23
    t26 = h2 * vk1
    t27 = ve1i * t2
    t28 = t26 * t27
    t30 = h2 * k0
    t31 = ve2i * delta
    t32 = t30 * t31
    t34 = t30 * t1
    t36 = t2 * ve2i
    t37 = t12 * t36
    t39 = delta * depst
    t40 = t12 * t39
    t42 = t12 * t27
    t44 = k1 * t7
    t45 = t44 * t9
    t46 = k1 * t2
    t47 = k0 * ve1i
    t48 = t46 * t47
    t50 = -0.2d1 * t5 + 0.2d1 * t10 - 0.2d1 * t14 + 0.2d1 * t18 + 0.2d1 * t21 + 0.2d1 * t24 + 0.2&
          &d1 * t28 + 0.2d1 * t32 + 0.2d1 * t34 +0.2d1 * t37 - 0.2d1 * t40 + 0.2d1 * t42 + t45 + &
          &0.2d1 * t48
    t51 = vk0 * e0i
    t52 = t3 * t51
    t54 = t3 * t17
    t56 = t3 * t20
    t58 = t3 * t23
    t61 = t8 * vk1 * ve1i
    t64 = k0 * depst
    t65 = vh2 * delta * t64
    t67 = t3 * t47
    t69 = vk0 * ve2i

    t70 = t8 * t69
    t71 = t30 * depst
    t74 = t7 * vk0 * t23
    t77 = 0.2d1 * t2 * k0 * t23
    t78 = vk1 * t7
    t79 = t78 * t47
    t81 = t2 ** 2
    t82 = vk1 * t81
    t83 = t82 * t9
    t84 = vh1 * t2
    t86 = 0.2d1 * t84 * t47
    t87 = vh1 * t7
    t88 = t87 * t9
    t89 = -0.2d1 * t52 + 0.2d1 * t54 + 0.2d1 * t56 + 0.2d1 * t58 + 0.2d1 * t61 - 0.2d1 * t65 + 0.&
          &2d1 * t67 + t70 - 0.2d1 * t71 + t74 + t77 + 0.2d1 * t79 + t83 + t86 + t88
    t90 = t50 + t89
    t105 = h1 * vk0
    t108 = h1 * delta
    t113 = h1 * k0
    t134 = 0.4d1 * t84 * vh2 + 0.4d1 * t16 * vh1 + 0.4d1 * t84 * k0 +t82 * vk0 + 0.2d1 * t78 * vh&
           &2 + 0.2d1 * t26 * t2 + 0.2d1 * t78 * k0 + 0.2d1 * t105 * t2 + 0.4d1 * t108 * vh2 + 0.&
           &4d1 * h1 * h2 + 0.4d1 * t113 * delta + 0.4d1 * t30 * delta + 0.2d1 * t12 * t2 + 0.2d1&
           & * t44 * vk0 + 0.4d1 * t46 * vh2 + 0.4d1 * t16 * k1 + 0.4d1 * t46* k0 + 0.2d1 * t87 *&
           & vk0 + 0.4d1 * t3 * k0 + 0.2d1 * t8 * vk0

    t135 = 0.1d1 / t134
    e1f = e1i + t1 - t90 * delta * t135
    t137 = t113 * depst
    t139 = t105 * t13
    t141 = t105 * t36
    t143 = vh2 * ve2i
    t144 = t108 * t143
    t146 = t105 * t39
    t148 = t46 * t51
    t150 = t44 * t69
    t152 = k0 * ve2i
    t153 = t46 * t152
    t155 = t46 * t143
    t157 = t46 * t4
    t160 = k1 * delta * t64
    t162 = t3 * t152
    t164 = t113 * t31
    t166 = t84 * t51
    t168 = 0.2d1 * t137 + 0.2d1 * t139 - t45 - t70 - 0.2d1 * t141 - 0.2d1 * t144 + 0.2d1 * t146 +&
           & 0.2d1 * t148 - 0.2d1 * t150 - 0.2d1 *t153 - 0.2d1 * t155 + 0.2d1 * t157 + 0.2d1 * t1&
           &60 - 0.2d1 * t162 - 0.2d1 * t164 + 0.2d1 * t166
    t169 = t87 * t69
    t171 = t84 * t152
    t173 = t84 * t143
    t176 = 0.2d1 * t84 * t4
    t179 = 0.2d1 * vh1 * delta * t64
    t180 = t78 * t51
    t181 = t82 * t69
    t182 = t78 * t152
    t183 = t78 * t143
    t184 = t78 * t4
    t186 = vk1 * t2 * t64
    t188 = 0.2d1 * t105 * t27
    t190 = 0.2d1 * t113 * t1
    t191 = -0.2d1 * t169 - 0.2d1 * t171 - 0.2d1 * t173 + t176 + t179 + t180 - t181 - t182 - t183 &
           &+ t184 + t186 - t188 - t190 + t74 + t77 + t79 - t88
    t192 = t168 + t191
    e2f = e2i + t31 + delta * t192 * t135
    t195 = -t137 - t139 - t5 + t10 - t14 + t18 + t21 + t24 + t28 + t32 + t34 + t37 - t40
    t197 = t42 + t45 + t48 - t52 + t54 + t56 + t58 + t61 - t65 + t67 + t70 - t71 + t141 + t144
    t200 = -t146 - t148 + t150 + t153 + t155 - t157 - t160 + t162 + t164 - t166 + t169 + t171 + t&
           &173
    t203 = -t176 - t179 - t180 + t181 + t182 + t183 - t184 - t186 + t188 + t190 + t79 + t83 + t86&
           & + 0.2d1 * t88
    t205 = 0.2d1 * t195 + 0.2d1 * t197 + 0.2d1 * t200 + t203
    e0f = e0i - t31 + depst - t1 + t205 * delta * t135
    ve1f = ve1i - 0.2d1 * t90 * t135
    ve2f = ve2i + 0.2d1 * t192 * t135
    t212 = t192 ** 2
    t214 = t134 ** 2
    t215 = 0.1d1 / t214
    t218 = ve2i * t192
    t219 = 0.1d1 / delta
    t220 = t219 * t135
    t231 = ve2i ** 2
    dissip = t212 * t2 * t215 * vh2 + (0.4d1 * t218 * t220 * vh2 + 0.4d1 * t212 / t2 * t215 * h2)&
             & * t7 / 0.3d1 + (t231 * vh2 + 0.4d1 * t218 * t220 * h2) * t2 / 0.2d1 + t231 * h2 * &
             &delta
    t249 = t31 - depst + t1
    dsigma = vk0 * t205 * t2 * t135 / 0.3d1 + (0.2d1 * k0 * t205 * t220 - vk0 * t219 * t249&
             ) * t2 / 0.2d1 - k0 * t249 + t51 * delta
end subroutine
