      SUBROUTINE BRE2EC(K0,K1,K2,ETA1,ETA2,E1I,E2I,A,T,B,E2P,PW,E2F)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.
C
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.
C
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================

C     CALCUL DE E2F AVEC HYPOTHESE D ECOULEMENT ETAGE 2

      IMPLICIT NONE


C-----------------------------------------------------------------------
      REAL*8 K0 ,K1 ,K2
      REAL*8 A ,B ,E1I ,E2F ,E2I ,E2P ,ETA1
      REAL*8 ETA2 ,PW ,T ,T10 ,T101 ,T106 ,T108
      REAL*8 T109 ,T11 ,T110 ,T111 ,T112 ,T113 ,T115
      REAL*8 T116 ,T117 ,T118 ,T119 ,T12 ,T120 ,T121
      REAL*8 T122 ,T123 ,T124 ,T125 ,T126 ,T128 ,T129
      REAL*8 T131 ,T132 ,T133 ,T141 ,T144 ,T149 ,T154
      REAL*8 T158 ,T162 ,T17 ,T172 ,T175 ,T191 ,T193
      REAL*8 T2 ,T20 ,T200 ,T21 ,T211 ,T214 ,T219
      REAL*8 T228 ,T229 ,T230 ,T232 ,T234 ,T235 ,T237
      REAL*8 T238 ,T239 ,T240 ,T242 ,T244 ,T246 ,T247
      REAL*8 T249 ,T250 ,T252 ,T253 ,T254 ,T255 ,T256
      REAL*8 T257 ,T260 ,T261 ,T263 ,T265 ,T267 ,T268
      REAL*8 T27 ,T270 ,T273 ,T276 ,T279 ,T280 ,T291
      REAL*8 T292 ,T295 ,T298 ,T299 ,T301 ,T304 ,T305
      REAL*8 T31 ,T310 ,T313 ,T330 ,T331 ,T333 ,T336
      REAL*8 T34 ,T36 ,T37 ,T38 ,T39 ,T42 ,T43
      REAL*8 T44 ,T45 ,T48 ,T49 ,T5 ,T50 ,T51
      REAL*8 T52 ,T53 ,T54 ,T55 ,T56 ,T57 ,T58
      REAL*8 T6 ,T60 ,T61 ,T62 ,T63 ,T64 ,T65
      REAL*8 T67 ,T7 ,T71 ,T72 ,T73 ,T75 ,T76
      REAL*8 T78 ,T8 ,T81 ,T82 ,T83 ,T84 ,T85
      REAL*8 T87 ,T9 ,T90 ,T92 ,T95 ,T96
C-----------------------------------------------------------------------
      T2 = A * K0
      T5 = ETA1 * K1
      T6 = K1 * ETA2
      T7 = K0 * ETA2
      T8 = K2 * ETA1
      T9 = ETA1 ** 2
      T10 = K1 ** 2
      T11 = T10 * T9
      T12 = T10 * ETA1
      T17 = K1 * T9
      T20 = ETA2 ** 2
      T21 = T20 * T10
      T27 = K0 ** 2
      T31 = K2 ** 2
      T34 = SQRT(T11 + 0.2D1 * ETA2 * T12 - 0.2D1 * T7 * T5 + 0.2D1 * K2
     # * T17 + T21 + 0.2D1 * K0 * T20 * K1 - 0.2D1 * T8 * T6 + T20 * T27
     # - 0.2D1 * T8 * T7 + T9 * T31)
      T36 = 0.1D1 / ETA1
      T37 = T36 * (T5 + T6 + T7 + T8 - T34)
      T38 = 0.1D1 / ETA2
      T39 = T * T38
      T42 = EXP(-T39 * T37 / 0.2D1)
      T43 = T27 * K0
      T44 = ETA2 * T43
      T45 = K1 * K2
      T48 = 0.3D1 * B * T45 * T44
      T49 = ETA2 * T10
      T50 = K0 * T31
      T51 = E2P * T50
      T52 = T51 * T49
      T53 = T31 * T6
      T54 = ETA1 * T2
      T55 = T54 * T53
      T56 = K1 * T34
      T57 = T51 * T56
      T58 = A * K2
      T60 = K0 * T58 * T11
      T61 = K2 * T27
      T62 = B * T61
      T63 = T62 * T56
      T64 = B * T50
      T65 = T64 * T56
      T67 = T54 * T31 * T34
      T71 = K0 * ETA1 * K1
      T72 = T71 * A * K2 * T34
      T73 = T27 * E2I
      T75 = 0.4D1 * T53 * T73
      T76 = ETA2 * T27
      T78 = ETA1 * A
      T81 = 0.2D1 * K1 * T78 * K2 * T76
      T82 = T48 + T52 - T55 + T57 + T60 - T63 - T65 + T67 + T72 - T75 -
     #T81
      T83 = T10 * K1
      T84 = T83 * ETA1
      T85 = T27 * E1I
      T87 = T31 * E1I
      T90 = PW * T31
      T92 = ETA2 * T83
      T95 = T31 * K2
      T96 = ETA1 * T95
      T101 = K1 * PW
      T106 = -T85 * T84 - T87 * T44 - T87 * T84 + T90 * T49 - T85 * T92
     #- T87 * T92 - K0 * PW * T96 - B * T27 * T96 - T101 * T96 + T2 * T9
     # * T95 + T85 * T96
      T108 = T10 * E1I
      T109 = T108 * T96
      T110 = T108 * T44
      T111 = T27 * T34
      T112 = E2P * T31
      T113 = T112 * T111
      T115 = T90 * K0 * T34
      T116 = B * T31
      T117 = T116 * T111
      T118 = T90 * T56
      T119 = E1I * T34
      T120 = T27 * T10
      T121 = T120 * T119
      T122 = T31 * T27
      T123 = T122 * T119
      T124 = T31 * T10
      T125 = T124 * T119
      T126 = T43 * E2I
      T128 = 0.2D1 * T49 * T126
      T129 = T95 * E2I
      T131 = 0.2D1 * T12 * T129
      T132 = T109 - T110 + T113 - T115 - T117 - T118 + T121 + T123 + T12
     #5 - T128 + T131
      T133 = T27 * ETA1
      T141 = K1 * A
      T144 = T27 * A
      T149 = E2P * T95
      T154 = A * T31
      T158 = E2I * ETA1
      T162 = 0.2D1 * T133 * T129 - 0.2D1 * T31 * ETA2 * T126 + T112 * T4
     #4 - T141 * T20 * T43 - T90 * T12 - T144 * T21 + 0.2D1 * T49 * B *
     #T43 - 0.2D1 * T12 * T149 - T133 * T149 - T64 * T12 + 0.2D1 * K0 *
     #T154 * T17 + 0.2D1 * K2 * T120 * T158
      T172 = K2 * T49
      T175 = K1 * K0
      T191 = E2P * T61
      T193 = T31 * K1
      T200 = -0.2D1 * T31 * T49 * E2I * K0 + 0.4D1 * K0 * T124 * T158 -
     #0.4D1 * T172 * T73 - B * T175 * T96 - 0.4D1 * K2 * T108 * T76 - 0.
     #2D1 * E1I * K1 * K2 * T44 - 0.3D1 * K1 * T87 * T76 - 0.3D1 * T31 *
     # T108 * T7 - T191 * T12 + 0.2D1 * PW * T193 * T7 + 0.2D1 * B * T19
     #3 * T76
      T211 = A * T76
      T214 = K0 * K2 * PW
      T219 = T27 * K1
      T228 = -ETA1 * T154 * T76 + E2P * T45 * T44 + 0.3D1 * PW * T45 * T
     #76 - 0.2D1 * B * T122 * T5 - T211 * T12 - T214 * T12 - T62 * T12 -
     # 0.3D1 * T51 * T12 + 0.4D1 * T31 * T219 * T158 - 0.4D1 * K2 * T6 *
     # T126 + 0.4D1 * T71 * T129
      T229 = T200 + T228
      T230 = T191 * T49
      T232 = 0.3D1 * T214 * T49
      T234 = 0.3D1 * T62 * T49
      T235 = PW * ETA2
      T237 = 0.2D1 * T120 * T235
      T238 = T90 * T76
      T239 = T116 * T44
      T240 = T54 * T172
      T242 = 0.3D1 * T71 * T149
      T244 = K2 * K0 * T10
      T246 = 0.2D1 * T244 * T119
      T247 = K2 * T219
      T249 = 0.2D1 * T247 * T119
      T250 = K1 * T50
      T252 = 0.2D1 * T250 * T119
      T253 = T230 + T232 + T234 + T237 + T238 + T239 + T240 - T242 + T24
     #6 + T249 + T252
      T254 = T211 * T56
      T255 = T191 * T56
      T256 = T214 * T56
      T257 = K0 * E1I
      T260 = 0.2D1 * K1 * T257 * T96
      T261 = K2 * T257
      T263 = 0.2D1 * T261 * T84
      T265 = K2 * T85 * T12
      T267 = T31 * T85 * T5
      T268 = T64 * T49
      T270 = 0.2D1 * T261 * T92
      T273 = 0.2D1 * E2P * T193 * T76
      T276 = 0.2D1 * E2P * T122 * T5
      T279 = 0.2D1 * PW * T50 * T5
      T280 = -T254 + T255 - T256 + T260 - T263 - T265 + T267 + T268 - T2
     #70 + T273 - T276 - T279
      T291 = 0.1D1 / (T120 + T124 + 0.2D1 * T244 + 0.2D1 * T247 + T122 +
     # 0.2D1 * T250) / T34
      T292 = T291 * (T82 + T106 + T132 + T162 + T229 + T253 + T280) * T4
     #2
      T295 = T36 * (T5 + T6 + T7 + T8 + T34)
      T298 = EXP(-T39 * T295 / 0.2D1)
      T299 = T48 + T52 - T55 - T57 + T60 + T63 + T65 - T67 - T72 - T75 -
     # T81
      T301 = T109 - T110 - T113 + T115 + T117 + T118 - T121 - T123 - T12
     #5 - T128 + T131
      T304 = T230 + T232 + T234 + T237 + T238 + T239 + T240 - T242 - T24
     #6 - T249 - T252
      T305 = T254 - T255 + T256 + T260 - T263 - T265 + T267 + T268 - T27
     #0 + T273 - T276 - T279
      T310 = T291 * (T299 + T106 + T301 + T162 + T229 + T304 + T305) * T
     #298
      T313 = A * T + B - E2P
      T330 = ((K1 + K0) * K2 + T175) ** 2
      T331 = 0.1D1 / T330
      T333 = T292 / 0.2D1 - T310 / 0.2D1 + T331 * (T31 * (T27 * T313 + K
     #0 * (K1 * T313 - T78 + PW) + T101) + T45 * (K0 * T313 - T78 + PW)
     #* K0 + ETA2 * A * T219)
      T336 = T333 * K1
      E2F = 0.1D1 / (T8 - T7) * (-T * ETA2 * T2 + ETA2 *
     #K0 * T333 + ETA1 * T336 + ETA2 * T336 - T235 + ETA2 * ETA1 * (-T29
     #2 * T38 * T37 / 0.4D1 + T310 * T38 * T295 / 0.4D1 + T331 * (T31 *
     #(T144 + K0 * T141) + T58 * T219)) - K0 * B * ETA2 + K2 * E2P * ETA
     #1)

      END
