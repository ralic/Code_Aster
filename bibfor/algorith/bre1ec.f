      SUBROUTINE BRE1EC(K0,K1,K2,ETA1,ETA2,E1I,E2I,A,T,B,E2P,PW,E1F)

C     ROUTINE ANCIENNEMENT NOMMEE FLE1EC

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 08/12/2009   AUTEUR DEBONNIERES P.DE-BONNIERES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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

      IMPLICIT REAL*8(A-Z)

      T2 = ETA1 * K1
      T3 = K1 * ETA2
      T4 = K0 * ETA2
      T5 = K2 * ETA1
      T6 = ETA1 ** 2
      T7 = K1 ** 2
      T8 = T7 * T6
      T9 = T7 * ETA1
      T14 = K1 * T6
      T17 = ETA2 ** 2
      T18 = T17 * T7
      T24 = K0 ** 2
      T28 = K2 ** 2
      T31 = SQRT(T8 + 0.2D1 * ETA2 * T9 - 0.2D1 * T4 * T2 + 0.2D1 * K2 *
     # T14 + T18 + 0.2D1 * K0 * T17 * K1 - 0.2D1 * T5 * T3 + T17 * T24 -
     # 0.2D1 * T5 * T4 + T6 * T28)
      T33 = 0.1D1 / ETA1
      T36 = 0.1D1 / ETA2 * T
      T39 = EXP(-T36 * T33 * (T2 + T3 + T4 + T5 - T31) / 0.2D1)
      T40 = ETA2 * T24
      T41 = T7 * E1I
      T44 = 0.4D1 * K2 * T41 * T40
      T45 = T28 * K2
      T46 = E2P * T45
      T48 = ETA1 * K0 * K1
      T50 = 0.3D1 * T48 * T46
      T51 = K1 * T31
      T52 = K2 * T24
      T53 = E2P * T52
      T54 = T53 * T51
      T56 = ETA1 * A
      T59 = 0.2D1 * K1 * T56 * K2 * T40
      T61 = K0 * K2 * PW
      T62 = T61 * T9
      T63 = T24 * K0
      T64 = ETA2 * T63
      T68 = 0.2D1 * E1I * K1 * K2 * T64
      T69 = T7 * K1
      T70 = T69 * ETA1
      T71 = T28 * E1I
      T72 = T71 * T70
      T73 = T71 * T64
      T74 = T24 * E1I
      T75 = T74 * T70
      T76 = B * T28
      T77 = T76 * T64
      T78 = PW * T28
      T79 = T78 * T40
      T80 = -T44 - T50 + T54 - T59 - T62 - T68 - T72 - T73 - T75 + T77 +
     # T79
      T82 = T24 * T7
      T84 = 0.2D1 * T82 * PW * ETA2
      T85 = ETA1 * T45
      T86 = T41 * T85
      T87 = T74 * T85
      T89 = A * K0
      T90 = T89 * T6 * T45
      T91 = K1 * PW
      T92 = T91 * T85
      T94 = B * T24 * T85
      T96 = K0 * PW * T85
      T97 = ETA2 * T69
      T98 = T71 * T97
      T99 = T74 * T97
      T100 = ETA2 * T7
      T101 = T78 * T100
      T102 = E1I * T31
      T103 = T82 * T102
      T104 = T84 + T86 + T87 + T90 - T92 - T94 - T96 - T98 - T99 + T101 
     #+ T103
      T106 = T78 * T51
      T107 = T24 * T31
      T108 = T76 * T107
      T109 = E2P * T28
      T110 = T109 * T107
      T111 = T41 * T64
      T112 = K0 * E1I
      T115 = 0.2D1 * K1 * T112 * T85
      T116 = T45 * E2I
      T117 = T24 * ETA1
      T119 = 0.2D1 * T117 * T116
      T121 = 0.2D1 * T9 * T116
      T122 = T63 * E2I
      T124 = 0.2D1 * T100 * T122
      T125 = T28 * T7
      T126 = T125 * T102
      T127 = T28 * T24
      T128 = T127 * T102
      T129 = T117 * T46
      T130 = -T106 - T108 + T110 - T111 + T115 + T119 + T121 - T124 + T1
     #26 + T128 - T129
      T140 = T28 * ETA2
      T143 = K1 * K2
      T147 = K0 * T28
      T148 = B * T147
      T152 = E2P * T147
      T154 = K2 * T100
      T155 = ETA1 * T89
      T157 = -0.2D1 * T9 * T46 + 0.2D1 * T100 * B * T63 - A * T24 * T18 
     #- T78 * T9 + T109 * T64 - 0.2D1 * T140 * T122 + 0.3D1 * PW * T143 
     #* T40 + T148 * T100 - T53 * T9 + T53 * T100 + T152 * T100 + T155 *
     # T154
      T160 = T24 * E2I
      T163 = E2I * ETA1
      T164 = T24 * K1
      T177 = A * T28
      T187 = T28 * K1
      T194 = -0.4D1 * T154 * T160 + 0.4D1 * T28 * T164 * T163 - 0.4D1 * 
     #K2 * T3 * T122 + 0.4D1 * T48 * T116 - T148 * T9 - K1 * A * T17 * T
     #63 + 0.2D1 * K0 * T177 * T14 + K0 * A * K2 * T8 + 0.3D1 * B * T143
     # * T64 + 0.2D1 * E2P * T187 * T40 - 0.2D1 * PW * T147 * T2
      T197 = 0.2D1 * B * T127 * T2
      T198 = B * T52
      T199 = T198 * T51
      T200 = T148 * T51
      T203 = T48 * A * K2 * T31
      T205 = T28 * T74 * T2
      T208 = 0.2D1 * K2 * T82 * T163
      T211 = 0.4D1 * K0 * T125 * T163
      T214 = 0.3D1 * K1 * T71 * T40
      T217 = 0.3D1 * T28 * T41 * T4
      T218 = K1 * T140
      T220 = 0.4D1 * T218 * T160
      T222 = K2 * T74 * T9
      T223 = -T197 - T199 - T200 + T203 + T205 + T208 + T211 - T214 - T2
     #17 - T220 - T222
      T228 = 0.2D1 * T28 * T100 * E2I * K0
      T231 = 0.2D1 * PW * T187 * T4
      T232 = K2 * T164
      T234 = 0.2D1 * T232 * T102
      T235 = K1 * T147
      T237 = 0.2D1 * T235 * T102
      T238 = A * T40
      T239 = T238 * T51
      T241 = T78 * K0 * T31
      T242 = T238 * T9
      T244 = T155 * T28 * T31
      T246 = E2P * T143 * T64
      T247 = T198 * T9
      T249 = 0.3D1 * T152 * T9
      T250 = -T228 + T231 + T234 + T237 - T239 - T241 - T242 + T244 + T2
     #46 - T247 - T249
      T251 = T155 * T218
      T252 = K2 * T112
      T254 = 0.2D1 * T252 * T97
      T257 = 0.2D1 * B * T187 * T40
      T259 = ETA1 * T177 * T40
      T262 = 0.2D1 * E2P * T127 * T2
      T264 = K2 * K0 * T7
      T266 = 0.2D1 * T264 * T102
      T267 = T61 * T51
      T268 = T152 * T51
      T269 = K1 * K0
      T271 = B * T269 * T85
      T273 = 0.2D1 * T252 * T70
      T275 = 0.3D1 * T61 * T100
      T277 = 0.3D1 * T198 * T100
      T278 = -T251 - T254 + T257 - T259 - T262 + T266 - T267 + T268 - T2
     #71 - T273 + T275 + T277
      T289 = 0.1D1 / (T82 + T125 + 0.2D1 * T264 + 0.2D1 * T232 + T127 + 
     #0.2D1 * T235) / T31
      T296 = EXP(-T36 * T33 * (T2 + T3 + T4 + T5 + T31) / 0.2D1)
      T297 = -T44 - T50 - T54 - T59 - T62 - T68 - T72 - T73 - T75 + T77 
     #+ T79
      T298 = T84 + T86 + T87 + T90 - T92 - T94 - T96 - T98 - T99 + T101 
     #- T103
      T300 = T106 + T108 - T110 - T111 + T115 + T119 + T121 - T124 - T12
     #6 - T128 - T129
      T303 = -T197 + T199 + T200 - T203 + T205 + T208 + T211 - T214 - T2
     #17 - T220 - T222
      T305 = -T228 + T231 - T234 - T237 + T239 + T241 - T242 - T244 + T2
     #46 - T247 - T249
      T306 = -T251 - T254 + T257 - T259 - T262 - T266 + T267 - T268 - T2
     #71 - T273 + T275 + T277
      T314 = A * T + B - E2P
      T331 = ((K1 + K0) * K2 + T269) ** 2
      E1F = T289 * (T80 + T104 + T130 + T157 + T194 + T22
     #3 + T250 + T278) * T39 / 0.2D1 - T289 * (T297 + T298 + T300 + T157
     # + T194 + T303 + T305 + T306) * T296 / 0.2D1 + 0.1D1 / T331 * (T28
     # * (T24 * T314 + K0 * (K1 * T314 - T56 + PW) + T91) + T143 * (K0 *
     # T314 - T56 + PW) * K0 + ETA2 * A * T164)

      END
