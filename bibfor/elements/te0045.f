      SUBROUTINE TE0045(OPTION,NOMTE)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16      OPTION,NOMTE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 22/06/2009   AUTEUR FLEJOU J-L.FLEJOU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     CALCUL DES CARACTERISTIQUES DE STRUCTURES POUR LES ELEMENTS
C     DISCRETS : MECA_DIS_T_N         MECA_DIS_T_L
C                MECA_DIS_TR_N        MECA_DIS_TR_L
C                MECA_2D_DIS_T_N      MECA_2D_DIS_T_L
C                MECA_2D_DIS_TR_N     MECA_2D_DIS_TR_L
C     ------------------------------------------------------------------
C IN  : OPTION : NOM DE L'OPTION A CALCULER
C IN  : NOMTE  : NOM DU TYPE_ELEMENT
C     ------------------------------------------------------------------
C TOLE  CRP_20
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C     ------------------------------------------------------------------
      CHARACTER*16 CH16,NOMCMD,TYPRES
      CHARACTER*19 NOMFON
      REAL*8       MAT1(78), MAT2(144),ZERO, UN, DEUX, TROIS, PGL(3,3)
      REAL*8       R8BID
      INTEGER      INFODI,IREPM
C     ------------------------------------------------------------------
C
      ZERO  = 0.D0
      DEUX  = 2.D0
      TROIS = 3.D0
C
      INFODI = 1
      CALL GETRES(NOMFON,TYPRES,NOMCMD)
      IF (NOMCMD.EQ.'POST_ELEM') THEN
         CALL INFDIS('SYMM',INFODI,R8BID)
      ENDIF
      IF (OPTION.EQ.'MASS_INER')   THEN
         CALL JEVECH ('PCADISM' ,'L',LMASS)
         CALL JEVECH ('PGEOMER' ,'L',LCOOR )
         CALL JEVECH ('PCAORIE' ,'L',LORIEN)
         CALL JEVECH ('PMASSINE','E',LCASTR)
         DO 2 I = 0,9
            ZR(LCASTR+I) = ZERO
 2       CONTINUE
         CALL MATROT ( ZR(LORIEN) , PGL )
C
C        ============ ELEMENT DE TYPE POI1 ============
C
         CALL INFDIS('REPM',IREPM,R8BID)
         IF ( NOMTE. EQ. 'MECA_DIS_T_N' ) THEN
            IF (INFODI.EQ.1) THEN
               NNO = 1
               NC  = 3
               N   = 6
               IF ( IREPM .EQ. 2 ) THEN
                  CALL UTPSLG ( NNO, NC, PGL, ZR(LMASS), MAT1 )
               ELSE
                  DO 10 I = 1 , N
                     MAT1(I) = ZR(LMASS+I-1)
10                CONTINUE
               ENDIF
C              --- MASSE ---
               DO 12 I = 1 , N
                  ZR(LCASTR) = ZR(LCASTR) + MAT1(I)
12             CONTINUE
               ZR(LCASTR) = ZR(LCASTR) + MAT1(2) + MAT1(4) + MAT1(5)
               ZR(LCASTR) = ZR(LCASTR) / TROIS
C              --- CDG ---
               ZR(LCASTR+1) = ZR(LCOOR  )
               ZR(LCASTR+2) = ZR(LCOOR+1)
               ZR(LCASTR+3) = ZR(LCOOR+2)
C
            ELSEIF (INFODI.EQ.2) THEN
               NNO = 1
               NC  = 3
               N   = 9
               IF ( IREPM .EQ. 2 ) THEN
                  CALL UTPPLG ( NNO, NC, PGL, ZR(LMASS), MAT2 )
               ELSE
                  DO 11 I = 1 , N
                     MAT2(I) = ZR(LMASS+I-1)
11                CONTINUE
               ENDIF
C              --- MASSE ---
               DO 13 I = 1 , N
                  ZR(LCASTR) = ZR(LCASTR) + MAT2(I)
13             CONTINUE
               ZR(LCASTR) = ZR(LCASTR) / TROIS
C              --- CDG ---
               ZR(LCASTR+1) = ZR(LCOOR  )
               ZR(LCASTR+2) = ZR(LCOOR+1)
               ZR(LCASTR+3) = ZR(LCOOR+2)
            ENDIF
C
         ELSEIF ( NOMTE .EQ. 'MECA_DIS_TR_N' ) THEN
            IF (INFODI.EQ.1) THEN
               NNO = 1
               NC  = 6
               N   = 21
               IF ( IREPM .EQ. 2 ) THEN
                  CALL UTPSLG ( NNO, NC, PGL, ZR(LMASS), MAT1 )
               ELSE
                  DO 20 I = 1 , N
                     MAT1(I) = ZR(LMASS+I-1)
20                CONTINUE
               ENDIF
C              --- MASSE ---
               DO 22 I = 1 , 6
                  ZR(LCASTR) = ZR(LCASTR) + MAT1(I)
22             CONTINUE
               ZR(LCASTR) = ZR(LCASTR) + MAT1(2) + MAT1(4) + MAT1(5)
               ZR(LCASTR) = ZR(LCASTR) / TROIS
C              --- CDG ---
               ZR(LCASTR+1) = ZR(LCOOR  )
               ZR(LCASTR+2) = ZR(LCOOR+1)
               ZR(LCASTR+3) = ZR(LCOOR+2)
C              --- INERTIE ---
               ZR(LCASTR+4) = MAT1(10)
               ZR(LCASTR+5) = MAT1(15)
               ZR(LCASTR+6) = MAT1(21)
               ZR(LCASTR+7) = MAT1(14)
               ZR(LCASTR+8) = MAT1(19)
               ZR(LCASTR+9) = MAT1(20)
C
            ELSEIF (INFODI.EQ.2) THEN
               NNO = 1
               NC  = 6
               N   = 36
               IF ( IREPM .EQ. 2 ) THEN
                  CALL UTPPLG ( NNO, NC, PGL, ZR(LMASS), MAT2 )
               ELSE
                  DO 23 I = 1 , N
                     MAT2(I) = ZR(LMASS+I-1)
23                CONTINUE
               ENDIF
C              --- MASSE ---
               DO 24 I = 1 , 6
                  ZR(LCASTR) = ZR(LCASTR) + MAT2(I)
24             CONTINUE
               ZR(LCASTR) = ZR(LCASTR) / TROIS
C              --- CDG ---
               ZR(LCASTR+1) = ZR(LCOOR  )
               ZR(LCASTR+2) = ZR(LCOOR+1)
               ZR(LCASTR+3) = ZR(LCOOR+2)
C              --- INERTIE ---
               ZR(LCASTR+4) = MAT2(22)
               ZR(LCASTR+5) = MAT2(29)
               ZR(LCASTR+6) = MAT2(36)
               ZR(LCASTR+7) = MAT2(28)
               ZR(LCASTR+8) = MAT2(34)
               ZR(LCASTR+9) = MAT2(35)
            ENDIF
C
         ELSEIF ( NOMTE. EQ. 'MECA_2D_DIS_T_N' ) THEN
            IF (INFODI.EQ.1) THEN
               NNO = 1
               NC  = 2
               N   = 3
               IF ( IREPM .EQ. 2 ) THEN
                  CALL UT2MLG ( NNO, NC, PGL, ZR(LMASS), MAT1 )
               ELSE
                  DO 14 I = 1 , N
                     MAT1(I) = ZR(LMASS+I-1)
14                CONTINUE
               ENDIF
C              --- MASSE ---
               DO 15 I = 1 , N
                  ZR(LCASTR) = ZR(LCASTR) + MAT1(I)
15             CONTINUE
               ZR(LCASTR) = ZR(LCASTR) + MAT1(2)
               ZR(LCASTR) = ZR(LCASTR) / DEUX
C              --- CDG ---
               ZR(LCASTR+1) = ZR(LCOOR  )
               ZR(LCASTR+2) = ZR(LCOOR+1)
C
            ELSEIF (INFODI.EQ.2) THEN
               NNO = 1
               NC  = 2
               N   = 4
               IF ( IREPM .EQ. 2 ) THEN
                  CALL UT2PLG ( NNO, NC, PGL, ZR(LMASS), MAT2 )
               ELSE
                  DO 16 I = 1 , N
                     MAT2(I) = ZR(LMASS+I-1)
16                CONTINUE
               ENDIF
C              --- MASSE ---
               DO 17 I = 1 , N
                  ZR(LCASTR) = ZR(LCASTR) + MAT2(I)
17             CONTINUE
               ZR(LCASTR) = ZR(LCASTR) / DEUX
C              --- CDG ---
               ZR(LCASTR+1) = ZR(LCOOR  )
               ZR(LCASTR+2) = ZR(LCOOR+1)
            ENDIF
C
         ELSEIF ( NOMTE .EQ. 'MECA_2D_DIS_TR_N' ) THEN
            IF (INFODI.EQ.1) THEN
               NNO = 1
               NC  = 3
               N   = 6
               IF ( IREPM .EQ. 2 ) THEN
                  CALL UT2MLG ( NNO, NC, PGL, ZR(LMASS), MAT1 )
               ELSE
                  DO 27 I = 1 , N
                     MAT1(I) = ZR(LMASS+I-1)
27                CONTINUE
               ENDIF
C              --- MASSE ---
               ZR(LCASTR) = ZR(LCASTR) + MAT1(1) +DEUX*MAT1(2) +MAT1(3)
               ZR(LCASTR) = ZR(LCASTR) / DEUX
C              --- CDG ---
               ZR(LCASTR+1) = ZR(LCOOR  )
               ZR(LCASTR+2) = ZR(LCOOR+1)
C              --- INERTIE ---
               ZR(LCASTR+3) = MAT1(6)
C
            ELSEIF (INFODI.EQ.2) THEN
               NNO = 1
               NC  = 3
               N   = 9
               IF ( IREPM .EQ. 2 ) THEN
                  CALL UT2PLG ( NNO, NC, PGL, ZR(LMASS), MAT2 )
               ELSE
                  DO 25 I = 1 , N
                     MAT2(I) = ZR(LMASS+I-1)
25                CONTINUE
               ENDIF
C              --- MASSE ---
               ZR(LCASTR) = ZR(LCASTR) / DEUX
C              --- CDG ---
               ZR(LCASTR+1) = ZR(LCOOR  )
               ZR(LCASTR+2) = ZR(LCOOR+1)
C              --- INERTIE ---
               ZR(LCASTR+3) = MAT2(9)
            ENDIF
C
C        ============ ELEMENT DE TYPE SEG2 ============
         ELSEIF ( NOMTE .EQ. 'MECA_DIS_T_L' ) THEN
            IF (INFODI.EQ.1) THEN
               NNO = 2
               NC  = 3
               N   = 21
               IF ( IREPM .EQ. 2 ) THEN
                  CALL UTPSLG ( NNO, NC, PGL, ZR(LMASS), MAT1 )
               ELSE
                  DO 30 I = 1 , N
                     MAT1(I) = ZR(LMASS+I-1)
30                CONTINUE
               ENDIF
C              --- MASSE ---
               DO 32 I = 1 , N
                  ZR(LCASTR) = ZR(LCASTR) + MAT1(I)
32             CONTINUE
               ZR(LCASTR) = ZR(LCASTR)+MAT1(2)+MAT1(4)+MAT1(5)+MAT1(7)
     &                 + MAT1(8)+MAT1(9)+MAT1(11)+MAT1(12)+MAT1(13)
     &                 + MAT1(14)+MAT1(16)+MAT1(17)+MAT1(18)+MAT1(19)
     &                 + MAT1(20)
               ZR(LCASTR) = ZR(LCASTR) / TROIS
C              --- CDG ---
               ZR(LCASTR+1) = ( ZR(LCOOR  ) + ZR(LCOOR+3) ) / DEUX
               ZR(LCASTR+2) = ( ZR(LCOOR+1) + ZR(LCOOR+4) ) / DEUX
               ZR(LCASTR+3) = ( ZR(LCOOR+2) + ZR(LCOOR+5) ) / DEUX
            ELSEIF (INFODI.EQ.2) THEN
               NNO = 2
               NC  = 3
               N   = 36
               IF ( IREPM .EQ. 2 ) THEN
                  CALL UTPPLG ( NNO, NC, PGL, ZR(LMASS), MAT2 )
               ELSE
                  DO 31 I = 1 , N
                     MAT2(I) = ZR(LMASS+I-1)
31                CONTINUE
               ENDIF
C              --- MASSE ---
               DO 33 I = 1 , N
                  ZR(LCASTR) = ZR(LCASTR) + MAT2(I)
33             CONTINUE
               ZR(LCASTR) = ZR(LCASTR) / TROIS
C              --- CDG ---
               ZR(LCASTR+1) = ( ZR(LCOOR  ) + ZR(LCOOR+3) ) / DEUX
               ZR(LCASTR+2) = ( ZR(LCOOR+1) + ZR(LCOOR+4) ) / DEUX
               ZR(LCASTR+3) = ( ZR(LCOOR+2) + ZR(LCOOR+5) ) / DEUX
            ENDIF
C
         ELSEIF ( NOMTE .EQ. 'MECA_DIS_TR_L' ) THEN
            IF (INFODI.EQ.1) THEN
               NNO = 2
               NC  = 6
               N   = 78
               IF ( IREPM .EQ. 2 ) THEN
                  CALL UTPSLG ( NNO, NC, PGL, ZR(LMASS), MAT1 )
               ELSE
                  DO 40 I = 1 , N
                     MAT1(I) = ZR(LMASS+I-1)
40                CONTINUE
               ENDIF
C              --- MASSE ---
               ZR(LCASTR) =  MAT1(2)  + MAT1(4)  + MAT1(5)  +
     &                       MAT1(35) + MAT1(43) + MAT1(44)
               DO 42 I = 1,3
                  I1 = 21 + I
                  I2 = 28 + I
                  I3 = 36 + I
                  ZR(LCASTR) = ZR(LCASTR)+MAT1(I1)+MAT1(I2)+MAT1(I3)
42             CONTINUE
               ZR(LCASTR) = DEUX * ZR(LCASTR)
               ZR(LCASTR) = ZR(LCASTR) + MAT1(1) +MAT1(3) +MAT1(6) +
     &                        MAT1(28)+MAT1(36)+MAT1(45)
               ZR(LCASTR) = ZR(LCASTR) / TROIS
C              --- CDG ---
               ZR(LCASTR+1) = ( ZR(LCOOR  ) + ZR(LCOOR+3) ) / DEUX
               ZR(LCASTR+2) = ( ZR(LCOOR+1) + ZR(LCOOR+4) ) / DEUX
               ZR(LCASTR+3) = ( ZR(LCOOR+2) + ZR(LCOOR+5) ) / DEUX
C
            ELSEIF (INFODI.EQ.2) THEN
               NNO = 2
               NC  = 6
               N   = 144
               IF ( IREPM .EQ. 2 ) THEN
                  CALL UTPPLG ( NNO, NC, PGL, ZR(LMASS), MAT2 )
               ELSE
                  DO 41 I = 1 , N
                     MAT2(I) = ZR(LMASS+I-1)
41                CONTINUE
               ENDIF
C              --- MASSE ---
               DO 43 I = 1 , N
                  ZR(LCASTR) = ZR(LCASTR) + MAT2(I)
43             CONTINUE
               ZR(LCASTR) = ZR(LCASTR) / TROIS
C              --- CDG ---
               ZR(LCASTR+1) = ( ZR(LCOOR  ) + ZR(LCOOR+3) ) / DEUX
               ZR(LCASTR+2) = ( ZR(LCOOR+1) + ZR(LCOOR+4) ) / DEUX
               ZR(LCASTR+3) = ( ZR(LCOOR+2) + ZR(LCOOR+5) ) / DEUX
            ENDIF
C
         ELSEIF ( NOMTE .EQ. 'MECA_2D_DIS_T_L' ) THEN
            IF (INFODI.EQ.1) THEN
               NNO = 2
               NC  = 2
               N   = 10
               IF ( IREPM .EQ. 2 ) THEN
                  CALL UT2MLG ( NNO, NC, PGL, ZR(LMASS), MAT1 )
               ELSE
                  DO 34 I = 1 , N
                     MAT1(I) = ZR(LMASS+I-1)
34                CONTINUE
               ENDIF
C           --- MASSE ---
               DO 35 I = 1 , N
                  ZR(LCASTR) = ZR(LCASTR) + MAT1(I)
35             CONTINUE
               ZR(LCASTR) = ZR(LCASTR)+MAT1(2)+MAT1(4)+MAT1(5)+MAT1(7)
     &                     + MAT1(8)+MAT1(9)
               ZR(LCASTR) = ZR(LCASTR) / DEUX
C              --- CDG ---
               ZR(LCASTR+1) = ( ZR(LCOOR  ) + ZR(LCOOR+2) ) / DEUX
               ZR(LCASTR+2) = ( ZR(LCOOR+1) + ZR(LCOOR+3) ) / DEUX
C
            ELSEIF (INFODI.EQ.2) THEN
               NNO = 2
               NC  = 2
               N   = 16
               IF ( IREPM .EQ. 2 ) THEN
                  CALL UT2PLG ( NNO, NC, PGL, ZR(LMASS), MAT2 )
               ELSE
                  DO 39 I = 1 , N
                     MAT2(I) = ZR(LMASS+I-1)
39                CONTINUE
               ENDIF
C              --- MASSE ---
               DO 37 I = 1 , N
                  ZR(LCASTR) = ZR(LCASTR) + MAT2(I)
37             CONTINUE
               ZR(LCASTR) = ZR(LCASTR) / DEUX
C              --- CDG ---
               ZR(LCASTR+1) = ( ZR(LCOOR  ) + ZR(LCOOR+2) ) / DEUX
               ZR(LCASTR+2) = ( ZR(LCOOR+1) + ZR(LCOOR+3) ) / DEUX
            ENDIF
C
         ELSEIF ( NOMTE .EQ. 'MECA_2D_DIS_TR_L' ) THEN
            IF (INFODI.EQ.1) THEN
               NNO = 2
               NC  = 3
               N   = 21
               IF ( IREPM .EQ. 2 ) THEN
                  CALL UT2MLG ( NNO, NC, PGL, ZR(LMASS), MAT1 )
               ELSE
                  DO 36 I = 1 , N
                     MAT1(I) = ZR(LMASS+I-1)
36                CONTINUE
               ENDIF
C              --- MASSE ---
               ZR(LCASTR) = MAT1(2)  + MAT1(7)  + MAT1(8)  +
     &                     MAT1(11) + MAT1(12) + MAT1(14)
               ZR(LCASTR) = DEUX * ZR(LCASTR)
               ZR(LCASTR) = ZR(LCASTR) + MAT1(1)  + MAT1(3)  +
     &                      MAT1(10) + MAT1(15)
               ZR(LCASTR) = ZR(LCASTR) / DEUX
C              --- CDG ---
               ZR(LCASTR+1) = ( ZR(LCOOR  ) + ZR(LCOOR+2) ) / DEUX
               ZR(LCASTR+2) = ( ZR(LCOOR+1) + ZR(LCOOR+3) ) / DEUX
C
            ELSEIF (INFODI.EQ.2) THEN
               NNO = 2
               NC  = 3
               N   = 36
               IF ( IREPM .EQ. 2 ) THEN
                  CALL UT2PLG ( NNO, NC, PGL, ZR(LMASS), MAT2 )
               ELSE
                  DO 99 I = 1 , N
                     MAT2(I) = ZR(LMASS+I-1)
99                CONTINUE
               ENDIF
C              --- MASSE ---
               ZR(LCASTR) = ZR(LCASTR) / DEUX
C              --- CDG ---
               ZR(LCASTR+1) = ( ZR(LCOOR  ) + ZR(LCOOR+2) ) / DEUX
               ZR(LCASTR+2) = ( ZR(LCOOR+1) + ZR(LCOOR+3) ) / DEUX
            ENDIF
         ENDIF
C
      ELSE
         CH16 = OPTION
         CALL U2MESK('F','ELEMENTS2_47',1,CH16)
      ENDIF
      END
