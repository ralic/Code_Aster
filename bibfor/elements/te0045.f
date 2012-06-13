      SUBROUTINE TE0045(OPTION,NOMTE)
      IMPLICIT          NONE
      INCLUDE 'jeveux.h'
      CHARACTER*16      OPTION,NOMTE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C
C
C     ------------------------------------------------------------------
      CHARACTER*8    K8BID
      CHARACTER*16   CH16,KMESS(4)
      REAL*8         MAT1(78),MAT2(144),ZERO,DEUX,TROIS,PGL(3,3),R8BID
      INTEGER        INFODI,IREPM,NBTERM,NNO,NC,NDIM,ITYPE,I,I1,I2,I3
      INTEGER        LMASS,LCOOR,LORIEN,LCASTR,IBID
      PARAMETER     (ZERO=0.D0,DEUX=2.D0,TROIS=3.D0)
C     ------------------------------------------------------------------
C
      CALL INFDIS('SYMM',INFODI,R8BID,K8BID)
      IF ( OPTION.NE.'MASS_INER' ) THEN
         CH16 = OPTION
         CALL U2MESK('F','ELEMENTS2_47',1,CH16)
      ENDIF

C     ON VERIFIE QUE LES CARACTERISTIQUES ONT ETE AFFECTEES
C     LE CODE DU DISCRET
      CALL INFDIS('CODE',IBID,R8BID,NOMTE)
C     LE CODE STOKE DANS LA CARTE
      CALL INFDIS('TYDI',INFODI,R8BID,K8BID)
      IF (INFODI.NE.IBID) THEN
         CALL U2MESK('F+','DISCRETS_25',1,NOMTE)
         CALL INFDIS('DUMP',IBID,R8BID,'F+')
      ENDIF
C     DISCRET DE TYPE MASSE
      CALL INFDIS('DISM',INFODI,R8BID,K8BID)
      IF (INFODI.EQ.0) THEN
         CALL U2MESK('A+','DISCRETS_26',1,NOMTE)
         CALL INFDIS('DUMP',IBID,R8BID,'A+')
      ENDIF

C     MATRICES DE MASSE SYMETRIQUE ?
      CALL INFDIS('SYMM',INFODI,R8BID,K8BID)
C --- INFORMATIONS SUR LES DISCRETS :
C        NBTERM   = NOMBRE DE COEFFICIENTS DANS K
C        NNO      = NOMBRE DE NOEUDS
C        NC       = NOMBRE DE COMPOSANTE PAR NOEUD
C        NDIM     = DIMENSION DE L'ELEMENT
C        ITYPE    = TYPE DE L'ELEMENT
      CALL INFTED(NOMTE,INFODI,NBTERM,NNO,NC,NDIM,ITYPE)
C      NEQ = NNO*NC

      CALL JEVECH ('PCADISM' ,'L',LMASS)
      CALL JEVECH ('PGEOMER' ,'L',LCOOR )
      CALL JEVECH ('PCAORIE' ,'L',LORIEN)
      CALL JEVECH ('PMASSINE','E',LCASTR)
      DO 2 I = 0 , 9
         ZR(LCASTR+I) = ZERO
2     CONTINUE
      CALL MATROT(ZR(LORIEN),PGL )

C     REPERE DE LA MATRICE DE MASSE ?
      CALL INFDIS('REPM',IREPM,R8BID,K8BID)
      IF (INFODI.EQ.1) THEN
         IF ( IREPM .EQ. 2 ) THEN
            IF ( NDIM.EQ.3 ) THEN
               CALL UTPSLG(NNO,NC,PGL,ZR(LMASS),MAT1)
            ELSE
               CALL UT2MLG(NNO,NC,PGL,ZR(LMASS),MAT1)
            ENDIF
         ELSE
            DO 10 I = 1 , NBTERM
               MAT1(I) = ZR(LMASS+I-1)
10          CONTINUE
         ENDIF
      ELSEIF (INFODI.EQ.2) THEN
         IF ( IREPM .EQ. 2 ) THEN
            IF ( NDIM.EQ.3 ) THEN
               CALL UTPPLG(NNO,NC,PGL,ZR(LMASS),MAT2)
            ELSE
               CALL UT2PLG(NNO,NC,PGL,ZR(LMASS),MAT2)
            ENDIF
         ELSE
            DO 11 I = 1 , NBTERM
               MAT2(I) = ZR(LMASS+I-1)
11          CONTINUE
         ENDIF
      ENDIF

C
C     ============ ELEMENT DE TYPE POI1 ============
      IF ( NOMTE. EQ. 'MECA_DIS_T_N' ) THEN
C        --- CDG ---
         ZR(LCASTR+1) = ZR(LCOOR  )
         ZR(LCASTR+2) = ZR(LCOOR+1)
         ZR(LCASTR+3) = ZR(LCOOR+2)
         IF (INFODI.EQ.1) THEN
C           --- MASSE ---
            DO 12 I = 1 , NBTERM
               ZR(LCASTR) = ZR(LCASTR) + MAT1(I)
12          CONTINUE
            ZR(LCASTR) = ZR(LCASTR) + MAT1(2) + MAT1(4) + MAT1(5)
            ZR(LCASTR) = ZR(LCASTR) / TROIS
         ELSEIF (INFODI.EQ.2) THEN
C           --- MASSE ---
            DO 13 I = 1 , NBTERM
               ZR(LCASTR) = ZR(LCASTR) + MAT2(I)
13          CONTINUE
            ZR(LCASTR) = ZR(LCASTR) / TROIS
         ENDIF
C
      ELSEIF ( NOMTE .EQ. 'MECA_DIS_TR_N' ) THEN
C        --- CDG ---
         ZR(LCASTR+1) = ZR(LCOOR  )
         ZR(LCASTR+2) = ZR(LCOOR+1)
         ZR(LCASTR+3) = ZR(LCOOR+2)
         IF (INFODI.EQ.1) THEN
C           --- MASSE ---
            DO 22 I = 1 , NC
               ZR(LCASTR) = ZR(LCASTR) + MAT1(I)
22          CONTINUE
            ZR(LCASTR) = ZR(LCASTR) + MAT1(2) + MAT1(4) + MAT1(5)
            ZR(LCASTR) = ZR(LCASTR) / TROIS
C           --- INERTIE ---
            ZR(LCASTR+4) = MAT1(10)
            ZR(LCASTR+5) = MAT1(15)
            ZR(LCASTR+6) = MAT1(21)
            ZR(LCASTR+7) = MAT1(14)
            ZR(LCASTR+8) = MAT1(19)
            ZR(LCASTR+9) = MAT1(20)
         ELSEIF (INFODI.EQ.2) THEN
C           --- MASSE ---
            DO 24 I = 1 , NC
               ZR(LCASTR) = ZR(LCASTR) + MAT2(I)
24          CONTINUE
            ZR(LCASTR) = ZR(LCASTR) / TROIS
C           --- INERTIE ---
            ZR(LCASTR+4) = MAT2(22)
            ZR(LCASTR+5) = MAT2(29)
            ZR(LCASTR+6) = MAT2(36)
            ZR(LCASTR+7) = MAT2(28)
            ZR(LCASTR+8) = MAT2(34)
            ZR(LCASTR+9) = MAT2(35)
         ENDIF
C
      ELSEIF ( NOMTE. EQ. 'MECA_2D_DIS_T_N' ) THEN
C        --- CDG ---
         ZR(LCASTR+1) = ZR(LCOOR  )
         ZR(LCASTR+2) = ZR(LCOOR+1)
         IF (INFODI.EQ.1) THEN
C           --- MASSE ---
            DO 15 I = 1 , NBTERM
               ZR(LCASTR) = ZR(LCASTR) + MAT1(I)
15          CONTINUE
            ZR(LCASTR) = ZR(LCASTR) + MAT1(2)
            ZR(LCASTR) = ZR(LCASTR) / DEUX
         ELSEIF (INFODI.EQ.2) THEN
C           --- MASSE ---
            DO 17 I = 1 , NBTERM
               ZR(LCASTR) = ZR(LCASTR) + MAT2(I)
17          CONTINUE
            ZR(LCASTR) = ZR(LCASTR) / DEUX
         ENDIF
C
      ELSEIF ( NOMTE .EQ. 'MECA_2D_DIS_TR_N' ) THEN
C        --- CDG ---
         ZR(LCASTR+1) = ZR(LCOOR  )
         ZR(LCASTR+2) = ZR(LCOOR+1)
         IF (INFODI.EQ.1) THEN
C           --- MASSE ---
            ZR(LCASTR) = ZR(LCASTR) + MAT1(1) +DEUX*MAT1(2) +MAT1(3)
            ZR(LCASTR) = ZR(LCASTR) / DEUX
C           --- INERTIE ---
            ZR(LCASTR+3) = MAT1(6)
         ELSEIF (INFODI.EQ.2) THEN
C           --- MASSE ---
            ZR(LCASTR) = ZR(LCASTR) / DEUX
C           --- INERTIE ---
            ZR(LCASTR+3) = MAT2(9)
         ENDIF
C
C     ============ ELEMENT DE TYPE SEG2 ============
      ELSEIF ( NOMTE .EQ. 'MECA_DIS_T_L' ) THEN
C        --- CDG ---
         ZR(LCASTR+1) = ( ZR(LCOOR  ) + ZR(LCOOR+3) ) / DEUX
         ZR(LCASTR+2) = ( ZR(LCOOR+1) + ZR(LCOOR+4) ) / DEUX
         ZR(LCASTR+3) = ( ZR(LCOOR+2) + ZR(LCOOR+5) ) / DEUX
         IF (INFODI.EQ.1) THEN
C           --- MASSE ---
            DO 32 I = 1 , NBTERM
               ZR(LCASTR) = ZR(LCASTR) + MAT1(I)
32          CONTINUE
            ZR(LCASTR) = ZR(LCASTR)+MAT1(2)+MAT1(4)+MAT1(5)+MAT1(7)
     &              + MAT1(8)+MAT1(9)+MAT1(11)+MAT1(12)+MAT1(13)
     &              + MAT1(14)+MAT1(16)+MAT1(17)+MAT1(18)+MAT1(19)
     &              + MAT1(20)
            ZR(LCASTR) = ZR(LCASTR) / TROIS
         ELSEIF (INFODI.EQ.2) THEN
C           --- MASSE ---
            DO 33 I = 1 , NBTERM
               ZR(LCASTR) = ZR(LCASTR) + MAT2(I)
33          CONTINUE
            ZR(LCASTR) = ZR(LCASTR) / TROIS
         ENDIF
C
      ELSEIF ( NOMTE .EQ. 'MECA_DIS_TR_L' ) THEN
C        --- CDG ---
         ZR(LCASTR+1) = ( ZR(LCOOR  ) + ZR(LCOOR+3) ) / DEUX
         ZR(LCASTR+2) = ( ZR(LCOOR+1) + ZR(LCOOR+4) ) / DEUX
         ZR(LCASTR+3) = ( ZR(LCOOR+2) + ZR(LCOOR+5) ) / DEUX
         IF (INFODI.EQ.1) THEN
C           --- MASSE ---
            ZR(LCASTR) =  MAT1(2)  + MAT1(4)  + MAT1(5)  +
     &                    MAT1(35) + MAT1(43) + MAT1(44)
            DO 42 I = 1,3
               I1 = 21 + I
               I2 = 28 + I
               I3 = 36 + I
               ZR(LCASTR) = ZR(LCASTR)+MAT1(I1)+MAT1(I2)+MAT1(I3)
42          CONTINUE
            ZR(LCASTR) = DEUX * ZR(LCASTR)
            ZR(LCASTR) = ZR(LCASTR) + MAT1(1) +MAT1(3) +MAT1(6) +
     &                     MAT1(28)+MAT1(36)+MAT1(45)
            ZR(LCASTR) = ZR(LCASTR) / TROIS
         ELSEIF (INFODI.EQ.2) THEN
C           --- MASSE ---
            DO 43 I = 1 , NBTERM
               ZR(LCASTR) = ZR(LCASTR) + MAT2(I)
43          CONTINUE
            ZR(LCASTR) = ZR(LCASTR) / TROIS
         ENDIF
C
      ELSEIF ( NOMTE .EQ. 'MECA_2D_DIS_T_L' ) THEN
C        --- CDG ---
         ZR(LCASTR+1) = ( ZR(LCOOR  ) + ZR(LCOOR+2) ) / DEUX
         ZR(LCASTR+2) = ( ZR(LCOOR+1) + ZR(LCOOR+3) ) / DEUX
         IF (INFODI.EQ.1) THEN
C        --- MASSE ---
            DO 35 I = 1 , NBTERM
               ZR(LCASTR) = ZR(LCASTR) + MAT1(I)
35          CONTINUE
            ZR(LCASTR) = ZR(LCASTR)+MAT1(2)+MAT1(4)+MAT1(5)+MAT1(7)
     &                  + MAT1(8)+MAT1(9)
            ZR(LCASTR) = ZR(LCASTR) / DEUX
         ELSEIF (INFODI.EQ.2) THEN
C           --- MASSE ---
            DO 37 I = 1 , NBTERM
               ZR(LCASTR) = ZR(LCASTR) + MAT2(I)
37          CONTINUE
            ZR(LCASTR) = ZR(LCASTR) / DEUX
         ENDIF
C
      ELSEIF ( NOMTE .EQ. 'MECA_2D_DIS_TR_L' ) THEN
C        --- CDG ---
         ZR(LCASTR+1) = ( ZR(LCOOR  ) + ZR(LCOOR+2) ) / DEUX
         ZR(LCASTR+2) = ( ZR(LCOOR+1) + ZR(LCOOR+3) ) / DEUX
         IF (INFODI.EQ.1) THEN
C           --- MASSE ---
            ZR(LCASTR) = MAT1(2)  + MAT1(7)  + MAT1(8)  +
     &                   MAT1(11) + MAT1(12) + MAT1(14)
            ZR(LCASTR) = DEUX * ZR(LCASTR)
            ZR(LCASTR) = ZR(LCASTR) + MAT1(1)  + MAT1(3)  +
     &                   MAT1(10) + MAT1(15)
            ZR(LCASTR) = ZR(LCASTR) / DEUX
         ELSEIF (INFODI.EQ.2) THEN
C           --- MASSE ---
            ZR(LCASTR) = ZR(LCASTR) / DEUX
         ENDIF
      ENDIF
C
      END
