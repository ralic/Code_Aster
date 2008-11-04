      SUBROUTINE RC32SN ( TYPZ, LIEU, NUMSIP, PI, MI,
     +                    NUMSIQ, PJ, MJ, SEISME, MSE, SNIJ )
      IMPLICIT   NONE
      INTEGER             NUMSIP, NUMSIQ
      REAL*8              PI, MI(*), PJ, MJ(*), MSE(*), SNIJ
      LOGICAL             SEISME
      CHARACTER*4         LIEU
      CHARACTER*(*)       TYPZ
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 03/11/2008   AUTEUR MACOCCO K.MACOCCO 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
C     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
C     CALCUL DU SN OU SN*
C
C IN  : TYPZ   : 'SN_SITU'  : CALCUL DU SN POUR LA SITUATION
C              : 'SN*_SITU' : CALCUL DU SN* POUR LA SITUATION
C              : 'SN_COMB'  : CALCUL DU SN POUR COMBINAISON SITUATION
C              : 'SN*_COMB' : CALCUL DU SN* POUR COMBINAISON SITUATION
C IN  : LIEU   : ='ORIG' : ORIGINE DU SEGEMNT, ='EXTR' : EXTREMITE
C IN  : NUMSIP : NUMERO SITUATION DE L'ETAT STABILISE P
C IN  : PI     : PRESSION ASSOCIEE A L'ETAT STABILISE I
C IN  : MI     : EFFORTS ASSOCIEES A L'ETAT STABILISE I (6)
C IN  : NUMSIQ : NUMERO SITUATION DE L'ETAT STABILISE Q
C IN  : PJ     : PRESSION ASSOCIEE A L'ETAT STABILISE J
C IN  : MJ     : EFFORTS ASSOCIEES A L'ETAT STABILISE J (6)
C IN  : PJ     : PRESSION ASSOCIEE A L'ETAT STABILISE J
C IN  : SEISME : =.FALSE. SI PAS DE SEISME, =.TRUE. SINON
C IN  : MSE    : EFFORTS DUS AU SEISME
C VAR : SNIJ   : AMPLITUDE DE VARIATION DES CONTRAINTES LINEARISEES
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER         ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8          ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16      ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL         ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8     ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                     ZK24
      CHARACTER*32                              ZK32
      CHARACTER*80                                       ZK80
      COMMON /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32    JEXNOM, JEXNUM, JEXATR
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER      ICMP, JSIGU, ICMPS, INDICE, NBINST, LONG, I1,
     +             NBTHEP, NBTHEQ, JTHER, ITH, NUMTH, JTHUN, JORIG
      REAL*8       PIJ, MIJ(12), SN, SIJ(6), SIGU, SNMA(6), SNMI(6),
     +             SQMA(6), SQMI(6), SN1, SN2
      CHARACTER*4  TYP2
      CHARACTER*8  K8B, TYPE, KNUMES, KNUMET 
C DEB ------------------------------------------------------------------
      TYPE = TYPZ
      SN = 0.D0
      SN1 = 0.D0
      SN2 = 0.D0
C
C --- CONTRAINTES LINEAIRISEES DUES AUX CHARGEMENTS UNITAIRES
C
      CALL JEVEUO ( '&&RC3200.MECA_UNIT .'//LIEU, 'L', JSIGU )
C
C --- DIFFERENCE DE PRESSION ENTRE LES ETATS I ET J
C
      PIJ = PI - PJ
C
C --- VARIATION DE MOMENT RESULTANT
C
      DO 10 ICMP = 1 , 12
         MIJ(ICMP) = MI(ICMP) - MJ(ICMP)
   10 CONTINUE
C
C --- CALCUL DES CONTRAINTES LINEAIRISEES PAR COMBINAISON LINEAIRE
C     POUR LE CHARGEMENT PIJ, MIJ
C
      DO 30 ICMPS = 1 , 6
         SIJ(ICMPS) = 0.D0
         DO 20 ICMP = 1 , 12
            SIGU = ZR(JSIGU-1+78+6*(ICMP-1)+ICMPS)
            SIJ(ICMPS) = SIJ(ICMPS) + MIJ(ICMP)*SIGU
   20    CONTINUE
C ------ PRESSION
         SIGU = ZR(JSIGU-1+78+72+ICMPS)
         SIJ(ICMPS) = SIJ(ICMPS) + PIJ*SIGU
   30 CONTINUE
C
C --- ON BOUCLE SUR LES INSTANTS DU THERMIQUE DE P
C
      IF ( NUMSIP .NE. 0 ) THEN
         KNUMES = 'S       '
         CALL CODENT ( NUMSIP , 'D0' , KNUMES(2:8)  )
         CALL JELIRA ( JEXNOM('&&RC3200.SITU_THERMIQUE',KNUMES),
     &                                          'LONUTI', NBTHEP, K8B )
         IF ( NBTHEP .EQ. 0 ) THEN
            NBINST = 0
            INDICE = 1
            IF( TYPE .EQ.  'SN_COMB' .OR.
     &          TYPE .EQ. 'SN*_COMB' ) THEN
               TYP2 = 'COMB'
            ELSEIF( TYPE .EQ.  'SN_SITU' .OR.
     &              TYPE .EQ. 'SN*_SITU' ) THEN
               TYP2 = 'SITU'
            ENDIF
            IF ( SEISME ) THEN
               CALL RC32S0 ( TYP2, MIJ, PIJ, MSE, ZR(JSIGU+78),  
     &                                          NBINST, ZR(INDICE), SN )
            ELSE
               CALL RC32ST ( TYP2, SIJ, NBINST, ZR(INDICE), SN )
            END IF
            SNIJ = MAX( SNIJ , SN )
            DO 12 I1 = 1,6
               SNMI(I1) = SIJ(I1)
               SNMA(I1) = SIJ(I1)
 12         CONTINUE
         ELSE
            CALL JEVEUO ( JEXNOM('&&RC3200.SITU_THERMIQUE',KNUMES),
     &                                                     'L', JTHER )
            DO 100 ITH = 1 , NBTHEP
               NUMTH = ZI(JTHER+ITH-1)
               KNUMET = 'T       '
               CALL CODENT ( NUMTH , 'D0' , KNUMET(2:8)  )
             CALL JELIRA ( JEXNOM('&&RC3200.THER_UNIT .'//LIEU,KNUMET),
     &                                            'LONUTI', LONG, K8B )
             CALL JEVEUO ( JEXNOM('&&RC3200.THER_UNIT .'//LIEU,KNUMET),
     &                                                     'L', JTHUN )
               NBINST = 2
               IF( TYPE .EQ. 'SN_COMB' ) THEN
                 INDICE = JTHUN + 6*NBINST
                 TYP2 = 'COMB'
               ELSEIF( TYPE .EQ. 'SN*_COMB' ) THEN
                 INDICE = JTHUN + 12*NBINST
                 TYP2 = 'COMB'
               ELSEIF( TYPE .EQ. 'SN_SITU' ) THEN
                 INDICE = JTHUN + 6*NBINST
                 TYP2 = 'SITU'
               ELSEIF( TYPE .EQ. 'SN*_SITU' ) THEN
                 INDICE = JTHUN + 12*NBINST
                 TYP2 = 'SITU'
               ENDIF
               IF ( SEISME ) THEN
                 CALL RC32S0 ( TYP2, MIJ, PIJ, MSE, ZR(JSIGU+78),  
     &                                          NBINST, ZR(INDICE), SN )
               ELSE
                 IF (TYP2.EQ.'SITU') THEN
                   CALL RC32ST ( TYP2, SIJ, NBINST, ZR(INDICE), SN )
                 ELSE
                   SN = 0.D0
                   DO 14 I1 = 1,6
                     SNMI(I1) = SIJ(I1) + ZR(INDICE+I1-1)
                     SNMA(I1) = SIJ(I1) + ZR(INDICE+6+I1-1)
 14                CONTINUE
                 ENDIF
               END IF
               SNIJ = MAX( SNIJ , SN )
 100       CONTINUE
        END IF
      END IF
C
C
C --- ON BOUCLE SUR LES INSTANTS DU THERMIQUE DE Q
C
      IF ( NUMSIQ .NE. 0 ) THEN
         KNUMES = 'S       '
         CALL CODENT ( NUMSIQ , 'D0' , KNUMES(2:8)  )
         CALL JELIRA ( JEXNOM('&&RC3200.SITU_THERMIQUE',KNUMES),
     &                                          'LONUTI', NBTHEQ, K8B )
         IF ( NBTHEQ .EQ. 0 ) THEN
            NBINST = 0
            INDICE = 1
            IF( TYPE .EQ.  'SN_COMB' .OR.
     &          TYPE .EQ. 'SN*_COMB' ) THEN
               TYP2 = 'COMB'
            ELSEIF( TYPE .EQ.  'SN_SITU' .OR.
     &              TYPE .EQ. 'SN*_SITU' ) THEN
               TYP2 = 'SITU'
            ENDIF
            IF ( SEISME ) THEN
               CALL RC32S0 ( TYP2, MIJ, PIJ, MSE, ZR(JSIGU+78), NBINST, 
     &                                                ZR(INDICE), SN )
            ELSE
               CALL RC32ST ( TYP2, SIJ, NBINST, ZR(INDICE), SN )
            END IF
            SNIJ = MAX( SNIJ , SN )
            IF ( TYP2.EQ.'COMB' .AND. NBTHEP.NE.0 ) THEN
               CALL RCTRES ( SNMI, SN1 )
               CALL RCTRES ( SNMA, SN2 )
               SN = MAX( SN1, SN2 )
               SNIJ = MAX( SNIJ , SN )
            END IF
         ELSE
            CALL JEVEUO ( JEXNOM('&&RC3200.SITU_THERMIQUE',KNUMES),
     &                                                     'L', JTHER )
            DO 110 ITH = 1 , NBTHEQ
               NUMTH = ZI(JTHER+ITH-1)
               KNUMET = 'T       '
               CALL CODENT ( NUMTH , 'D0' , KNUMET(2:8)  )
             CALL JELIRA ( JEXNOM('&&RC3200.THER_UNIT .'//LIEU,KNUMET),
     &                                            'LONUTI', LONG, K8B )
             CALL JEVEUO ( JEXNOM('&&RC3200.THER_UNIT .'//LIEU,KNUMET),
     &                                                     'L', JTHUN )
               NBINST = 2
               IF( TYPE .EQ.  'SN_COMB' ) THEN
                 INDICE = JTHUN + 6*NBINST
                 TYP2 = 'COMB'
               ELSEIF( TYPE .EQ. 'SN*_COMB' ) THEN
                 INDICE = JTHUN + 12*NBINST
                 TYP2 = 'COMB'
               ELSEIF( TYPE .EQ. 'SN_SITU' ) THEN
                 INDICE = JTHUN + 6*NBINST
                 TYP2 = 'SITU'
               ELSEIF( TYPE .EQ. 'SN*_SITU' ) THEN
                 INDICE = JTHUN + 12*NBINST
                 TYP2 = 'SITU'
               ENDIF
               IF ( SEISME ) THEN
                  CALL RC32S0 ( TYP2, MIJ, PIJ, MSE, ZR(JSIGU+78),  
     &                                          NBINST, ZR(INDICE), SN )
               ELSE
                 IF (TYP2.EQ.'SITU') THEN
                   CALL RC32ST ( TYP2, SIJ, NBINST, ZR(INDICE), SN )
                 ELSE
                   DO 114 I1 = 1,6
                     SQMI(I1) = SNMA(I1) - ZR(INDICE+I1-1)
                     SQMA(I1) = SNMI(I1) - ZR(INDICE+6+I1-1)
 114               CONTINUE
                   CALL RCTRES ( SQMI, SN1 )
                   CALL RCTRES ( SQMA, SN2 )
                   SN = MAX( SN1, SN2 )
                 ENDIF
               END IF
               SNIJ = MAX( SNIJ , SN )
 110        CONTINUE
         END IF
      END IF
C
      END
