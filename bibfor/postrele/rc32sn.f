      SUBROUTINE RC32SN ( TYPZ, LIEU, NUMSIP, PI, MI,
     +                    NUMSIQ, PJ, MJ, SEISME, MSE, SNIJ )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNOM
      INTEGER             NUMSIP, NUMSIQ
      REAL*8              PI, MI(*), PJ, MJ(*), MSE(*), SNIJ
      LOGICAL             SEISME
      CHARACTER*4         LIEU
      CHARACTER*(*)       TYPZ
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C
      INTEGER      ICMP, JSIGU, ICMPS, NBINST, LONG, I1,
     +             NBTHEP, NBTHEQ, JTHER, ITH, NUMTH, JTHUN,
     +             INDICP,INDICQ
      REAL*8       PIJ, MIJ(12), SN, SIJ(6), SIGU,
     +             SQMA(6), SQMI(6), SN1, SN2, SNTH(6)
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
            INDICP = 1
            IF( TYPE .EQ.  'SN_COMB' .OR.
     &          TYPE .EQ. 'SN*_COMB' ) THEN
               TYP2 = 'COMB'
            ELSEIF( TYPE .EQ.  'SN_SITU' .OR.
     &              TYPE .EQ. 'SN*_SITU' ) THEN
               TYP2 = 'SITU'
            ENDIF
            IF ( SEISME ) THEN
               CALL RC32S0 ( TYP2, MIJ, PIJ, MSE, ZR(JSIGU+78),
     &                                          NBINST, ZR(INDICP), SN )
            ELSE
               CALL RC32ST ( SIJ, NBINST, ZR(INDICP), SN )
            END IF
            SNIJ = MAX( SNIJ , SN )
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
                 INDICP = JTHUN + 6*NBINST
                 TYP2 = 'COMB'
               ELSEIF( TYPE .EQ. 'SN*_COMB' ) THEN
                 INDICP = JTHUN + 12*NBINST
                 TYP2 = 'COMB'
               ELSEIF( TYPE .EQ. 'SN_SITU' ) THEN
                 INDICP = JTHUN + 6*NBINST
                 TYP2 = 'SITU'
               ELSEIF( TYPE .EQ. 'SN*_SITU' ) THEN
                 INDICP = JTHUN + 12*NBINST
                 TYP2 = 'SITU'
               ENDIF
               DO 14 I1 = 1,6
                 SNTH(I1) = ZR(INDICP+6+I1-1) -ZR(INDICP+I1-1)
 14            CONTINUE
               IF ( SEISME ) THEN
                 CALL RC32S0 ( TYP2, MIJ, PIJ, MSE, ZR(JSIGU+78),
     &                                          NBINST, SNTH, SN )
               ELSE
                 IF (TYP2.EQ.'SITU') THEN
                   CALL RC32ST ( SIJ, NBINST, SNTH, SN )
                 ELSE
                   SN = 0.D0
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
            INDICQ = 1
            IF( TYPE .EQ.  'SN_COMB' .OR.
     &          TYPE .EQ. 'SN*_COMB' ) THEN
               TYP2 = 'COMB'
            ELSEIF( TYPE .EQ.  'SN_SITU' .OR.
     &              TYPE .EQ. 'SN*_SITU' ) THEN
               TYP2 = 'SITU'
            ENDIF
            IF ( SEISME ) THEN
               CALL RC32S0 ( TYP2, MIJ, PIJ, MSE, ZR(JSIGU+78), NBINST,
     &                                                ZR(INDICQ), SN )
            ELSE
               CALL RC32ST ( SIJ, NBINST, ZR(INDICQ), SN )
            END IF
            SNIJ = MAX( SNIJ , SN )
            IF ( TYP2.EQ.'COMB' .AND. NBTHEP.NE.0 ) THEN
               IF (SEISME) THEN
                 CALL RC32S0 ( TYP2, MIJ, PIJ, MSE, ZR(JSIGU+78),
     &                                      NBINST,SNTH, SN )
               ELSE
                 CALL RC32ST ( SIJ, 2, SNTH, SN )
               ENDIF
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
                 INDICQ = JTHUN + 6*NBINST
                 TYP2 = 'COMB'
               ELSEIF( TYPE .EQ. 'SN*_COMB' ) THEN
                 INDICQ = JTHUN + 12*NBINST
                 TYP2 = 'COMB'
               ELSEIF( TYPE .EQ. 'SN_SITU' ) THEN
                 INDICQ = JTHUN + 6*NBINST
                 TYP2 = 'SITU'
               ELSEIF( TYPE .EQ. 'SN*_SITU' ) THEN
                 INDICQ = JTHUN + 12*NBINST
                 TYP2 = 'SITU'
               ENDIF
               DO 24 I1 = 1,6
                 SNTH(I1) = ZR(INDICQ+6+I1-1) -ZR(INDICQ+I1-1)
 24            CONTINUE
               IF (TYP2.EQ.'SITU') THEN
                 IF (SEISME) THEN
                   CALL RC32S0 ( TYP2, MIJ, PIJ, MSE, ZR(JSIGU+78),
     &                                         NBINST, SNTH, SN )
                 ELSE
                   CALL RC32ST ( SIJ, NBINST, SNTH, SN )
                 ENDIF
               ELSE
                 IF ( NBTHEP .NE. 0 ) THEN
                   DO 114 I1 = 1,6
                     SQMI(I1) = ZR(INDICP+I1-1) - ZR(INDICQ+6+I1-1)
                     SQMA(I1) = ZR(INDICP+6+I1-1) - ZR(INDICQ+I1-1)
 114               CONTINUE
                   IF (SEISME) THEN
                     CALL RC32S0 ( TYP2, MIJ, PIJ, MSE, ZR(JSIGU+78),
     &                                       NBINST, SQMI, SN1 )
                     CALL RC32S0 ( TYP2, MIJ, PIJ, MSE, ZR(JSIGU+78),
     &                                       NBINST, SQMA, SN2 )
                     SN = MAX( SN1, SN2 )
                   ELSE
                     CALL RC32ST ( SIJ, NBINST, SQMI, SN1 )
                     CALL RC32ST ( SIJ, NBINST, SQMA, SN2 )
                     SN = MAX( SN1, SN2 )
                   ENDIF
                 ENDIF
               ENDIF
               SNIJ = MAX( SNIJ , SN )
 110        CONTINUE
         END IF
      END IF
C
      END
