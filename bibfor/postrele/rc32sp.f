      SUBROUTINE RC32SP(TYPZ,LIEU,NUMSIP,PI,MI,NUMSIQ,PJ,MJ,SEISME,MSE,
     &                  SPIJ,TYPEKE,SPMECA,SPTHER)
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNOM
      INTEGER           NUMSIP,NUMSIQ
      REAL*8            PI,MI(*),PJ,MJ(*),MSE(*),SPIJ(2),TYPEKE,
     &                  SPMECA(2),SPTHER(2)
      LOGICAL           SEISME
      CHARACTER*4       LIEU
      CHARACTER*(*)     TYPZ
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     ------------------------------------------------------------------
C     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
C     CALCUL DU SP

C     ------------------------------------------------------------------
C IN  : TYPZ   : 'SP_SITU'  : CALCUL DU SP POUR LA SITUATION
C              : 'SP_COMB'  : CALCUL DU SP POUR COMBINAISON SITUATION
C IN  : LIEU   : ='ORIG' : ORIGINE DU SEGEMNT, ='EXTR' : EXTREMITE
C IN  : SEISME : =.FALSE. SI PAS DE SEISME, =.TRUE. SINON
C IN  : NUMSIP : NUMERO SITUATION DE L'ETAT STABILISE P
C IN  : PI     : PRESSION ASSOCIEE A L'ETAT STABILISE I
C IN  : MI     : EFFORTS ASSOCIEES A L'ETAT STABILISE I
C IN  : NUMSIQ : NUMERO SITUATION DE L'ETAT STABILISE Q
C IN  : PJ     : PRESSION ASSOCIEE A L'ETAT STABILISE J
C IN  : MJ     : EFFORTS ASSOCIEES A L'ETAT STABILISE J
C IN  : MSE    : EFFORTS DUS AU SEISME
C OUT : SPIJ   : AMPLITUDE DE VARIATION DES CONTRAINTES TOTALES
C OUT : SPMECA : AMPLITUDE DE VARIATION DES CONTRAINTES MECANIQUES
C OUT : SPTHER : AMPLITUDE DE VARIATION DES CONTRAINTES THERMIQUES

      INTEGER      ICMP,JSIGU,ICMPS,LONG,NBINST,NBTHEP,NBTHEQ,JTHER,
     +             NUMTH,JTHUNQ,I1,JTHUNP,JTHUN
      REAL*8       PIJ,MIJ(12),SP,SIJ(6),SIGU,SIJ0(6),
     +             SQMA(6),SQMI(6),SP1,SP2,SPTH(6),SPQMA(2),
     +             SPQMI(2),SQTH(6)
      CHARACTER*4  TYP2
      CHARACTER*8  K8B, TYPE, KNUMES, KNUMET
C DEB ------------------------------------------------------------------
      TYPE = TYPZ
C
      SPIJ(1) = 0.D0
      SPIJ(2) = 0.D0
      SPTHER(1) = 0.D0
      SPTHER(2) = 0.D0
      SPMECA(1) = 0.D0
      SPMECA(2) = 0.D0
      DO 8 I1 = 1 , 6
        SQMA(I1) = 0.D0
        SQMI(I1) = 0.D0
  8   CONTINUE

C --- CONTRAINTES LINEAIRISEES DUES AUX CHARGEMENTS UNITAIRES

      CALL JEVEUO('&&RC3200.MECA_UNIT .'//LIEU,'L',JSIGU)

C --- DIFFERENCE DE PRESSION ENTRE LES ETATS I ET J

      PIJ = PI - PJ

C --- VARIATION DE MOMENT RESULTANT

      DO 10 ICMP = 1,12
        MIJ(ICMP) = MI(ICMP) - MJ(ICMP)
   10 CONTINUE

C --- CALCUL DES CONTRAINTES EN PEAU PAR COMBINAISON LINEAIRE
C     POUR LE CHARGEMENT PIJ, MIJ

      DO 30 ICMPS = 1,6
        SIJ(ICMPS) = 0.D0
        SIJ0(ICMPS) = 0.D0
        DO 20 ICMP = 1,12
          SIGU = ZR(JSIGU-1+6*(ICMP-1)+ICMPS)
          SIJ(ICMPS) = SIJ(ICMPS) + MIJ(ICMP)*SIGU
   20   CONTINUE
C ----- PRESSION
        SIGU = ZR(JSIGU-1+72+ICMPS)
        SIJ(ICMPS) = SIJ(ICMPS) + PIJ*SIGU
   30 CONTINUE


C CAS DE KE_MECA (PAS DE PARTITION MECANIQUE - THERMIQUE)

C --- ON BOUCLE SUR LES INSTANTS DU THERMIQUE DE P

        IF (NUMSIP.NE.0) THEN
          KNUMES = 'S       '
          CALL CODENT(NUMSIP,'D0',KNUMES(2:8))
          CALL JELIRA(JEXNOM('&&RC3200.SITU_THERMIQUE',KNUMES),'LONUTI',
     &                NBTHEP,K8B)
          IF (NBTHEP.EQ.0) THEN
            NBINST = 0
            JTHUN = 1
            TYP2 = '????'
            IF( TYPE .EQ. 'SP_COMB' ) THEN
              TYP2 = 'COMB'
            ELSEIF( TYPE .EQ. 'SP_SITU' ) THEN
              TYP2 = 'SITU'
            ENDIF
            IF ( SEISME ) THEN
               CALL RC32S0 ( TYP2, MIJ, PIJ, MSE, ZR(JSIGU), NBINST,
     &                                                   ZR(JTHUN), SP )
            ELSE
               CALL RC32ST ( SIJ, NBINST, ZR(JTHUN), SP )
            END IF
            SPIJ(1) = MAX(SPIJ(1),SP)
            IF (TYP2.EQ.'COMB') SPIJ(2) = MAX(SPIJ(2),SP)
          ELSE
            CALL JEVEUO(JEXNOM('&&RC3200.SITU_THERMIQUE',KNUMES),'L',
     &                  JTHER)
              NUMTH = ZI(JTHER)
              KNUMET = 'T       '
              CALL CODENT(NUMTH,'D0',KNUMET(2:8))
              CALL JELIRA(JEXNOM('&&RC3200.THER_UNIT .'//LIEU,KNUMET),
     &                    'LONUTI',LONG,K8B)
              CALL JEVEUO(JEXNOM('&&RC3200.THER_UNIT .'//LIEU,KNUMET),
     &                    'L',JTHUNP)
              NBINST = 2
              TYP2 = '????'
              IF( TYPE .EQ. 'SP_COMB' ) THEN
                 TYP2 = 'COMB'
              ELSEIF( TYPE .EQ. 'SP_SITU' ) THEN
                 TYP2 = 'SITU'
              ENDIF
              DO 14 I1 = 1,6
                  SPTH(I1) = ZR(JTHUNP+6+I1-1) -ZR(JTHUNP+I1-1)
 14           CONTINUE
              IF (TYP2.EQ.'SITU') THEN
                IF ( SEISME ) THEN
                  CALL RC32S0 ( TYP2, MIJ, PIJ, MSE, ZR(JSIGU), 1,
     &                                                 SPTH, SP )
                ELSE
                  CALL RC32ST ( SIJ, NBINST, SPTH, SP)
                ENDIF
                SPIJ(1) = MAX(SPIJ(1),SP)
C --- CALCUL DE KE_THER POUR LA SITUATION P
                IF (TYPEKE.GT.0.D0) THEN
                  CALL RC32S2 ( SIJ0,SPTH, SPTHER )
                END IF
              END IF
          END IF
        END IF


C --- ON BOUCLE SUR LES INSTANTS DU THERMIQUE DE Q

        IF (NUMSIQ.NE.0) THEN
          KNUMES = 'S       '
          CALL CODENT(NUMSIQ,'D0',KNUMES(2:8))
          CALL JELIRA(JEXNOM('&&RC3200.SITU_THERMIQUE',KNUMES),'LONUTI',
     &                NBTHEQ,K8B)
          IF (NBTHEQ.EQ.0) THEN
            NBINST = 0
            JTHUN = 1
            TYP2 = '????'
            IF( TYPE .EQ. 'SP_COMB' ) THEN
              TYP2 = 'COMB'
            ELSEIF( TYPE .EQ. 'SP_SITU' ) THEN
              TYP2 = 'SITU'
            ENDIF
            IF ( SEISME ) THEN
              CALL RC32S0 ( TYP2, MIJ, PIJ, MSE, ZR(JSIGU), NBINST,
     &                                                 ZR(JTHUN), SP )
            ELSE
              CALL RC32ST ( SIJ, NBINST, ZR(JTHUN), SP )
            END IF
            SPIJ(1) = MAX(SPIJ(1),SP)
            IF (TYP2.EQ.'COMB') SPIJ(2) = MAX(SPIJ(2),SP)
C - CAS NBQ = 0 / NBP != 0
            IF ( TYP2.EQ.'COMB' .AND. NBTHEP.NE.0 ) THEN
              IF ( SEISME ) THEN
                CALL RC32S0 ( TYP2, MIJ, PIJ, MSE, ZR(JSIGU), 1,
     &                                                 SPTH, SP )
                SPIJ(1) = SP
              ELSE
                CALL RC32S2 ( SIJ,SPTH, SPIJ )
              END IF
            END IF
          ELSE
            CALL JEVEUO(JEXNOM('&&RC3200.SITU_THERMIQUE',KNUMES),'L',
     &                  JTHER)
            NUMTH = ZI(JTHER)
            KNUMET = 'T       '
            CALL CODENT(NUMTH,'D0',KNUMET(2:8))
            CALL JELIRA(JEXNOM('&&RC3200.THER_UNIT .'//LIEU,KNUMET),
     &                  'LONUTI',LONG,K8B)
            CALL JEVEUO(JEXNOM('&&RC3200.THER_UNIT .'//LIEU,KNUMET),
     &                  'L',JTHUNQ)
            NBINST = 2
            TYP2 = '????'
            IF( TYPE .EQ. 'SP_COMB' ) THEN
              TYP2 = 'COMB'
            ELSEIF( TYPE .EQ. 'SP_SITU' ) THEN
              TYP2 = 'SITU'
            ENDIF
            IF (TYP2.EQ.'SITU') THEN
              IF ( SEISME ) THEN
                CALL RC32S0 ( TYP2, MIJ, PIJ, MSE, ZR(JSIGU), NBINST,
     &                                               SPTH, SP )
              ELSE
                CALL RC32ST ( SIJ, NBINST, SPTH, SP )
              ENDIF
              SPIJ(1) = SP
            ELSE
C - CAS NBP = 0 / NBQ != 0
              IF (NBTHEP.EQ.0) THEN
                DO 113 I1 = 1,6
                  SQTH(I1) = ZR(JTHUNQ+I1-1) - ZR(JTHUNQ+6+I1-1)
 113            CONTINUE
                IF ( SEISME ) THEN
                  CALL RC32S0 (TYP2, MIJ, PIJ, MSE, ZR(JSIGU), NBINST,
     &                                          SQTH, SP )
                  SPIJ(1) = SP
                ELSE
                  CALL RC32S2 ( SIJ,SQTH, SPIJ )
                ENDIF
              ELSE
                DO 114 I1 = 1,6
                   SQMI(I1) = ZR(JTHUNP+I1-1) - ZR(JTHUNQ+6+I1-1)
                   SQMA(I1) = ZR(JTHUNP+6+I1-1) - ZR(JTHUNQ+I1-1)
 114            CONTINUE
                IF ( SEISME ) THEN
                  CALL RC32S0 (TYP2, MIJ, PIJ, MSE, ZR(JSIGU), 1,
     &                                          SQMI, SP1 )
                  CALL RC32S0 (TYP2, MIJ, PIJ, MSE, ZR(JSIGU), 1,
     &                                          SQMA, SP2 )
                  SPIJ(1) = MAX(SP1,SP2)
                  SPIJ(2) = MIN(SP1,SP2)
                ELSE
                  CALL RC32S2 ( SIJ,SQMI, SPQMI )
                  CALL RC32S2 ( SIJ,SQMA, SPQMA )
                  SPIJ(1) = MAX(SPQMA(1),SPQMI(1))
                  SPIJ(2) = MIN(SPQMA(1),SPQMI(1))
                ENDIF
C --- CALCUL DE KE_THER POUR LA COMBINAISON DES SITUATIONS P ET Q
                IF (TYPEKE.GT.0.D0) THEN
                  CALL RC32S2 ( SIJ0,SQMI, SPQMI )
                  CALL RC32S2 ( SIJ0,SQMA, SPQMA )
                  SPTHER(1) = MAX(SPQMA(1),SPQMI(1))
                  SPTHER(2) = MIN(SPQMA(1),SPQMI(1))
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF

C CAS DE KE_MIXTE (PARTITION MECANIQUE - THERMIQUE)

      IF (TYPEKE.GT.0.D0) THEN

C --- CALCUL DE KE_MECA
        NBINST = 0
        JTHUN = 1
        TYP2 = '????'
        IF( TYPE .EQ. 'SP_COMB' ) THEN
          TYP2 = 'COMB'
        ELSEIF( TYPE .EQ. 'SP_SITU' ) THEN
          TYP2 = 'SITU'
        ENDIF
        IF ( SEISME ) THEN
          CALL RC32S0 ( TYP2, MIJ, PIJ, MSE, ZR(JSIGU), NBINST,
     &                                                 ZR(JTHUN), SP )
        ELSE
          CALL RC32ST ( SIJ, NBINST, ZR(JTHUN), SP )
        END IF
        SPMECA(1) =SP
C
      END IF

      END
