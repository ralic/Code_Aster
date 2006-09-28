      SUBROUTINE RC32SP(TYPZ,LIEU,NUMSIP,PI,MI,NUMSIQ,PJ,MJ,SEISME,MSE,
     &                  SPIJ,TYPEKE,SPMECA,SPTHER)
      IMPLICIT   NONE
      INTEGER           NUMSIP,NUMSIQ
      REAL*8         PI,MI(*),PJ,MJ(*),MSE(*),SPIJ,TYPEKE,SPMECA,SPTHER
      LOGICAL           SEISME
      CHARACTER*4       LIEU
      CHARACTER*(*)     TYPZ
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C OUT : SPMECA   : AMPLITUDE DE VARIATION DES CONTRAINTES MECANIQUES
C OUT : SPTHER   : AMPLITUDE DE VARIATION DES CONTRAINTES THERMIQUES
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32 JEXNOM,JEXNUM,JEXATR
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------

      INTEGER ICMP,JSIGU,ICMPS,LONG,NBINST,NBTHER,JTHER,ITH,NUMTH,JTHUN
      REAL*8       PIJ,MIJ(12),SP,SIJ(6),SIGU,SIJ0(6)
      CHARACTER*4  TYP2
      CHARACTER*8  K8B, TYPE, KNUMES, KNUMET
C DEB ------------------------------------------------------------------
      TYPE = TYPZ

C --- CONTRAINTES LINEAIRISEES DUES AUX CHARGEMENTS UNITAIRES

      CALL JEVEUO('&&RC3200.MECA_UNIT .'//LIEU,'L',JSIGU)

C --- DIFFERENCE DE PRESSION ENTRE LES ETATS I ET J

      PIJ = PJ - PI

C --- VARIATION DE MOMENT RESULTANT

      DO 10 ICMP = 1,12
        MIJ(ICMP) = MJ(ICMP) - MI(ICMP)
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

C      IF (TYPEKE.LT.0.D0) THEN


C --- ON BOUCLE SUR LES INSTANTS DU THERMIQUE DE P

        IF (NUMSIP.NE.0) THEN
          KNUMES = 'S       '
          CALL CODENT(NUMSIP,'D0',KNUMES(2:8))
          CALL JELIRA(JEXNOM('&&RC3200.SITU_THERMIQUE',KNUMES),'LONUTI',
     &                NBTHER,K8B)
          IF (NBTHER.EQ.0) THEN
            NBINST = 0
            JTHUN = 1
            IF( TYPE .EQ. 'SP_COMB' ) THEN
              TYP2 = 'COMB'
            ELSEIF( TYPE .EQ. 'SP_SITU' ) THEN
              TYP2 = 'SITU'
            ENDIF
            IF ( SEISME ) THEN
               CALL RC32S0 ( TYP2, MIJ, PIJ, MSE, ZR(JSIGU), NBINST,
     &                                                   ZR(JTHUN), SP )
            ELSE
               CALL RC32ST ( TYP2, SIJ, NBINST, ZR(JTHUN), SP )
            END IF
            SPIJ = MAX(SPIJ,SP)
          ELSE
            CALL JEVEUO(JEXNOM('&&RC3200.SITU_THERMIQUE',KNUMES),'L',
     &                  JTHER)
            DO 60 ITH = 1,NBTHER
              NUMTH = ZI(JTHER+ITH-1)
              KNUMET = 'T       '
              CALL CODENT(NUMTH,'D0',KNUMET(2:8))
              CALL JELIRA(JEXNOM('&&RC3200.THER_UNIT .'//LIEU,KNUMET),
     &                    'LONUTI',LONG,K8B)
              CALL JEVEUO(JEXNOM('&&RC3200.THER_UNIT .'//LIEU,KNUMET),
     &                    'L',JTHUN)
              NBINST = LONG / 24
              IF( TYPE .EQ. 'SP_COMB' ) THEN
                 TYP2 = 'COMB'
              ELSEIF( TYPE .EQ. 'SP_SITU' ) THEN
                 TYP2 = 'SITU'
              ENDIF
              IF ( SEISME ) THEN
                 CALL RC32S0 ( TYP2, MIJ, PIJ, MSE, ZR(JSIGU), NBINST,
     &                                                   ZR(JTHUN), SP )
              ELSE
                 CALL RC32ST ( TYP2, SIJ, NBINST, ZR(JTHUN), SP )
              END IF
              SPIJ = MAX(SPIJ,SP)
   60       CONTINUE
          END IF
        END IF


C --- ON BOUCLE SUR LES INSTANTS DU THERMIQUE DE Q

        IF (NUMSIQ.NE.0) THEN
          KNUMES = 'S       '
          CALL CODENT(NUMSIQ,'D0',KNUMES(2:8))
          CALL JELIRA(JEXNOM('&&RC3200.SITU_THERMIQUE',KNUMES),'LONUTI',
     &                NBTHER,K8B)
          IF (NBTHER.EQ.0) THEN
            NBINST = 0
            JTHUN = 1
            IF( TYPE .EQ. 'SP_COMB' ) THEN
              TYP2 = 'COMB'
            ELSEIF( TYPE .EQ. 'SP_SITU' ) THEN
              TYP2 = 'SITU'
            ENDIF
            IF ( SEISME ) THEN
              CALL RC32S0 ( TYP2, MIJ, PIJ, MSE, ZR(JSIGU), NBINST,
     &                                                   ZR(JTHUN), SP )
            ELSE
              CALL RC32ST ( TYP2, SIJ, NBINST, ZR(JTHUN), SP )
            END IF
            SPIJ = MAX(SPIJ,SP)
          ELSE
            CALL JEVEUO(JEXNOM('&&RC3200.SITU_THERMIQUE',KNUMES),'L',
     &                  JTHER)
            DO 70 ITH = 1,NBTHER
              NUMTH = ZI(JTHER+ITH-1)
              KNUMET = 'T       '
              CALL CODENT(NUMTH,'D0',KNUMET(2:8))
              CALL JELIRA(JEXNOM('&&RC3200.THER_UNIT .'//LIEU,KNUMET),
     &                    'LONUTI',LONG,K8B)
              CALL JEVEUO(JEXNOM('&&RC3200.THER_UNIT .'//LIEU,KNUMET),
     &                    'L',JTHUN)
              NBINST = LONG / 24
              IF( TYPE .EQ. 'SP_COMB' ) THEN
                TYP2 = 'COMB'
              ELSEIF( TYPE .EQ. 'SP_SITU' ) THEN
                TYP2 = 'SITU'
              ENDIF
              IF ( SEISME ) THEN
                CALL RC32S0 ( TYP2, MIJ, PIJ, MSE, ZR(JSIGU), NBINST,
     &                                                   ZR(JTHUN), SP )
              ELSE
                CALL RC32ST ( TYP2, SIJ, NBINST, ZR(JTHUN), SP )
              END IF
              SPIJ = MAX(SPIJ,SP)
   70       CONTINUE
          END IF
        END IF


C CAS DE KE_MIXTE (PARTITION MECANIQUE - THERMIQUE)

C      ELSE IF (TYPEKE.GT.0.D0) THEN
      IF (TYPEKE.GT.0.D0) THEN

C --- ON BOUCLE SUR LES INSTANTS DU THERMIQUE DE P

        IF (NUMSIP.NE.0) THEN
          KNUMES = 'S       '
          CALL CODENT(NUMSIP,'D0',KNUMES(2:8))
          CALL JELIRA(JEXNOM('&&RC3200.SITU_THERMIQUE',KNUMES),'LONUTI',
     &                NBTHER,K8B)
          NBINST = 0
          JTHUN = 1
          IF( TYPE .EQ. 'SP_COMB' ) THEN
            TYP2 = 'COMB'
          ELSEIF( TYPE .EQ. 'SP_SITU' ) THEN
            TYP2 = 'SITU'
          ENDIF
          IF ( SEISME ) THEN
            CALL RC32S0 ( TYP2, MIJ, PIJ, MSE, ZR(JSIGU), NBINST,
     &                                                   ZR(JTHUN), SP )
          ELSE
            CALL RC32ST ( TYP2, SIJ, NBINST, ZR(JTHUN), SP )
          END IF
          SPMECA = MAX(SPMECA,SP)

          IF (NBTHER.NE.0) THEN
            CALL JEVEUO(JEXNOM('&&RC3200.SITU_THERMIQUE',KNUMES),'L',
     &                  JTHER)
            DO 80 ITH = 1,NBTHER
              NUMTH = ZI(JTHER+ITH-1)
              KNUMET = 'T       '
              CALL CODENT(NUMTH,'D0',KNUMET(2:8))
              CALL JELIRA(JEXNOM('&&RC3200.THER_UNIT .'//LIEU,KNUMET),
     &                    'LONUTI',LONG,K8B)
              CALL JEVEUO(JEXNOM('&&RC3200.THER_UNIT .'//LIEU,KNUMET),
     &                    'L',JTHUN)
              NBINST = LONG / 24
              TYP2 = 'SITU'
              CALL RC32ST ( TYP2, SIJ0, NBINST, ZR(JTHUN), SP )
              SPTHER = MAX(SPTHER,SP)
   80       CONTINUE
          END IF
        END IF


C --- ON BOUCLE SUR LES INSTANTS DU THERMIQUE DE Q

        IF (NUMSIQ.NE.0) THEN
          KNUMES = 'S       '
          CALL CODENT(NUMSIQ,'D0',KNUMES(2:8))
          CALL JELIRA(JEXNOM('&&RC3200.SITU_THERMIQUE',KNUMES),'LONUTI',
     &                NBTHER,K8B)
          IF (NBTHER.NE.0) THEN
            CALL JEVEUO(JEXNOM('&&RC3200.SITU_THERMIQUE',KNUMES),'L',
     &                  JTHER)
            DO 90 ITH = 1,NBTHER
              NUMTH = ZI(JTHER+ITH-1)
              KNUMET = 'T       '
              CALL CODENT(NUMTH,'D0',KNUMET(2:8))
              CALL JELIRA(JEXNOM('&&RC3200.THER_UNIT .'//LIEU,KNUMET),
     &                    'LONUTI',LONG,K8B)
              CALL JEVEUO(JEXNOM('&&RC3200.THER_UNIT .'//LIEU,KNUMET),
     &                    'L',JTHUN)
              NBINST = LONG / 24
              TYP2 = 'SITU'
              CALL RC32ST ( TYP2, SIJ0, NBINST, ZR(JTHUN), SP )
              SPTHER = MAX(SPTHER,SP)
   90       CONTINUE
          END IF
        END IF

C      ELSE
C        CALL UTMESS('F','RC32SP','PB AVEC TYPEKE')
      END IF

      END
