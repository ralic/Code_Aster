      SUBROUTINE RC32SP ( LIEU, SEISME, NUMSIP, PI, MI, NUMSIQ, PJ, MJ,
     +                    MSE, SPIJ )
      IMPLICIT   NONE
      INTEGER             NUMSIP, NUMSIQ
      REAL*8              PI, MI(*), PJ, MJ(*), MSE(*), SPIJ
      LOGICAL             SEISME
      CHARACTER*4         LIEU
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 16/10/2002   AUTEUR JMBHH01 J.M.PROIX 
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
C     CALCUL DU SP
C
C     ------------------------------------------------------------------
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

      INTEGER      ICMP, JSIGU, ICMPS, LONG, NBINST, IRET,
     +             NBTHER, JTHER, ITH, NUMTH, JTHUN
      REAL*8       PIJ, MIJ(6), SP, SIJ(6), SIJS(6), SIGU
      CHARACTER*8 K8B, KNUMES, KNUMET 
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
C
C --- CONTRAINTES LINEAIRISEES DUES AUX CHARGEMENTS UNITAIRES
C
      CALL JEVEUO ( '&&RC3200.MECA_UNIT .'//LIEU, 'L', JSIGU )
C
C --- DIFFERENCE DE PRESSION ENTRE LES ETATS I ET J
C
CC      PIJ = ABS( PI - PJ )
      PIJ = PJ - PI
C
C --- VARIATION DE MOMENT RESULTANT
C
      DO 10 ICMP = 1 , 6
         MIJ(ICMP) = MJ(ICMP) - MI(ICMP)
   10 CONTINUE
C
C
C --- CALCUL DES CONTRAINTES EN PEAU PAR COMBINAISON LINEAIRE
C     POUR LE CHARGEMENT PIJ, MIJ
C
      DO 30 ICMPS = 1 , 6
         SIJ(ICMPS) = 0.D0
         DO 20 ICMP = 1 , 6
            SIGU = ZR(JSIGU-1+6*(ICMP-1)+ICMPS)
            SIJ(ICMPS) = SIJ(ICMPS) + MIJ(ICMP)*SIGU
   20    CONTINUE
C ------ PRESSION
         SIGU = ZR(JSIGU-1+36+ICMPS)
         SIJ(ICMPS) = SIJ(ICMPS) + PIJ*SIGU
   30 CONTINUE
C
C
C --- CALCUL DES CONTRAINTES LINEAIRISEES PAR COMBINAISON LINEAIRE
C     POUR LE CHARGEMENT MSE (SEISME)
C
      IF ( SEISME ) THEN
         DO 50 ICMPS = 1,6
            SIJS(ICMPS) = 0.D0
            DO 40 ICMP = 1,6
               SIGU = ZR(JSIGU-1+6*(ICMP-1)+ICMPS)
               SIJS(ICMPS) = SIJS(ICMPS) + MSE(ICMP)*SIGU
   40       CONTINUE
   50    CONTINUE
      END IF
C
C
C --- ON BOUCLE SUR LES INSTANTS DU THERMIQUE DE P
C
      IF ( NUMSIP .NE. 0 ) THEN
         KNUMES = 'S       '
         CALL CODENT ( NUMSIP , 'D0' , KNUMES(2:8)  )
         CALL JELIRA ( JEXNOM('&&RC3200.SITU_THERMIQUE',KNUMES),
     &                                          'LONUTI', NBTHER, K8B )
         IF ( NBTHER .EQ. 0 ) THEN
            NBINST = 0
            JTHUN = 1
            CALL RC32ST ( SIJ, NBINST, ZR(JTHUN), SEISME, SIJS, SP )
            SPIJ = MAX( SPIJ , SP )
         ELSE
            CALL JEVEUO ( JEXNOM('&&RC3200.SITU_THERMIQUE',KNUMES),
     &                                                     'L', JTHER )
            DO 100 ITH = 1 , NBTHER
               NUMTH = ZI(JTHER+ITH-1)
               KNUMET = 'T       '
               CALL CODENT ( NUMTH , 'D0' , KNUMET(2:8)  )
             CALL JELIRA ( JEXNOM('&&RC3200.THER_UNIT .'//LIEU,KNUMET),
     &                                            'LONUTI', LONG, K8B )
             CALL JEVEUO ( JEXNOM('&&RC3200.THER_UNIT .'//LIEU,KNUMET),
     &                                                     'L', JTHUN )
               NBINST = LONG / 12
               CALL RC32ST ( SIJ, NBINST, ZR(JTHUN), SEISME, SIJS, SP)
               SPIJ = MAX( SPIJ , SP )
 100        CONTINUE
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
     &                                          'LONUTI', NBTHER, K8B )
         IF ( NBTHER .EQ. 0 ) THEN
            NBINST = 0
            JTHUN = 1
            CALL RC32ST ( SIJ, NBINST, ZR(JTHUN), SEISME, SIJS, SP )
            SPIJ = MAX( SPIJ , SP )
         ELSE
            CALL JEVEUO ( JEXNOM('&&RC3200.SITU_THERMIQUE',KNUMES),
     &                                                     'L', JTHER )
            DO 110 ITH = 1 , NBTHER
               NUMTH = ZI(JTHER+ITH-1)
               KNUMET = 'T       '
               CALL CODENT ( NUMTH , 'D0' , KNUMET(2:8)  )
             CALL JELIRA ( JEXNOM('&&RC3200.THER_UNIT .'//LIEU,KNUMET),
     &                                            'LONUTI', LONG, K8B )
             CALL JEVEUO ( JEXNOM('&&RC3200.THER_UNIT .'//LIEU,KNUMET),
     &                                                     'L', JTHUN )
               NBINST = LONG / 12
               CALL RC32ST ( SIJ, NBINST, ZR(JTHUN), SEISME, SIJS, SP )
               SPIJ = MAX( SPIJ , SP )
 110        CONTINUE
         END IF
      END IF
C
      CALL JEDEMA()
      END
