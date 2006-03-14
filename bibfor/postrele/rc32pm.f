      SUBROUTINE RC32PM ( LIEU, SEISME, PI, MI, MSE, PM, PB, PMPB )
      IMPLICIT   NONE
      REAL*8              PI, MI(*), MSE(*), PM, PB, PMPB
      LOGICAL             SEISME
      CHARACTER*4         LIEU
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 13/03/2006   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     CALCUL DU PM_PB
C
C IN  : LIEU   : ='ORIG' : ORIGINE DU SEGEMNT, ='EXTR' : EXTREMITE
C IN  : SEISME : =.FALSE. SI PAS DE SEISME, =.TRUE. SINON
C IN  : MI     : EFFORTS ASSOCIEES A L'ETAT STABILISE (6+6)
C IN  : MSE    : EFFORTS DUS AU SEISME
C VAR : PM     : CONTRAINTE EQUIVALENTE PRIMAIRE DE MEMBRANE
C VAR : PB     : CONTRAINTE EQUIVALENTE PRIMAIRE DE FLEXION
C VAR : PMPB   : CONTRAINTE EQUIVALENTE PRIMAIRE DE MEMBRANE+FLEXION
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
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER    ICMPS, ICMP, JSIGU, NBINST
      REAL*8     STH(6), SIJ(6), SIGU, PMIJ, PBIJ, PMPBIJ
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
C
C --- CONTRAINTES DUES AUX CHARGEMENTS UNITAIRES
C
      CALL JEVEUO ( '&&RC3200.MECA_UNIT .'//LIEU, 'L', JSIGU )
C
C --- PAS DE THERMIQUE POUR LES CRITERES DE NIVEAU 0
C
      NBINST = 0
      DO 2 ICMP = 1 , 6
         STH(ICMP) = 0.D0
 2    CONTINUE
C-----------------------------------------------------------------------
C
C                            CALCUL DU PM
C
C-----------------------------------------------------------------------
C --- CALCUL DES CONTRAINTES PAR COMBINAISON LINEAIRE
C     POUR LE CHARGEMENT MI (RECUP M_0)
C
      DO 100 ICMPS = 1 , 6
         SIJ(ICMPS) = 0.D0
         DO 102 ICMP = 1 , 12
            SIGU = ZR(JSIGU-1+156+6*(ICMP-1)+ICMPS)
            SIJ(ICMPS) = SIJ(ICMPS) + MI(ICMP)*SIGU
 102     CONTINUE
C ------ PRESSION
         SIGU = ZR(JSIGU-1+156+72+ICMPS)
         SIJ(ICMPS) = SIJ(ICMPS) + PI*SIGU
 100  CONTINUE
C
      IF ( SEISME ) THEN
         CALL RC32S0 ('COMB',MI,PI,MSE,ZR(JSIGU+156), NBINST,STH, PMIJ )
      ELSE
         CALL RC32ST ( 'COMB', SIJ, NBINST, STH, PMIJ )
      END IF
      PM = MAX( PMIJ, PM )
C
C-----------------------------------------------------------------------
C
C                            CALCUL DU PB
C
C-----------------------------------------------------------------------
C --- CALCUL DES CONTRAINTES PAR COMBINAISON LINEAIRE
C     POUR LE CHARGEMENT MI (RECUP M_1)
C
      DO 110 ICMPS = 1 , 6
         SIJ(ICMPS) = 0.D0
         DO 112 ICMP = 1 , 12
            SIGU = ZR(JSIGU-1+234+6*(ICMP-1)+ICMPS)
            SIJ(ICMPS) = SIJ(ICMPS) + MI(ICMP)*SIGU
 112     CONTINUE
C ------ PRESSION
         SIGU = ZR(JSIGU-1+234+72+ICMPS)
         SIJ(ICMPS) = SIJ(ICMPS) + PI*SIGU
 110  CONTINUE
C
      IF ( SEISME ) THEN
         CALL RC32S0 ( 'COMB',MI,PI,MSE,ZR(JSIGU+234), NBINST,STH,PBIJ )
      ELSE
         CALL RC32ST ( 'COMB',SIJ, NBINST, STH, PBIJ )
      END IF
      PB = MAX( PBIJ, PB )
C
C-----------------------------------------------------------------------
C
C                            CALCUL DU PMPB
C
C-----------------------------------------------------------------------
C --- CALCUL DES CONTRAINTES PAR COMBINAISON LINEAIRE
C     POUR LE CHARGEMENT MI (RECUP LINEARISEES)
C
      DO 120 ICMPS = 1 , 6
         SIJ(ICMPS) = 0.D0
         DO 122 ICMP = 1 , 12
            SIGU = ZR(JSIGU-1+78+6*(ICMP-1)+ICMPS)
            SIJ(ICMPS) = SIJ(ICMPS) + MI(ICMP)*SIGU
 122     CONTINUE
C ------ PRESSION
         SIGU = ZR(JSIGU-1+78+72+ICMPS)
         SIJ(ICMPS) = SIJ(ICMPS) + PI*SIGU
 120  CONTINUE
C
      IF ( SEISME ) THEN
         CALL RC32S0 ( 'COMB',MI,PI,MSE,ZR(JSIGU+78),NBINST,STH,PMPBIJ )
      ELSE
         CALL RC32ST ( 'COMB',SIJ, NBINST, STH, PMPBIJ )
      END IF
      PMPB = MAX( PMPBIJ, PMPB )
C
      CALL JEDEMA()
      END
