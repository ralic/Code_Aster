      SUBROUTINE RC32S0 ( TYPE, MM, PR, MSE, SIGUN, NBINST, STH, SNP )
      IMPLICIT   NONE
      INTEGER             NBINST
      REAL*8              MM(*),PR,MSE(*),SIGUN(*),STH(6*NBINST),SNP
      CHARACTER*4         TYPE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 19/06/2007   AUTEUR VIVAN L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
C     CALCUL DU SN MAX SUR LES INSTANTS ET LES 64 POSSIBILITES DE SIGNE
C     POUR LES CMP DE SEISME
C     64 POSSIBLITES DE SIGNE DE MSE
C
C IN  : TYPE   : ='COMB'  +-SIGM_M + SIGM_TH
C                ='SITU'    SIGM_M + SIGM_TH
C IN  : MM     : EFFORTS ASSOCIEES A L'ETAT DE CONTRAINTE MECANIQUE
C IN  : PR     : PRESSION
C IN  : MSE    : EFFORTS DUS AU SEISME
C IN  : SIGUN  : CONTRAINTES UNITAIRES
C IN  : NBINST : NOMBRE D'INTANTS DE CALCUL THERMOMECA
C IN  : STH    : CONTRAINTES LINEARISEES OU EN PEAU ( THERMOMECA)
C OUT : SNP    : AMPLITUDE DE VARIATION DES CONTRAINTES DE TRESCA
C     ------------------------------------------------------------------
C
      INTEGER  I1, I2, I3, I4, I5, I6, I, IT, IT1, ICMPS, ICMP
      REAL*8   MTT(6), MTC(6), SIJ(6), SIJT(6), SNP1, STH1, TRESCA
      REAL*8   SIGT, SIGC, SIGP
      REAL*8   E1(2), E2(2), E3(2), E4(2), E5(2), E6(2)
C DEB ------------------------------------------------------------------
C
      SNP = 0.D0
C
      DO 2 I = 1 , 2
         I1 = 2*(I-2)+1
         E1(I) = I1
         E2(I) = I1
         E3(I) = I1
         E4(I) = I1
         E5(I) = I1
         E6(I) = I1
 2    CONTINUE
C
C --- CALCUL MECANIQUE :
C     ----------------
      IF ( NBINST .EQ. 0 ) THEN
        DO 70 I1 = 1,2
          MTT(1) = MM(1) + MSE(1)*E1(I1)
          MTC(1) = MM(7) + MSE(7)*E1(I1)
          DO 60 I2 = 1,2
            MTT(2) = MM(2) + MSE(2)*E2(I2)
            MTC(2) = MM(8) + MSE(8)*E2(I2)
            DO 50 I3 = 1,2
              MTT(3) = MM(3) + MSE(3)*E3(I3)
              MTC(3) = MM(9) + MSE(9)*E3(I3)
              DO 40 I4 = 1,2
                MTT(4) = MM(4) + MSE(4)*E4(I4)
                MTC(4) = MM(10) + MSE(10)*E4(I4)
                DO 30 I5 = 1,2
                  MTT(5) = MM(5) + MSE(5)*E5(I5)
                  MTC(5) = MM(11) + MSE(11)*E5(I5)
                  DO 20 I6 = 1,2
                    MTT(6) = MM(6) + MSE(6)*E6(I6)
                    MTC(6) = MM(12) + MSE(12)*E6(I6)
C
                    DO 10 ICMPS = 1 , 6
                      SIJ(ICMPS) = 0.D0
                      DO 12 ICMP = 1 , 6 
                        SIGT = SIGUN(6*(ICMP-1)+ICMPS)
                        SIGC = SIGUN(6*(ICMP+6-1)+ICMPS)
                        SIJ(ICMPS) = SIJ(ICMPS) + MTT(ICMP)*SIGT
                        SIJ(ICMPS) = SIJ(ICMPS) + MTC(ICMP)*SIGC
 12                   CONTINUE
                      SIGP = SIGUN(72+ICMPS)
                      SIJ(ICMPS) = SIJ(ICMPS) + PR*SIGP
 10                 CONTINUE
C
                    CALL RCTRES ( SIJ, TRESCA )
                    SNP1 = TRESCA
                    SNP = MAX( SNP , SNP1 )
C
 20               CONTINUE
 30             CONTINUE
 40           CONTINUE
 50         CONTINUE
 60       CONTINUE
 70     CONTINUE
C
C --- CALCUL THERMOMECANIQUE (DEPENDANT DU TEMPS)
C     -------------------------------------------
      ELSE
        DO 170 I1 = 1,2
          MTT(1) = MM(1) + MSE(1)*E1(I1)
          MTC(1) = MM(7) + MSE(7)*E1(I1)
          DO 160 I2 = 1,2
            MTT(2) = MM(2) + MSE(2)*E2(I2)
            MTC(2) = MM(8) + MSE(8)*E2(I2)
            DO 150 I3 = 1,2
              MTT(3) = MM(3) + MSE(3)*E3(I3)
              MTC(3) = MM(9) + MSE(9)*E3(I3)
              DO 140 I4 = 1,2
                MTT(4) = MM(4) + MSE(4)*E4(I4)
                MTC(4) = MM(10) + MSE(10)*E4(I4)
                DO 130 I5 = 1,2
                  MTT(5) = MM(5) + MSE(5)*E5(I5)
                  MTC(5) = MM(11) + MSE(11)*E5(I5)
                  DO 120 I6 = 1,2
                    MTT(6) = MM(6) + MSE(6)*E6(I6)
                    MTC(6) = MM(12) + MSE(12)*E6(I6)
C
                    DO 110 ICMPS = 1 , 6
                      SIJ(ICMPS) = 0.D0
                      DO 112 ICMP = 1 , 6 
                        SIGT = SIGUN(6*(ICMP-1)+ICMPS)
                        SIGC = SIGUN(6*(ICMP+6-1)+ICMPS+6)
                        SIJ(ICMPS) = SIJ(ICMPS) + MTT(ICMP)*SIGT
                        SIJ(ICMPS) = SIJ(ICMPS) + MTC(ICMP)*SIGC
 112                  CONTINUE
                      SIGP = SIGUN(72+ICMPS)
                      SIJ(ICMPS) = SIJ(ICMPS) + PR*SIGP
 110                CONTINUE
                    IF ( TYPE .EQ. 'COMB' ) THEN
                      DO 200 IT = 1,NBINST
                        DO 202 I = 1,2
                          DO 204 ICMP = 1,6
                            STH1 = STH((IT-1)*6+ICMP)
                            SIJT(ICMP) = SIJ(ICMP)*E1(I) + STH1
 204                      CONTINUE
                          CALL RCTRES ( SIJT, TRESCA )
                          SNP1 = TRESCA
                          SNP = MAX( SNP , SNP1 )
 202                    CONTINUE
 200                  CONTINUE
                    ELSE
                      DO 210 IT = 1,NBINST
                        DO 212 ICMP = 1,6
                          STH1 = STH((IT-1)*6+ICMP)
                          SIJT(ICMP) = SIJ(ICMP) + STH1
 212                    CONTINUE
                        CALL RCTRES ( SIJT, TRESCA )
                        SNP1 = TRESCA
                        SNP = MAX( SNP , SNP1 )
 210                  CONTINUE
                    ENDIF
C
 120              CONTINUE
 130            CONTINUE
 140          CONTINUE
 150        CONTINUE
 160      CONTINUE
 170    CONTINUE
      END IF
C
      END
