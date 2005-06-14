      SUBROUTINE RC32S0 ( TYPE, MM, PR, MSE, SIGUN, NBINST, STH, SNP )
      IMPLICIT   NONE
      INTEGER             NBINST
      REAL*8              MM(6),PR,MSE(6),SIGUN(*),STH(6*NBINST),SNP
      CHARACTER*4         TYPE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 30/05/2005   AUTEUR CIBHHLV L.VIVAN 
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
      REAL*8   MT(6), SIJ(6), SIJT(6), SNP1, STH1, EQUI(6), SIG
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
          MT(1) = MM(1) + MSE(1)*E1(I1)
          DO 60 I2 = 1,2
            MT(2) = MM(2) + MSE(2)*E2(I2)
            DO 50 I3 = 1,2
              MT(3) = MM(3) + MSE(3)*E3(I3)
              DO 40 I4 = 1,2
                MT(4) = MM(4) + MSE(4)*E4(I4)
                DO 30 I5 = 1,2
                  MT(5) = MM(5) + MSE(5)*E5(I5)
                  DO 20 I6 = 1,2
                    MT(6) = MM(6) + MSE(6)*E6(I6)
C
                    DO 10 ICMPS = 1 , 6
                      SIJ(ICMPS) = 0.D0
                      DO 12 ICMP = 1 , 6 
                        SIG = SIGUN(6*(ICMP-1)+ICMPS)
                        SIJ(ICMPS) = SIJ(ICMPS) + MT(ICMP)*SIG
 12                   CONTINUE
                      SIG = SIGUN(36+ICMPS)
                      SIJ(ICMPS) = SIJ(ICMPS) + PR*SIG
 10                 CONTINUE
C
                    CALL FGEQUI ( SIJ, 'SIGM', 3, EQUI )
                    SNP1 = EQUI(2)
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
          MT(1) = MM(1) + MSE(1)*E1(I1)
          DO 160 I2 = 1,2
            MT(2) = MM(2) + MSE(2)*E2(I2)
            DO 150 I3 = 1,2
              MT(3) = MM(3) + MSE(3)*E3(I3)
              DO 140 I4 = 1,2
                MT(4) = MM(4) + MSE(4)*E4(I4)
                DO 130 I5 = 1,2
                  MT(5) = MM(5) + MSE(5)*E5(I5)
                  DO 120 I6 = 1,2
                    MT(6) = MM(6) + MSE(6)*E6(I6)
C
                    DO 110 ICMPS = 1 , 6
                      SIJ(ICMPS) = 0.D0
                      DO 112 ICMP = 1 , 6 
                        SIG = SIGUN(6*(ICMP-1)+ICMPS)
                        SIJ(ICMPS) = SIJ(ICMPS) + MT(ICMP)*SIG
 112                  CONTINUE
                      SIG = SIGUN(36+ICMPS)
                      SIJ(ICMPS) = SIJ(ICMPS) + PR*SIG
 110                CONTINUE
                    IF ( TYPE .EQ. 'COMB' ) THEN
                      DO 200 IT = 1,NBINST
                        DO 202 I = 1,2
                          DO 204 ICMP = 1,6
                            STH1 = STH((IT-1)*6+ICMP)
                            SIJT(ICMP) = SIJ(ICMP)*E1(I) + STH1
 204                      CONTINUE
                          CALL FGEQUI ( SIJT, 'SIGM', 3, EQUI )
                          SNP1 = EQUI(2)
                          SNP = MAX( SNP , SNP1 )
 202                    CONTINUE
 200                  CONTINUE
                    ELSE
                      DO 210 IT = 1,NBINST
                        DO 212 ICMP = 1,6
                          STH1 = STH((IT-1)*6+ICMP)
                          SIJT(ICMP) = SIJ(ICMP) + STH1
 212                    CONTINUE
                        CALL FGEQUI ( SIJT, 'SIGM', 3, EQUI )
                        SNP1 = EQUI(2)
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
