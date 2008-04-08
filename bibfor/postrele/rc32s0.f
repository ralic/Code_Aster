      SUBROUTINE RC32S0 ( TYPE, MM, PR, MSE, SIGUN, NBINST, STH, SNP )
      IMPLICIT   NONE
      INTEGER             NBINST
      REAL*8              MM(*),PR,MSE(*),SIGUN(*),STH(6*NBINST),SNP
      CHARACTER*4         TYPE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 07/04/2008   AUTEUR GALENNE E.GALENNE 
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
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      INTEGER VALI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER  I1, I2, I3, I4, I5, I6, I, IT, IT1, ICMPS, ICMP, JCORP
      INTEGER  I11, I21, I31, I41, I51, I61
      REAL*8   MTT(6), MTC(6), SIJ(6), SIJT(6), SNP1, STH1, TRESCA
      REAL*8   SIGT, SIGC, SIGP
      REAL*8   E1(2), E2(2), E3(2), E4(2), E5(2), E6(2)
C DEB ------------------------------------------------------------------
C
      SNP = 0.D0
C ON DISTINGUE LE CAS CLASSIQUE DU CAS CORPS/TUBU POUR OPTIMISER 
C LES PERFORMANCES      
      CALL JEVEUO ('&&RC3200.CORPS', 'L ', JCORP )
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
        DO 60 I2 = 1,2
         MTT(2) = MM(2) + MSE(2)*E2(I2)
         DO 50 I3 = 1,2
          MTT(3) = MM(3) + MSE(3)*E3(I3)
          DO 40 I4 = 1,2
           MTT(4) = MM(4) + MSE(4)*E4(I4)
           DO 30 I5 = 1,2
            MTT(5) = MM(5) + MSE(5)*E5(I5)
            DO 20 I6 = 1,2
             MTT(6) = MM(6) + MSE(6)*E6(I6)
C CAS CORPS/TUBULURE             
             IF (ZL(JCORP)) THEN
               DO 71 I11 = 1,2
                MTC(1) = MM(7) + MSE(7)*E1(I11)
                DO 61 I21 = 1,2
                 MTC(2) = MM(8) + MSE(8)*E2(I21)
                 DO 51 I31 = 1,2
                  MTC(3) = MM(9) + MSE(9)*E3(I31)
                  DO 41 I41 = 1,2
                   MTC(4) = MM(10) + MSE(10)*E4(I41)
                   DO 31 I51 = 1,2
                    MTC(5) = MM(11) + MSE(11)*E5(I51)
                    DO 21 I61 = 1,2
                     MTC(6) = MM(12) + MSE(12)*E6(I61)
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
 21                 CONTINUE
 31                CONTINUE
 41               CONTINUE
 51              CONTINUE
 61             CONTINUE
 71            CONTINUE
C
             ELSE
               DO 101 ICMPS = 1 , 6
                 SIJ(ICMPS) = 0.D0
                 DO 102 ICMP = 1 , 6 
                   SIGT = SIGUN(6*(ICMP-1)+ICMPS)
                   SIJ(ICMPS) = SIJ(ICMPS) + MTT(ICMP)*SIGT
 102             CONTINUE
                 SIGP = SIGUN(72+ICMPS)
                 SIJ(ICMPS) = SIJ(ICMPS) + PR*SIGP
 101           CONTINUE
               CALL RCTRES ( SIJ, TRESCA )
               SNP1 = TRESCA
               SNP = MAX( SNP , SNP1 )
C
             ENDIF
 20          CONTINUE
 30         CONTINUE
 40        CONTINUE
 50       CONTINUE
 60      CONTINUE
 70     CONTINUE
C
C --- CALCUL THERMOMECANIQUE (DEPENDANT DU TEMPS)
C     -------------------------------------------
      ELSE
        DO 170 I1 = 1,2
         MTT(1) = MM(1) + MSE(1)*E1(I1)
         DO 160 I2 = 1,2
          MTT(2) = MM(2) + MSE(2)*E2(I2)
          DO 150 I3 = 1,2
           MTT(3) = MM(3) + MSE(3)*E3(I3)
           DO 140 I4 = 1,2
            MTT(4) = MM(4) + MSE(4)*E4(I4)
            DO 130 I5 = 1,2
             MTT(5) = MM(5) + MSE(5)*E5(I5)
             DO 120 I6 = 1,2
              MTT(6) = MM(6) + MSE(6)*E6(I6)
C CAS CORPS/TUBULURE             
              IF (ZL(JCORP)) THEN
               DO 171 I11 = 1,2
                MTC(1) = MM(7) + MSE(7)*E1(I11)
                DO 161 I21 = 1,2
                 MTC(2) = MM(8) + MSE(8)*E2(I21)
                 DO 151 I31 = 1,2
                  MTC(3) = MM(9) + MSE(9)*E3(I31)
                  DO 141 I41 = 1,2
                   MTC(4) = MM(10) + MSE(10)*E4(I41)
                   DO 131 I51 = 1,2
                    MTC(5) = MM(11) + MSE(11)*E5(I51)
                    DO 121 I61 = 1,2
                     MTC(6) = MM(12) + MSE(12)*E6(I61)
              
                     DO 110 ICMPS = 1 , 6
                      SIJ(ICMPS) = 0.D0
                      DO 112 ICMP = 1 , 6 
                        SIGT = SIGUN(6*(ICMP-1)+ICMPS)
                        SIGC = SIGUN(6*(ICMP+6-1)+ICMPS)
                        SIJ(ICMPS) = SIJ(ICMPS) + MTT(ICMP)*SIGT
                        SIJ(ICMPS) = SIJ(ICMPS) + MTC(ICMP)*SIGC
 112                  CONTINUE
                      SIGP = SIGUN(72+ICMPS)
                      SIJ(ICMPS) = SIJ(ICMPS) + PR*SIGP
 110                 CONTINUE
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
 121                CONTINUE
 131               CONTINUE
 141              CONTINUE
 151             CONTINUE
 161            CONTINUE
 171           CONTINUE
C              
              ELSE
                DO 1101 ICMPS = 1 , 6
                  SIJ(ICMPS) = 0.D0
                  DO 1121 ICMP = 1 , 6 
                    SIGT = SIGUN(6*(ICMP-1)+ICMPS)
                    SIJ(ICMPS) = SIJ(ICMPS) + MTT(ICMP)*SIGT
 1121             CONTINUE
                  SIGP = SIGUN(72+ICMPS)
                  SIJ(ICMPS) = SIJ(ICMPS) + PR*SIGP
 1101           CONTINUE
                IF ( TYPE .EQ. 'COMB' ) THEN
                  DO 2001 IT = 1,NBINST
                    DO 2021 I = 1,2
                      DO 2041 ICMP = 1,6
                        STH1 = STH((IT-1)*6+ICMP)
                        SIJT(ICMP) = SIJ(ICMP)*E1(I) + STH1
 2041                 CONTINUE
                      CALL RCTRES ( SIJT, TRESCA )
                      SNP1 = TRESCA
                      SNP = MAX( SNP , SNP1 )
 2021              CONTINUE
 2001             CONTINUE
                ELSE
                  DO 2101 IT = 1,NBINST
                    DO 2121 ICMP = 1,6
                      STH1 = STH((IT-1)*6+ICMP)
                      SIJT(ICMP) = SIJ(ICMP) + STH1
 2121               CONTINUE
                    CALL RCTRES ( SIJT, TRESCA )
                    SNP1 = TRESCA
                    SNP = MAX( SNP , SNP1 )
 2101             CONTINUE
                ENDIF
              ENDIF
C
 120         CONTINUE
 130        CONTINUE
 140       CONTINUE
 150      CONTINUE
 160     CONTINUE
 170    CONTINUE
      END IF
C
      END
