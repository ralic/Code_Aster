      SUBROUTINE DICORN (IRMETG,NBT,NEQ,ITERAT,ICODMA,UL,DUL,UTL,
     &                   SIM,VARIM1,VARIM2,VARIM3,
     &                   KLV,KLV2,VARIP1,VARIP2,VARIP3)
C ----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER IRMETG,NBT,NEQ,ITERAT,ICODMA
      REAL*8  UL(NEQ),DUL(NEQ),UTL(NEQ)
      REAL*8  SIM(NEQ),VARIM1,VARIM2,VARIM3
      REAL*8  KLV(NBT),KLV2(NBT),VARIP1,VARIP2,VARIP3
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/04/2004   AUTEUR JMBHH01 J.M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C     RELATION DE COMPORTEMENT "ASSE_CORN" (CORNIERE).
C
C ----------------------------------------------------------------------
C
C IN  : IRMETG : VAUT 1 SI ON CALCULE L'OPTION "RIGI_MECA_TANG"
C       NBT    : NOMBRE DE VALEURS POUR LA DEMI-MATRICE
C       NEQ    : NOMBRE DE DDL DE L'ELEMENT
C       ITERAT : NUMERO DE L'ITERATION DE NEWTON
C       ICODMA : ADRESSE DU MATERIAU CODE
C       UL     : DEPLACEMENT PRECEDENT REPERE LOCAL (DIM NEQ)
C       DUL    : INCREMENT DE DEPLACEMENT REPERE LOCAL (DIM NEQ)
C       UTL    : DEPLACEMENT COURANT REPERE LOCAL (DIM NEQ)
C       SIM    : EFFORTS GENERALISES A L'INSTANT PRECEDENT (DIM NEQ)
C       VARIM$ : VARIABLES INTERNES A L'INSTANT PRECEDENT (3 VALEURS)
C
C OUT : KLV    :                                (DIM NBT)
C       KLV2   :                                (DIM NBT)
C       VARIP$ : VARIABLES INTERNES REACTUALISEES (3 VALEURS)
C
C***************** DECLARATION DES VARIABLES LOCALES *******************
C
      PARAMETER    ( NBRE1 = 14 )
      REAL*8       NU1,MU1,NU2,MU2, KY,KZ,KRX,KRZ
      REAL*8       SI(12),K01(78),K02(78),KLC(144), VALRE1(NBRE1)
      CHARACTER*2  CODRE1(NBRE1)
      CHARACTER*8  NOMPAR, NOMRE1(NBRE1)
C
C************ FIN DES DECLARATIONS DES VARIABLES LOCALES ***************
C
C****************************** DATA ***********************************
C
      DATA NOMRE1/'NU_1','MU_1','DXU_1','DRYU_1','C_1',
     &            'NU_2','MU_2','DXU_2','DRYU_2','C_2',
     &            'KY','KZ','KRX','KRZ'/
C
C ----------------------------------------------------------------------
C --- DEFINITION DES PARAMETRES
C
      ZERO = 0.D0
      NBPAR = 0
      NOMPAR = ' '
      VALPAR = 0.D0
      CALL R8INIR (NBRE1,ZERO,VALRE1,1)
C
C --- CARACTERISTIQUES DU MATERIAU
C    (LES DEFINITIONS DE DRYU1 ET DRYU2 SYMETRISENT LA MATRICE)
C
      CALL RCVALA(ICODMA,' ','ASSE_CORN',NBPAR,NOMPAR,VALPAR,NBRE1,
     &             NOMRE1,VALRE1,CODRE1, 'FM' )
C
      NU1   = VALRE1(1)
      MU1   = VALRE1(2)
      DXU1  = VALRE1(3)
      C1    = VALRE1(5)
      NU2   = VALRE1(6)
      MU2   = VALRE1(7)
      DXU2  = VALRE1(8)
      C2    = VALRE1(10)
      KY    = VALRE1(11)
      KZ    = VALRE1(12)
      KRX   = VALRE1(13)
      KRZ   = VALRE1(14)
      DRYU1 = NU1 * DXU1 / MU1
      DRYU2 = NU2 * DXU2 / MU2
C
C --- CONSTANTES DE LA RELATION DE COMPORTEMENT
C
      A1 = 1.D0
      A2 = 1.D0
      DBAR1 = C1**(A1+1.D0)/(1.D0-C1**A1)
      DBAR2 = C2**(A2+1.D0)/(1.D0-C2**A2)
C
C --- ECRITURE DANS LE REPERE LOCAL DE K01 ET K02 (MATRICES DE
C     RAIDEUR TANGENTE INITIALES POUR LES DEUX MECANISMES)
C
      CALL DIKINI (NBT,NU1,MU1,DXU1,DRYU1,NU2,MU2,DXU2,DRYU2,
     &             KY,KZ,KRX,KRZ,K01,K02)
C
C ======================================================================
C                  DEBUT DU TRAITEMENT DE L'ASSEMBLAGE
C ======================================================================
C
C --- DUR  : INCREMENT DE LONGUEUR DANS L'AXE DE L'ELEMENT
C --- DRYR : INCREMENT DE ROTATION
C
      DUR = DUL(7)-DUL(1)
      DRYR = DUL(11)-DUL(5)
      UU = UTL(7)-UTL(1)
      TT = UTL(11)-UTL(5)
      UI = UL(7)-UL(1)
      TI = UL(11)-UL(5)
C
C -*-*-*-*       TEST POUR SAVOIR SI L'ON DECHARGE OU NON      *-*-*-*-*
C
      IF ((((UU*DUR).GT.0.D0.AND.(UI*DUR).GE.0.D0).OR.
     &   ((TT*DRYR).GT.0.D0.AND.(TI*DRYR).GE.0.D0)).AND.
     &    IRMETG.NE.1) THEN
C
C ======================================================================
C                       ON NE DECHARGE PAS
C ======================================================================
C
         VARIP2 = 0.D0
C
C -*-*-*-* TEST POUR DETERMINER LE MECANISME OU L'ON SE TROUVE *-*-*-*-*
C
         IF (VARIM1.LE.1.D0) THEN
C
C ====================================
C ====== ON EST EN MECANISME 1 =======
C ====================================
C
           CALL VECMA (K01,NBT,KLC,NEQ)
           CALL PMAVEC ('ZERO',NEQ,KLC,DUL,SI)
           PI = SQRT ( (UI/DXU1)**2 + (TI/DRYU1)**2 )
C
C ****** TEST SUR LE NUMERO D'ITERATION
C
           IF (ITERAT.EQ.1) THEN
C
C ****** CAS DE LA PREMIERE ITERATION
C
             P1 = VARIM1
             G1 = DBAR1*P1
             RG1 = 0.5D0*(-G1+SQRT(G1**2 + 4.D0*G1))
             FEQ1 = SQRT ( (SIM(7)/NU1)**2 + (SIM(11)/MU1)**2 )
C
C **** TEST SUR LA POSITION PAR RAPPORT A LA SLU1
C
             IF (VARIM3.NE.0.D0) THEN
C
C **** ON EST SUR LA SLU1
C
               IF (VARIM1.EQ.0.D0) THEN
                 DNSDU2 = K01(1)
                 DMSDT2 = K01(15)
               ELSE
                 DNSDU2 = RG1*NU1/DXU1/P1
                 IF (DUR.EQ.0.D0) DNSDU2 = K01(1)
                 DMSDT2 = RG1*MU1/DRYU1/P1
                 IF (DRYR.EQ.0.D0) DMSDT2 = K01(15)
               ENDIF
C
               DNSDT2 = 0.D0
               SI(7) = SIM(7) + DNSDU2*DUR
               SI(11) = SIM(11) + DMSDT2*DRYR
               SI(1) = -SI(7)
               SI(5) = -SI(11)
C
               FEQ1 = SQRT( (SI(7)/NU1)**2 + (SI(11)/MU1)**2 )
C
C ** TEST DE CHANGEMENT DE MECANISME
C
               IF (FEQ1.LT.C1) THEN
C
C ** ON RESTE EN MECANISME 1
C
                  P1 = FEQ1**2/(1.D0-FEQ1)/DBAR1
                  IF (P1.LT.VARIM1) THEN
                    CALL DICOR1 (K01,VARIM1,SIM,DUR,DRYR,
     &                           DNSDU2,DMSDT2,DNSDT2,VARIP1,VARIP3,SI)
                  ELSE
                    CALL DICOR2 (K01,P1,P1,DUR,DRYR,DXU1,DRYU1,
     &                           FEQ1,NU1,MU1,UU,TT,SI,DNSDU2,DMSDT2,
     &                           DNSDT2,VARIP1,VARIP3,SI)
                  END IF
C
                  CALL DICOR3 (K01,DUR,DRYR,SIM,SI,DNSDU,DMSDT,DNSDT)
C
               ELSE
C
C ** ON PASSE EN MECANISME 2
C
                  CALL DICOR4 (K02,SIM,SI,PI,UI,TI,DXU1,DXU2,
     &                         DRYU1,DRYU2,NU1,NU2,MU1,MU2,
     &                         FEQ1,C1,DBAR2,UU,TT,DUR,DRYR,
     &                         P2,UTOT,TTOT,DNSDU,DMSDT,DNSDT,
     &                         DNSDU2,DMSDT2,DNSDT2)
                  VARIP1 = SQRT ( (UTOT/DXU1)**2 + (TTOT/DRYU1)**2 )
                  VARIP2 = P2
                  VARIP3 = 2.D0
C
               END IF
C
             ELSE
C
C **** ON EST SOUS LA SLU1
C
               CALL DICOR1 (K01,VARIM2,SIM,DUR,DRYR,
     &                      DNSDU2,DMSDT2,DNSDT2,VARIP2,VVV,SI)
               FEQ1 = SQRT ( ((SIM(7)+K01(1)*DUR)/NU1)**2
     &                     + ((SIM(11)+K01(15)*DRYR)/MU1)**2 )
C
C ** TEST POUR SAVOIR SI L'ON RESTE SOUS LA SLU1
C
               IF (RG1.GE.FEQ1.AND.ABS(FEQ1-RG1).GT.(1.D-3*FEQ1)) THEN
C
C ** ON RESTE SOUS LA SLU1
C
                 CALL DICOR0 (K01,VARIM1,VARIP1,VARIP3,DNSDU,
     &                        DMSDT,DNSDT)
C
               ELSE
C
C ** ON VA SUR LA SLU1
C
                 FEQ1 = SQRT ( (SI(7)/NU1)**2 + (SI(11)/MU1)**2 )
C
C  TEST DE CHANGEMENT DE MECANISME
C
                 IF (FEQ1.LT.C1) THEN
C
C  ON RESTE EN MECANISME 1
C
                    P1 = FEQ1**2/(1.D0-FEQ1)/DBAR1
                    CALL DICOR2 (K01,P1,P1,DUR,DRYR,DXU1,DRYU1,
     &                           FEQ1,NU1,MU1,UU,TT,SI,DNSDU2,DMSDT2,
     &                           DNSDT2,VARIP1,VARIP3,SI)
                    CALL DICOR3 (K01,DUR,DRYR,SIM,SI,
     &                           DNSDU,DMSDT,DNSDT)
C
                 ELSE
C
C  ON PASSE EN MECANISME 2
C
                    CALL DICOR4 (K02,SIM,SI,PI,UI,TI,DXU1,DXU2,DRYU1,
     &                           DRYU2,NU1,NU2,MU1,MU2,FEQ1,C1,DBAR2,
     &                           UU,TT,DUR,DRYR,P2,UTOT,TTOT,
     &                           DNSDU,DMSDT,DNSDT,DNSDU2,DMSDT2,DNSDT2)
C
                    VARIP1 = 0.D0
                    VARIP2 = P2
                    VARIP3 = 2.D0
                    IF ((UU*DUR).GT.0.D0)  VARIP1 = ABS(UTOT/DXU1)
                    IF ((TT*DRYR).GT.0.D0) VARIP1 = ABS(TTOT/DRYU1)
C
                 END IF
C
               ENDIF
C
             ENDIF
C
           ELSEIF (ITERAT.GE.2) THEN
C
C ****** CAS DES ITERATIONS 2 ET SUIVANTES
C
                 VARIP1 = SQRT ( (UU/DXU1)**2 + (TT/DRYU1)**2 )
                 P1 = VARIP1
C
                 IF (P1.LE.1.D0) THEN
C
C **** ON RESTE EN MECANISME 1
C
                    FEQ1 = SQRT ( ((SIM(7)+K01(1)*DUR)/NU1)**2
     &                          + ((SIM(1)+K01(15)*DRYR)/MU1)**2 )
                    G1 = DBAR1*P1
                    RG1 = 0.5D0*(-G1+SQRT(G1**2 + 4.D0*G1))
C
                    IF (P1.LT.VARIM1.OR.FEQ1.LT.RG1) THEN
                      CALL DICOR1 (K01,VARIM1,SIM,DUR,DRYR,
     &                             DNSDU2,DMSDT2,DNSDT2,VARIP1,
     &                             VARIP3,SI)
                    ELSE
                      CALL DICOR2 (K01,VARIM2,P1,DUR,DRYR,DXU1,
     &                             DRYU1,RG1,NU1,MU1,UU,TT,SIM,DNSDU2,
     &                             DMSDT2,DNSDT2,VARIP2,VARIP3,SI)
                    END IF
C
                    CALL DICOR3 (K01,DUR,DRYR,SIM,SI,
     &                           DNSDU,DMSDT,DNSDT)
C
                 ELSE
C
C **** ON PASSE EN MECANISME 2
C

                    G1 = DBAR1 * VARIM1
                    RG1 = 0.5D0 * (-G1 + SQRT(G1**2 + 4.D0*G1))
                    CALL DICOR5 (K02,SIM,P1,PI,UI,TI,DXU1,DXU2,
     &                           DRYU1,DRYU2,NU1,NU2,MU1,MU2,
     &                           C1,DBAR2,UU,TT,DUR,DRYR,
     &                           DNSDU,DMSDT,DNSDT,DNSDU2,DMSDT2,
     &                           DNSDT2,SI,VARIP2,VARIP3)
C
                 END IF
C
           ENDIF
C
C -*-*-*-*-*-*-*-*-*-*-*-* FIN DU MECANISME 1 *-*-*-*-*-*-*-*-*-*-*-*-*
C
         ELSE
C
C ====================================
C ====== ON EST EN MECANISME 2 =======
C ====================================
C
           DP2 = SQRT ( (DUR/DXU2)**2 + (DRYR/DRYU2)**2 )
           P2 = VARIM2
           VARIP2 = P2 + DP2
           VARIP1 = VARIM1
           G2 = DBAR2*P2
           RG2 = 0.5D0*(-G2+SQRT(G2**2 + 4.D0*G2))
           FEQ2 = SQRT ( (SIM(7)/NU2)**2 + (SIM(11)/MU2)**2 )
C
C ****** TEST SUR LA POSITION PAR RAPPORT A LA SLU2
C
           IF (VARIM3.NE.0.D0) THEN
C
C ****** ON EST SUR LA SLU2
C
              FEQ2 = SQRT ( ((SIM(7)+K02(1)*DUR)/NU2)**2
     &                    + ((SIM(11)+K02(15)*DRYR)/MU2)**2 )
C
              IF (FEQ2.LT.RG2) THEN
                CALL DICOR0 (K02,VARIM2,VARIP2,VARIP3,DNSDU,DMSDT,DNSDT)
                CALL DICOR0 (K02,VARIM2,VARIP2,VARIP3,
     &                       DNSDU2,DMSDT2,DNSDT2)
              ELSE
                CALL DICOR6 (K02,SIM,P2,DP2,DBAR2,DUR,DRYR,NU2,MU2,
     &                       DXU2,DRYU2,VARIP3,DNSDU,DMSDT,
     &                       DNSDT,DNSDU2,DMSDT2,DNSDT2)
              END IF
C
           ELSEIF (VARIM3.EQ.0.D0) THEN
C
C ****** ON EST SOUS LA SLU2
C
              FEQ2 = SQRT ( ((SIM(7)+K02(1)*DUR)/NU2)**2
     &                    + ((SIM(11)+K02(15)*DRYR)/MU2)**2 )
C
C **** TEST POUR SAVOIR SI L'ON RESTE SOUS LA SLU
C
              IF (RG2.GE.FEQ2.AND.ABS(FEQ2-RG2).GT.(1.D-3*FEQ2)) THEN
C
C **** ON RESTE SOUS LA SLU2
C
                CALL DICOR0 (K02,VARIM2,VARIP2,VARIP3,DNSDU,DMSDT,DNSDT)
                CALL DICOR0 (K02,VARIM2,VARIP2,VARIP3,
     &                       DNSDU2,DMSDT2,DNSDT2)
C
              ELSE
C
C **** ON REVIENT SUR LA SLU2
C
                COEF2 = RG2/FEQ2
                IF (ITERAT.EQ.1) THEN
                  SI(7) = (SIM(7)+K02(1)*DUR)*COEF2
                  SI(11) = (SIM(11)+K02(15)*DRYR)*COEF2
                  IF (DUR.NE.0.D0)  DNSDU = (SI(7)-SIM(7))/DUR
                  IF (DRYR.NE.0.D0) DMSDT = (SI(11)-SIM(11))/DRYR
                ENDIF
                CALL DICOR6 (K02,SIM,P2,DP2,DBAR2,DUR,DRYR,NU2,MU2,
     &                       DXU2,DRYU2,VARIP3,DNSDU,DMSDT,
     &                       DNSDT,DNSDU2,DMSDT2,DNSDT2)
                VARIP2 = P2
C
              ENDIF
C
           ENDIF
C
C -*-*-*-*-*-*-*-*-*-*-*-* FIN DU MECANISME 2 *-*-*-*-*-*-*-*-*-*-*-*-*
C
         ENDIF
C
      ELSE
C
C ======================================================================
C                             ON DECHARGE
C ======================================================================
C
         IF (VARIM1.LE.1.D0) THEN
               CALL DICOR0 (K01,VARIM1,VARIP1,PLOUF,DNSDU,DMSDT,DNSDT)
               CALL DICOR0 (K01,VARIM2,VARIP2,VARIP3,
     &                      DNSDU2,DMSDT2,DNSDT2)
               IF (IRMETG.EQ.1) THEN
                  P1 = VARIM1
                  G1 = DBAR1*P1
                  RG1 = 0.5D0*(-G1+SQRT(G1**2 + 4.D0*G1))
                  IF (P1*VARIM3.NE.0.D0) DNSDU2 = RG1*NU1/DXU1/P1
                  IF (P1*VARIM3.NE.0.D0) DMSDT2 = RG1*MU1/DRYU1/P1
               ENDIF
         ELSE
               CALL DICOR0 (K02,VARIM1,VARIP1,PLOUF,DNSDU,DMSDT,DNSDT)
               CALL DICOR0 (K02,VARIM2,VARIP2,VARIP3,
     &                      DNSDU2,DMSDT2,DNSDT2)
               IF (IRMETG.EQ.1) THEN
                  P2 = VARIM2
                  G2 = DBAR2*P2
                  RG2 = 0.5D0*(-G2+SQRT(G2**2 + 4.D0*G2))
                  IF (P2*VARIM3.NE.0.D0) DNSDU2 = RG2*NU2/DXU2/P2
                  IF (P2*VARIM3.NE.0.D0) DMSDT2 = RG2*MU2/DRYU2/P2
               ENDIF
         ENDIF
C
      ENDIF
C
C ======================================================================
C                         PARAMETRES EN SORTIE
C ======================================================================
C
C --- ECRITURE DE LA MATRICE TANGENTE EN REPERE LOCAL
C
      CALL DIKFIN (NBT,DNSDU,DNSDT,DMSDT,DNSDU2,DNSDT2,DMSDT2,
     &             KY,KZ,KRX,KRZ,KLV,KLV2)
C ----------------------------------------------------------------------
C
 9999 CONTINUE
      END
