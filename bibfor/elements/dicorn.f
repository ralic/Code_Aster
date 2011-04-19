      SUBROUTINE DICORN (IRMETG,NBT,NEQ,ITERAT,ICODMA,UL,DUL,UTL,
     &                   SIM,VARIM,
     &                   KLV,KLV2,VARIP)
C ----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER IRMETG,NBT,NEQ,ITERAT,ICODMA
      REAL*8  UL(NEQ),DUL(NEQ),UTL(NEQ)
      REAL*8  SIM(NEQ),VARIM(7)
      REAL*8  KLV(NBT),KLV2(NBT),VARIP(7)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 20/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_20
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
C       VARIM$ : VARIABLES INTERNES A L'INSTANT PRECEDENT (7 VALEURS)
C
C OUT : KLV    :                                (DIM NBT)
C       KLV2   :                                (DIM NBT)
C       VARIP$ : VARIABLES INTERNES REACTUALISEES (7 VALEURS)
C
C***************** DECLARATION DES VARIABLES LOCALES *******************
C
      PARAMETER    ( NBRE1 = 15 )
      REAL*8       NU1,MU1,NU2,MU2,KY,KZ,KRX,KRZ,RP0
      REAL*8       SI(12),K01(78),K02(78),KLC(144), VALRE1(NBRE1)
      INTEGER CODRE1(NBRE1)
      CHARACTER*8  NOMPAR, NOMRE1(NBRE1)
C
C************ FIN DES DECLARATIONS DES VARIABLES LOCALES ***************
C
C****************************** DATA ***********************************
C
      DATA NOMRE1/'NU_1','MU_1','DXU_1','DRYU_1','C_1',
     &            'NU_2','MU_2','DXU_2','DRYU_2','C_2',
     &            'KY','KZ','KRX','KRZ','R_P0'/
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
      CALL RCVALA (ICODMA,' ','ASSE_CORN',NBPAR,NOMPAR,VALPAR,NBRE1,
     &             NOMRE1,VALRE1,CODRE1, 1)
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
C      DRYU1 = NU1 * DXU1 / MU1
C      DRYU2 = NU2 * DXU2 / MU2
      DRYU1 = VALRE1(4)
      DRYU2 = VALRE1(9)
      RP0   = VALRE1(15)
C      RP0   = 1.D4
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
     &             KY,KZ,KRX,KRZ,K01,K02,RP0)
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
C      INDECH = 0
C
C -*-*-*-*       TEST POUR SAVOIR SI L'ON DECHARGE OU NON      *-*-*-*-*
C
C      IF ((((UU*DUR).GT.0.D0.AND.(UI*DUR).GE.0.D0).OR.
C     &   ((TT*DRYR).GT.0.D0.AND.(TI*DRYR).GE.0.D0))) INDECH = 1

      IF (IRMETG.NE.1) THEN
C
C ======================================================================
C                       FULL_MECA
C ======================================================================
C
         VARIP(2) = 0.D0
C
C -*-*-*-* TEST POUR DETERMINER LE MECANISME OU L'ON SE TROUVE *-*-*-*-*
C
         IF (VARIM(1).LE.1.D0.OR.VARIM(3).EQ.1.D0) THEN
C
C ====================================
C ====== ON EST EN MECANISME 1 =======
C ====================================
C
           CALL VECMA (K01,NBT,KLC,NEQ)
           CALL PMAVEC ('ZERO',NEQ,KLC,DUL,SI)
           PI = VARIM(1)
C
C ****** TEST SUR LE NUMERO D'ITERATION
C
           IF (ITERAT.EQ.1) THEN
C
C ****** CAS DE LA PREMIERE ITERATION
C
             P1 = VARIM(1)
             G1 = DBAR1*P1
             RG1 = 0.5D0*(-G1+SQRT(G1**2 + 4.D0*G1))
C
C **** TEST SUR LA POSITION PAR RAPPORT A LA SLU1
C
C
C **** ON EST SUR LA SLU1
C
               IF (VARIM(1).EQ.0.D0) THEN
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
                  U2 = P1*DXU1*SI(7)/NU1/FEQ1
                  T2 = P1*DRYU1*SI(11)/MU1/FEQ1
                  UTOT = U2+VARIM(4)
                  TTOT = T2+VARIM(5)
C
                  IF (DUR.NE.0.D0)  DNSDU2 = SI(7)/UTOT
                  IF (DUR.EQ.0.D0)  DNSDU2 = K01(1)
                  IF (DRYR.NE.0.D0) DMSDT2 = SI(11)/TTOT
                  IF (DRYR.EQ.0.D0) DMSDT2 = K01(15)
                  DNSDT2 = 0.D0
                  SI(7) = DNSDU2*UU
                  SI(11) = DMSDT2*TT
                  VARIP(1) = P1
                  VARIP(2) = VARIM(2)
                  VARIP(3) = 1.0D0
C
                  CALL DICOR3 (K01,DUR,DRYR,SIM,SI,DNSDU,DMSDT,DNSDT)
C
                  DO 5 I = 4, 7
                    VARIP(I) = VARIM(I)
    5             CONTINUE
               ELSE
C
C ** ON PASSE EN MECANISME 2
C
                  U2 = UI - VARIM(4)
                  T2 = TI - VARIM(5)
                  CALL DICOR4 (K02,SIM,SI,PI,U2,T2,DXU1,DXU2,
     &                         DRYU1,DRYU2,NU1,NU2,MU1,MU2,
     &                         FEQ1,C1,DBAR2,UU,TT,DUR,DRYR,
     &                         P2,UTOT,TTOT,DNSDU,DMSDT,DNSDT,
     &                         DNSDU2,DMSDT2,DNSDT2)
                  VARIP(4) = UTOT - SI(7)/K02(1)
                  VARIP(5) = TTOT - SI(11)/K02(15)
                  VARIP(6) = SI(7)
                  VARIP(7) = SI(11)
                  U2 = UTOT - VARIM(4)
                  T2 = TTOT - VARIM(5)
                  VARIP(1) = SQRT( (U2/DXU1)**2 + (T2/DRYU1)**2 )
                  VARIP(2) = P2
                  VARIP(3) = 2.D0
C
               END IF
C
C             ELSE
C
C **** ON EST SOUS LA SLU1
C
C
C             ENDIF
C
           ELSEIF (ITERAT.GE.2) THEN
C
C ****** CAS DES ITERATIONS 2 ET SUIVANTES
C
                 U2 = UU - VARIM(4)
                 T2 = TT - VARIM(5)
                 VARIP(1) = SQRT ( (U2/DXU1)**2 + (T2/DRYU1)**2 )
                 P1 = VARIP(1)
C
                 IF (P1.LE.1.D0) THEN
C
C **** ON RESTE EN MECANISME 1
C
                    G1 = DBAR1*P1
                    RG1 = 0.5D0*(-G1+SQRT(G1**2 + 4.D0*G1))
                    DNSDU2 = RG1*NU1/DXU1/P1
                    IF (DUR.EQ.0.D0) DNSDU2 = K01(1)
                    DMSDT2 = RG1*MU1/DRYU1/P1
                    IF (DRYR.EQ.0.D0) DMSDT2 = K01(15)
C
                    DNSDT2 = 0.D0

C
                    CALL DICOR2 (K01,VARIM(2),P1,DUR,DRYR,DXU1,
     &                           DRYU1,RG1,NU1,MU1,U2,T2,SIM,DNSDU2,
     &                           DMSDT2,DNSDT2,VARIP(2),VARIP(3),SI)
C
                    CALL DICOR3 (K01,DUR,DRYR,SIM,SI,DNSDU,DMSDT,DNSDT)
                    DO 10 I = 4, 7
                      VARIP(I) = VARIM(I)
   10               CONTINUE
C
                 ELSE
C
C **** ON PASSE EN MECANISME 2
C

                    G1 = DBAR1 * VARIM(1)
                    RG1 = 0.5D0 * (-G1 + SQRT(G1**2 + 4.D0*G1))
                    U2 = UI - VARIM(4)
                    T2 = TI - VARIM(5)
                    CALL DICOR5 (K02,SIM,P1,PI,U2,T2,DXU1,DXU2,
     &                           DRYU1,DRYU2,NU1,NU2,MU1,MU2,
     &                           C1,DBAR2,UU,TT,DUR,DRYR,
     &                           DNSDU,DMSDT,DNSDT,DNSDU2,DMSDT2,
     &                           DNSDT2,SI,VARIP(2),VARIP(3))
                    VARIP(4) = UU - SI(7)/K02(1)
                    VARIP(5) = TT - SI(11)/K02(15)
                    VARIP(6) = SI(7)
                    VARIP(7) = SI(11)
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
           P2 = VARIM(2)
           VARIP(1) = VARIM(1)
           G2 = DBAR2*P2
           RG2 = 0.5D0*(-G2+SQRT(G2**2 + 4.D0*G2))
C ****** TEST SUR LA POSITION PAR RAPPORT A LA SLU2
C
           IF (VARIM(3).EQ.2.D0) THEN
C
C ****** ON EST SUR LA SLU2
C
              DNSDU2 = RG2*NU2/DXU2/P2
              DMSDT2 = RG2*MU2/DRYU2/P2
              FEQ2 = SQRT ( ((SIM(7)+DNSDU2*DUR)/NU2)**2
     &                    + ((SIM(11)+DMSDT2*DRYR)/MU2)**2 )
              IF (FEQ2.LT.RG2) THEN
                CALL DICOR0 (K02,VARIM(2),VARIP(2),VARIP(3),
     &                       DNSDU,DMSDT,DNSDT)
                CALL DICOR0 (K02,VARIM(2),VARIP(2),VARIP(3),
     &                       DNSDU2,DMSDT2,DNSDT2)
                DO 15 I = 4, 7
                  VARIP(I) = VARIM(I)
   15           CONTINUE
              ELSE
                IF (ITERAT.EQ.1) THEN
                  IF (FEQ2.GE.C2) THEN
                    CALL U2MESS('I','ELEMENTS_26')
                  ENDIF
                  SI(7) = SIM(7) + DNSDU2*DUR
                  SI(11) = SIM(11) + DMSDT2*DRYR
                  VARIP(2) = FEQ2**2/(1.D0-FEQ2)/DBAR2
                  UR2 = (VARIP(2)*SI(7)/FEQ2-P2*SIM(7)/RG2)/NU2
                  TR2 = (VARIP(2)*SI(11)/FEQ2-P2*SIM(11)/RG2)/MU2
                  U2 = UR2*DXU2
                  T2 = TR2*DRYU2
                  UTOT = U2+UI
                  TTOT = T2+TI
C
                  IF (DUR.NE.0.D0)  DNSDU2 = SI(7)/UTOT
                  IF (DUR.EQ.0.D0)  DNSDU2 = K02(1)
                  IF (DRYR.NE.0.D0) DMSDT2 = SI(11)/TTOT
                  IF (DRYR.EQ.0.D0) DMSDT2 = K02(15)
                  DNSDT2 = 0.D0
                  VARIP(4) = UTOT - SI(7)/K02(1)
                  VARIP(5) = TTOT - SI(11)/K02(15)
                  VARIP(6) = SI(7)
                  VARIP(7) = SI(11)
                  SI(7) = DNSDU2*UU
                  SI(11) = DMSDT2*TT
                  CALL U2MESS('I','ELEMENTS_27')
                  VARIP(3) = 2.0D0
C
                  CALL DICOR3 (K02,DUR,DRYR,SIM,SI,DNSDU,DMSDT,DNSDT)
                ELSE
                  U2 = DUR + P2*SIM(7)*DXU2/RG2/NU2
                  T2 = DRYR + P2*SIM(11)*DRYU2/RG2/MU2
                  VARIP(2) = SQRT ( (U2/DXU2)**2 + (T2/DRYU2)**2 )
                  G2 = DBAR2*VARIP(2)
                  FEQ2 = 0.5D0*(-G2+SQRT(G2**2 + 4.D0*G2))
                  DNSDU2 = FEQ2*NU2/DXU2/VARIP(2)
                  IF (DUR.EQ.0.D0) DNSDU2 = K02(1)
                  DMSDT2 = FEQ2*MU2/DRYU2/VARIP(2)
                  IF (DRYR.EQ.0.D0) DMSDT2 = K02(15)
                  DNSDT2 = 0.D0
                  SI(7) = U2*FEQ2*NU2/DXU2/VARIP(2)
                  SI(11) = T2*FEQ2*MU2/DRYU2/VARIP(2)
                  CALL U2MESS('I','ELEMENTS_27')
                  CALL DICOR3 (K02,DUR,DRYR,SIM,SI,DNSDU,DMSDT,DNSDT)
                  VARIP(3) = 2.D0
                  VARIP(4) = UU - SI(7)/K02(1)
                  VARIP(5) = TT - SI(11)/K02(15)
                  VARIP(6) = SI(7)
                  VARIP(7) = SI(11)
                ENDIF
              END IF
C
           ELSEIF (VARIM(3).EQ.0.D0) THEN
C
C ****** ON EST SOUS LA SLU2
C
              FEQ2 = SQRT ( ((SIM(7)+K02(1)*DUR)/NU2)**2
     &                    + ((SIM(11)+K02(15)*DRYR)/MU2)**2 )
C
C **** TEST POUR SAVOIR SI L'ON RESTE SOUS LA SLU
C
              IF (FEQ2.LE.RG2) THEN
C
C **** ON RESTE SOUS LA SLU2
C
                SI(7) = SIM(7) + K02(1)*DUR
                SI(11) = SIM(11) + K02(15)*DRYR
                TEST = VARIM(6)*SI(7)/NU2**2+VARIM(7)*SI(11)/MU2**2
                IF (TEST.LT.0.D0) THEN
                  IF (ITERAT.EQ.1) THEN
                    FEQ1 = SQRT( (SI(7)/NU1)**2 + (SI(11)/MU1)**2 )
                    IF (FEQ1.GE.C1) THEN
                      CALL U2MESS('I','ELEMENTS_28')
                      GOTO 19
                    ENDIF
                    CALL U2MESS('I','ELEMENTS_29')
C
C ** ON REPASSE EN MECANISME 1
C
                    P1 = FEQ1**2/(1.D0-FEQ1)/DBAR1
                    U2 = P1*DXU1*SI(7)/NU1/FEQ1
                    T2 = P1*DRYU1*SI(11)/MU1/FEQ1
                    UTOT = U2+VARIM(4)
                    TTOT = T2+VARIM(5)
                    DU2 = UTOT-UI
                    DRY2 = TTOT-TI
                    FEQ2 = SQRT ( ((SIM(7)+K02(1)*DU2)/NU2)**2
     &                          + ((SIM(11)+K02(15)*DRY2)/MU2)**2 )
                    IF (FEQ2.GT.RG2) THEN
                      CALL U2MESS('I','ELEMENTS_30')
                    ENDIF
C
                    IF (DUR.NE.0.D0)  DNSDU2 = SI(7)/UTOT
                    IF (DUR.EQ.0.D0)  DNSDU2 = K01(1)
                    IF (DRYR.NE.0.D0) DMSDT2 = SI(11)/TTOT
                    IF (DRYR.EQ.0.D0) DMSDT2 = K01(15)
                    DNSDT2 = 0.D0
                    SI(7) = DNSDU2*UU
                    SI(11) = DMSDT2*TT
                    VARIP(1) = P1
                    VARIP(2) = VARIM(2)
                    VARIP(3) = 1.0D0
C
                    CALL DICOR3 (K01,DUR,DRYR,SIM,SI,DNSDU,DMSDT,DNSDT)
C
                  ELSE
C
C ****** CAS DES ITERATIONS 2 ET SUIVANTES
C
                    U2 = UU - VARIM(4)
                    T2 = TT - VARIM(5)
                    VARIP(1) = SQRT ( (U2/DXU1)**2 + (T2/DRYU1)**2 )
                    P1 = VARIP(1)
                    CALL U2MESS('I','ELEMENTS_29')
C
                    IF (P1.GT.1.D0) THEN
                      CALL U2MESS('I','ELEMENTS_28')
                      GOTO 19
                    ENDIF
C
C **** ON EST EN MECANISME 1
C
                    G1 = DBAR1*P1
                    RG1 = 0.5D0*(-G1+SQRT(G1**2 + 4.D0*G1))
                    DNSDU2 = RG1*NU1/DXU1/P1
                    IF (DUR.EQ.0.D0) DNSDU2 = K01(1)
                    DMSDT2 = RG1*MU1/DRYU1/P1
                    IF (DRYR.EQ.0.D0) DMSDT2 = K01(15)
C
                    DNSDT2 = 0.D0

C
                    CALL DICOR2 (K01,VARIM(2),P1,DUR,DRYR,DXU1,
     &                           DRYU1,RG1,NU1,MU1,U2,T2,SIM,DNSDU2,
     &                           DMSDT2,DNSDT2,VARIP(2),VARIP(3),SI)
C
                    CALL DICOR3 (K01,DUR,DRYR,SIM,SI,DNSDU,DMSDT,DNSDT)
C
                  ENDIF
                  GOTO 20
                ENDIF
   19           CONTINUE
                CALL DICOR0 (K02,VARIM(2),VARIP(2),VARIP(3),
     &                       DNSDU,DMSDT,DNSDT)
                CALL DICOR0 (K02,VARIM(2),VARIP(2),VARIP(3),
     &                       DNSDU2,DMSDT2,DNSDT2)
   20           CONTINUE
                DO 25 I = 4, 7
                  VARIP(I) = VARIM(I)
   25           CONTINUE
C
              ELSE
C
C **** ON REVIENT SUR LA SLU2
C
                IF (ITERAT.EQ.1) THEN
                  SI(7) = SIM(7) + K02(1)*DUR
                  SI(11) = SIM(11) + K02(15)*DRYR
                  VARIP(2) = FEQ2**2/(1.D0-FEQ2)/DBAR2

                  UR2 = (VARIP(2)*SI(7)/FEQ2-P2*VARIM(6)/RG2)/NU2
                  TR2 = (VARIP(2)*SI(11)/FEQ2-P2*VARIM(7)/RG2)/MU2
                  U2 = UR2*DXU2
                  T2 = TR2*DRYU2
                  UTOT = U2+UI+(VARIM(6)-SIM(7))/K02(1)
                  TTOT = T2+TI+(VARIM(7)-SIM(11))/K02(15)
C
                  IF (DUR.NE.0.D0)  DNSDU2 = SI(7)/UTOT
                  IF (DUR.EQ.0.D0)  DNSDU2 = K02(1)
                  IF (DRYR.NE.0.D0) DMSDT2 = SI(11)/TTOT
                  IF (DRYR.EQ.0.D0) DMSDT2 = K02(15)
                  DNSDT2 = 0.D0
                  VARIP(4) = UTOT - SI(7)/K02(1)
                  VARIP(5) = TTOT - SI(11)/K02(15)
                  VARIP(6) = SI(7)
                  VARIP(7) = SI(11)
                  SI(7) = DNSDU2*UU
                  SI(11) = DMSDT2*TT
C
                  CALL U2MESS('I','ELEMENTS_27')
                  VARIP(3) = 2.0D0
                  CALL DICOR3 (K02,DUR,DRYR,SIM,SI,DNSDU,DMSDT,DNSDT)
                ELSE

                  U2 = DUR + P2*VARIM(6)*DXU2/RG2/NU2
     &                     - (VARIM(6)-SIM(7))/K02(1)
                  T2 = DRYR + P2*VARIM(7)*DRYU2/RG2/MU2
     &                     - (VARIM(7)-SIM(11))/K02(15)
                  VARIP(2) = SQRT ( (U2/DXU2)**2 + (T2/DRYU2)**2 )
                  G2 = DBAR2*VARIP(2)
                  FEQ2 = 0.5D0*(-G2+SQRT(G2**2 + 4.D0*G2))
                  DNSDU2 = FEQ2*NU2/DXU2/VARIP(2)
                  IF (DUR.EQ.0.D0) DNSDU2 = K02(1)
                  DMSDT2 = FEQ2*MU2/DRYU2/VARIP(2)
                  IF (DRYR.EQ.0.D0) DMSDT2 = K02(15)
                  DNSDT2 = 0.D0
                  SI(7) = U2*FEQ2*NU2/DXU2/VARIP(2)
                  SI(11) = T2*FEQ2*MU2/DRYU2/VARIP(2)
                  CALL U2MESS('I','ELEMENTS_27')
                  CALL DICOR3 (K02,DUR,DRYR,SIM,SI,DNSDU,DMSDT,DNSDT)
                  VARIP(3) = 2.D0
                  VARIP(4) = UU - SI(7)/K02(1)
                  VARIP(5) = TT - SI(11)/K02(15)
                  VARIP(6) = SI(7)
                  VARIP(7) = SI(11)
                ENDIF
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
C                             RIGI_MECA_TANG
C ======================================================================
C
         IF (VARIM(1).LE.1.D0.OR.VARIM(3).EQ.1.D0) THEN
               CALL DICOR0 (K01,VARIM(1),VARIP(1),PLOUF,
     &                      DNSDU,DMSDT,DNSDT)
               CALL DICOR0 (K01,VARIM(2),VARIP(2),VARIP(3),
     &                      DNSDU2,DMSDT2,DNSDT2)
               VARIP(3) = VARIM(3)
               P1 = VARIM(1)
               G1 = DBAR1*P1
               RG1 = 0.5D0*(-G1+SQRT(G1**2 + 4.D0*G1))
               IF (P1.NE.0.D0) DNSDU2 = RG1*NU1/DXU1/P1
               IF (P1.NE.0.D0) DMSDT2 = RG1*MU1/DRYU1/P1
         ELSE
               CALL DICOR0 (K02,VARIM(1),VARIP(1),PLOUF,
     &                      DNSDU,DMSDT,DNSDT)
               CALL DICOR0 (K02,VARIM(2),VARIP(2),VARIP(3),
     &                      DNSDU2,DMSDT2,DNSDT2)
               VARIP(3) = VARIM(3)
               P2 = VARIM(2)
               G2 = DBAR2*P2
               RG2 = 0.5D0*(-G2+SQRT(G2**2 + 4.D0*G2))
               IF (VARIM(3).EQ.2.D0) DNSDU2 = RG2*NU2/DXU2/P2
               IF (VARIM(3).EQ.2.D0) DMSDT2 = RG2*MU2/DRYU2/P2
         ENDIF
         DO 30 I = 4, 7
            VARIP(I) = VARIM(I)
   30    CONTINUE
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
      END
