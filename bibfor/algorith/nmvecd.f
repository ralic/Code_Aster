       SUBROUTINE NMVECD (IMATE, MATE, NMAT, MATCST, HOOK, DT, TP, 
     &                    P, NP, BETA, NB, EP, RM, DM,
     &                    DSGDE, DSGDB, DSGDP, DRBDE, DRPDE, 
     &                    RB, RP, DRBDB, DRBDP, DRPDB, DRPDP, 
     &                    ETATF, IER)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/04/2004   AUTEUR DURAND C.DURAND 
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
C TOLE CRP_21
C-----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER       IMATE, NMAT, NP , NB, IER  
      REAL*8        MATE(NMAT,2), HOOK(6,6)
      REAL*8        P(NP), BETA(NB), EP(*), RM, DM
      REAL*8        DSGDE(NB,NB), DSGDB(NB,NB)
      REAL*8        DSGDP(NB,NP), RB(NB), RP(NP), DRBDB(NB,NB)
      REAL*8        DRBDP(NB,NP), DRPDB(NP,NB), DRPDP(NP,NP)
      REAL*8        DT, TP, DRBDE(NB,NB), DRPDE(NP,NB)
      CHARACTER*3   MATCST
      CHARACTER*7   ETATF(3)
C-----------------------------------------------------------------------
C     INTEGRATION DE LA LOI DE COMPORTEMENT VISCO PLASTIQUE DE
C     CHABOCHE AVEC ENDOMAGEMENT
C     METHODE ITERATIVE D'EULER IMPLICITE
C
C     EQUATIONS ET DERIVEES DES RESIDUS: RB ET RP
C-----------------------------------------------------------------------
C-- ARGUMENTS
C------------
C
C IN   MATE    : PARAMETRE MATERIAU A L'INSTANT T
C      IMATE   : ADRESSE DU MATERIAU CODE
C      NMAT    : DIMENSION DE MATE
C      MATCST  : 'OUI' SI MATERIAU CST ENTRE T- ET T
C                'NAP' SI LE PARAMETRE K_D EST UNE NAPPE
C                'NON' SINON
C      HOOK    : OPERATEUR DE HOOK
C      DT      : INCREMENT DE TEMPS
C      TP      : TEMPERATURE A T+
C      P       : INCONNUES ASSOCIEES AUX VARIABLES D'ETAT
C      NP      : NOMBRE D'INCONNUES ASSOCIEES AUX VARIABLES D'ETAT
C      BETA    : INCONNUES ASSOCIEES AUX CONTRAINTES
C      NB      : NOMBRE D'INCONNUES ASSOCIEES AUX CONTRAINTES
C      RM      : VARIABLES INTERNES A T-
C      DM      : VARIABLES INTERNES A T-
C      EP      : DEFORMATIONS TOTALES ET THERMIQUE A T ET 
C                VISCOPLASTIQUE A T-      
C OUT  DSGDE   : DERIVEES DE LA FONCTION ASSOCIEE PAR E
C      DSGDB   : DERIVEES DE LA FONCTION ASSOCIEE PAR BETA
C      DSGDP   : DERIVEES DE LA FONCTION ASSOCIEE PAR P
C      RB      : RESIDU ASSOCIEE AU VECTEUR CONTRAINTES
C      RP      : RESIDU ASSOCIEE AUX INCONNUS
C      DRBDB   : DERIVEES DE LA FONCTION ASSOCIEE PAR BETA
C      DRBDP   : DERIVEES DE LA FONCTION ASSOCIEE PAR P
C      DRPDB   : DERIVEES DE LA FONCTION ASSOCIEE PAR BETA
C      DRPDP   : DERIVEES DE LA FONCTION ASSOCIEE PAR P
CIN/OUT ETATF   : ETAT MECANIQUE DE L'ELEMENT
C      IER     : CODE DE RETOUR D'ERREUR
C                0=OK
C                1=NOOK
C
C INFO P(1)=RPOINT,  P(2)=DPOINT
C-----------------------------------------------------------------------
      INTEGER     K, J, I
      REAL*8      DAMMAX, EPSI, R8PREM
      PARAMETER  (DAMMAX = 0.99D0)
C
      REAL*8     R8GAEM, DKRON(6,6), SC, DSCDR, UNSSC, ARG
      REAL*8     CA1, CA0, DRPDSC, DRPDSE, UNSSEM, EC, UNMD, EA
      REAL*8     SY, UNSM, GK, GR, GA, EPSVPM(6), EPST(6),EPSTH(6)
      REAL*8     DSEDB(6), DSEDB2(6,6), SE, D, R, GN, SEMSY, GM, CRIT
      REAL*8     DHEDE(6,6), DHEDB(6,6), DHEDP(6,2), XHI, P2, EPSVP(6)
      REAL*8     P2DXHI, P2DP2, HE(6), DXHIDB(6), KXHI, DKXIDX
C
      DATA DKRON /1.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0,
     &            0.D0, 1.D0, 0.D0, 0.D0, 0.D0, 0.D0,
     &            0.D0, 0.D0, 1.D0, 0.D0, 0.D0, 0.D0,
     &            0.D0, 0.D0, 0.D0, 1.D0, 0.D0, 0.D0,
     &            0.D0, 0.D0, 0.D0, 0.D0, 1.D0, 0.D0,
     &            0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 1.D0/
C-----------------------------------------------------------------------
C-- 1. INITIALISATIONS
C   ===================
      EPSI   = SQRT( R8PREM() )
C     EPSI   = 1.D-15
      IER = 0
      SY   = MATE(1,2)
      GN   = MATE(4,2)
      IF (MATE(4,2).LE.0.D0) GOTO 05001
      GM   = MATE(5,2)
      IF (MATE(5,2).LE.0.D0) GOTO 05001
      UNSM = 1.D0/MATE(5,2)
      GK   = MATE(6,2)
      GR   = MATE(7,2)
      GA   = MATE(8,2)
      SE = 0.D0
      CALL R8INIR (NB,0.D0,DSEDB,1)
      CALL R8INIR (NB*NB,0.D0,DSEDB2,1)
      CALL R8INIR (NP*NP,0.D0,DRPDP,1)
      CALL R8INIR (NP*NB,0.D0,DRPDB,1)
      CALL R8INIR (NB*NB,0.D0,DRBDB,1)
      CALL R8INIR (NB*NP,0.D0,DRBDP,1)
      CALL R8INIR (NB,0.D0,RB,1)
      CALL R8INIR (NP,0.D0,RP,1)
      CALL LCEQVN (NB,EP(1),EPSTH)
      CALL LCEQVN (NB,EP(7),EPSVPM)
      CALL LCEQVN (NB,EP(13),EPST)
C
C-- 1.1. CRITERE DE VM, DERIVEE PREMIERE ET SECONDE
C   ----------------------------------------------
      CALL LCDVMI (BETA, 0.D0, CRIT, DSEDB, DSEDB2, SE)
C
C-- 1.2. ETAT COURANT DU SYSTEME
C   ----------------------------
      D = DM+DT*P(2)
      IF (D.GE.DAMMAX) THEN
        P(2) = 0.D0
        D = DAMMAX
        ETATF(3)='DAMMAXO'
      ENDIF
      UNMD = 1.D0-D
      R = RM+DT*P(1)
      DO 00121 I=1,NB
        EPSVP(I)=EPSVPM(I)+DT*P(1)/UNMD*DSEDB(I)
00121 CONTINUE
C
C-- 1.3. CONTRAINTES ET DERIVEES
C   ----------------------------
      DO 00131 I=1,NB
        HE(I)=0.D0
        DO 00131 K=1,NB
          HE(I)=HE(I)+UNMD*HOOK(I,K)*(EPST(K)-EPSTH(K)-EPSVP(K))
00131 CONTINUE 
C
      DO 00132 I=1,NB
        DO 01321 J=1,NB
          DHEDE(I,J)=0.D0
          DHEDE(I,J)=DHEDE(I,J)+UNMD*HOOK(I,J)
01321   CONTINUE 
C
        DO 01322 J=1,NB
          DHEDB(I,J)=0.D0
          DO 01322 K=1,NB
            DHEDB(I,J)=DHEDB(I,J)-HOOK(I,K)*DT*P(1)*DSEDB2(K,J)
01322   CONTINUE 
C
        DHEDP(I,1)=0.D0
        DO 01323 K=1,NB
          DHEDP(I,1)=DHEDP(I,1)-HOOK(I,K)*DT*DSEDB(K)
01323   CONTINUE 
C
        DHEDP(I,2)=0.D0
        DO 01324 K=1,NB
          DHEDP(I,2)=DHEDP(I,2)-DT*HOOK(I,K)*(EPST(K)-EPSTH(K)-
     &               EPSVP(K))
01324   CONTINUE 
C
        DSGDP(I,1)= 0.D0
        DSGDP(I,2)= 0.D0
        DO 01325 J=1,NB
          DSGDE(I,J)= 0.D0
          DSGDB(I,J)= DKRON(I,J)
01325   CONTINUE 
C
00132 CONTINUE
C
C-- 2. EQUATIONS EN BETA ET DERIVEES
C   ================================
      DO 00021 I=1,NB
        RB(I)=BETA(I)-HE(I)
        DO 00211 J=1,NB
          DRBDE(I,J)=-DHEDE(I,J)
          DRBDB(I,J)=-DHEDB(I,J)+DKRON(I,J)
00211   CONTINUE
        DRBDP(I,1)=-DHEDP(I,1)
        DRBDP(I,2)=-DHEDP(I,2)
00021 CONTINUE
C
C-- 3. EQUATIONS EN P(1) ET DERIVEES
C   ================================
C--  3.1. SI ELASTIQUE
C    -----------------
      IF (ETATF(1).EQ.'ELASTIC' .AND. SE.LE.(SY*UNMD)) THEN
       RP(1)=P(1)-0.D0
        DO 00311 J=1,6
          DRPDE(1,J)=0.D0
          DRPDB(1,J)=0.D0
00311   CONTINUE
        DRPDP(1,1)=1.D0
        DRPDP(1,2)=0.D0
C
C--  3.2. SI PLASTIQUE
C    -----------------
      ELSE
        SEMSY = SE/UNMD-SY
        IF (R.LE.EPSI) THEN
           SC = GK*EPSI**UNSM
           DSCDR = 0.D0
        ELSE
           SC = GK*EXP(LOG(R)*UNSM)
           DSCDR = SC/R*UNSM*DT
        ENDIF
        IF (R.LE.1.D-5.AND.P(1).LE.0.D0) THEN
          GN = GN*GM/(GN+GM)
          SC = GK*DT**UNSM
        ENDIF
        IF (SC.LE.EPSI) THEN
          UNSSC = 1.D0/EPSI
        ELSE
          UNSSC = 1.D0/SC
        ENDIF
        IF (SEMSY.LE.EPSI) THEN
          UNSSEM = 1.D0/EPSI
        ELSE
          UNSSEM = 1.D0/SEMSY
        ENDIF
        ARG = GN*(LOG(SEMSY)-LOG(SC))
        IF (ARG.GT.LOG(0.1D0/DT)) THEN
          CA1 = -GN*(0.1D0/DT)**((GN-1.D0)/GN)
          CA0 = (GN-1.D0)*(0.1D0/DT)
          RP(1)= P(1)+ CA1*SEMSY*UNSSC + CA0
          DRPDSC = -CA1*SEMSY*UNSSC*UNSSC
          DRPDSE =  CA1*UNSSC
          ETATF(2)='TANGENT'
        ELSE
          RP(1)= P(1) - EXP(ARG)
          DRPDSC = EXP(ARG)*GN*UNSSC
          DRPDSE = - GN*UNSSEM*EXP(ARG)
          ETATF(2)='EXPONEN'
        ENDIF
        DO 00321 J=1,NB
          DRPDE(1,J)= 0.D0
          DRPDB(1,J)= DRPDSE/UNMD*DSEDB(J)
00321   CONTINUE
        DRPDP(1,1)= 1.D0 + DRPDSC*DSCDR
        DRPDP(1,2)= DRPDSE*SE*DT/UNMD/UNMD
        IF (ETATF(1).EQ.'ELASTIC') ETATF(1)='PLASTIC'
      ENDIF
C
C-- 4. EQUATIONS EN P(2) ET DERIVEES
C   ================================
      CALL NMVEXI (BETA, SE, DSEDB, NB, MATE, NMAT, XHI, DXHIDB)
      KXHI = MATE(9,2)
      DKXIDX = 0.D0
      IF (XHI.LE.EPSI .OR. D.EQ.DAMMAX) THEN
        P2    =0.D0
        P2DP2 =0.D0
        P2DXHI=0.D0
      ELSE
        IF (MATCST.EQ.'NAP') THEN
          CALL NMVEKX (IMATE, TP, XHI, KXHI, DKXIDX)
          MATE(9,2) = KXHI
        ENDIF
C
        EA = -KXHI*LOG(UNMD)
        EC = GR*LOG(XHI/GA)
        P2     = EXP(EC)*EXP(EA)
        RP(2)  = P(2)-P2
        P2DP2 =  P2*KXHI*DT/(1.D0-D)
        P2DXHI = P2*(GR/XHI-LOG(1.D0-D)*DKXIDX)
      ENDIF
C
      DO 00422 J=1,NB
        DRPDE(2,J)=0.D0
        DRPDB(2,J)=-P2DXHI*DXHIDB(J)
00422 CONTINUE
      DRPDP(2,1)=0.D0
      DRPDP(2,2)=1.D0-P2DP2
C
      GOTO 09999
C
C-- 5. ERREURS
C   ----------
05001 CONTINUE
      IER = 1
C
09999 CONTINUE
      END
