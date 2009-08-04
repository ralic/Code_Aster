      SUBROUTINE NMCHAT(MATEL,MAT,NBVAR,MEMO,VISC,PLAST,SIGMDV,DEPSDV,
     &                  PM,DP,NDIMSI,DT,RPVP,QP,VIM,DSIDEP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/08/2009   AUTEUR MEUNIER S.MEUNIER 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE PROIX J-M.PROIX
C.======================================================================
      IMPLICIT NONE
C ----ARGUMENTS
      INTEGER   MEMO,VISC,NBVAR
      REAL*8    MAT(*),MATEL(*),SIGMDV(6),DSIDEP(6,6),PLAST,DP,PM,RPVP
      REAL*8    DEPSDV(6),VIM(*),DT,QP
C ----VARIABLES LOCALES
      REAL*8    E,NU,DEUXMU,SIGEDV(6),DSIDE(6,6),S(6),PDEV(6,6),RPVM
      REAL*8    R8PREM,ALFAM(6),ALFA2M(6),RAC2,MU,TROISK,R0,RINF,B,CINF
      REAL*8    K,W,GAMMA0,AINF,C2INF,GAMM20,VALDEN,KVI,PP,RP,CP,GAMMAP
      REAL*8    MP,C2P,GAMM2P,M2P,VP,CORR,DVP,DENOMI,DGAMAP,DCP,DMP
      REAL*8    DGAM2P,DC2P,DM2P,AP,EP,E2P,BP,B2P,DDENOM,IP,DAP,DEP,DE2P
      REAL*8    DBP,DB2P,SEQ,L1P,L2P,L22P,L3P,HP,H2P,ISP,IAP,IA2P,H1S
      REAL*8    H1A1,H1A2,H2A1,H2A2,GQ0,GQMAX,MUMEM,QM,GQP,DRP,H2S,RPM
      LOGICAL   PLASTI
      INTEGER   NDIMSI,I,J,L

      RAC2   = SQRT(2.D0)
      PLASTI=(PLAST.GE.0.5D0)
      E      = MATEL(3)
      NU     = MATEL(4)
      DEUXMU = E/(1.D0+NU)
      MU = DEUXMU / 2.D0
      TROISK = E/(1.D0-2.D0*NU)
      R0     = MAT(1)
      RINF   = MAT(2)
      B      = MAT(3)
      CINF   = MAT(4)
      K      = MAT(5)
      W      = MAT(6)
      GAMMA0 = MAT(7)
      AINF   = MAT(8)
      C2INF  = MAT(9)
      GAMM20 = MAT(10)
      IF (VISC.EQ.1) THEN
         VALDEN= MAT(11)
         KVI   = MAT(12)
      ENDIF

      DO 140 I=1,NDIMSI
         SIGEDV(I) = SIGMDV(I) + DEUXMU * DEPSDV(I)
 140  CONTINUE
C
C --- MISE AU FORMAT DES CONTRAINTES DE RAPPEL :
C     ========================================
      DO 10 I=1,3
        ALFAM(I) = VIM(I+2)
        IF (NBVAR.EQ.2) THEN
          ALFA2M(I) = VIM(I+8)
        ELSE
          ALFA2M(I)=0.D0
        ENDIF
  10  CONTINUE

      DO 20 I=4,NDIMSI
        ALFAM(I) = VIM(I+2)*RAC2
        IF (NBVAR.EQ.2) THEN
           ALFA2M(I) = VIM(I+8)*RAC2
        ELSE
          ALFA2M(I)=0.D0
        ENDIF
  20  CONTINUE
      IF (MEMO.EQ.0) THEN
         RPM    = RINF + (R0-RINF)*EXP(-B*PM)
      ELSEIF (MEMO.EQ.1) THEN
         RPVM   = VIM(15)
         RPM    = RPVM + R0
         QM     = VIM(16)
      ENDIF

C
C --- NITIALISATION :
C     -------------
      CALL MATINI(6,6,0.D0,DSIDE)
      CALL MATINI(6,6,0.D0,DSIDEP)
C
C --- PARTIE ELASTIQUE DE LA MATRICE TANGENTE :
C     ---------------------------------------
      DO 170 I=1, NDIMSI
        DSIDEP(I,I) = DEUXMU
 170  CONTINUE
C
      DO 180 I=1, 3
         DO 190 J=1, 3
            DSIDEP(I,J) = DSIDEP(I,J) + TROISK/3.D0 - DEUXMU/3.D0
 190     CONTINUE
 180  CONTINUE
C
C --- PARTIE PLASTIQUE DE LA MATRICE TANGENTE :
C        =======================================
C ---   CALCUL DES DERIVEES PAR RAPPORT A LA DEFORMATION PLASTIQUE
C ---   CUMULEE DES CARACTERISTIQUES D'ECROUISSAGE DU MATERIAU :
C       ------------------------------------------------------
      IF (PLASTI) THEN
C
C ---     VISCOSIFICATION: SI DP=0 (RIGI_MECA_TANG), ON IMPOSE
C ---     DP = EPSILON > 0 POUR DERIVER LE TERME (DP/DT)**(1/N)
          IF (DP.EQ.0.D0) DP = R8PREM()
C
          PP     = PM + DP
          IF (MEMO.EQ.0) THEN
              RP     = RINF + (R0-RINF)*EXP(-B*PP)
          ELSEIF (MEMO.EQ.1) THEN
              RP    = RPVP + R0
          ENDIF
          CP     = CINF * (1.D0 + (K-1.D0)*EXP(-W*PP))
          GAMMAP = GAMMA0 * (AINF + (1.D0-AINF)*EXP(-B*PP))
          MP     = CP/(1.D0+GAMMAP*DP)
          C2P     = C2INF * (1.D0 + (K-1.D0)*EXP(-W*PP))
          GAMM2P = GAMM20 * (AINF + (1.D0-AINF)*EXP(-B*PP))
          M2P     = C2P/(1.D0+GAMM2P*DP)
          IF (VISC.EQ.1) THEN
             VP     = KVI*((DP/DT)**(1.D0/VALDEN))
             CORR = DP/DT
             CORR = (CORR**(1.D0/VALDEN))/CORR
             DVP = KVI*CORR/(VALDEN*DT)
          ELSE
             VP=0.D0
             DVP=0.D0
          ENDIF
          DENOMI = RP + (3.D0*MU+MP+M2P)*DP + VP
          DGAMAP = -B*GAMMA0*(1.D0-AINF)*EXP(-B*PP)
          DCP    = -W*CINF*(K-1.D0)*EXP(-W*PP)
          IF (MEMO.EQ.0) THEN
             DRP    = -B*(R0-RINF)*EXP(-B*PP)
          ELSEIF (MEMO.EQ.1) THEN
             GQ0 = MAT(14)
             GQMAX = MAT(15)
             MUMEM=MAT(16)
             GQP=GQMAX+(GQ0-GQMAX)*EXP(-2.D0*MUMEM*QP)
             DRP=(GQP-RPM)/(1.D0+B*DP)-2.D0*MUMEM*(GQ0-GQMAX)*(QP-QM)
             DRP=B*DRP/(1.D0+B*DP)
          ENDIF
          DMP    =  DCP/(1.D0+GAMMAP*DP)
     &            - CP*(DGAMAP*DP+GAMMAP)/
     &                 ((1.D0+GAMMAP*DP)*(1.D0+GAMMAP*DP))
C
          DGAM2P = -B*GAMM20*(1.D0-AINF)*EXP(-B*PP)
          DC2P    = -W*C2INF*(K-1.D0)*EXP(-W*PP)
          DM2P    =  DC2P/(1.D0+GAMM2P*DP)
     &            - C2P*(DGAM2P*DP+GAMM2P)/
     &                 ((1.D0+GAMM2P*DP)*(1.D0+GAMM2P*DP))
C
          AP     =  (RP+VP)/DENOMI
          EP     =  -MP
          E2P    = -M2P
          BP     = - 2.D0/3.D0*MP*(RP+VP)/DENOMI
          B2P     = - 2.D0/3.D0*M2P*(RP+VP)/DENOMI
          DDENOM =  DRP + 3.D0*MU + MP +M2P + (DMP+DM2P)*DP + DVP
          IP     =  1.D0/DENOMI - DDENOM*DP/(DENOMI*DENOMI)
          DAP    = (DRP+DVP)/DENOMI - (RP+VP)*DDENOM/DENOMI/DENOMI
          DEP    =  -DMP
          DE2P=-DM2P
          DBP = -2.D0/3.D0*(DMP*AP+MP*DAP)
          DB2P = -2.D0/3.D0*(DM2P*AP+M2P*DAP)
          SEQ = 0.D0
          DO 200 I = 1, NDIMSI
             S(I) = AP*SIGEDV(I)+BP*ALFAM(I)+B2P*ALFA2M(I)
             SEQ  = SEQ + S(I)*S(I)
 200      CONTINUE
          SEQ = SQRT(1.5D0*SEQ)
          L1P = AP*AP/SEQ
          L2P = AP*BP/SEQ
          L22P = AP*B2P/SEQ
          L3P = 0.D0
          DO 210 I = 1, NDIMSI
            L3P=L3P+(DAP*SIGEDV(I)+DBP*ALFAM(I)+DB2P*ALFA2M(I))*S(I)
 210      CONTINUE
          L3P =  1.5D0/SEQ*L3P
          L3P =  L3P - DRP - DVP
          HP  =  DEP*DP/DENOMI + EP*IP
          H2P  =  DE2P*DP/DENOMI + E2P*IP
          ISP = -1.5D0*IP*L1P/L3P
          IAP = -1.5D0*IP*L2P/L3P
          IA2P = -1.5D0*IP*L22P/L3P
          H1S = -1.5D0*HP*L1P/L3P
          H1A1 = -1.5D0*HP*L2P/L3P
          H2S = -1.5D0*H2P*L1P/L3P
          H1A2 = -1.5D0*HP*L22P/L3P
          H2A1 = -1.5D0*H2P*L2P/L3P
          H2A2 = -1.5D0*H2P*L22P/L3P
C
          DO 220 I=1, NDIMSI
            DSIDE(I,I) = -6.D0*MU*MU*DP/DENOMI
            DO 230 J=1, NDIMSI
              DSIDE(I,J) = DSIDE(I,J)
     &                       - 6.D0*MU*MU*ISP    * SIGEDV(I)*SIGEDV(J)
     &                       - 4.D0*MU*MU*H1A1* ALFAM(I)*ALFAM(J)
     &                       - 4.D0*MU*MU*H1A2* ALFA2M(I)*ALFAM(J)
     &                       - 4.D0*MU*MU*H2A1* ALFAM(I)*ALFA2M(J)
     &                       - 4.D0*MU*MU*H2A2* ALFA2M(I)*ALFA2M(J)
     &                       - 6.D0*MU*MU*IAP    * ALFAM(I)*SIGEDV(J)
     &                       - 6.D0*MU*MU*IA2P    * ALFA2M(I)*SIGEDV(J)
     &                       - 4.D0*MU*MU*H1S * SIGEDV(I)*ALFAM(J)
     &                       - 4.D0*MU*MU*H2S * SIGEDV(I)*ALFA2M(J)
 230        CONTINUE
 220      CONTINUE
C
C ---   MATRICE DE PROJECTION DEVIATORIQUE :
C       ----------------------------------
          CALL MATINI(6, 6,0.D0,PDEV)

          PDEV(1,1) =  2.D0/3.D0
          PDEV(2,2) =  2.D0/3.D0
          PDEV(3,3) =  2.D0/3.D0
          PDEV(4,4) =  1.D0
          PDEV(5,5) =  1.D0
          PDEV(6,6) =  1.D0
          PDEV(1,2) = -1.D0/3.D0
          PDEV(1,3) = -1.D0/3.D0
          PDEV(2,3) = -1.D0/3.D0
          PDEV(2,1) = -1.D0/3.D0
          PDEV(3,1) = -1.D0/3.D0
          PDEV(3,2) = -1.D0/3.D0
          DO 250 I=1, NDIMSI
            DO 260 J=1, NDIMSI
              DO 270 L=1, NDIMSI
                DSIDEP(I,J) = DSIDEP(I,J) + DSIDE(I,L)*PDEV(L,J)
 270          CONTINUE
 260        CONTINUE
 250      CONTINUE

      ENDIF
      END
