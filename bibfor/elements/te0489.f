      SUBROUTINE TE0489(OPTION,NOMTE)
      IMPLICIT NONE
      CHARACTER*16 OPTION,NOMTE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/10/2011   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C
C     BUT:
C       CALCUL DES INDICATEURS LOCAUX DE DECHARGE
C                 ET DE PERTE DE RADIALITE POUR LES NORMES :
C              VMIS      : SECOND INVARIANT DU TENSEUR DES CONTRAINTES
C                          DEVIATORIQUES
C              TOTAL     : SECOND INVARIANT DU TENSEUR DES CONTRAINTES
C              VMIS_CINE : SECOND INVARIANT DU DEVIATEUR DU TENSEUR
C                          SIGMA - X
C                          OU SIGMA EST LE TENSEUR DES CONTRAINTES
C                          ET X     EST LE TENSEUR DE RAPPEL
C              TOTAL_CINE: SECOND INVARIANT DU TENSEUR SIGMA - X

C    ON NOTE SIGMA2 = SIGMA(M,T+DT)
C            SIGMA1 = SIGMA(M,T)
C            DSIGMA = SIGMA2 - SIGMA1

C    A)LES INDICATEURS LOCAUX DE DECHARGE :
C      I = (NORME(SIGMA2) - NORME(SIGMA1)/NORME(SIGMA2)
C               SONT CALCULES :

C    B)LES INDICATEURS LOCAUX DE PERTE DE RADIALITE :
C      I = 1 - ABS(DSIGMA : SIGMA1)/(NORME(DSIGMA)*NORME(SIGMA1))
C               SONT CALCULES :


C          ELEMENTS ISOPARAMETRIQUES 2D ET 3D

C          OPTIONS : 'DERA_ELGA'

C     ENTREES  ---> OPTION : OPTION DE CALCUL
C              ---> NOMTE  : NOM DU TYPE ELEMENT
C
C ......................................................................
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER MXCMEL
      PARAMETER (MXCMEL=162)
      INTEGER NBPGMX
      PARAMETER (NBPGMX=27)

      INTEGER NBSIGM,JTAB(7)
      INTEGER IRET,NPG1,IPOIDS,IVF,IDFDE,JGANO,ICOMPO
      INTEGER IDERA1, IDERA2
      INTEGER NBCMP, IMATE, IDECAL, ISIG
      INTEGER NNO,NBSIG,NNOS,ISIGTM,ISIGTP,IDVAR1
      INTEGER IDVAR2,NBSIG2,NPG,I,K,NDIM,IGAU,ICODRE
      INTEGER NBVARI, JPROLP,JVALEP, NBVALP, IBID

      REAL*8 SIGMA1(MXCMEL),  SIGMA2(MXCMEL)
      REAL*8 SIGT1(MXCMEL),   SIGT2(MXCMEL)
      REAL*8 DCHAV(NBPGMX),   DCHAT(NBPGMX)
      REAL*8 DCHAX(NBPGMX),   DCHAY(NBPGMX)
      REAL*8 RADIV(NBPGMX),   RADIT(NBPGMX)
      REAL*8 COSANG(MXCMEL)
      REAL*8 XRAPEL(NBPGMX*6), SIGMX(6)
      REAL*8 PM, PP, DP, RP, RP0
      REAL*8 TRX1,TRX2
      REAL*8 X1(MXCMEL),X2(MXCMEL)
      REAL*8 TRSIG1,TRSIG2
      REAL*8 E, DSDE, SIGY, ALFAFA, COCO, UNSURN
      REAL*8 ZERO,UNTIER,CST1,DUM,TP,RESU,RBID
      REAL*8 ZERNOR, R8PREM, NORSIG, DCHAXM
      REAL*8 VALRES(3)

      REAL*8 TRACE, SQRT
      REAL*8 SIGEQN 

      CHARACTER*4  FAMI
      CHARACTER*8  TYPE
      CHARACTER*8  NOMRES(3)
      CHARACTER*16 COMPOR
C
C ----------------------------------------------------------------------
C
C ---- INITIALISATIONS :
C      ---------------
      ZERO   = 0.0D0
      UNTIER = 1.0D0/3.0D0
      ZERNOR = 1000.0D0*R8PREM()
      FAMI   = 'RIGI'
      NBVALP = 0
      IBID = 0

      DO 10 I = 1,MXCMEL
        SIGT1(I) = ZERO
        SIGT2(I) = ZERO
        SIGMA1(I) = ZERO
        SIGMA2(I) = ZERO
        X1(I) = ZERO
        X2(I) = ZERO
        XRAPEL(I) = ZERO
   10 CONTINUE
C
      DO 20 I = 1,NBPGMX
        DCHAV(I) = ZERO
        DCHAT(I) = ZERO
        DCHAX(I) = ZERO
        DCHAY(I) = ZERO
        RADIV(I) = ZERO
        RADIT(I) = ZERO
        COSANG(I) = ZERO
   20 CONTINUE

C ----     DIMENSION DE L'ELEMENT :
C ----     NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT :
      NBSIG = NBSIGM()

C ---- RECUPERATION DU NOMBRE DE COMPOSANTES 
        CALL TECACH('OON','PDERAMG',7,JTAB,IRET)
        IDERA1 = JTAB(1)
        NBCMP  = JTAB(2)/JTAB(3)

C ---- RECUPERATION DE LA CARTE DE COMPORTEMENT :
C      -----------------------------------------------------
      CALL JEVECH('PCOMPOR','L',ICOMPO)
      COMPOR = ZK16(ICOMPO)
C
C ---- RECUPERATION DES CONTRAINTES A L'INSTANT T :
C
      CALL TECACH('OOO','PCONTMR',3,JTAB,IRET)
      NPG = JTAB(3)
      ISIGTM = JTAB(1)
C
C ---- RECUPERATION DES CONTRAINTES A L'INSTANT T+DT :
C
      CALL JEVECH('PCONTPR','L',ISIGTP)

      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,IDFDE,JGANO)
      CALL ASSERT(NPG.EQ.NPG1)
C
C ---- AFFECTATION DES VECTEURS DE TRAVAIL SIGMA1 ET SIGMA2
C ---- REPRESENTANT LES TENSEURS DE CONTRAINTES RESPECTIVEMENT
C ---- AUX INSTANTS T ET T+DT :
C      ----------------------
        K = 0
C
        DO 40 IGAU = 1,NPG
          DO 30 I = 1,NBSIG
            K = K + 1
            SIGT1(I+ (IGAU-1)*NBSIG) = ZR(ISIGTM+K-1)
            SIGT2(I+ (IGAU-1)*NBSIG) = ZR(ISIGTP+K-1)
            SIGMA1(I+ (IGAU-1)*NBSIG) = ZR(ISIGTM+K-1)
            SIGMA2(I+ (IGAU-1)*NBSIG) = ZR(ISIGTP+K-1)
   30     CONTINUE
   40   CONTINUE

      IF (COMPOR(1:14).NE.'VMIS_CINE_LINE') THEN
C
C ---- CAS DE LA NORME VMIS : CALCUL DES DEVIATEURS DES CONTRAINTES
C ---- SIGMA1 ET SIGMA2 :
C      ----------------

        DO 70 IGAU = 1,NPG

          TRSIG1 = SIGMA1(1+ (IGAU-1)*NBSIG) +
     &             SIGMA1(2+ (IGAU-1)*NBSIG) + SIGMA1(3+ (IGAU-1)*NBSIG)
          
          TRSIG2 = SIGMA2(1+ (IGAU-1)*NBSIG) +
     &             SIGMA2(2+ (IGAU-1)*NBSIG) + SIGMA2(3+ (IGAU-1)*NBSIG)
          SIGMA1(1+ (IGAU-1)*NBSIG) = SIGMA1(1+ (IGAU-1)*NBSIG) -
     &                                UNTIER*TRSIG1
          SIGMA1(2+ (IGAU-1)*NBSIG) = SIGMA1(2+ (IGAU-1)*NBSIG) -
     &                                UNTIER*TRSIG1
          SIGMA1(3+ (IGAU-1)*NBSIG) = SIGMA1(3+ (IGAU-1)*NBSIG) -
     &                                UNTIER*TRSIG1

          SIGMA2(1+ (IGAU-1)*NBSIG) = SIGMA2(1+ (IGAU-1)*NBSIG) -
     &                                UNTIER*TRSIG2
          SIGMA2(2+ (IGAU-1)*NBSIG) = SIGMA2(2+ (IGAU-1)*NBSIG) -
     &                                UNTIER*TRSIG2
          SIGMA2(3+ (IGAU-1)*NBSIG) = SIGMA2(3+ (IGAU-1)*NBSIG) -
     &                                UNTIER*TRSIG2
   70   CONTINUE
C
C ---- DANS LE CAS D'UN ECROUISSAGE CINEMATIQUE
C ---- RECUPERATION DES COMPOSANTES DU TENSEUR DE RAPPEL
C ---- AUX INSTANTS T ET T+DT :
C      ----------------------
      ELSE IF (COMPOR(1:14).EQ.'VMIS_CINE_LINE') THEN
        
        CALL JEVECH('PVARIMR','L',IDVAR1)
        CALL JEVECH('PVARIPR','L',IDVAR2)

C ----   AFFECTATION DES VECTEURS DE TRAVAIL X1 ET X2 REPRESENTANT
C ----   LES TENSEURS DE RAPPEL RESPECTIVEMENT AUX INSTANTS T
C ----   ET T+DT :
C        -------
        NBSIG2 = 7

        DO 60 IGAU = 1,NPG
          DO 50 I = 1,NBSIG
            X1(I+ (IGAU-1)*NBSIG) = ZR(IDVAR1+I+ (IGAU-1)*NBSIG2-1)
            X2(I+ (IGAU-1)*NBSIG) = ZR(IDVAR2+I+ (IGAU-1)*NBSIG2-1)
   50     CONTINUE
   60   CONTINUE

C ---- CAS DE LA NORME TOTAL_CINE : CALCUL DES CONTRAINTES
C ---- (SIGMA1-X1) ET (SIGMA2-X2) :
C      -------------------------
C
        K = 0
        DO 100 IGAU = 1,NPG
          DO 90 I = 1,NBSIG
          K = K + 1

            SIGT1(I+ (IGAU-1)*NBSIG) = ZR(ISIGTM+K-1) -
     &                                  X1(I+ (IGAU-1)*NBSIG)
            SIGT2(I+ (IGAU-1)*NBSIG) = ZR(ISIGTP+K-1) -
     &                                  X2(I+ (IGAU-1)*NBSIG)
            SIGMA1(I+ (IGAU-1)*NBSIG) = ZR(ISIGTM+K-1) -
     &                                  X1(I+ (IGAU-1)*NBSIG)
            SIGMA2(I+ (IGAU-1)*NBSIG) = ZR(ISIGTP+K-1) -
     &                                  X2(I+ (IGAU-1)*NBSIG)
   90     CONTINUE
  100   CONTINUE

C ---- CAS DE LA NORME VMIS_CINE : CALCUL DES DEVIATEURS DES CONTRAINTES
C ---- (SIGMA1-X1) ET (SIGMA2-X2) :
C      -------------------------
C
        DO 80 IGAU = 1,NPG

          TRSIG1 = SIGMA1(1+ (IGAU-1)*NBSIG) +
     &             SIGMA1(2+ (IGAU-1)*NBSIG) + SIGMA1(3+ (IGAU-1)*NBSIG)
          
          TRSIG2 = SIGMA2(1+ (IGAU-1)*NBSIG) +
     &             SIGMA2(2+ (IGAU-1)*NBSIG) + SIGMA2(3+ (IGAU-1)*NBSIG)

          TRX1 = X1(1+ (IGAU-1)*NBSIG) + X1(2+ (IGAU-1)*NBSIG) +
     &           X1(3+ (IGAU-1)*NBSIG)

          TRX2 = X2(1+ (IGAU-1)*NBSIG) + X2(2+ (IGAU-1)*NBSIG) +
     &           X2(3+ (IGAU-1)*NBSIG)

          SIGMA1(1+ (IGAU-1)*NBSIG) = SIGMA1(1+ (IGAU-1)*NBSIG) -
     &                                UNTIER* (TRSIG1-TRX1)
          SIGMA1(2+ (IGAU-1)*NBSIG) = SIGMA1(2+ (IGAU-1)*NBSIG) -
     &                                UNTIER* (TRSIG1-TRX1)
          SIGMA1(3+ (IGAU-1)*NBSIG) = SIGMA1(3+ (IGAU-1)*NBSIG) -
     &                                UNTIER* (TRSIG1-TRX1)
          SIGMA1(4+ (IGAU-1)*NBSIG) = SIGMA1(4+ (IGAU-1)*NBSIG)

          SIGMA2(1+ (IGAU-1)*NBSIG) = SIGMA2(1+ (IGAU-1)*NBSIG) -
     &                                UNTIER* (TRSIG2-TRX2)
          SIGMA2(2+ (IGAU-1)*NBSIG) = SIGMA2(2+ (IGAU-1)*NBSIG) -
     &                                UNTIER* (TRSIG2-TRX2)
          SIGMA2(3+ (IGAU-1)*NBSIG) = SIGMA2(3+ (IGAU-1)*NBSIG) -
     &                                UNTIER* (TRSIG2-TRX2)
          SIGMA2(4+ (IGAU-1)*NBSIG) = SIGMA2(4+ (IGAU-1)*NBSIG)

          IF (NDIM.EQ.3) THEN
            SIGMA1(5+ (IGAU-1)*NBSIG) = SIGMA1(5+ (IGAU-1)*NBSIG)
            SIGMA1(6+ (IGAU-1)*NBSIG) = SIGMA1(6+ (IGAU-1)*NBSIG)

            SIGMA2(5+ (IGAU-1)*NBSIG) = SIGMA2(5+ (IGAU-1)*NBSIG)
            SIGMA2(6+ (IGAU-1)*NBSIG) = SIGMA2(6+ (IGAU-1)*NBSIG)
          END IF
   80   CONTINUE

      END IF

C ---- CALCUL DE L'INDICATEUR LOCAL DE DECHARGE DCHA :
C ----  I = (NORME(SIGMA2) - NORME(SIGMA1))/NORME(SIGMA2) :
C      --------------------------------------------------
      CALL DCHAPG(SIGT1 ,SIGT2 ,NPG,NBSIG,DCHAT)
      CALL DCHAPG(SIGMA1,SIGMA2,NPG,NBSIG,DCHAV)

C ---- CALCUL DE L'INDICATEUR LOCAL DE PERTE DE RADIALITE RADI:
C ----  I = 1- ABS(SIGMA1:DSIGMA)/(NORME(SIGMA1)*NORME(DSIGMA) :
C      -------------------------------------------------------
      CALL RADIPG(SIGT1 ,SIGT2 ,NPG,NBSIG,RADIT,COSANG)
      CALL RADIPG(SIGMA1,SIGMA2,NPG,NBSIG,RADIV,COSANG)
C
C ---- CALCUL DE L'INDICATEUR LOCAL DCHA_X et DCHA_Y
C ---- CALCUL DU TENSEUR DE RAPPEL X
C      -------------------------------------------------------
C
C --- ON NE TRAITE QUE LES LOIS SUIVANTES
C        - VMIS_ISOT_LINE
C        - VMIS_ISOT_TRAC
C        - VMIS_ISOT_PUIS

      IF (COMPOR(1:9).EQ.'VMIS_ISOT') THEN

C - ON RECUPERE 
C      > LES INDICATEURS A L'INSTANT T-DT 
C      > LES VARIABLES INTERNES AUX INSTANTS T et T+DT
C      > LES PARAMETERS MATERIAUX A INSTANT T

        READ(ZK16(ICOMPO-1+2),'(I16)')NBVARI
        CALL JEVECH('PVARIMR','L',IDVAR1)
        CALL JEVECH('PVARIPR','L',IDVAR2)
        CALL JEVECH('PMATERC','L',IMATE)
C
C --- BOUCLE SUR LES POINTS DE GAUSS
C
        DO 200 IGAU = 1,NPG
          IDECAL = (IGAU-1)*6        
C --- DEFORMATION PLASTIQUE CUMULEE
          PM = ZR(IDVAR1-1+(IGAU-1)*NBVARI+1)
          PP = ZR(IDVAR2-1+(IGAU-1)*NBVARI+1)
          DP = PP - PM
          DCHAXM = ZR(IDERA1-1+(IGAU-1)*NBCMP+3)
C
          IF(PM.LE.ZERNOR) THEN
            DCHAX(IGAU) = 1.D0
          ELSE IF (ABS(DCHAXM+2.D0).GT.ZERNOR) THEN
            IF(DP.GT.ZERNOR) THEN
              IF(COSANG(IGAU).GT.ZERNOR) THEN
                DCHAX(IGAU) =  2.D0 
              ELSE
                DCHAX(IGAU) = -2.D0
              ENDIF
            ELSE IF(DP.LE.ZERNOR) THEN
              IF(ABS(DCHAXM+1.D0).GT.ZERNOR) THEN
C --- RECUPERATION DES CARACTERISTIQUES DE LA LOI DE COMPORTEMENT     
                IF (COMPOR.EQ.'VMIS_ISOT_LINE') THEN
                  NOMRES(1) = 'D_SIGM_EPSI'
                  NOMRES(2) = 'SY' 
                  CALL RCVALB(FAMI,IGAU,1,'+',ZI(IMATE),' ','ECRO_LINE',
     &                      IBID,' ',0.D0,2,NOMRES,VALRES,ICODRE, 2)
                  DSDE  = VALRES(1)
                  RP0   = VALRES(2)
C
                  NOMRES(1) = 'E'
                  CALL RCVALB(FAMI,IGAU,1,'+',ZI(IMATE),' ','ELAS',
     &                     IBID,' ',0.D0,1,NOMRES,VALRES,ICODRE, 2)
                  E     = VALRES(1)
                  RP    = DSDE*E/(E-DSDE)*PM+RP0

                ELSEIF (COMPOR(10:14) .EQ. '_PUIS') THEN
                  NOMRES(1) = 'E'
                  CALL RCVALB(FAMI,IGAU,1,'+',ZI(IMATE),' ','ELAS',
     &                      IBID,' ',0.D0,1,NOMRES,VALRES,ICODRE, 2)
                  E     = VALRES(1)

                  NOMRES(1)='SY'
                  NOMRES(2)='A_PUIS'
                  NOMRES(3)='N_PUIS'
                  CALL RCVALB(FAMI,IGAU,1,'+',ZI(IMATE),' ','ECRO_PUIS',
     &                         IBID,' ',0.D0,3,NOMRES,VALRES,ICODRE, 2)
                  SIGY   = VALRES(1)
                  ALFAFA = VALRES(2)
                  COCO   = E/ALFAFA/SIGY
                  UNSURN = 1.D0/VALRES(3)
                  RP     = SIGY * (COCO*PP)**UNSURN + SIGY

                ELSEIF (COMPOR(10:14) .EQ. '_TRAC') THEN
                  CALL RCVARC(' ','TEMP','-',FAMI,IGAU,1,TP,IRET)
                  CALL RCTYPE(ZI(IMATE),1,'TEMP',TP,RESU,TYPE)
                  IF (TYPE.EQ.'TEMP') CALL U2MESS('F','CALCULEL_31')
                  CALL RCTRAC(ZI(IMATE),1,'SIGM',RESU,JPROLP,JVALEP,
     &                        NBVALP,E)
                  CALL RCFONC('S',1,JPROLP,JVALEP,NBVALP,RP0,DUM,
     &                        DUM,DUM,DUM,DUM,DUM,DUM,DUM)
                  CALL RCFONC('V',1,JPROLP,JVALEP,NBVALP,RBID,RBID,
     &                        RBID,PM,RP,RBID,RBID,RBID,RBID)
                ELSE
                  CALL U2MESS('F','ELEMENTS_32')
                ENDIF
C --- CALCUL DE X                                      
                CST1 = (RP-RP0)/RP
                DO 210 ISIG = 1, NBSIG
                   XRAPEL(IDECAL+ISIG)=SIGT1(ISIG+(IGAU-1)*NBSIG)*CST1
210             CONTINUE
C
                DO 220 ISIG = 1, NBSIG
                  SIGMX(ISIG) = SIGT1(ISIG+(IGAU-1)*NBSIG)
     &                          - XRAPEL(IDECAL+ISIG)
220             CONTINUE
C
                TRACE=(SIGMX(1)+SIGMX(2)+SIGMX(3))/3.D0
                SIGMX(1) = SIGMX(1)-TRACE
                SIGMX(2) = SIGMX(2)-TRACE
                SIGMX(3) = SIGMX(3)-TRACE
                SIGEQN = SQRT(1.5D0)*NORSIG(SIGMX,NBSIG)
C
                IF(ABS(SIGEQN-RP0).LE.ZERNOR) THEN
                  DCHAX(IGAU) =  -1.D0
                ELSE
                  DCHAX(IGAU) =  -2.D0
                  DCHAY(IGAU) =  NORSIG(SIGMX,NBSIG)/RP0
                ENDIF
              ELSE IF(ABS(DCHAXM+1.D0).LE.ZERNOR) THEN                
                DO 230 ISIG = 1, NBSIG
                  SIGMX(ISIG) = SIGT1(ISIG)-
     &                          ZR(IDERA1-1+(IGAU-1)*NBCMP+2+ISIG)
230             CONTINUE
                TRACE=(SIGMX(1)+SIGMX(2)+SIGMX(3))/3.D0
                SIGMX(1) = SIGMX(1)-TRACE
                SIGMX(2) = SIGMX(2)-TRACE
                SIGMX(3) = SIGMX(3)-TRACE
C
                SIGEQN = SQRT(1.5D0)*NORSIG(SIGMX,NBSIG)
                IF (COMPOR.EQ.'VMIS_ISOT_LINE') THEN
                  NOMRES(1) = 'SY'
                  CALL RCVALB(FAMI,IGAU,1,'+',ZI(IMATE),' ',
     &            'ECRO_LINE',0,' ',0.D0,1,NOMRES,VALRES,ICODRE, 2)
                  RP0   = VALRES(1)
                ELSEIF (COMPOR(10:14) .EQ. '_PUIS') THEN
                  NOMRES(1)='SY'
                  CALL RCVALB(FAMI,IGAU,1,'+',ZI(IMATE),' ',
     &            'ECRO_PUIS',0,' ',0.D0,1,NOMRES,VALRES,ICODRE, 2)
                  RP0   = VALRES(1)
                ELSEIF (COMPOR(10:14) .EQ. '_TRAC') THEN
                  CALL RCVARC(' ','TEMP','-',FAMI,IGAU,1,TP,IRET)
                  CALL RCTYPE(ZI(IMATE),1,'TEMP',TP,RESU,TYPE)
                  IF (TYPE.EQ.'TEMP') CALL U2MESS('F','CALCULEL_31')
                  CALL RCTRAC(ZI(IMATE),1,'SIGM',RESU,JPROLP,JVALEP,
     &                        NBVALP,E)
                  CALL RCFONC('S',1,JPROLP,JVALEP,NBVALP,RP0,DUM,
     &                        DUM,DUM,DUM,DUM,DUM,DUM,DUM)
                  CALL RCFONC('V',1,JPROLP,JVALEP,NBVALP,RBID,RBID,
     &                        RBID,PM,RP,RBID,RBID,RBID,RBID)
                ELSE
                  CALL U2MESS('F','ELEMENTS_32')
                ENDIF
                IF(SIGEQN.LE.RP0) THEN 
                  DCHAX(IGAU) = -1.D0
                ELSE
                  DCHAX(IGAU) = -2.D0
                  DCHAY(IGAU) = NORSIG(SIGMX,NBSIG)/RP0
                  DO 240 ISIG = 1, NBSIG
                    XRAPEL((IGAU-1)*6+ISIG)=ZR(IDERA1+(IGAU-1)*NBCMP
     &                                      +3+ISIG)
240              CONTINUE
                ENDIF
              ENDIF
            ENDIF
          ELSE
C  DCHA_X = -2 AU PAS PRECEDENT T-DT
C        ON STOCKE DANS LE PAS COURANT T, LES VALEURS OBTENUES 
C        AU PAS PRECEDENT (T-DT)
            DCHAX(IGAU) = -2.D0
            DCHAY(IGAU) = ZR(IDERA1+(IGAU-1)*NBCMP-1+4)
            DO 250 ISIG = 1, NBSIG
              XRAPEL((IGAU-1)*6+ISIG)=ZR(IDERA1+(IGAU-1)*NBCMP+3+ISIG)
250         CONTINUE
          ENDIF            
200     CONTINUE
        ENDIF         
C
C ---- RECUPERATION ET AFFECTATION DU VECTEUR EN SORTIE
C ---- AVEC LE VECTEUR DES INDICATEURS LOCAUX :
C      --------------------------------------
      CALL JEVECH('PDERAPG','E',IDERA2)
      DO 170 IGAU = 1,NPG
        ZR(IDERA2+(IGAU-1)*NBCMP-1+1)   = DCHAV(IGAU)
        ZR(IDERA2+(IGAU-1)*NBCMP-1+2)   = DCHAT(IGAU)
        ZR(IDERA2+(IGAU-1)*NBCMP-1+3)   = DCHAX(IGAU)
        ZR(IDERA2+(IGAU-1)*NBCMP-1+4)   = DCHAY(IGAU)
        DO 260 ISIG = 1, NBSIG
           ZR(IDERA2+(IGAU-1)*NBCMP+3+ISIG)=XRAPEL((IGAU-1)*6+ISIG)
260     CONTINUE
        ZR(IDERA2+(IGAU-1)*NBCMP-1+11)  = RADIV(IGAU)
        ZR(IDERA2+(IGAU-1)*NBCMP-1+12)  = RADIT(IGAU)
  170 CONTINUE
C
      END
