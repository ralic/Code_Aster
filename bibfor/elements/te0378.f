      SUBROUTINE TE0378 (OPTION,NOMTE)
      IMPLICIT NONE
      CHARACTER*16 OPTION,NOMTE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/07/2006   AUTEUR MEUNIER S.MEUNIER 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C     BUT:
C         CALCUL DE L'INDICATEUR D'ERREUR EN QUANTITE D'INTERET
C         SUR UN ELEMENT 2D AVEC LA METHODE DES RESIDUS EXPLICITES.
C         OPTION : 'QIRE_ELEM_SIGM'
C
C ----------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER NPG,IPG,NNO,NNOS,TYP,TYPV,IPOIDS,IVF,IDFDE
      INTEGER IGEOM,IBID,IADP,IADD,IFORP,IFORD,IERR,IROTP,IROTD,IMATE
      INTEGER IVOIS,NBS,NBNA,JNO,NBCMP,NDIM,JGANO,IRET,IS,IPESP,IPESD
      INTEGER JTIME,NIV,IPP,IRP,IPD,IRD,IREFP1,IREFP2,IREFD1,IREFD2
      INTEGER I
      INTEGER IATYMA,INO,ITABP(7),ITABD(7)

      REAL*8 DFDX(9),DFDY(9),HE,HF,POIDS
      REAL*8 FORPX,FORPY,FORDX,FORDY,FPPX,FPPY,FPDX,FPDY
      REAL*8 FRPX(9),FRPY(9),FRDX(9),FRDY(9),RHO,R8BID
      REAL*8 ERREST,COEFF,ORIEN
      REAL*8 TERPL1,TERMO1,TERPL2,TERMO2,TERPL3,TERMO3
      REAL*8 SGP11(3),SGP22(3),SGP12(3),SGD11(3),SGD22(3),SGD12(3)
      REAL*8 JAC(3),NX(3),NY(3),TX(3),TY(3)
      REAL*8 CHPX(3),CHPY(3),CHDX(3),CHDY(3)
      REAL*8 SOPL11(3),SOPL22(3),SOPL12(3),SOMO11(3),SOMO22(3),SOMO12(3)
      REAL*8 CHPLX(3),CHPLY(3),CHMOX(3),CHMOY(3)
      REAL*8 SIPL11(3),SIPL22(3),SIPL12(3),SIMO11(3),SIMO22(3),SIMO12(3)
      REAL*8 INST,INTPL,INTMO,S
      REAL*8 SIGP11(3),SIGP22(3),SIGP12(3),SIGD11(3),SIGD22(3),SIGD12(3)
      REAL*8 DSPX,DSPY,DSDX,DSDY,NUPLUS,NUMOIN
      
      CHARACTER*2 CODRET(1),FORM,FORMV,NOEU
      CHARACTER*8 TYPEMA,TYPMAV
      CHARACTER*16 PHENOM
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
C
      CALL JEVECH ('PGEOMER', 'L', IGEOM )
      CALL TECACH ('OOO','PCONTNOP',3,ITABP,IRET)
      CALL TECACH ('OOO','PCONTNOD',3,ITABD,IRET)
      CALL JEVECH ('PFRVOLUP','L',IFORP)
      CALL JEVECH ('PFRVOLUD','L',IFORD)
      CALL JEVECH ('PERREUR','E',IERR)
      CALL JEVECH ('PTEMPSR','L',JTIME)
C
C ----- CALCUL DU COEFFICIENT S ----------------------------------------
C
      CALL JEVECH('PCONSTR','L',IS)

      S=ZR(IS-1+1)
      INST=ZR(JTIME-1+1)
      IADP=ITABP(1)
      IADD=ITABD(1)
      NBCMP=ITABP(2)/NNO

C ----------------------------------------------------------------------
C ---------------- CALCUL DU PREMIER TERME DE L'ERREUR -----------------
C ----------------------------------------------------------------------
C
C ----- CALCUL DU DIAMETRE HE DU CERCLE CIRCONSCRIT A L'ELEMENT --------
C
      NDIM=2
      NIV=1 
      CALL UTHK(NOMTE,IGEOM,HE,NDIM,IBID,IBID,IBID,IBID,NIV,IBID)
C
C ----- CALCUL DES FORCES DE PESANTEUR ET DE ROTATION ---------------
C ------- INITIALISATION DES FORCES ------------------------------------
C
      FPPX=0.D0
      FPPY=0.D0
      FPDX=0.D0
      FPDY=0.D0
      CALL R8INIR(9,0.D0,FRPX,1)
      CALL R8INIR(9,0.D0,FRPY,1)
      CALL R8INIR(9,0.D0,FRDX,1)
      CALL R8INIR(9,0.D0,FRDY,1)
C
C ------- TEST D'EXISTENCE DES CARTES DE PESA ET ROTA ------------------
C
      CALL TECACH('ONN','PPESANRP',1,IPP,IRET)
      CALL TECACH('ONN','PROTATRP',1,IRP,IRET)
      CALL TECACH('ONN','PPESANRD',1,IPD,IRET)
      CALL TECACH('ONN','PROTATRD',1,IRD,IRET)
      IF (IPP.NE.0.OR.IRP.NE.0.OR.IPD.NE.0.OR.IRD.NE.0) THEN
         CALL JEVECH('PMATERC','L',IMATE)
         CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,CODRET)
         CALL RCVALA(ZI(IMATE),' ',PHENOM,1,' ',R8BID,1,'RHO',
     &                 RHO,CODRET,'FM')
C
C ------- CALCUL DE LA FORCE DE PESANTEUR PB. PRIMAL -------------------
C
         IF (IPP.NE.0) THEN
           CALL JEVECH('PPESANRP','L',IPESP)
           FPPX=RHO*ZR(IPESP)*ZR(IPESP+1)
           FPPY=RHO*ZR(IPESP)*ZR(IPESP+2)
         ENDIF
C
C ------- CALCUL DE LA FORCE DE PESANTEUR PB. DUAL ---------------------
C
         IF (IPD.NE.0) THEN
           CALL JEVECH('PPESANRD','L',IPESD)
           FPDX=RHO*ZR(IPESD)*ZR(IPESD+1)
           FPDY=RHO*ZR(IPESD)*ZR(IPESD+2)
         ENDIF
C
C ------- CALCUL DE LA FORCE DE ROTATION AUX PTS DE GAUSS PB. PRIMAL ---
C
         IF (IRP.NE.0)THEN
           CALL JEVECH('PROTATRP','L',IROTP)
           CALL RESROT (ZR(IROTP),ZR(IGEOM),ZR(IVF),RHO,NNO,NPG,
     &                  FRPX,FRPY)
         ENDIF
C
C ------- CALCUL DE LA FORCE DE ROTATION AUX PTS DE GAUSS PB. DUAL -----
C
         IF (IRD.NE.0)THEN
           CALL JEVECH('PROTATR','L',IROTD)
           CALL RESROT (ZR(IROTD),ZR(IGEOM),ZR(IVF),RHO,NNO,NPG,
     &                  FRDX,FRDY)
         ENDIF
      ENDIF
C
C ----- CALCUL DU TERME D'ERREUR AVEC INTEGRATION DE GAUSS -------------
C
      TERPL1=0.D0
      TERMO1=0.D0
C
      DO 10 IPG=1,NPG
C
C ------- CALCUL DES DERIVEES DES FONCTIONS DE FORMES /X ET /Y ---------
C
        CALL DFDM2D (NNO,IPG,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,POIDS)
C
C ------- CALCUL L'ORIENTATION DE LA MAILLE ----------------------------
C
        CALL UTJAC(.TRUE.,IGEOM,IPG,IDFDE,0,IBID,NNO,ORIEN)
C
C ------- CALCUL DE LA DIVERGENCE ET DE LA NORME DE SIGMA PB. PRIMAL ---
C
        IBID = 1
        CALL ERMEV2(NOMTE,NNO,IPG,IGEOM,IVF,IADP,NBCMP,DFDX,DFDY,
     &              POIDS,IBID,
     &              DSPX,DSPY,R8BID)
C
C ------- CALCUL DE LA DIVERGENCE ET DE LA NORME DE SIGMA PB. DUAL -----
C
        IBID = 0
        CALL ERMEV2(NOMTE,NNO,IPG,IGEOM,IVF,IADD,NBCMP,DFDX,DFDY,
     &              POIDS,IBID,
     &              DSDX,DSDY,R8BID)
C
C ------- CALCUL DE L'EFFORT VOLUMIQUE PB. PRIMAL ----------------------
C
        FORPX=ZR(IFORP-1+2*(IPG-1)+1)+FPPX+FRPX(IPG)
        FORPY=ZR(IFORP-1+2*(IPG-1)+2)+FPPY+FRPY(IPG)
C
C ------- CALCUL DE L'EFFORT VOLUMIQUE PB. DUAL ------------------------
C
        FORDX=ZR(IFORD-1+2*(IPG-1)+1)+FPDX+FRDX(IPG)
        FORDY=ZR(IFORD-1+2*(IPG-1)+2)+FPDY+FRDY(IPG)
C
C ------- CALCUL DES TERMES D'ERREUR AVEC INTEGRATION DE GAUSS ---------
C
        TERPL1=TERPL1+((S*(FORPX+DSPX)+(1.D0/S)*(FORDX+DSDX))**2
     &                +(S*(FORPY+DSPY)+(1.D0/S)*(FORDY+DSDY))**2)*POIDS
C
        TERMO1=TERMO1+((S*(FORPX+DSPX)-(1.D0/S)*(FORDX+DSDX))**2
     &                +(S*(FORPY+DSPY)-(1.D0/S)*(FORDY+DSDY))**2)*POIDS
C
  10  CONTINUE
C
      TERPL1=(HE**2)*(SQRT(TERPL1))**2
      TERMO1=(HE**2)*(SQRT(TERMO1))**2   
C     
C ----------------------------------------------------------------------
C ------------ FIN DU CALCUL DU PREMIER TERME DE L'ERREUR --------------
C ----------------------------------------------------------------------
C
C ----------------------------------------------------------------------
C ----------- CALCUL DU DEUXIEME ET TROISIEME TERME DE L'ERREUR --------
C ----------------------------------------------------------------------
C
      CALL JEVECH('PFORCEP','L',IREFP1)
      CALL JEVECH('PFORCED','L',IREFD1)
      CALL JEVECH('PPRESSP','L',IREFP2)
      CALL JEVECH('PPRESSD','L',IREFD2)
      CALL JEVECH('PVOISIN','L',IVOIS)

C ----- TEST SUR LE TYPE DE LA MAILLE COURANTE -------------------------
C
      TYP=ZI(IVOIS+7)
      IATYMA=ZI(IREFP1+3)
      TYPEMA=ZK8(IATYMA-1+TYP)      
C
      FORM=TYPEMA(1:2)
      IF (FORM.EQ.'TR') THEN
        NBS=3
        ELSE
        NBS=4
      ENDIF
      NOEU=TYPEMA(5:5)
      IF (NOEU.EQ.'6'.OR.NOEU.EQ.'8'.OR.NOEU.EQ.'9') THEN
        NBNA=3
        ELSE
        NBNA=2
      ENDIF
C
C ----- BOUCLE SUR LES ARETES ------------------------------------------
C
      TERPL2=0.D0
      TERMO2=0.D0
      TERPL3=0.D0
      TERMO3=0.D0
      DO 20 INO=1,NBS
C
C ----- CALCUL DU DIAMETRE HF DU CERCLE CIRCONSCRIT À L'ARETE ----------
C
        IF (INO.EQ.NBS) THEN
          JNO = 1
          ELSE
          JNO = INO+1
        ENDIF
        HF=SQRT((ZR(IGEOM-1+2*(INO-1)+1)-ZR(IGEOM-1+2*(JNO-1)+1))**2
     &         +(ZR(IGEOM-1+2*(INO-1)+2)-ZR(IGEOM-1+2*(JNO-1)+2))**2)
C
C ----- CALCUL DE NORMALES, TANGENTES ET JACOBIENS AUX POINTS DE L'ARETE
C
          CALL CALNOR('2D',INO,IBID,IBID,NBS,NBNA,IBID,IGEOM,IBID,
     &                IBID,IBID,ORIEN,HF,
     &                JAC,NX,NY,R8BID,TX,TY)
C
C ----- TEST DU TYPE DE VOISIN -----------------------------------------
C
        TYPV=ZI(IVOIS+7+INO)
        IF (TYPV.NE.0) THEN
          TYPMAV=ZK8(IATYMA-1+TYPV)      
          FORMV=TYPMAV(1:2)
C
C ----------------------------------------------------------------------
C --------------- CALCUL DU DEUXIEME TERME DE L'ERREUR -----------------
C ----------------------------------------------------------------------
C
          IF (FORMV.EQ.'TR'.OR.
     &        FORMV.EQ.'QU') THEN
C
C ------- CALCUL DU SAUT DE CONTRAINTE ENTRE ELEMENTS PB. PRIMAL -------
C
          CALL ERMES2(INO,TYPEMA,TYPMAV,IREFP1,IVOIS,IADP,NBCMP,
     &                SGP11,SGP22,SGP12)
C
C ------- CALCUL DU SAUT DE CONTRAINTE ENTRE ELEMENTS PB. DUAL ---------
C
          CALL ERMES2(INO,TYPEMA,TYPMAV,IREFD1,IVOIS,IADD,NBCMP,
     &                SGD11,SGD22,SGD12)
C
C ------- CALCUL DU SAUT DE CONTRAINTE ENTRE ELEMENTS PB. GLOBAL -------
C
          DO 201 I=1,NBNA
            SOPL11(I)=S*SGP11(I)+(1.D0/S)*SGD11(I)
            SOPL22(I)=S*SGP22(I)+(1.D0/S)*SGD22(I)
            SOPL12(I)=S*SGP12(I)+(1.D0/S)*SGD12(I)
            SOMO11(I)=S*SGP11(I)-(1.D0/S)*SGD11(I)
            SOMO22(I)=S*SGP22(I)-(1.D0/S)*SGD22(I)
            SOMO12(I)=S*SGP12(I)-(1.D0/S)*SGD12(I)
  201     CONTINUE
C
C ------- CALCUL DE L'INTEGRALE SUR LE BORD ----------------------------
C     
          CALL R8INIR(3,0.D0,CHPLX,1)
          CALL R8INIR(3,0.D0,CHPLY,1)
          CALL R8INIR(3,0.D0,CHMOX,1)
          CALL R8INIR(3,0.D0,CHMOY,1)
C
          CALL INTENC(NBNA,JAC,CHPLX,CHPLY,SOPL11,SOPL22,SOPL12,NX,NY,
     &                INTPL)
C
          CALL INTENC(NBNA,JAC,CHMOX,CHMOY,SOMO11,SOMO22,SOMO12,NX,NY,
     &                INTMO)
C
C ------- CALCUL DU TERME D'ERREUR AVEC INTEGRATION DE NEWTON-COTES ----
C
          TERPL2=TERPL2+0.5D0*HF*(SQRT(INTPL))**2
          TERMO2=TERMO2+0.5D0*HF*(SQRT(INTMO))**2
C
C ----------------------------------------------------------------------
C --------------- CALCUL DU TROISIEME TERME DE L'ERREUR ----------------
C ----------------------------------------------------------------------
C
          ELSE IF (FORMV.EQ.'SE') THEN
C
C ------- CALCUL EFFORTS SURFACIQUES ET DES CONTRAINTES PB. PRIMAL -----
C
           CALL ERMEB2(INO,IREFP1,IREFP2,IVOIS,IGEOM,IADP,TYPEMA,NBCMP,
     &                 INST,NX,NY,TX,TY,SIGP11,SIGP22,SIGP12,CHPX,CHPY)
C
C ------- CALCUL EFFORTS SURFACIQUES ET DES CONTRAINTES PB. DUAL -------
C
            CALL ERMEB2(INO,IREFD1,IREFD2,IVOIS,IGEOM,IADD,TYPEMA,NBCMP,
     &                  INST,NX,NY,TX,TY,SIGD11,SIGD22,SIGD12,CHDX,CHDY)
C
C ------- CALCUL EFFORTS SURFACIQUES ET DES CONTRAINTES PB. GLOBAL -----
C
          DO 202 I=1,NBNA
            CHPLX(I)=S*CHPX(I)+(1.D0/S)*CHDX(I)
            CHPLY(I)=S*CHPY(I)+(1.D0/S)*CHDY(I)
            SIPL11(I)=S*SIGP11(I)+(1.D0/S)*SIGD11(I)
            SIPL22(I)=S*SIGP22(I)+(1.D0/S)*SIGD22(I)
            SIPL12(I)=S*SIGP12(I)+(1.D0/S)*SIGD12(I)
C
            CHMOX(I)=S*CHPX(I)-(1.D0/S)*CHDX(I)
            CHMOY(I)=S*CHPY(I)-(1.D0/S)*CHDY(I)
            SIMO11(I)=S*SIGP11(I)-(1.D0/S)*SIGD11(I)
            SIMO22(I)=S*SIGP22(I)-(1.D0/S)*SIGD22(I)
            SIMO12(I)=S*SIGP12(I)-(1.D0/S)*SIGD12(I)
  202     CONTINUE
C
C ------- CALCUL DE L'INTEGRALE SUR LE BORD ----------------------------
C     
            CALL INTENC(NBNA,JAC,CHPLX,CHPLY,SIPL11,SIPL22,SIPL12,NX,NY,
     &                  INTPL)
C
            CALL INTENC(NBNA,JAC,CHMOX,CHMOY,SIMO11,SIMO22,SIMO12,NX,NY,
     &                  INTMO)
C
C ------- CALCUL DU TERME D'ERREUR AVEC INTEGRATION DE NEWTON-COTES ----
C
            TERPL3=TERPL3+HF*(SQRT(INTPL))**2
            TERMO3=TERMO3+HF*(SQRT(INTMO))**2
C
        ENDIF
C
      ENDIF
C
  20  CONTINUE
C
C ----------------------------------------------------------------------
C ------- FIN DU CALCUL DU DEUXIEME ET TROISIEME TERME DE L'ERREUR -----
C ----------------------------------------------------------------------
C
C ----------------------------------------------------------------------
C ------------ MISE EN MEMOIRE DES DIFFERENTS TERMES DE IERR -----------
C ----------------------------------------------------------------------
C
      IF (NBNA.EQ.3) THEN
        COEFF=SQRT(96.D0)
        ELSE
        COEFF=SQRT(24.D0)
      ENDIF
C
      NUPLUS=SQRT(TERPL1+TERPL2+TERPL3)
      NUMOIN=SQRT(TERMO1+TERMO2+TERMO3)
      ERREST=(1.D0/4.D0)*(NUPLUS-NUMOIN)/COEFF
C
      ZR(IERR)=ERREST
C
      ERREST=(1.D0/4.D0)*(SQRT(TERPL1)-SQRT(TERMO1))/COEFF
C
      ZR(IERR+3)=ERREST
C
      ERREST=(1.D0/4.D0)*(SQRT(TERPL3)-SQRT(TERMO3))/COEFF
C
      ZR(IERR+5)=ERREST
C
      ERREST=(1.D0/4.D0)*(SQRT(TERPL2)-SQRT(TERMO2))/COEFF
C
      ZR(IERR+7)=ERREST
C
      CALL JEDEMA()
C
      END
