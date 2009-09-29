      SUBROUTINE ERMEB2(INO,IREF1,IREF2,IVOIS,IGEOM,ISIG,TYPEMA,NBCMP,
     &                  INST,NX,NY,TX,TY,SIG11,SIG22,SIG12,CHX,CHY)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/09/2009   AUTEUR GNICOLAS G.NICOLAS 
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
C =====================================================================
C  ERREUR EN MECANIQUE - TERME DE BORD - DIMENSION 2
C  **        **                   *                *
C =====================================================================
C
C     BUT:
C         TROISIEME TERME DE L'ESTIMATEUR D'ERREUR EN RESIDU EXPLICITE :
C         CALCUL DE LA DIFFERENCE ENTRE LES EFFORTS APPLIQUES SUR LE  
C         BORD ET LA CONTRAINTE NORMALE.
C
C
C     ARGUMENTS:
C     ----------
C
C      ENTREE :
C-------------
C IN   INO      : NUMERO DE L'ARETE
C IN   IREF1    : ADRESSE DES CHARGEMENTS DE TYPE FORCE
C IN   IREF2    : ADRESSE DES CHARGEMENTS DE TYPE PRESSION
C IN   IVOIS    : ADRESSE DES VOISINS
C IN   IGEOM    : ADRESSE DE LA GEOMETRIE
C IN   ISIG     : ADRESSE DES CONTRAINTES AUX NOEUDS
C IN   TYPEMA   : TYPE DE LA MAILLE COURANTE
C               'QU4', 'QU8', 'QU9'
C               'TR3', 'TR6', 'TR7'
C IN   NBCMP    : NOMBRE DE COMPOSANTES DU VECTEUR CONTRAINTE PAR NOEUD
C IN   INST     : INSTANT DE CALCUL
C IN   NX       : VECTEUR DES ABSCISSES DES NORMALES AUX NOEUDS
C IN   NY       : VECTEUR DES ORDONNEES DES NORMALES AUX NOEUDS
C IN   TX       : VECTEUR DES ABSCISSES DES TANGENTES AUX NOEUDS
C IN   TY       : VECTEUR DES ORDONNEES DES TANGENTES AUX NOEUDS
C
C      SORTIE :
C-------------
C OUT  SIG11    : VECTEUR DES CONTRAINTES AUX NOEUDS 
C                 ( COMPOSANTE SIG11 )
C OUT  SIG22    : VECTEUR DES CONTRAINTES AUX NOEUDS 
C                 ( COMPOSANTE SIG22 )
C OUT  SIG12    : VECTEUR DES CONTRAINTES AUX NOEUDS 
C                 ( COMPOSANTE SIG12 )
C OUT  CHX      : VECTEUR DES CHARGEMENTS AUX NOEUDS SELON X 
C OUT  CHY      : VECTEUR DES CHARGEMENTS AUX NOEUDS SELON Y
C
C ......................................................................
C
      IMPLICIT NONE
C 
C DECLARATION PARAMETRES D'APPEL
      INTEGER INO,IREF1,IREF2,IVOIS,IGEOM,ISIG,NBCMP
      REAL*8 INST,NX(3),NY(3),TX(3),TY(3),SIG11(3),SIG22(3),SIG12(3)
      REAL*8 CHX(3),CHY(3)
      CHARACTER*8 TYPEMA
C 
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------

      INTEGER        ZI
      COMMON /IVARJE/ZI(1)
      REAL*8         ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16     ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL        ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8    ZK8
      CHARACTER*16          ZK16
      CHARACTER*24                  ZK24
      CHARACTER*32                          ZK32
      CHARACTER*80                                  ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)

C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
C DECLARATION VARIABLES LOCALES
C
      INTEGER IAGD,IADE1,IADE2,IAVA1,IAVA2,IAPTM1,IAPTM2,IGD1,IGD2,IACMP
      INTEGER NCMPM1,NCMPM2,NBS,NBNA,JNO,MNO,IMAV
      INTEGER IER1,IER2,IER3,IER4,IER5,IER6
      INTEGER IENT1,IENT2,NUMGD1,NUMGD2

      REAL*8  PR,CI,FX,FY,VALPAR(3),PRC(3),CIC(3),FXC(3), FYC(3)

      CHARACTER*2 FORM,NOEU
      CHARACTER*4 NOMPAR(3)
      CHARACTER*8 PRF,CIF,FXF,FYF
      CHARACTER*19 NOMGD1,NOMGD2

      LOGICAL FLAG

C ----------------------------------------------------------------------
C
C              X1          X2          X3
C               o-----------o-----------o
C              INO         MNO         JNO
C
C         POINTS  1 --> INO PREMIER POINT DE L'ARETE COURANTE
C                 2 --> JNO DEUXIEME POINT  DE L'ARETE COURANTE
C                 3 --> MNO NOEUD MILIEU S'IL EXISTE
C
C
C ------- RECHERCHE DES ADRESSES POUR LES CHARGES SUR LES SEGMENTS ----
C
      IAGD=ZI(IREF1+4)
C
      IADE1=ZI(IREF1+6)
      IAVA1=ZI(IREF1+7)
      IAPTM1=ZI(IREF1+8)
      IF (IADE1.NE.0) THEN
        IGD1=ZI(IADE1)
        IACMP=ZI(IREF1+5)
        NCMPM1=ZI(IACMP-1+IGD1)
      ENDIF
C
      IADE2=ZI(IREF2+4)
      IAVA2=ZI(IREF2+5)
      IAPTM2=ZI(IREF2+6)
      IF (IADE2.NE.0) THEN
        IGD2   = ZI(IADE2)
        IACMP  = ZI(IREF1+5)
        NCMPM2 = ZI(IACMP-1+IGD2)
      ENDIF
C	
C ----- TESTS SUR LA MAILLE COURANTE -----------------------------------
C
      FORM=TYPEMA(1:2)
      NOEU=TYPEMA(3:3)
C
      IF (FORM.EQ.'TR') THEN
        NBS=3
        ELSE
        NBS=4
      ENDIF
      IF (NOEU.EQ.'3'.OR.NOEU.EQ.'4') THEN
        NBNA=2
        ELSE
        NBNA=3
      ENDIF
C
      IF (INO.EQ.NBS) THEN
        JNO=1
        ELSE
        JNO=INO+1
      ENDIF
C
C --------------------
C
      NOMGD1=' '
      NOMGD2=' '
      IF (IADE1.NE.0) THEN
        IMAV=ZI(IVOIS+INO)
        IF (IAPTM1.EQ.0) THEN
C         CARTE CONSTANTE
          IENT1=1
        ELSE
C         LA CARTE A ETE ETENDUE
          IENT1=ZI(IAPTM1-1+IMAV)
        ENDIF
          NUMGD1=ZI(IREF1+9)
          NOMGD1=ZK8(IAGD-1+NUMGD1)
      ENDIF
C
      IF (IADE2.NE.0) THEN
        IMAV=ZI(IVOIS+INO)
        IF (IAPTM2.EQ.0) THEN
C         CARTE CONSTANTE
          IENT2=1
        ELSE
C       LA CARTE A ETE ETENDUE
          IENT2=ZI(IAPTM2-1+IMAV)
        ENDIF
          NUMGD2=ZI(IREF2+7)
          NOMGD2=ZK8(IAGD-1+NUMGD2)
      ENDIF
C
C ----- CALCUL DES CHARGES APPLIQUEES SUR LE BORD ----------------------
C ------- RECUPERATION DES PRESSIONS -----------------------------------
C
      FLAG=.TRUE.
      CALL R8INIR(3,0.D0,CHX,1)
      CALL R8INIR(3,0.D0,CHY,1)

      IF (NOMGD2(1:6).EQ.'PRES_R') THEN
        PR=ZR(IAVA2-1+(IENT2-1)*NCMPM2+1)
        CI=ZR(IAVA2-1+(IENT2-1)*NCMPM2+2)
C
        IF (ABS(PR).GT.1.D-15.OR.ABS(CI).GT.1.D-15) THEN
C
          FLAG=.FALSE.
C
          CHX(1)=-PR*NX(1)+CI*TX(1)         
          CHY(1)=-PR*NY(1)+CI*TY(1)
          CHX(2)=-PR*NX(2)+CI*TX(2)         
          CHY(2)=-PR*NY(2)+CI*TY(2)
C
          IF (NBNA.EQ.3) THEN
            CHX(3)=-PR*NX(3)+CI*TX(3)         
            CHY(3)=-PR*NY(3)+CI*TY(3)
          ENDIF
        ENDIF
C
      ELSE IF (NOMGD2(1:6).EQ.'PRES_F') THEN
        PRF=ZK8(IAVA2-1+(IENT2-1)*NCMPM2+1)
        CIF=ZK8(IAVA2-1+(IENT2-1)*NCMPM2+2)
C
        IF (PRF.NE.'&FOZERO'.OR.
     &      CIF.NE.'&FOZERO') THEN
C
          FLAG=.FALSE.
C
          NOMPAR(1)='X'
          NOMPAR(2)='Y'
          NOMPAR(3)='INST'
          VALPAR(1)=ZR(IGEOM-1+2*(INO-1)+1)
          VALPAR(2)=ZR(IGEOM-1+2*(INO-1)+2)
          VALPAR(3)=INST
          CALL FOINTE('FM',PRF,3,NOMPAR,VALPAR,PRC(1),IER1)
          CALL FOINTE('FM',CIF,3,NOMPAR,VALPAR,CIC(1),IER2)
C
          VALPAR(1)=ZR(IGEOM-1+2*(JNO-1)+1)
          VALPAR(2)=ZR(IGEOM-1+2*(JNO-1)+2)
          VALPAR(3)=INST             
          CALL FOINTE('FM',PRF,3,NOMPAR,VALPAR,PRC(2),IER3)
          CALL FOINTE('FM',CIF,3,NOMPAR,VALPAR,CIC(2),IER4)
C
          CHX(1)=-PRC(1)*NX(1)+CIC(1)*TX(1)         
          CHY(1)=-PRC(1)*NY(1)+CIC(1)*TY(1)
          CHX(2)=-PRC(2)*NX(2)+CIC(2)*TX(2)         
          CHY(2)=-PRC(2)*NY(2)+CIC(2)*TY(2)
C
          IF (NBNA.EQ.3) THEN
            MNO=NBS+INO
            VALPAR(1)=ZR(IGEOM-1+2*(MNO-1)+1)
            VALPAR(2)=ZR(IGEOM-1+2*(MNO-1)+2)
            VALPAR(3)=INST
            CALL FOINTE('FM',PRF,3,NOMPAR,VALPAR,PRC(3),IER5)
            CALL FOINTE('FM',CIF,3,NOMPAR,VALPAR,CIC(3),IER6)
C
            CHX(3)=-PRC(3)*NX(3)+CIC(3)*TX(3)         
            CHY(3)=-PRC(3)*NY(3)+CIC(3)*TY(3)
C
          ENDIF
        ENDIF
C
C ------- RECUPERATION DES FORCES --------------------------------------
C
      ELSE IF (NOMGD1(1:6).EQ.'FORC_R') THEN
        FX=ZR(IAVA1-1+(IENT1-1)*NCMPM1+1)
        FY=ZR(IAVA1-1+(IENT1-1)*NCMPM1+2)
C
        IF (ABS(FX).GT.1.D-15.OR.
     &      ABS(FY).GT.1.D-15) THEN
C
          FLAG=.FALSE.
C
          CALL R8INIR(2,FX,CHX,1)
          CALL R8INIR(2,FY,CHY,1)
C
          IF (NBNA.EQ.3) THEN
            CHX(3)=FX
            CHY(3)=FY
          ENDIF
C
        ENDIF
C
      ELSE IF (NOMGD1(1:6).EQ.'FORC_F') THEN
        FXF=ZK8(IAVA1-1+(IENT1-1)*NCMPM1+1)
        FYF=ZK8(IAVA1-1+(IENT1-1)*NCMPM1+2)
C
        IF (FXF.NE.'&FOZERO'.OR.FYF.NE.'&FOZERO') THEN
C
          FLAG=.FALSE.
C
          NOMPAR(1)='X'
          NOMPAR(2)='Y'
          NOMPAR(3)='INST'
          VALPAR(1)=ZR(IGEOM-1+2*(INO-1)+1)
          VALPAR(2)=ZR(IGEOM-1+2*(INO-1)+2)
          VALPAR(3)=INST
          CALL FOINTE('FM',FXF,3,NOMPAR,VALPAR,FXC(1),IER1)
          CALL FOINTE('FM',FYF,3,NOMPAR,VALPAR,FYC(1),IER2)
C
          VALPAR(1)=ZR(IGEOM-1+2*(JNO-1)+1)
          VALPAR(2)=ZR(IGEOM-1+2*(JNO-1)+2)
          VALPAR(3)=INST
          CALL FOINTE('FM',FXF,3,NOMPAR,VALPAR,FXC(2),IER3)
          CALL FOINTE('FM',FYF,3,NOMPAR,VALPAR,FYC(2),IER4)
C
          CALL R8INIR(2,FXC,CHX,1)
          CALL R8INIR(2,FYC,CHY,1)
C
          IF (NBNA.EQ.3) THEN
            MNO=NBS+INO
            VALPAR(1)=ZR(IGEOM-1+2*(MNO-1)+1)
            VALPAR(2)=ZR(IGEOM-1+2*(MNO-1)+2)
            VALPAR(3)=INST
            CALL FOINTE('FM',FXF,3,NOMPAR,VALPAR,FXC(3),IER5)
            CALL FOINTE('FM',FYF,3,NOMPAR,VALPAR,FYC(3),IER6)
            CHX(3)=FXC(3)
            CHY(3)=FYC(3)
          ENDIF
        ENDIF
      ENDIF
C
C ------- RECUPERATION DE SIGMA SUR LA MAILLE COURANTE -----------------
C
      IF (FLAG) THEN
C ----- PAS DE CHARGEMENT EXPLICITE SUR LE BORD ==> ON PREND SIGMA NUL
        CALL R8INIR(3,0.D0,SIG11,1)
        CALL R8INIR(3,0.D0,SIG22,1)
        CALL R8INIR(3,0.D0,SIG12,1)
C
      ELSE
C      
        SIG11(1)=ZR(ISIG-1+NBCMP*(INO-1)+1)
        SIG22(1)=ZR(ISIG-1+NBCMP*(INO-1)+2)
        SIG12(1)=ZR(ISIG-1+NBCMP*(INO-1)+4)
C
        SIG11(2)=ZR(ISIG-1+NBCMP*(JNO-1)+1)
        SIG22(2)=ZR(ISIG-1+NBCMP*(JNO-1)+2)
        SIG12(2)=ZR(ISIG-1+NBCMP*(JNO-1)+4)
C
        IF (NBNA.EQ.3) THEN
          MNO=NBS+INO
C
          SIG11(3)=ZR(ISIG-1+NBCMP*(MNO-1)+1)
          SIG22(3)=ZR(ISIG-1+NBCMP*(MNO-1)+2)
          SIG12(3)=ZR(ISIG-1+NBCMP*(MNO-1)+4)
C
        ENDIF
      ENDIF
C
      END
