      SUBROUTINE TE0375(OPTION,NOMTE)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16 OPTION,NOMTE
C ......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 04/11/2004   AUTEUR G8BHHXD X.DESROCHES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_20

C    - FONCTION REALISEE:  CALCUL DE L'ERREUR SUR UN ELEMENT 3D AVEC LA
C                          METHODE DES RESIDUS
C                          OPTION : 'ERRE_ELGA_NORE  '

C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER NPG1,I,KP,NNO,TYV
      INTEGER IPOIDS,IVF,IDFDE,IGEOM,IFM
      INTEGER IAD,IFOR,IERR,IPES,IROT,IMATE,IADE1,IAVA1
      INTEGER IVOIS,IAREPE,JCELD,JCELV,IAVAL1
      INTEGER JAD,JADV,IGREL,IEL,NCMPM1
      INTEGER JNO,NBNV,NCHER
      INTEGER NBCMP,ITAB(7)
      INTEGER INOV,IMAV,JKP,NOE(9,6,3)
      REAL*8 A(3,3),B(3),H1,H2,H3,H4
      REAL*8 DFDX(27),DFDY(27),DFDZ(27),H,HH,POID
      REAL*8 FORX,FORY,FORZ,FPX,FPY,FPZ
      REAL*8 FRX(27),FRY(27),FRZ(27)
      REAL*8 FX(9),FY(9),FZ(9)
      REAL*8 POIDS1(9),POIDS2(9),POIDS(9),RHO
      REAL*8 SIG11,SIG22,SIG33,SIG12,SIG13,SIG23,SIGDIF(6)
      REAL*8 DSIG11,DSIG22,DSIG33,DSIG12,DSIG13,DSIG23
      REAL*8 DSIG21,DSIG31,DSIG32
      REAL*8 SPG11,SPG22,SPG33,SPG12,SPG13,SPG23
      REAL*8 DSX,DSY,DSZ,SX(9,9),SY(9,9),SZ(9,9)
      REAL*8 TER,ERREST,NOR,NORSIG,SIGCAL,NUEST,COEFF
      REAL*8 TER2,TER3,HF,NORM,NX,NY,NZ,JACO
      REAL*8 PR(9),VALPAR(4),INST,ZERO
      LOGICAL PRES,FORC,FAUX
      CHARACTER*2 CODRET
      CHARACTER*4 NOMPAR(4)
      CHARACTER*8 TYPMAV,ELREFE
      CHARACTER*8 PRF,FXF,FYF,FZF,ELREF2,NOMGD2
      CHARACTER*16 PHENOM
      CHARACTER*19 NOMGD1

C --- INITIALISATION DU TABLEAU DES NUMEROS DE NOEUDS FACE PAR FACE

C    HEXAEDRES A 8,20 ET 27 NOEUDS
C    PENTAEDRES A 6 ET 15 NOEUDS
      DATA NOE/1,2,3,4,9,10,11,12,21,3,7,8,4,15,19,16,11,24,5,8,7,6,20,
     &     19,18,17,26,1,5,6,2,13,17,14,9,22,2,6,7,3,14,18,15,10,23,4,8,
     &     5,1,16,20,13,12,25,1,2,3,7,8,9,3*0,4,6,5,15,14,13,3*0,1,4,5,
     &     2,10,13,11,7,0,2,5,6,3,11,14,12,8,0,1,3,6,4,9,12,15,10,0,9*0,
     &     1,2,3,5,6,7,3*0,2,4,3,9,10,6,3*0,3,4,1,10,8,7,3*0,1,4,2,8,9,
     &     5,3*0,9*0,9*0/
C    TETRAEDRES A 4 ET 10 NOEUDS

C     ------------------------------------------------------------------
      CALL JEMARQ()

      CALL ELREF1(ELREFE)

      FAUX = .FALSE.
      ZERO = 0.D0
      NOMGD2 = ' '
      IFM = IUNIFI('MESSAGE')

      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,IDFDE,JGANO)

      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL TECACH('OOO','PCONTNO',3,ITAB,IRET)
      CALL JEVECH('PFRVOLU','L',IFOR)
      CALL JEVECH('PERREUR','E',IERR)
      CALL JEVECH('PTEMPSR','L',JTIME)
      INST  = ZR(JTIME-1+1)
      IAD   = ITAB(1)
      NBCMP = ITAB(2)/NNO

C
C -------- CALCUL DU DIAMETRE H ---------------------------------------

      IF (ELREFE.EQ.'TE4' .OR. ELREFE.EQ.'T10') THEN

C   TETRAEDRE : H = DIAMETRE DU CERCLE CIRCONSCRIT

        A(1,1) = ZR(IGEOM+3) - ZR(IGEOM)
        A(2,1) = ZR(IGEOM+6) - ZR(IGEOM)
        A(3,1) = ZR(IGEOM+9) - ZR(IGEOM)
        A(1,2) = ZR(IGEOM+4) - ZR(IGEOM+1)
        A(2,2) = ZR(IGEOM+7) - ZR(IGEOM+1)
        A(3,2) = ZR(IGEOM+10) - ZR(IGEOM+1)
        A(1,3) = ZR(IGEOM+5) - ZR(IGEOM+2)
        A(2,3) = ZR(IGEOM+8) - ZR(IGEOM+2)
        A(3,3) = ZR(IGEOM+11) - ZR(IGEOM+2)
        B(1) = (A(1,1)* (ZR(IGEOM+3)+ZR(IGEOM))+
     &         A(1,2)* (ZR(IGEOM+4)+ZR(IGEOM+1))+
     &         A(1,3)* (ZR(IGEOM+5)+ZR(IGEOM+2)))/2.0D0
        B(2) = (A(2,1)* (ZR(IGEOM+6)+ZR(IGEOM))+
     &         A(2,2)* (ZR(IGEOM+7)+ZR(IGEOM+1))+
     &         A(2,3)* (ZR(IGEOM+8)+ZR(IGEOM+2)))/2.0D0
        B(3) = (A(3,1)* (ZR(IGEOM+9)+ZR(IGEOM))+
     &         A(3,2)* (ZR(IGEOM+10)+ZR(IGEOM+1))+
     &         A(3,3)* (ZR(IGEOM+11)+ZR(IGEOM+2)))/2.0D0

        CALL MGAUSS(A,B,3,3,1,ZERO,FAUX)
        H = 2.D0*SQRT((ZR(IGEOM)-B(1))**2+ (ZR(IGEOM+1)-B(2))**2+
     &      (ZR(IGEOM+2)-B(3))**2)



      ELSE IF (ELREFE.EQ.'HE8' .OR. ELREFE.EQ.'H20' .OR.
     &         ELREFE.EQ.'H27') THEN

C   HEXAEDRE : H = PLUS GRANDE DIAGONALE

        H1 = SQRT((ZR(IGEOM)-ZR(IGEOM+18))**2+
     &       (ZR(IGEOM+1)-ZR(IGEOM+19))**2+
     &       (ZR(IGEOM+2)-ZR(IGEOM+20))**2)
        H2 = SQRT((ZR(IGEOM+3)-ZR(IGEOM+21))**2+
     &       (ZR(IGEOM+4)-ZR(IGEOM+22))**2+
     &       (ZR(IGEOM+5)-ZR(IGEOM+23))**2)
        IF (H1.GT.H2) THEN
          H = H1
        ELSE
          H = H2
        END IF
        H3 = SQRT((ZR(IGEOM+6)-ZR(IGEOM+12))**2+
     &       (ZR(IGEOM+7)-ZR(IGEOM+13))**2+
     &       (ZR(IGEOM+8)-ZR(IGEOM+14))**2)
        IF (H3.GT.H) THEN
          H = H3
        END IF
        H4 = SQRT((ZR(IGEOM+9)-ZR(IGEOM+15))**2+
     &       (ZR(IGEOM+10)-ZR(IGEOM+16))**2+
     &       (ZR(IGEOM+11)-ZR(IGEOM+17))**2)
        IF (H4.GT.H) THEN
          H = H4
        END IF


      ELSE IF (ELREFE.EQ.'PE6' .OR. ELREFE.EQ.'P15') THEN

C   PENTAEDRE : H = PLUS GRANDE DIAGONALE

        H1 = SQRT((ZR(IGEOM)-ZR(IGEOM+12))**2+
     &       (ZR(IGEOM+1)-ZR(IGEOM+13))**2+
     &       (ZR(IGEOM+2)-ZR(IGEOM+14))**2)
        H2 = SQRT((ZR(IGEOM+3)-ZR(IGEOM+15))**2+
     &       (ZR(IGEOM+4)-ZR(IGEOM+16))**2+
     &       (ZR(IGEOM+5)-ZR(IGEOM+17))**2)
        IF (H1.GT.H2) THEN
          H = H1
        ELSE
          H = H2
        END IF
        H3 = SQRT((ZR(IGEOM+6)-ZR(IGEOM+9))**2+
     &       (ZR(IGEOM+7)-ZR(IGEOM+10))**2+
     &       (ZR(IGEOM+8)-ZR(IGEOM+11))**2)
        IF (H3.GT.H) THEN
          H = H3
        END IF
        H1 = SQRT((ZR(IGEOM)-ZR(IGEOM+15))**2+
     &       (ZR(IGEOM+1)-ZR(IGEOM+16))**2+
     &       (ZR(IGEOM+2)-ZR(IGEOM+17))**2)
        H2 = SQRT((ZR(IGEOM+3)-ZR(IGEOM+9))**2+
     &       (ZR(IGEOM+4)-ZR(IGEOM+10))**2+
     &       (ZR(IGEOM+5)-ZR(IGEOM+11))**2)
        IF (H1.GT.H2) THEN
          HH = H1
        ELSE
          HH = H2
        END IF
        H3 = SQRT((ZR(IGEOM+6)-ZR(IGEOM+12))**2+
     &       (ZR(IGEOM+7)-ZR(IGEOM+13))**2+
     &       (ZR(IGEOM+8)-ZR(IGEOM+14))**2)
        IF (H3.GT.HH) THEN
          HH = H3
        END IF
        IF (HH.GT.H) THEN
          H = HH
        END IF

      ELSE
        CALL UTMESS('F','TE0375','TYPE ELEMENT INCONNU')
      END IF

C ------ INITIALISATION DES FORCES --------------------------------

      FPX = 0.D0
      FPY = 0.D0
      FPZ = 0.D0
      DO 10 JKP = 1,27
        FRX(JKP) = 0.D0
        FRY(JKP) = 0.D0
        FRZ(JKP) = 0.D0
   10 CONTINUE

C ------ TEST D'EXISTENCE DES CARTES DE PESA ET ROTA -----------------

      CALL TECACH('ONN','PPESANR',1,IP,IRET)
      CALL TECACH('ONN','PROTATR',1,IR,IRET)
      IF (IP.NE.0 .OR. IR.NE.0) THEN
        CALL JEVECH('PMATERC','L',IMATE)
        CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,CODRET)
        CALL RCVALA(ZI(IMATE),' ',PHENOM,1,' ',R8BID,1,'RHO',RHO,CODRET,
     &              'FM')

C -----------CALCUL DE LA FORCE DE PESANTEUR --------------------------

        IF (IP.NE.0) THEN
          CALL JEVECH('PPESANR','L',IPES)
          FPX = RHO*ZR(IPES)*ZR(IPES+1)
          FPY = RHO*ZR(IPES)*ZR(IPES+2)
          FPZ = RHO*ZR(IPES)*ZR(IPES+3)
        END IF

C -----------CALCUL DE LA FORCE DE ROTATION AUX POINTS DE GAUSS--------

        IF (IR.NE.0) THEN
          CALL JEVECH('PROTATR','L',IROT)
          CALL RESR3D(ZR(IROT),ZR(IGEOM),ZR(IVF),RHO,NNO,NPG1,FRX,FRY,
     &                FRZ)
        END IF
      END IF

C --------- CALCUL DU PREMIER TERME DE L'ERREUR (RESIDUS D'EQUILIBRE)---
C --------- *********************************** ------------------------


      TER = 0.D0
      NORSIG = 0.D0
      DO 30 KP = 1,NPG1
        L = (KP-1)*NNO
        CALL DFDM3D(NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,DFDZ,POID)

C ----------CALCUL DE LA DIVERGENCE ET DE LA NORME DE SIGMA -----------

        DSIG11 = 0.D0
        DSIG12 = 0.D0
        DSIG13 = 0.D0
        DSIG21 = 0.D0
        DSIG22 = 0.D0
        DSIG23 = 0.D0
        DSIG31 = 0.D0
        DSIG32 = 0.D0
        DSIG33 = 0.D0

        SPG11 = 0.D0
        SPG22 = 0.D0
        SPG33 = 0.D0
        SPG12 = 0.D0
        SPG13 = 0.D0
        SPG23 = 0.D0

        DO 20 I = 1,NNO
          SIG11 = ZR(IAD-1+NBCMP* (I-1)+1)
          SIG22 = ZR(IAD-1+NBCMP* (I-1)+2)
          SIG33 = ZR(IAD-1+NBCMP* (I-1)+3)
          SIG12 = ZR(IAD-1+NBCMP* (I-1)+4)
          SIG13 = ZR(IAD-1+NBCMP* (I-1)+5)
          SIG23 = ZR(IAD-1+NBCMP* (I-1)+6)

          DSIG11 = DSIG11 + SIG11*DFDX(I)
          DSIG12 = DSIG12 + SIG12*DFDY(I)
          DSIG13 = DSIG13 + SIG13*DFDZ(I)
          DSIG21 = DSIG21 + SIG12*DFDX(I)
          DSIG22 = DSIG22 + SIG22*DFDY(I)
          DSIG23 = DSIG23 + SIG23*DFDZ(I)
          DSIG31 = DSIG31 + SIG13*DFDX(I)
          DSIG32 = DSIG32 + SIG23*DFDY(I)
          DSIG33 = DSIG33 + SIG33*DFDZ(I)

          SPG11 = SPG11 + SIG11*ZR(IVF+L+I-1)
          SPG22 = SPG22 + SIG22*ZR(IVF+L+I-1)
          SPG33 = SPG33 + SIG33*ZR(IVF+L+I-1)
          SPG12 = SPG12 + SIG12*ZR(IVF+L+I-1)
          SPG13 = SPG13 + SIG13*ZR(IVF+L+I-1)
          SPG23 = SPG23 + SIG23*ZR(IVF+L+I-1)
   20   CONTINUE

        DSX = DSIG11 + DSIG12 + DSIG13
        DSY = DSIG21 + DSIG22 + DSIG23
        DSZ = DSIG31 + DSIG32 + DSIG33

        FORX = ZR(IFOR+3*KP-3) + FPX + FRX(KP)
        FORY = ZR(IFOR+3*KP-2) + FPY + FRY(KP)
        FORZ = ZR(IFOR+3*KP-1) + FPZ + FRZ(KP)

        TER = TER + ((FORX+DSX)**2+ (FORY+DSY)**2+ (FORZ+DSZ)**2)*POID
        NOR = SPG11**2 + SPG22**2 + SPG33**2 +
     &        2.D0* (SPG12**2+SPG13**2+SPG23**2)
        NORSIG = NORSIG + NOR*POID
   30 CONTINUE


C ----------- CALCUL DU DEUXIEME ET TROISIEME TERME DE L'ERREUR ------

      CALL JEVECH('PFORCE','L',IREF1)
      CALL JEVECH('PPRESS','L',IREF2)
      CALL JEVECH('PVOISIN','L',IVOIS)

C -------- RECHERCHE DES ADRESSES POUR OBTENIR SIGMA SUR LES VOISINS ---

      IAREPE = ZI(IREF1)
      JCELD = ZI(IREF1+1)
      JCELV = ZI(IREF1+2)
      IAGD = ZI(IREF1+4)

C ------- RECHERCHE DES ADRESSES POUR LES CHARGES SUR LES FACES ----

        IADE1 = ZI(IREF1+6)
        IAVA1 = ZI(IREF1+7)
        IAPTM1 = ZI(IREF1+8)
      IF (IADE1 .NE. 0) THEN
        IGD1 = ZI(IADE1)
        IACMP = ZI(IREF1+5)
        NCMPM1= ZI(IACMP-1+IGD1)
C       CALL JELIRA (JEXNUM('&CATA.GD.NOMCMP',IGD),'LONMAX',NCMPM1,KBID)
      ENDIF
C
        IADE2 = ZI(IREF2+6)
        IAVA2 = ZI(IREF2+7)
        IAPTM2 = ZI(IREF2+8)
      IF (IADE2 .NE. 0) THEN
        IGD2 = ZI(IADE2)
        IACMP = ZI(IREF2+5)
        NCMPM2= ZI(IACMP-1+IGD2)
C       CALL JELIRA (JEXNUM('&CATA.GD.NOMCMP',IGD),'LONMAX',NCMPM2,KBID)
      ENDIF
C
C --------- TEST SUR LE TYPE DE LA MAILLE COURANTE --------------------

      IATYMA = ZI(IREF1+3)

C      CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',TY),TYPEMA)


C --------- INITIALISATION DES FACES DES ELEMENTS 3D ------------------

      ELREF2 = ' '
      IF (ELREFE.EQ.'HE8') THEN
        NBF = 6
        NSOMM = 4
        ITYP = 1
        ELREFE = 'QU4'
        NBNV = 8
        NDEGRE = 1
        DO 40 I = 1,4
          POIDS1(I) = 1.D0
   40   CONTINUE
      ELSE IF (ELREFE.EQ.'H20') THEN
        NBF = 6
        NSOMM = 4
        ITYP = 1
        ELREFE = 'QU8'
        NBNV = 20
        NDEGRE = 2
        DO 50 I = 1,4
          POIDS1(I) = 1.D0/9.D0
   50   CONTINUE
        DO 60 I = 5,8
          POIDS1(I) = 4.D0/9.D0
   60   CONTINUE
        POIDS1(9) = 16.D0/9.D0
      ELSE IF (ELREFE.EQ.'H27') THEN
        NBF = 6
        ITYP = 1
        ELREFE = 'QU9'
        NBNV = 27
        NDEGRE = 2
        DO 70 I = 1,4
          POIDS1(I) = 1.D0/9.D0
   70   CONTINUE
        DO 80 I = 5,8
          POIDS1(I) = 4.D0/9.D0
   80   CONTINUE
        POIDS1(9) = 16.D0/9.D0

      ELSE IF (ELREFE.EQ.'PE6') THEN
        NBF = 5
        ITYP = 2
        ELREFE = 'TR3'
        ELREF2 = 'QU4'
        NBNV = 6
        NDEGRE = 1
        DO 90 I = 1,3
          POIDS1(I) = 1.D0/6.D0
   90   CONTINUE
        DO 100 I = 1,4
          POIDS2(I) = 1.D0
  100   CONTINUE

      ELSE IF (ELREFE.EQ.'P15') THEN
        NBF = 5
        ITYP = 2
        ELREFE = 'TR6'
        ELREF2 = 'QU8'
        NBNV = 15
        NDEGRE = 2
        DO 110 I = 1,3
          POIDS1(I) = 0.D0
  110   CONTINUE
        DO 120 I = 4,6
          POIDS1(I) = 1.D0/6.D0
  120   CONTINUE
        DO 130 I = 1,4
          POIDS2(I) = 1.D0/9.D0
  130   CONTINUE
        DO 140 I = 5,8
          POIDS2(I) = 4.D0/9.D0
  140   CONTINUE
        POIDS2(9) = 16.D0/9.D0

      ELSE IF (ELREFE.EQ.'TE4') THEN
        NSOMM = 3
        NBF = 4
        ITYP = 3
        ELREFE = 'TR3'
        NBNV = 4
        NDEGRE = 1
        DO 150 I = 1,3
          POIDS1(I) = 1.D0/6.D0
  150   CONTINUE

      ELSE IF (ELREFE.EQ.'T10') THEN
        NSOMM = 3
        NBF = 4
        ITYP = 3
        ELREFE = 'TR6'
        NBNV = 10
        NDEGRE = 2
        DO 160 I = 1,3
          POIDS1(I) = 0.D0
  160   CONTINUE
        DO 170 I = 4,6
          POIDS1(I) = 1.D0/6.D0
  170   CONTINUE

      ELSE
        CALL UTMESS('F','TE0375','TYPE MAILLE INCONNU')
      END IF

      CALL ELREF4 ( ELREFE, 'NOEU', NDIM, NNO, NNOS, NPG, IPOIDS,
     &              IVF, IDFDX, JGANO )
      IDFDY = IDFDX + 1

CAS DES PENTAEDRES

      IF (NBF.EQ.5) THEN
         CALL ELREF4 ( ELREF2, 'NOEU', NDIM, NNO2, NNOS, NPG2, IPOIDS,
     &                 IFF2, IDFDX2, JGANO )
         IDFDY2 = IDFDX2 + 1
      END IF

      TER2 = 0.D0
      TER3 = 0.D0

C -------BOUCLE SUR LES FACES -----------------------------------------

      DO 300 IFA = 1,NBF

C ------TEST DU TYPE DE VOISIN -----------------------------------------

        TYV = ZI(IVOIS+7+IFA)
        IF (TYV.NE.0) THEN

          IF (IFA.GE.3 .AND. NBF.EQ.5) THEN
            NNO = NNO2
            NPG = NPG2
            IDFDX = IDFDX2
            IDFDY = IDFDY2
            DO 180 IPG = 1,NPG
              POIDS(IPG) = POIDS2(IPG)
  180       CONTINUE
            NSOMM = 4

          ELSE

            DO 190 IPG = 1,NPG
              POIDS(IPG) = POIDS1(IPG)
  190       CONTINUE
            NSOMM = 3

          END IF

C  CALCUL DES PRODUITS VECTORIELS OMI VECTORIEL OMJ

          DO 210 IN = 1,NNO
            INO = NOE(IN,IFA,ITYP)
            I = IGEOM + 3* (INO-1) - 1
            DO 200 JN = 1,NNO
              JNO = NOE(JN,IFA,ITYP)
              J = IGEOM + 3* (JNO-1) - 1
              SX(IN,JN) = ZR(I+2)*ZR(J+3) - ZR(I+3)*ZR(J+2)
              SY(IN,JN) = ZR(I+3)*ZR(J+1) - ZR(I+1)*ZR(J+3)
              SZ(IN,JN) = ZR(I+1)*ZR(J+2) - ZR(I+2)*ZR(J+1)
  200       CONTINUE
  210     CONTINUE

C  CALCUL DU DIAMETRE HF DE LA FACE

          NDI = 0
          NIV = 1
          CALL UTHK(NOMTE,IGEOM,HF,NDI,NOE,NSOMM,ITYP,IFA,NIV,IFM)
          TYPMAV=ZK8(IATYMA-1+TYV)      

          IF (TYPMAV(1:4).EQ.'QUAD' .OR. TYPMAV(1:4).EQ.'TRIA') THEN

C ----------CALCUL DU 3 IEME TERME D'ERREUR ----------------------------

          IF (IADE1 .NE. 0) THEN

            IMAV = ZI(IVOIS+IFA)
            IF (IAPTM1 .EQ. 0) THEN
C              CARTE CONSTANTE
               IENT1 = 1
            ELSE
C            LA CARTE A ETE ETENDUE
               IENT1 = ZI(IAPTM1 -1 +IMAV)
            ENDIF
            NUMGD1 = ZI(IREF1+9)
            NOMGD1 = ZK8(IAGD-1+NUMGD1)
          ENDIF
C
          IF (IADE2 .NE. 0) THEN

            IMAV = ZI(IVOIS+IFA)
            IF (IAPTM2 .EQ. 0) THEN
C              CARTE CONSTANTE
               IENT2 = 1
            ELSE
C            LA CARTE A ETE ETENDUE
               IENT2 = ZI(IAPTM2 -1 +IMAV)
            ENDIF
            NUMGD2 = ZI(IREF2+9)
            NOMGD2 = ZK8(IAGD-1+NUMGD2)
          ENDIF

            PRES = .FALSE.
            FORC = .FALSE.

C   BOUCLE SUR LES PTS D'INTEGRATION DE LA FACE

            NORM = 0.0D0
            DO 250 IPG = 1,NPG

C --- CALCUL DE LA NORMALE AU POINT IPG  -----------------

              KDEC = (IPG-1)*NNO*NDIM

              NX = 0.0D0
              NY = 0.0D0
              NZ = 0.0D0

              DO 230 I = 1,NNO
                IDEC = (I-1)*NDIM
                DO 220 J = 1,NNO
                  JDEC = (J-1)*NDIM
                  NX = NX - ZR(IDFDX+KDEC+IDEC)*ZR(IDFDY+KDEC+JDEC)*
     &                 SX(I,J)
                  NY = NY - ZR(IDFDX+KDEC+IDEC)*ZR(IDFDY+KDEC+JDEC)*
     &                 SY(I,J)
                  NZ = NZ - ZR(IDFDX+KDEC+IDEC)*ZR(IDFDY+KDEC+JDEC)*
     &                 SZ(I,J)
  220           CONTINUE
  230         CONTINUE

C   JACOBIEN
              JACO = SQRT(NX*NX+NY*NY+NZ*NZ)
              NX=NX/JACO
              NY=NY/JACO
              NZ=NZ/JACO
C
              INO = NOE(IPG,IFA,ITYP)
              IDEC = NBCMP* (INO-1)

C ---RECUPERATION DES CONTRAINTES AUX PTS DE GAUSS ----------

              DO 240 ICMP = 1,6
                SIGDIF(ICMP) = ZR(IAD+IDEC-1+ICMP)
  240         CONTINUE

              IF (NPG.EQ.6 .AND. IPG.LE.3) GO TO 250

C --- RECUPERATION DE LA PRESSION ----------------

              IF (NOMGD2(1:6).EQ.'PRES_R') THEN
                PRES = .TRUE.
                PRE = ZR(IAVA2-1+ (IENT2-1)*NCMPM2+1)
                PR(IPG) = PRE

              ELSE IF (NOMGD2(1:6).EQ.'PRES_F') THEN
                PRES = .TRUE.
                PRF = ZK8(IAVA2-1+ (IENT2-1)*NCMPM2+1)

                PR(IPG) = 0.D0
                IF (PRF.NE.'&FOZERO') THEN
                  NOMPAR(1) = 'X'
                  NOMPAR(2) = 'Y'
                  NOMPAR(3) = 'Z'
                  NOMPAR(4) = 'INST'
                  VALPAR(1) = ZR(IGEOM+3*INO-3)
                  VALPAR(2) = ZR(IGEOM+3*INO-2)
                  VALPAR(3) = ZR(IGEOM+3*INO-1)
                  VALPAR(4) = INST
                  CALL FOINTE('FM',PRF,4,NOMPAR,VALPAR,PR(IPG),IER)
                END IF
              END IF

C --- RECUPERATION DES FORCES  ----------------

              IF (NOMGD1(1:6).EQ.'FORC_R') THEN
                FXR = ZR(IAVA1-1+ (IENT1-1)*NCMPM1+1)
                FYR = ZR(IAVA1-1+ (IENT1-1)*NCMPM1+2)
                FZR = ZR(IAVA1-1+ (IENT1-1)*NCMPM1+3)

                IF (ABS(FXR).GT.1.D-15 .OR. ABS(FYR).GT.1.D-15 .OR.
     &              ABS(FZR).GT.1.D-15) THEN
                  FX(IPG) = FXR
                  FY(IPG) = FYR
                  FZ(IPG) = FZR
                  FORC = .TRUE.
                END IF

              ELSE IF (NOMGD1(1:6).EQ.'FORC_F') THEN
                FXF = ZK8(IAVA1-1+ (IENT1-1)*NCMPM1+1)
                FYF = ZK8(IAVA1-1+ (IENT1-1)*NCMPM1+2)
                FZF = ZK8(IAVA1-1+ (IENT1-1)*NCMPM1+3)

                FX(IPG) = 0.D0
                FY(IPG) = 0.D0
                FZ(IPG) = 0.D0
                IF (FXF.NE.'&FOZERO' .OR. FYF.NE.'&FOZERO' .OR.
     &              FZF.NE.'&FOZERO') THEN

                  NOMPAR(1) = 'X'
                  NOMPAR(2) = 'Y'
                  NOMPAR(3) = 'Z'
                  NOMPAR(4) = 'INST'
                  VALPAR(1) = ZR(IGEOM+3*INO-3)
                  VALPAR(2) = ZR(IGEOM+3*INO-2)
                  VALPAR(3) = ZR(IGEOM+3*INO-1)
                  VALPAR(4) = INST
                  CALL FOINTE('FM',FXF,4,NOMPAR,VALPAR,FX(IPG),IER1)
                  CALL FOINTE('FM',FYF,4,NOMPAR,VALPAR,FY(IPG),IER2)
                  CALL FOINTE('FM',FZF,4,NOMPAR,VALPAR,FZ(IPG),IER3)
                  FORC = .TRUE.

                END IF
              END IF

C --- CALCUL DU PRODUIT SCALAIRE SIGMA . N - G ----------------

              IF (PRES) THEN
                NORM = NORM + ((NX*SIGDIF(1)+NY*SIGDIF(4)+NZ*SIGDIF(5)+
     &                 PR(IPG)*NX)**2+ (NX*SIGDIF(4)+NY*SIGDIF(2)+
     &                 NZ*SIGDIF(6)+PR(IPG)*NY)**2+
     &                 (NX*SIGDIF(5)+NY*SIGDIF(6)+NZ*SIGDIF(3)+
     &                 PR(IPG)*NZ)**2)*POIDS(IPG)
              END IF
              IF (FORC) THEN
                NORM = NORM + ((NX*SIGDIF(1)+NY*SIGDIF(4)+NZ*SIGDIF(5)-
     &                 FX(IPG))**2+ (NX*SIGDIF(4)+NY*SIGDIF(2)+
     &                 NZ*SIGDIF(6)-FY(IPG))**2+
     &                 (NX*SIGDIF(5)+NY*SIGDIF(6)+NZ*SIGDIF(3)-
     &                 FZ(IPG))**2)*POIDS(IPG)
              END IF
  250       CONTINUE

            IF (NORM.LT.0.D0) NORM = -NORM
            TER3 = TER3 + SQRT(HF)*SQRT(NORM)

C ---------- CALCUL DU DEUXIEME TERME ----------------------------------

          ELSE IF (TYPMAV(1:4).EQ.'HEXA' .OR. TYPMAV(1:4).EQ.'PENT' .OR.
     &             TYPMAV(1:4).EQ.'TETR') THEN

CAS DES PENTAEDRES

            IF (NBF.EQ.5) THEN
              IF (TYPMAV(1:4).EQ.'HEXA' .OR.
     &            (TYPMAV(1:4).EQ.'PENT'.AND.IFA.GE.3)) THEN
                NNO = NNO2
                NPG = NPG2
                IDFDX = IDFDX2
                IDFDY = IDFDY2
              END IF
            END IF

CALCUL DE NUMEROTATION DU VOISIN -------

            ICONX1 = ZI(IREF1+10)
            ICONX2 = ZI(IREF1+11)
            JAD  = ICONX1-1+ZI(ICONX2+ZI(IVOIS)-1)
            JADV = ICONX1-1+ZI(ICONX2+ZI(IVOIS+IFA)-1)
C
            IMAV = ZI(IVOIS+IFA)
            IGREL = ZI(IAREPE+2* (IMAV-1))
            IEL = ZI(IAREPE+2* (IMAV-1)+1)
            IAVAL1 = JCELV - 1 + ZI(JCELD-1+ZI(JCELD-1+4+IGREL)+8)

C   BOUCLE SUR LES PTS D'INTEGRATION DE LA FACE

            NORM = 0.0D0
            DO 290 IPG = 1,NPG

C --- CALCUL DE LA NORMALE AU POINT IPG  -----------------

              KDEC = (IPG-1)*NNO*NDIM

              NX = 0.0D0
              NY = 0.0D0
              NZ = 0.0D0
              DO 270 I = 1,NNO
                IDEC = (I-1)*NDIM
                DO 260 J = 1,NNO
                  JDEC = (J-1)*NDIM

                  NX = NX - ZR(IDFDX+KDEC+IDEC)*ZR(IDFDY+KDEC+JDEC)*
     &                 SX(I,J)
                  NY = NY - ZR(IDFDX+KDEC+IDEC)*ZR(IDFDY+KDEC+JDEC)*
     &                 SY(I,J)
                  NZ = NZ - ZR(IDFDX+KDEC+IDEC)*ZR(IDFDY+KDEC+JDEC)*
     &                 SZ(I,J)
  260           CONTINUE
  270         CONTINUE

C   JACOBIEN
              JACO = SQRT(NX*NX+NY*NY+NZ*NZ)

              INO = NOE(IPG,IFA,ITYP)
              NCHER = ZI(JAD-1+INO)
              INOV = INDIIS(ZI(JADV),NCHER,1,NBNV)
              IDEC1 = NBCMP* (INO-1)
              IDEC2 = NBCMP* (INOV-1) + NBCMP*NBNV* (IEL-1)

C --- CALCUL DES SAUTS DE CONTRAINTES -----------------

              DO 280 ICMP = 1,6
                SIGDIF(ICMP) = ZR(IAD+IDEC1-1+ICMP) -
     &                         ZR(IAVAL1+IDEC2-1+ICMP)
  280         CONTINUE

              IF (NPG.EQ.6 .AND. IPG.LE.3) GO TO 290

C --- CALCUL DU PRODUIT SCALAIRE SIGMA . N -----------------

              NORM = NORM + ((NX*SIGDIF(1)+NY*SIGDIF(4)+NZ*SIGDIF(5))**
     &               2+ (NX*SIGDIF(4)+NY*SIGDIF(2)+NZ*SIGDIF(6))**2+
     &               (NX*SIGDIF(5)+NY*SIGDIF(6)+NZ*SIGDIF(3))**2)*
     &               POIDS(IPG)/JACO
  290       CONTINUE

            IF (NORM.LT.0.D0) NORM = -NORM
            TER2 = TER2 + 0.5D0*SQRT(HF)*SQRT(NORM)


          ELSE
            CALL UTMESS('F','TE0375','TYPE MAILLE INCONNU')
          END IF
        END IF
  300 CONTINUE

C------------MISE EN MEMOIRE DES DIFFERENTS TERMES DE L'ERREUR ---------

      IF (NDEGRE.EQ.2) THEN
        COEFF = SQRT(96.D0)
      ELSE IF (NDEGRE.EQ.1) THEN
        COEFF = SQRT(24.D0)
      END IF

      ERREST = (H*SQRT(TER)+TER2+TER3)/COEFF
C      TER1 = H * SQRT(TER)/COEFF
      TER2 = TER2/COEFF
      TER3 = TER3/COEFF
      SIGCAL = SQRT(NORSIG)
      NUEST = 100.D0*SQRT(ERREST**2/ (ERREST**2+NORSIG))

      ZR(IERR) = ERREST
      ZR(IERR+1) = NUEST
      ZR(IERR+2) = SIGCAL

  310 CONTINUE
      CALL JEDEMA()
      END
