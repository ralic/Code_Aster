      SUBROUTINE TE0375 ( OPTION , NOMTE )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/09/2002   AUTEUR G8BHHXD X.DESROCHES 
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
C TOLE CRP_20
C
C    - FONCTION REALISEE:  CALCUL DE L'ERREUR SUR UN ELEMENT 3D AVEC LA
C                          METHODE DES RESIDUS
C                          OPTION : 'ERRE_ELGA_NORE  '
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
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
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
C
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER            NPG1, I, K, KP, NNO, TY, TYV, NBPG(10)
      INTEGER            IFF,IPOIDS,IVF,IDFDE,IDFDK,IDFDN,IGEOM,IFM
      INTEGER            IAD, IFOR, IERR, IPES, IROT, IMATE, IADE1,IAVA1
      INTEGER            IREF, IVOIS, IAREPE, JCELD, JCELV, IAVAL1
      INTEGER            JAD, JADV, IGREL, IEL,  IGD, NCMPM1
      INTEGER            JNO, NBNV, NCHER
      INTEGER            INOV, IMAV, JKP, NOE(9,6,3)
      REAL*8             A(3,3), B(3), H1, H2, H3, H4
      REAL*8             DFDX(27), DFDY(27) ,DFDZ(27), H, HH, POID
      REAL*8             FORX, FORY, FORZ, FPX, FPY, FPZ
      REAL*8             FRX(27), FRY(27), FRZ(27)
      REAL*8             FX(9), FY(9), FZ(9)
      REAL*8             POIDS1(9), POIDS2(9), POIDS(9), RHO
      REAL*8             SIG11,SIG22,SIG33,SIG12,SIG13,SIG23,SIGDIF(6)
      REAL*8             DSIG11,DSIG22,DSIG33,DSIG12,DSIG13,DSIG23
      REAL*8             DSIG21,DSIG31,DSIG32
      REAL*8             SPG11,SPG22,SPG33,SPG12,SPG13,SPG23
      REAL*8             DSX,DSY,DSZ,SX(9,9),SY(9,9),SZ(9,9)
      REAL*8             TER, ERREST, NOR, NORSIG, SIGCAL, NUEST, COEFF
      REAL*8             TER2, TER3, HF, NORM, NX, NY, NZ, JACO, JACOB
      REAL*8             PR(9), VALPAR(4), INST, ZERO
      LOGICAL            PRES, FORC,FAUX
      CHARACTER*2        CODRET
      CHARACTER*4        NOMPAR(4)
      CHARACTER*8        K8B, MA, TYPEMA, TYPMAV, ELREFE

      CHARACTER*8        PRF,  FXF, FYF, FZF, ELREF2
      CHARACTER*16       PHENOM
      CHARACTER*19       MO, SIGMA, CARTE1, NOMGD1, CARTE2, NOMGD2
      CHARACTER*24       CARAC, CARAC2, CHVAL, CHVAL2



C --- INITIALISATION DU TABLEAU DES NUMEROS DE NOEUDS FACE PAR FACE
C
C    HEXAEDRES A 8,20 ET 27 NOEUDS
C    PENTAEDRES A 6 ET 15 NOEUDS
      DATA NOE/1,2,3,4,9,10,11,12,21,  3,7,8,4,15,19,16,11,24,
     &         5,8,7,6,20,19,18,17,26, 1,5,6,2,13,17,14,9,22,
     &         2,6,7,3,14,18,15,10,23, 4,8,5,1,16,20,13,12,25,
     &         1,2,3,7,8,9,3*0,        4,6,5,15,14,13,3*0,
     &         1,4,5,2,10,13,11,7,0,   2,5,6,3,11,14,12,8,0,
     &         1,3,6,4,9,12,15,10,0,   9*0,
     &         1,2,3,5,6,7,3*0,        2,4,3,9,10,6,3*0,
     &         3,4,1,10,8,7,3*0,       1,4,2,8,9,5,3*0,
     &         9*0,                    9*0/
C    TETRAEDRES A 4 ET 10 NOEUDS
C
      FAUX = .FALSE.
      ZERO = 0.D0
C
      CALL JEMARQ()
      CALL ELREF1(ELREFE)
C
      CARAC = '&INEL.'//ELREFE//'.CARACTE'
      CALL JEVETE(CARAC,'L',JIN)
      NDIM  = ZI(JIN+1-1)
      NNO   = ZI(JIN+2-1)
      NBFPG = ZI(JIN+3-1)
      DO 110 I = 1,NBFPG
         NBPG(I) = ZI(JIN+3-1+I)
  110 CONTINUE
      NPG1 = NBPG(1)
      CHVAL = '&INEL.'//ELREFE//'.FFORMES'
      CALL JEVETE(CHVAL,'L',JVAL)
      IPOIDS = JVAL + (NDIM+1)*NNO*NNO
      IVF    = IPOIDS + NPG1
      IDFDE  = IVF    + NPG1*NNO
      IDFDN  = IDFDE  + 1
      IDFDK  = IDFDN  + 1
C
      CALL JEVECH ('PGEOMER', 'L', IGEOM )
      CALL JEVECH ('PCONTNO', 'L', IAD   )
      CALL JEVECH ('PFRVOLU', 'L', IFOR  )
      CALL JEVECH ('PERREUR', 'E', IERR  )
      CALL JEVECH ('PTEMPSR', 'L', JTIME )
      INST = ZR(JTIME-1+1)
C
C -------- VERIFICATION DU SIGNE DU JACOBIEN :ARRET SI JACOB < 0
C
       IFM = IUNIFI('MESSAGE')
       CALL UTJAC(.FALSE.,IGEOM,IDFDE,IDFDK,IDFDN,1,IFM,NNO,JACOB)  
C
C -------- CALCUL DU DIAMETRE H ---------------------------------------
C
      IF (ELREFE .EQ. 'TETRA4  '.OR. ELREFE.EQ.'TETRA10 ') THEN
C
C   TETRAEDRE : H = DIAMETRE DU CERCLE CIRCONSCRIT
C
        A(1,1) = ZR(IGEOM+3) - ZR(IGEOM)
        A(2,1) = ZR(IGEOM+6) - ZR(IGEOM)
        A(3,1) = ZR(IGEOM+9) - ZR(IGEOM)
        A(1,2) = ZR(IGEOM+4) - ZR(IGEOM+1)
        A(2,2) = ZR(IGEOM+7) - ZR(IGEOM+1)
        A(3,2) = ZR(IGEOM+10) - ZR(IGEOM+1)
        A(1,3) = ZR(IGEOM+5)  - ZR(IGEOM+2)
        A(2,3) = ZR(IGEOM+8)  - ZR(IGEOM+2)
        A(3,3) = ZR(IGEOM+11) - ZR(IGEOM+2)
        B(1) =   (A(1,1)*(ZR(IGEOM+3) + ZR(IGEOM))
     &           +A(1,2)*(ZR(IGEOM+4) + ZR(IGEOM+1))
     &           +A(1,3)*(ZR(IGEOM+5) + ZR(IGEOM+2)))/2.0D0
        B(2) =   (A(2,1)*(ZR(IGEOM+6) + ZR(IGEOM))
     &           +A(2,2)*(ZR(IGEOM+7) + ZR(IGEOM+1))
     &           +A(2,3)*(ZR(IGEOM+8) + ZR(IGEOM+2)))/2.0D0
        B(3) =   (A(3,1)*(ZR(IGEOM+9) + ZR(IGEOM))
     &           +A(3,2)*(ZR(IGEOM+10) + ZR(IGEOM+1))
     &           +A(3,3)*(ZR(IGEOM+11) + ZR(IGEOM+2)))/2.0D0
C
        CALL MGAUSS(A,B,3,3,1,ZERO,FAUX)
        H = 2.D0 * SQRT((ZR(IGEOM)-B(1))**2+(ZR(IGEOM+1)-B(2))**2
     &               +(ZR(IGEOM+2)-B(3))**2)


C
       ELSE IF ( ELREFE .EQ. 'HEXA8   '.OR.
     &      ELREFE .EQ. 'HEXA20  '.OR.
     &      ELREFE .EQ. 'HEXA27  ') THEN
C
C   HEXAEDRE : H = PLUS GRANDE DIAGONALE
C
        H1 = SQRT((ZR(IGEOM) - ZR(IGEOM+18))**2 +
     &            (ZR(IGEOM+1) - ZR(IGEOM+19))**2 +
     &            (ZR(IGEOM+2) - ZR(IGEOM+20))**2)
        H2 = SQRT((ZR(IGEOM+3) - ZR(IGEOM+21))**2 +
     &            (ZR(IGEOM+4) - ZR(IGEOM+22))**2 +
     &            (ZR(IGEOM+5) - ZR(IGEOM+23))**2)
        IF (H1.GT.H2) THEN
          H = H1
        ELSE
          H = H2
        ENDIF
        H3 = SQRT((ZR(IGEOM+6) - ZR(IGEOM+12))**2 +
     &            (ZR(IGEOM+7) - ZR(IGEOM+13))**2 +
     &            (ZR(IGEOM+8) - ZR(IGEOM+14))**2)
        IF (H3.GT.H) THEN
          H = H3
        ENDIF
        H4 = SQRT((ZR(IGEOM+9) - ZR(IGEOM+15))**2 +
     &            (ZR(IGEOM+10) - ZR(IGEOM+16))**2 +
     &            (ZR(IGEOM+11) - ZR(IGEOM+17))**2)
        IF (H4.GT.H) THEN
          H = H4
        ENDIF

C
       ELSE IF (ELREFE .EQ. 'PENTA6  '.OR.
     &          ELREFE .EQ. 'PENTA15 ') THEN
C
C   PENTAEDRE : H = PLUS GRANDE DIAGONALE
C
        H1 = SQRT((ZR(IGEOM) - ZR(IGEOM+12))**2 +
     &            (ZR(IGEOM+1) - ZR(IGEOM+13))**2 +
     &            (ZR(IGEOM+2) - ZR(IGEOM+14))**2)
        H2 = SQRT((ZR(IGEOM+3) - ZR(IGEOM+15))**2 +
     &            (ZR(IGEOM+4) - ZR(IGEOM+16))**2 +
     &            (ZR(IGEOM+5) - ZR(IGEOM+17))**2)
        IF (H1.GT.H2) THEN
          H = H1
        ELSE
          H = H2
        ENDIF
        H3 = SQRT((ZR(IGEOM+6) - ZR(IGEOM+9))**2 +
     &            (ZR(IGEOM+7) - ZR(IGEOM+10))**2 +
     &            (ZR(IGEOM+8) - ZR(IGEOM+11))**2)
        IF (H3.GT.H) THEN
          H = H3
        ENDIF
        H1 = SQRT((ZR(IGEOM) - ZR(IGEOM+15))**2 +
     &            (ZR(IGEOM+1) - ZR(IGEOM+16))**2 +
     &            (ZR(IGEOM+2) - ZR(IGEOM+17))**2)
        H2 = SQRT((ZR(IGEOM+3) - ZR(IGEOM+9))**2 +
     &            (ZR(IGEOM+4) - ZR(IGEOM+10))**2 +
     &            (ZR(IGEOM+5) - ZR(IGEOM+11))**2)
        IF (H1.GT.H2) THEN
          HH = H1
        ELSE
          HH = H2
        ENDIF
        H3 = SQRT((ZR(IGEOM+6) - ZR(IGEOM+12))**2 +
     &            (ZR(IGEOM+7) - ZR(IGEOM+13))**2 +
     &            (ZR(IGEOM+8) - ZR(IGEOM+14))**2)
        IF (H3.GT.HH) THEN
          HH = H3
        ENDIF
        IF (HH.GT.H) THEN
          H = HH
        ENDIF

      ELSE
        CALL UTMESS ('F','TE0375','TYPE ELEMENT INCONNU')
      ENDIF
C
C ------ INITIALISATION DES FORCES --------------------------------
C
      FPX = 0.D0
      FPY = 0.D0
      FPZ = 0.D0
      DO 50 JKP =1, 27
        FRX(JKP) = 0.D0
        FRY(JKP) = 0.D0
        FRZ(JKP) = 0.D0
   50 CONTINUE
C
C ------ TEST D'EXISTENCE DES CARTES DE PESA ET ROTA -----------------
C
      CALL TECACH(.TRUE.,.FALSE.,'PPESANR',1,IP)
      CALL TECACH(.TRUE.,.FALSE.,'PROTATR',1,IR)
      IF (IP .NE. 0 .OR. IR .NE.0) THEN
         CALL JEVECH ('PMATERC','L',IMATE)
         CALL RCCOMA ( ZI(IMATE),'ELAS',PHENOM,CODRET)
         CALL RCVALA ( ZI(IMATE),PHENOM,1,' ',R8BID,1,'RHO',
     +                 RHO, CODRET, 'FM' )
C
C -----------CALCUL DE LA FORCE DE PESANTEUR --------------------------
C
         IF (IP .NE. 0) THEN
           CALL JEVECH('PPESANR','L',IPES)
           FPX = RHO * ZR(IPES) * ZR(IPES+1)
           FPY = RHO * ZR(IPES) * ZR(IPES+2)
           FPZ = RHO * ZR(IPES) * ZR(IPES+3)
         ENDIF
C
C -----------CALCUL DE LA FORCE DE ROTATION AUX POINTS DE GAUSS--------
C
         IF (IR .NE. 0)THEN
           CALL JEVECH('PROTATR','L',IROT)
           CALL RESR3D ( ZR(IROT), ZR(IGEOM), ZR(IVF), RHO, NNO, NPG1,
     &              FRX, FRY, FRZ)
         ENDIF
      ENDIF
C
C --------- CALCUL DU PREMIER TERME DE L'ERREUR (RESIDUS D'EQUILIBRE)---
C --------- *********************************** ------------------------
C

      TER = 0.D0
      NORSIG = 0.D0
      DO 200 KP=1,NPG1
        L = (KP-1)*NNO
        K = L*3
        CALL DFDM3D(NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDN+K),
     &              ZR(IDFDK+K),ZR(IGEOM),DFDX,DFDY,DFDZ,POID)
C
C ----------CALCUL DE LA DIVERGENCE ET DE LA NORME DE SIGMA -----------
C
        DSIG11 = 0.D0
        DSIG12 = 0.D0
        DSIG13 = 0.D0
        DSIG21 = 0.D0
        DSIG22 = 0.D0
        DSIG23 = 0.D0
        DSIG31 = 0.D0
        DSIG32 = 0.D0
        DSIG33 = 0.D0
C
        SPG11 = 0.D0
        SPG22 = 0.D0
        SPG33 = 0.D0
        SPG12 = 0.D0
        SPG13 = 0.D0
        SPG23 = 0.D0
C
          DO 150 I=1,NNO
            SIG11 = ZR(IAD-1+6*(I-1)+1)
            SIG22 = ZR(IAD-1+6*(I-1)+2)
            SIG33 = ZR(IAD-1+6*(I-1)+3)
            SIG12 = ZR(IAD-1+6*(I-1)+4)
            SIG13 = ZR(IAD-1+6*(I-1)+5)
            SIG23 = ZR(IAD-1+6*(I-1)+6)
C
            DSIG11 = DSIG11+SIG11*DFDX(I)
            DSIG12 = DSIG12+SIG12*DFDY(I)
            DSIG13 = DSIG13+SIG13*DFDZ(I)
            DSIG21 = DSIG21+SIG12*DFDX(I)
            DSIG22 = DSIG22+SIG22*DFDY(I)
            DSIG23 = DSIG23+SIG23*DFDZ(I)
            DSIG31 = DSIG31+SIG13*DFDX(I)
            DSIG32 = DSIG32+SIG23*DFDY(I)
            DSIG33 = DSIG33+SIG33*DFDZ(I)
C
            SPG11 = SPG11 + SIG11*ZR(IVF+L+I-1)
            SPG22 = SPG22 + SIG22*ZR(IVF+L+I-1)
            SPG33 = SPG33 + SIG33*ZR(IVF+L+I-1)
            SPG12 = SPG12 + SIG12*ZR(IVF+L+I-1)
            SPG13 = SPG13 + SIG13*ZR(IVF+L+I-1)
            SPG23 = SPG23 + SIG23*ZR(IVF+L+I-1)
  150     CONTINUE
C
        DSX = DSIG11+DSIG12+DSIG13
        DSY = DSIG21+DSIG22+DSIG23
        DSZ = DSIG31+DSIG32+DSIG33
C
        FORX = ZR(IFOR+3*KP-3) + FPX + FRX(KP)
        FORY = ZR(IFOR+3*KP-2) + FPY + FRY(KP)
        FORZ = ZR(IFOR+3*KP-1) + FPZ + FRZ(KP)
C
        TER = TER + ((FORX+DSX)**2+(FORY+DSY)**2+(FORZ+DSZ)**2)*POID
        NOR = SPG11**2 + SPG22**2 + SPG33**2
     &   +2.D0*(SPG12**2 + SPG13**2 + SPG23**2)
        NORSIG = NORSIG + NOR * POID
  200 CONTINUE
C


C
C ----------- CALCUL DU DEUXIEME ET TROISIEME TERME DE L'ERREUR ------
C
      CALL JEVECH('PFORCE','L',IREF)
      CALL JEVECH('PPRESS','L',IREF2)
      CALL JEVECH('PVOISIN','L',IVOIS)
C
      MA = ZK24(IREF)
      MO = ZK24(IREF+1)
      SIGMA  = ZK24(IREF+2)
      CARTE1 = ZK24(IREF+3)
      CARTE2 = ZK24(IREF2+3)
C
C -------- RECHERCHE DES ADRESSES POUR OBTENIR SIGMA SUR LES VOISINS ---
C
      CALL JEVEUO(MO//'.REPE','L',IAREPE)
      CALL JEVEUO(SIGMA//'.CELD','L',JCELD)
      CALL JEVEUO(SIGMA//'.CELV','L',JCELV)
C
C ------- RECHERCHE DES ADRESSES POUR LES CHARGES SUR LES FACES ----
C
      IF (CARTE1 .NE. ' ') THEN
        CALL JEVEUO (CARTE1//'.DESC','L',IADE1)
        CALL JEVEUO (CARTE1//'.VALE','L',IAVA1)
        IGD = ZI(IADE1)

        CALL JELIRA (JEXNUM('&CATA.GD.NOMCMP',IGD),'LONMAX',NCMPM1,K8B)


      ENDIF
C
      IF (CARTE2 .NE. ' ') THEN
        CALL JEVEUO (CARTE2//'.DESC','L',IADE2)
        CALL JEVEUO (CARTE2//'.VALE','L',IAVA2)
        IGD = ZI(IADE2)

        CALL JELIRA (JEXNUM('&CATA.GD.NOMCMP',IGD),'LONMAX',NCMPM2,K8B)

      ENDIF
C
C --------- TEST SUR LE TYPE DE LA MAILLE COURANTE --------------------
C
      TY = ZI(IVOIS + 7)

      CALL JENUNO (JEXNUM('&CATA.TM.NOMTM',TY),TYPEMA)

C
C --------- INITIALISATION DES FACES DES ELEMENTS 3D ------------------
C
      IF (ELREFE . EQ. 'HEXA8   ') THEN
        NBF = 6
        NSOMM = 4
        ITYP = 1
        ELREFE = 'FACE4'
        NBNV = 8
        NDEGRE = 1
        DO 31 I=1,4
          POIDS1(I) = 1.D0
31      CONTINUE
      ELSE IF (ELREFE . EQ. 'HEXA20  ') THEN
        NBF = 6
        NSOMM = 4
        ITYP = 1
        ELREFE = 'FACE8'
        NBNV = 20
        NDEGRE = 2
        DO 32 I=1,4
          POIDS1(I) = 1.D0/9.D0
32      CONTINUE
        DO 33 I=5,8
          POIDS1(I) = 4.D0/9.D0
33      CONTINUE
        POIDS1(9) = 16.D0/9.D0
      ELSE IF (ELREFE . EQ. 'HEXA27  ') THEN
        NBF = 6
        ITYP = 1
        ELREFE = 'FACE9'
        NBNV = 27
        NDEGRE = 2
        DO 34 I=1,4
          POIDS1(I) = 1.D0/9.D0
34      CONTINUE
        DO 35 I=5,8
          POIDS1(I) = 4.D0/9.D0
35      CONTINUE
        POIDS1(9) = 16.D0/9.D0
C
      ELSE IF (ELREFE . EQ. 'PENTA6  ') THEN
        NBF = 5
        ITYP = 2
        ELREFE = 'FACE3'
        ELREF2 = 'FACE4'
        NBNV = 6
        NDEGRE = 1
        DO 39 I=1,3
          POIDS1(I) = 1.D0/6.D0
39      CONTINUE
        DO 40 I=1,4
          POIDS2(I) = 1.D0
40      CONTINUE
      ELSE IF (ELREFE . EQ. 'PENTA15 ') THEN
        NBF = 5
        ITYP = 2
        ELREFE = 'FACE6'
        ELREF2 = 'FACE8'
        NBNV = 15
        NDEGRE = 2
        DO 41 I=1,3
          POIDS1(I) = 0.D0
41      CONTINUE
        DO 42 I=4,6
          POIDS1(I) = 1.D0/6.D0
42      CONTINUE
        DO 43 I=1,4
          POIDS2(I) = 1.D0/9.D0
43      CONTINUE
        DO 44 I=5,8
          POIDS2(I) = 4.D0/9.D0
44      CONTINUE
        POIDS2(9) = 16.D0/9.D0
C
      ELSE IF (ELREFE . EQ. 'TETRA4  ') THEN
        NSOMM = 3
        NBF = 4
        ITYP = 3
        ELREFE = 'FACE3'
        NBNV = 4
        NDEGRE = 1
        DO 36 I=1,3
          POIDS1(I) = 1.D0/6.D0
36      CONTINUE
      ELSE IF (ELREFE . EQ. 'TETRA10 ') THEN
        NSOMM = 3
        NBF = 4
        ITYP = 3
        ELREFE = 'FACE6'
        NBNV = 10
        NDEGRE = 2
        DO 37 I=1,3
          POIDS1(I) = 0.D0
37      CONTINUE
        DO 38 I=4,6
          POIDS1(I) = 1.D0/6.D0
38      CONTINUE
C
      ELSE
        CALL UTMESS('F','TE0375','TYPE MAILLE INCONNU')
      ENDIF
C
      CARAC = '&INEL.'//ELREFE//'.CARACTE'
      CHVAL = '&INEL.'//ELREFE//'.FFORMES'
      IF(NBF.EQ.5) THEN
        CARAC2 = '&INEL.'//ELREF2//'.CARACTE'
        CHVAL2 = '&INEL.'//ELREF2//'.FFORMES'
      ENDIF
C
C   VERIFICATION DE L'EXISTENCE DES OBJETS RELATIFS AUX FACES
C
      CALL JEEXIN(CARAC,IRET)
      IF(IRET.LE.0) THEN
        CALL UTMESS('F','TE0375','L''OBJET CARAC DES FACES EST
     &               INEXISTANT')
      ENDIF
      CALL JEEXIN(CHVAL,IRET)
      IF(IRET.LE.0) THEN
        CALL UTMESS('F','TE0375','L''OBJET CHVAL DES FACES EST
     &               INEXISTANT')
      ENDIF
      IF(NBF.EQ.5) THEN
        CALL JEEXIN(CARAC2,IRET)
        IF(IRET.LE.0) THEN
          CALL UTMESS('F','TE0375','L''OBJET CARAC2 DES FACES EST
     &                 INEXISTANT')
        ENDIF
        CALL JEEXIN(CHVAL2,IRET)
        IF(IRET.LE.0) THEN
          CALL UTMESS('F','TE0375','L''OBJET CHVAL2 DES FACES EST
     &                 INEXISTANT')
        ENDIF
      ENDIF
C
      CALL JEVEUO(CARAC,'L',JIN)
      NDIM  = ZI(JIN+1-1)
      NNO   = ZI(JIN+2-1)
      NPG = NNO
C
      CALL JEVEUO(CHVAL,'L',IFF)
      IDFDX  = IFF    + NPG*NNO
      IDFDY  = IDFDX  + 1
C
CAS DES PENTAEDRES
C
      IF(NBF.EQ.5) THEN
        CALL JEVEUO(CARAC2,'L',JIN2)
        NNO2   = ZI(JIN2+2-1)
        NPG2 = NNO2
C
        CALL JEVEUO(CHVAL2,'L',IFF2)
        IDFDX2  = IFF2    + NPG2*NNO2
        IDFDY2  = IDFDX2  + 1
      ENDIF
C
      TER2 = 0.D0
      TER3 = 0.D0
C
C -------BOUCLE SUR LES FACES -----------------------------------------
C
      DO 300 IFA = 1,NBF
C
C ------TEST DU TYPE DE VOISIN -----------------------------------------
C
        TYV = ZI(IVOIS+7+IFA)
        IF (TYV .NE. 0) THEN
C
          IF ( IFA .GE .3 .AND. NBF .EQ. 5 )  THEN
            NNO = NNO2
            NPG = NPG2
            IDFDX  = IDFDX2
            IDFDY  = IDFDY2
            DO 305 IPG = 1,NPG
              POIDS(IPG) = POIDS2(IPG)
305         CONTINUE
            NSOMM = 4
C
          ELSE
C
            DO 306 IPG = 1,NPG
              POIDS(IPG) = POIDS1(IPG)
306         CONTINUE
            NSOMM = 3
C
          ENDIF
C
C  CALCUL DES PRODUITS VECTORIELS OMI VECTORIEL OMJ
C
      DO 301 IN = 1,NNO
        INO = NOE(IN,IFA,ITYP)
        I = IGEOM + 3*(INO-1) -1
          DO 302 JN = 1,NNO
            JNO = NOE(JN,IFA,ITYP)
            J = IGEOM + 3*(JNO-1) -1
              SX(IN,JN) = ZR(I+2) * ZR(J+3) - ZR(I+3) * ZR(J+2)
              SY(IN,JN) = ZR(I+3) * ZR(J+1) - ZR(I+1) * ZR(J+3)
              SZ(IN,JN) = ZR(I+1) * ZR(J+2) - ZR(I+2) * ZR(J+1)
302         CONTINUE
301     CONTINUE
C
C  CALCUL DU DIAMETRE HF DE LA FACE
C
      NDI=0
      NIV=1
      IFM=6
      CALL UTHK(NOMTE,IGEOM,HF,NDI,NOE,NSOMM,ITYP,IFA,NIV,IFM)
C
          CALL JENUNO (JEXNUM('&CATA.TM.NOMTM',TYV),TYPMAV)

C
          IF ( TYPMAV(1:4) .EQ. 'QUAD'.OR.
     &         TYPMAV(1:4) .EQ. 'TRIA') THEN
C
C ----------CALCUL DU 3 IEME TERME D'ERREUR ----------------------------
C
          IF (CARTE1 .NE. ' ') THEN

            IMAV = ZI(IVOIS+IFA)
            CALL JEEXIN (CARTE1//'.PTMA',IRET)
            IF (IRET .EQ. 0) THEN
C              CARTE CONSTANTE
               IENT1 = 1
            ELSE
C            LA CARTE A ETE ETENDUE
               CALL JEVEUO (CARTE1//'.PTMA','L',IAPTMA)
               IENT1 = ZI(IAPTMA -1 +IMAV)
            ENDIF
            NOMGD1 = ZK24(IREF+4)
          ENDIF
C
          IF (CARTE2 .NE. ' ') THEN

            IMAV = ZI(IVOIS+IFA)
            CALL JEEXIN (CARTE2//'.PTMA',IRET)
            IF (IRET .EQ. 0) THEN
C              CARTE CONSTANTE
               IENT2 = 1
            ELSE
C            LA CARTE A ETE ETENDUE
               CALL JEVEUO (CARTE2//'.PTMA','L',IAPTMA)
               IENT2 = ZI(IAPTMA -1 +IMAV)
            ENDIF
            NOMGD2 = ZK24(IREF2+4)
          ENDIF
C
            PRES = .FALSE.
            FORC = .FALSE.
C
C   BOUCLE SUR LES PTS D'INTEGRATION DE LA FACE
C
        NORM = 0.0D0
        DO 500 IPG = 1,NPG
C
C --- CALCUL DE LA NORMALE AU POINT IPG  -----------------
C
        KDEC = (IPG-1)*NNO*NDIM
C
         NX = 0.0D0
         NY = 0.0D0
         NZ = 0.0D0
C
         DO 103 I=1,NNO
         IDEC = (I-1)*NDIM
           DO 104 J=1,NNO
             JDEC = (J-1)*NDIM
          NX = NX - ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) * SX(I,J)
          NY = NY - ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) * SY(I,J)
          NZ = NZ - ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) * SZ(I,J)
104         CONTINUE
103       CONTINUE
C
C   JACOBIEN
        JACO = SQRT(NX*NX + NY*NY + NZ*NZ)
C
        INO = NOE(IPG,IFA,ITYP)
        IDEC = 6*(INO-1)
C
C ---RECUPERATION DES CONTRAINTES AUX PTS DE GAUSS ----------
C
      DO 304 ICMP=1,6
        SIGDIF(ICMP) = ZR(IAD+IDEC-1+ICMP)
  304 CONTINUE
C
      IF(NPG.EQ.6.AND.IPG.LE.3) GO TO 500
C
C --- RECUPERATION DE LA PRESSION ----------------
C
          IF (NOMGD2(1:6) .EQ. 'PRES_R') THEN
            PRES = .TRUE.
            PRE = ZR(IAVA2-1+(IENT2-1)*NCMPM2+1)
            PR(IPG) = PRE
C
          ELSE IF (NOMGD2(1:6) .EQ. 'PRES_F') THEN
            PRES = .TRUE.
            PRF = ZK8(IAVA2-1+(IENT2-1)*NCMPM2+1)
C
            IF (PRF .NE. '&FOZERO' ) THEN
              NOMPAR(1) = 'X'
              NOMPAR(2) = 'Y'
              NOMPAR(3) = 'Z'
              NOMPAR(4) = 'INST'
              VALPAR(1) = ZR(IGEOM+3*INO-3)
              VALPAR(2) = ZR(IGEOM+3*INO-2)
              VALPAR(3) = ZR(IGEOM+3*INO-1)
              VALPAR(4) = INST
              CALL FOINTE('FM',PRF,4,NOMPAR,VALPAR,PR(IPG),IER)
            ENDIF
          ENDIF
C
C --- RECUPERATION DES FORCES  ----------------
C
          IF (NOMGD1(1:6) .EQ. 'FORC_R') THEN
             FXR = ZR(IAVA1-1+(IENT1-1)*NCMPM1+1)
             FYR = ZR(IAVA1-1+(IENT1-1)*NCMPM1+2)
             FZR = ZR(IAVA1-1+(IENT1-1)*NCMPM1+3)
C
           IF (ABS(FXR) .GT. 1.D-15 .OR. ABS(FYR) .GT. 1.D-15
     &            .OR. ABS(FZR) .GT. 1.D-15) THEN
             FX(IPG) = FXR
             FY(IPG) = FYR
             FZ(IPG) = FZR
             FORC = .TRUE.
           ENDIF
C
         ELSE IF (NOMGD1(1:6) .EQ. 'FORC_F') THEN
             FXF = ZK8(IAVA1-1+(IENT1-1)*NCMPM1+1)
             FYF = ZK8(IAVA1-1+(IENT1-1)*NCMPM1+2)
             FZF = ZK8(IAVA1-1+(IENT1-1)*NCMPM1+3)
C
           IF (FXF .NE. '&FOZERO' .OR. FYF .NE. '&FOZERO'
     &         .OR.FZF .NE. '&FOZERO' ) THEN
C
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
C
           ENDIF
        ENDIF
C
C --- CALCUL DU PRODUIT SCALAIRE SIGMA . N - G ----------------
C
        IF(PRES) THEN
          NORM = NORM
     &    + ((NX*SIGDIF(1)+NY*SIGDIF(4)+NZ*SIGDIF(5)+PR(IPG)*NX)**2
     &    +  (NX*SIGDIF(4)+NY*SIGDIF(2)+NZ*SIGDIF(6)+PR(IPG)*NY)**2
     &    +  (NX*SIGDIF(5)+NY*SIGDIF(6)+NZ*SIGDIF(3)+PR(IPG)*NZ)**2)
     &    * POIDS(IPG)/JACO
        ENDIF
        IF(FORC) THEN
          NORM = NORM
     &    + ((NX*SIGDIF(1)+NY*SIGDIF(4)+NZ*SIGDIF(5)-FX(IPG))**2
     &    +  (NX*SIGDIF(4)+NY*SIGDIF(2)+NZ*SIGDIF(6)-FY(IPG))**2
     &    +  (NX*SIGDIF(5)+NY*SIGDIF(6)+NZ*SIGDIF(3)-FZ(IPG))**2)
     &    * POIDS(IPG)/JACO
        ENDIF
  500 CONTINUE
C
         IF(NORM.LT.0.D0) NORM=-NORM
         TER3 = TER3 + SQRT(HF)*SQRT(NORM)


C
C
C ---------- CALCUL DU DEUXIEME TERME ----------------------------------
C
        ELSE IF ( TYPMAV(1:4) .EQ. 'HEXA'.OR.
     &            TYPMAV(1:4) .EQ. 'PENT'.OR.
     &            TYPMAV(1:4) .EQ. 'TETR') THEN
C
CAS DES PENTAEDRES
C
        IF ( NBF .EQ. 5) THEN
          IF ( TYPMAV(1:4) .EQ. 'HEXA'.OR.(TYPMAV(1:4) .EQ. 'PENT'. AND.
     &          IFA . GE . 3 )) THEN
            NNO = NNO2
            NPG = NPG2
            IDFDX  = IDFDX2
            IDFDY  = IDFDY2
          ENDIF
        ENDIF
C
CALCUL DE NUMEROTATION DU VOISIN -------

        CALL JEVEUO(JEXNUM(MA//'.CONNEX',ZI(IVOIS)),'L',JAD)
        CALL JEVEUO(JEXNUM(MA//'.CONNEX',ZI(IVOIS+IFA)),'L',JADV)

        IMAV = ZI(IVOIS+IFA)
        IGREL = ZI(IAREPE+2*(IMAV-1))
        IEL = ZI(IAREPE+2*(IMAV-1)+1)
        IAVAL1=JCELV-1+ZI(JCELD-1+ZI(JCELD-1+4+IGREL)+8)
C
C   BOUCLE SUR LES PTS D'INTEGRATION DE LA FACE
C
        NORM = 0.0D0
        DO 400 IPG = 1,NPG
C
C --- CALCUL DE LA NORMALE AU POINT IPG  -----------------
C
        KDEC = (IPG-1)*NNO*NDIM
C
         NX = 0.0D0
         NY = 0.0D0
         NZ = 0.0D0
         DO 102 I=1,NNO
         IDEC = (I-1)*NDIM
          DO 102 J=1,NNO
          JDEC = (J-1)*NDIM
C
          NX = NX - ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) * SX(I,J)
          NY = NY - ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) * SY(I,J)
          NZ = NZ - ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) * SZ(I,J)
102     CONTINUE
C
C   JACOBIEN
        JACO = SQRT(NX*NX + NY*NY + NZ*NZ)
C
        INO = NOE(IPG,IFA,ITYP)
        NCHER = ZI(JAD-1+INO)
        INOV = INDIIS(ZI(JADV),NCHER,1,NBNV)
        IDEC1 = 6*(INO-1)
        IDEC2 = 6*(INOV-1)+6*NBNV*(IEL-1)
C
C --- CALCUL DES SAUTS DE CONTRAINTES -----------------
C
      DO 303 ICMP=1,6
        SIGDIF(ICMP) = ZR(IAD+IDEC1-1+ICMP) - ZR(IAVAL1+IDEC2-1+ICMP)
  303 CONTINUE
C
      IF(NPG.EQ.6.AND.IPG.LE.3) GO TO 400
C
C --- CALCUL DU PRODUIT SCALAIRE SIGMA . N -----------------
C
        NORM = NORM + ((NX*SIGDIF(1)+NY*SIGDIF(4)+NZ*SIGDIF(5))**2
     &              +  (NX*SIGDIF(4)+NY*SIGDIF(2)+NZ*SIGDIF(6))**2
     &              +  (NX*SIGDIF(5)+NY*SIGDIF(6)+NZ*SIGDIF(3))**2)
     &               * POIDS(IPG)/JACO
  400 CONTINUE
C
         IF(NORM.LT.0.D0) NORM=-NORM
         TER2 = TER2 +0.5D0*SQRT(HF)*SQRT(NORM)


        ELSE
          CALL UTMESS('F','TE0375','TYPE MAILLE INCONNU')
        ENDIF
      ENDIF
  300 CONTINUE
C
C------------MISE EN MEMOIRE DES DIFFERENTS TERMES DE L'ERREUR ---------
C
      IF (NDEGRE .EQ. 2) THEN
        COEFF = SQRT(96.D0)
      ELSE IF (NDEGRE .EQ. 1) THEN
        COEFF = SQRT(24.D0)
      ENDIF
C
      ERREST =( H * SQRT(TER) + TER2 + TER3)/COEFF
C      TER1 = H * SQRT(TER)/COEFF
      TER2 = TER2/COEFF
      TER3 = TER3/COEFF
      SIGCAL = SQRT(NORSIG)
      NUEST = 100.D0 * SQRT(ERREST**2/(ERREST**2 + NORSIG))
C
      ZR(IERR) = ERREST
      ZR(IERR+1) = NUEST
      ZR(IERR+2) = SIGCAL
      CALL JEDEMA()
      END
