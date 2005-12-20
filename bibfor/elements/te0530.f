      SUBROUTINE TE0530(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 16/12/2004   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ELEMENT SHB8PS A.COMBESCURE S.BAGUET INSA-LYON 2003
C TOLE CRP_20
C.......................................................................
      IMPLICIT REAL*8 (A-H,O-Z)

C          ELEMENT SHB8PS
C    FONCTION REALISEE:
C            OPTION : 'RIGI_MECA      '
C                            CALCUL DES MATRICES ELEMENTAIRES  3D
C            OPTION : 'RIGI_MECA_SENSI' OU 'RIGI_MECA_SENS_C'
C                            CALCUL DU VECTEUR ELEMENTAIRE -DK/DP*U
C     ENTREES  ---> OPTION : OPTION DE CALCUL
C              ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................

      PARAMETER (NBRES=2)
      CHARACTER*8 MODELI,ELREFE
      CHARACTER*2 CODRET(NBRES)
      CHARACTER*8 NOMRES(NBRES),NOMPAR(4)
      CHARACTER*16 NOMTE,OPTION,PHENOM,SOPTIO
      CHARACTER*24 CHVAL,CHCTE
      REAL*8 B(486),RE(24,24),SIGMA(30),WORK(200),D(36)
      REAL*8 DUSDX(45),UTOT(24),FSTAB(12),SIGMM(30),SIGMP(30)
      REAL*8 VALRES(NBRES),VALPAR(4),VARINT(10)
      INTEGER NBSIGM
      LOGICAL LSENS
      REAL*8 NU,E

C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
C------------FIN  COMMUNS NORMALISES  JEVEUX  --------------------------

      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)

C --- INITIALISATIONS :
C     -----------------
      CALL ASSERT(NDIM.EQ.3)
      CALL ASSERT(NNO.EQ.8)
      CALL ASSERT(NPG.EQ.5)
      NBSIG = 6
      NBINCO = NDIM*NNO
      DO 10 I = 1,200
        WORK(I) = 0.D0
   10 CONTINUE


      IF (OPTION.EQ.'RIGI_MECA' .OR. OPTION.EQ.'SIEF_ELGA_DEPL' .OR.
     &    OPTION.EQ.'FORC_NODA') THEN
C ----  RECUPERATION DES COORDONNEES DES CONNECTIVITES
C       GEOMETRIE Dans ZR(IGEOM)
        CALL JEVECH('PGEOMER','L',IGEOM)
C ----  RECUPERATION DU MATERIAU DANS ZI(IMATE)
        CALL JEVECH('PMATERC','L',IMATE)
        NOMRES(1) = 'E'
        NOMRES(2) = 'NU'
        NBV = 2
C ----   INTERPOLATION DES COEFFICIENTS EN FONCTION DE LA TEMPERATURE
C ----   ET DU TEMPS
C        -----------
        CALL JEVECH('PTEMPER','L',ITEMPE)
        TEMPM = 0.D0
        DO 401 I = 1,NNO
            TEMPM = TEMPM + ZR(ITEMPE+I-1)/NNO
  401   CONTINUE
        CALL RCVALA(ZI(IMATE),' ','ELAS',1,'TEMP',TEMPM,NBV,NOMRES,
     &              VALRES,CODRET,'FM')
        E = VALRES(1)
        NU = VALRES(2)
C ----  PARAMETRES MATERIAUX
        WORK(1) = E
        WORK(2) = NU
        YGOT = E
C ----  PARAMETRES MATERIAUX POUR LE CALCUL DE LA
C ----  MATRICE TANGENTE PLASTIQUE
C       MATRICE TANGENTE PLASTIQUE SI WORK(13)=1
        WORK(13) = 0
        WORK(14) = YGOT
C       WORK(150) : TYPE DE LOI DE COMPORTEMENT
C       1: SHB8PS PLEXUS
C       2: CONTRAINTES PLANES
C       3: 3D COMPLETE
        WORK(150) = 1
C       LAG=0 LAGRANGIEN REACTUALISE (EPS=EPSLIN)
C       LAG=1 LAGRANGIEN TOTAL (EPS=EPSLIN+EPSNL)
        LAG = 0
        WORK(3) = LAG
      END IF

C  ===========================================
C  -- MATRICE DE RIGIDITE
C  ===========================================
      IF (OPTION.EQ.'RIGI_MECA') THEN
        DO 30 I = 1,NBINCO
          DO 20 J = 1,NBINCO
            RE(I,J) = 0.D0
   20     CONTINUE
   30   CONTINUE
        CALL SHB8PS(OPTION,ZR(IGEOM),D,WORK,WORK(100),RE,WORK(150))
C        RECUPERATION ET AFFECTATION DU VECTEUR EN SORTIE
C        DEMI-MATRICE DE RIGIDITE
        CALL JEVECH('PMATUUR','E',IMATUU)
        K = 0
        DO 50 I = 1,NBINCO
          DO 40 J = 1,I
            K = K + 1
            ZR(IMATUU+K-1) = RE(I,J)
   40     CONTINUE
   50   CONTINUE

C  ===========================================
C  -- CONTRAINTES
C  ===========================================
      ELSE IF (OPTION.EQ.'SIEF_ELGA_DEPL') THEN
        DO 60 I = 1,NBSIG*NPG
          SIGMA(I) = 0.D0
   60   CONTINUE
        SIGMA(1) = WORK(150)
C        RECUPERATION DU CHAMP DE DEPLACEMENT SUR L'ELEMENT
C        DEPL Dans ZR(IDEPL)
        CALL JEVECH('PDEPLAR','L',IDEPL)
C        VECTEUR DES CONTRAINTES AUX POINTS D'INTEGRATION
        CALL SHB8PS(OPTION,ZR(IGEOM),D,WORK,ZR(IDEPL),WORK(150),SIGMA)
C        RECUPERATION ET AFFECTATION DU VECTEUR EN SORTIE
        CALL JEVECH('PCONTRR','E',ICONT)
        CALL R8INIR(12,0.D0,FSTAB,1)
        CALL SHBSFC(SIGMA,FSTAB,ZR(ICONT))

C  ===========================================
C  -- MATRICE DE RIGIDITE GEOMETRIQUE
C  ===========================================
      ELSE IF (OPTION.EQ.'RIGI_MECA_GE') THEN
        DO 80 I = 1,NBINCO
          DO 70 J = 1,NBINCO
            RE(I,J) = 0.D0
   70     CONTINUE
   80   CONTINUE
C        RECUPERATION DES COORDONNEES DES CONNECTIVITES
C        GEOMETRIE Dans ZR(IGEOM)
        CALL JEVECH('PGEOMER','L',IGEOM)
C        RECUPERATION DES CONTRAINRES DANS ZR(ICONT)
        CALL JEVECH('PCONTRR','L',ICONT)
        WORK(1) = 0

        CALL SHBCSF(ZR(ICONT),SIGMA,FSTAB)

        CALL SHB8PS(OPTION,ZR(IGEOM),WORK(20),WORK,SIGMA,RE,WORK)

C        ------ fin Impression num elem -------
C        RECUPERATION ET AFFECTATION DU VECTEUR EN SORTIE
C        DEMI-MATRICE DE RIGIDITE
        CALL JEVECH('PMATUUR','E',IMATUU)
        K = 0
        DO 100 I = 1,NBINCO
          DO 90 J = 1,I
            K = K + 1
            ZR(IMATUU+K-1) = RE(I,J)
   90     CONTINUE
  100   CONTINUE


C  ==============================================
C  -- VECTEUR DES CONTRAINTES AUX NOEUDS A PARTIR
C  -- DES CONTRAINTES AUX POINTS D'INTEGRATION
C  ==============================================
      ELSE IF (OPTION.EQ.'SIEF_ELNO_ELGA') THEN
        CALL JEVECH('PCONTRR','L',ICONT)
        CALL JEVECH('PSIEFNOR','E',ICHN)
C       LES NOEUDS 1 2 3 4 DE LA FACE INF PRENNENT LA VALEUR DU PG 1
C       LES NOEUDS 5 6 7 8 DE LA FACE SUP PRENNENT LA VALEUR DU PG 5
        CALL SHBCSF(ZR(ICONT),SIGMA,FSTAB)
        CALL PPGAN2(JGANO,NBSIG,SIGMA,ZR(ICHN))

C  ==============================================
C  -- VECTEUR DES FORCES INTERNES
C  ==============================================
      ELSE IF (OPTION.EQ.'FORC_NODA') THEN
        CALL JEVECH('PGEOMER','L',IGEOM)
C ----     CONTRAINTES AUX POINTS D'INTEGRATION
        CALL JEVECH('PCONTMR','L',ICONTM)
C         CHAMPS POUR LA REACTUALISATION DE LA GEOMETRIE
        CALL JEVECH('PDEPLMR','L',IDEPLM)
C ----     CONTRAINTES DE STABILISATION
C ---- PARAMETRES EN SORTIE
C ----     VECTEUR DES FORCES INTERNES (BT*SIGMA)
        CALL JEVECH('PVECTUR','E',IVECTU)
        
        CALL TECACH('ONN','PCOMPOR',1,ICOMPO,IRET)
        IF (ICOMPO.NE.0) THEN
          CALL JEVECH('PCOMPOR','L',ICOMPO)

        ENDIF
C        =============================================
C        -  ACTUALISATION : GEOM ORIG + DEPL DEBUT PAS
C        =============================================
          IF ((ZK16(ICOMPO+2) (1:5).EQ.'GREEN')) THEN
            DO 150 I = 1,3*NNO
              ZR(IGEOM+I-1) = ZR(IGEOM+I-1) + ZR(IDEPLM+I-1)
              WORK(100+I) = ZR(IDEPLM+I-1)
  150       CONTINUE
          ELSE IF ((ZK16(ICOMPO+2) (1:5).EQ.'PETIT')) THEN
            CALL R8INIR(24,0.D0,WORK(101),1)
          ELSE
            DO 152 I = 1,3*NNO
              WORK(100+I) = ZR(IDEPLM+I-1)
  152       CONTINUE
            
          END IF

C ----      CALCUL DES FORCES INTERNES BT.SIGMA
C           -----------------------------------
C           ON PASSE EN PARAMETRES
C           ZR(IGEOM) : GEOMETRIE CONFIG DEBUT PAS
C           WORK : PARAMETRES MATERIAU
C                  WORK(1)=E  WORK(2)=NU  WORK(3)=LAG
C           ZR(IDEPLM) : DEPLACEMENT
C           ZR(ICONTM) : CONTRAINTE DE CAUCHY DEBUT DE PAS
C           ZR(IVARIM) (DE 2 A 14) : CONTRAINTES DE STABILISATION
C           ON RECUPERE :
C           ZR(IVECTU) : FORCES INTERNES FIN DE PAS

C ----   INTERPOLATION DES COEFFICIENTS EN FONCTION DE LA TEMPERATURE
C ----   ET DU TEMPS
C        -----------
        CALL JEVECH('PTEMPER','L',ITEMPE)
        TEMPM = 0.D0
        DO 402 I = 1,NNO
            TEMPM = TEMPM + ZR(ITEMPE+I-1)/NNO
  402   CONTINUE
        CALL JEVECH('PMATERC','L',IMATE)
        NOMRES(1) = 'E'
        NOMRES(2) = 'NU'
        NBV = 2
        CALL RCVALA(ZI(IMATE),' ','ELAS',1,'TEMP',TEMPM,NBV,NOMRES,
     &             VALRES, CODRET,'FM')
        E = VALRES(1)
        NU = VALRES(2)
        WORK(1) = E
        WORK(2) = NU

C           FORCES DE STABIBILISATION DANS CONTM(7,7+12)

        ZR(IVECTU) = WORK(150)
        SOPTIO = 'FORC_NODA'

        CALL SHBCSF(ZR(ICONTM),SIGMA,FSTAB)
        CALL SHB8PS(SOPTIO,ZR(IGEOM),WORK,WORK(101),SIGMA,FSTAB,
     &            ZR(IVECTU))


      END IF

C  ###############################################################
C  -- ELASTOPLASTICITE
C  ###############################################################
      IF (OPTION.EQ.'FULL_MECA' .OR. OPTION.EQ.'RAPH_MECA' .OR.
     &    OPTION.EQ.'RIGI_MECA_TANG') THEN

C - PARAMETRES EN ENTREE

        CALL JEVECH('PGEOMER','L',IGEOM)
        CALL JEVECH('PMATERC','L',IMATE)
        CALL JEVECH('PCONTMR','L',ICONTM)
        CALL JEVECH('PVARIMR','L',IVARIM)
        CALL JEVECH('PDEPLMR','L',IDEPLM)
        CALL JEVECH('PDEPLPR','L',IDEPLP)
        CALL JEVECH('PCOMPOR','L',ICOMPO)
        CALL JEVECH('PCARCRI','L',ICARCR)

C - PARAMETRES EN SORTIE

        IF (OPTION(1:16).EQ.'RIGI_MECA_TANG' .OR.
     &      OPTION(1:9).EQ.'FULL_MECA') THEN
          CALL JEVECH('PMATUUR','E',IMATUU)
        END IF

        IF (OPTION(1:9).EQ.'RAPH_MECA' .OR.
     &      OPTION(1:9).EQ.'FULL_MECA') THEN
          CALL JEVECH('PVECTUR','E',IVECTU)
          CALL JEVECH('PCONTPR','E',ICONTP)
          CALL JEVECH('PVARIPR','E',IVARIP)
        END IF

C - PARAMETRES MATERIAU

        NOMRES(1) = 'E'
        NOMRES(2) = 'NU'
        NBV = 2
C ----  INTERPOLATION DES COEFFICIENTS EN FONCTION DE LA TEMPERATURE
C ----  ET DU TEMPS
C        -----------
        CALL JEVECH('PTEMPPR','L',ITEMPE)
        TEMPM = 0.D0
        DO 403 I = 1,NNO
            TEMPM = TEMPM + ZR(ITEMPE+I-1)/NNO
  403   CONTINUE
        CALL RCVALA(ZI(IMATE),' ','ELAS',1,'TEMP',TEMPM,NBV,NOMRES,
     &             VALRES, CODRET,'FM')
        E = VALRES(1)
        NU = VALRES(2)
C ----   PARAMETRES MATERIAU
        WORK(1) = E
        WORK(2) = NU
        YGOT = E
C ----       PARAMETRES MATERIAUX POUR LE CALCUL DE LA
C ----       MATRICE TANGENTE PLASTIQUE
C ----       MATRICE TANGENTE PLASTIQUE SI WORK(13)=1
        WORK(13) = 0
        WORK(14) = YGOT
C ----       WORK(150) : TYPE DE LOI DE COMPORTEMENT
C ----       1: SHB8PS PLEXUS
C ----       2: CONTRAINTES PLANES
C ----       3: 3D COMPLETE
        WORK(150) = 1
C ----       LAG=0 LAGRANGIEN REACTUALISE (EPS=EPSLIN)
C ----       LAG=1 LAGRANGIEN TOTAL (EPS=EPSLIN+EPSNL)
        LAG = 0
        WORK(3) = LAG
C  =============================================
C  -  ACTUALISATION : GEOM ORIG + DEPL DEBUT PAS
C  =============================================
        IF (ZK16(ICOMPO+2) (1:5).NE.'PETIT') THEN
          IF (OPTION(1:16).EQ.'RIGI_MECA_TANG' .OR.
     &        OPTION(1:9).EQ.'RAPH_MECA' .OR.
     &        OPTION(1:9).EQ.'FULL_MECA') THEN
            DO 160 I = 1,3*NNO
              ZR(IGEOM+I-1) = ZR(IGEOM+I-1) + ZR(IDEPLM+I-1)
  160       CONTINUE
          END IF
        END IF
C  =============================================
C  -  CALCUL DES CONTRAINTES
C  =============================================
        IF (OPTION(1:9).EQ.'RAPH_MECA' .OR.
     &      OPTION(1:9).EQ.'FULL_MECA') THEN

C ----   PARAMETRES MATERIAU
          D(1) = E
          D(2) = NU
          YGOT = E
C ----       PARAMETRES MATERIAUX POUR LE CALCUL DE LA
C ----       MATRICE TANGENTE PLASTIQUE
C ----       MATRICE TANGENTE PLASTIQUE SI WORK(13)=1
          WORK(13) = 0
          WORK(14) = YGOT
C ----       WORK(150) : TYPE DE LOI DE COMPORTEMENT
C ----       1: SHB8PS PLEXUS
C ----       2: CONTRAINTES PLANES
C ----       3: 3D COMPLETE
          WORK(150) = 1
C ----       LAG=0 LAGRANGIEN REACTUALISE (EPS=EPSLIN)
C ----       LAG=1 LAGRANGIEN TOTAL (EPS=EPSLIN+EPSNL)
          LAG = 0
          WORK(3) = LAG
          DO 170 I = 1,NBSIG*NPG
            SIGMA(I) = 0.D0
  170     CONTINUE
          SIGMA(1) = WORK(150)
          SOPTIO = 'SIEF_ELGA_DEPL  '
          CALL SHB8PS(SOPTIO,ZR(IGEOM),D,WORK,ZR(IDEPLP),WORK(150),
     &              SIGMA)

C ----      ON SAUVE LES GRADIENTS DE DEPL DANS DUSDX
C          CALL SHIFTD(WORK,DUSDX,45)
          CALL DCOPY(45,WORK,1,DUSDX,1)
C           CONTRAINTES PKII FIN DE PAS (=DEBUT PAS + INCR)
          CALL SHBCSF(ZR(ICONTM),SIGMM,FSTAB)
          DO 180 I = 1,NBSIG*NPG
            WORK(I) = SIGMA(I)
            SIGMA(I) = SIGMM(I) + SIGMA(I)
  180     CONTINUE

C ----      === CAS PLASTIQUE ===
          IF (ZK16(ICOMPO) (1:14).EQ.'VMIS_ISOT_TRAC') THEN

C ----      PROJ PLASTIQUE ENTREES DANS WORK, SORTIES DS SIGMA ET VARINT
C           --------------
C           ON PASSE :
C           WORK(1:30)= INCR DE CONTRAINTE ELASTIQUE
C           WORK(31:60)= CONTRAINTE EN DEBUT DE PAS
C           WORK(61:70)= VAR INTERNES EN DEBUT DE PAS (2 EN CHAQUE PG)
C           ON RECUPERE :
C           ZR(IVARIP)= VAR INTERNES EN FIN DE PAS (2 EN CHAQUE PG)
C           SIGMA(1:30)= CONTRAINTE FIN DE PAS (=DEBUT PAS + INCR)

            WORK(100) = ZI(IMATE)
C           PRECISION CALCUL
            WORK(101) = ZR(ICARCR-1+3)
C           NB ITERATIONS MAX AUTORISEES
            WORK(102) = ZR(ICARCR-1+1)
            DO 190 I = 1,NBSIG*NPG
              WORK(30+I) = SIGMM(I)
  190       CONTINUE
            DO 210 IP = 1,NPG
              DO 200 I = 1,2
                WORK(60+ (IP-1)*2+I) = ZR(IVARIM-1+ (IP-1)*2+I)
  200         CONTINUE
  210       CONTINUE
            SOPTIO = 'ECOUL_PLAS      '
            CALL R8INIR(10,0.D0,VARINT,1)
            CALL SHB8PS(SOPTIO,ZR(IGEOM),D,WORK,VARINT,WORK(150),SIGMA)
            DO 230 IP = 1,NPG
              DO 220 I = 1,2
                ZR(IVARIP-1+ (IP-1)*2+I) = VARINT((IP-1)*2+I)
  220         CONTINUE
  230       CONTINUE
          ELSEIF (ZK16(ICOMPO)(1:4).NE.'ELAS') THEN
            CALL UTMESS('F','SHB8','SEULS COMPORTEMENTS AUTORISES :'//
     &                 '"ELAS" ET "VMIS_ISOT_TRAC"')
          END IF
C ----      TRANSFO PKII DEBUT PAS EN CAUCHY FIN PAS ACTUALISE
          CALL SHBPKC(SIGMA,SIGMP,DUSDX)
        END IF

C  ===========================================
C  -  MATRICE DE RIGIDITE TANGENTE
C  ===========================================
        IF (OPTION(1:16).EQ.'RIGI_MECA_TANG' .OR.
     &      OPTION(1:9).EQ.'FULL_MECA') THEN
C ----   PARAMETRES MATERIAU
          WORK(1) = E
          WORK(2) = NU
          YGOT = E
C ----       PARAMETRES MATERIAUX POUR LE CALCUL DE LA
C ----       MATRICE TANGENTE PLASTIQUE
C ----       MATRICE TANGENTE PLASTIQUE SI WORK(13)=1
          WORK(13) = 0
          WORK(14) = YGOT
C ----       WORK(150) : TYPE DE LOI DE COMPORTEMENT
C ----       1: SHB8PS PLEXUS
C ----       2: CONTRAINTES PLANES
C ----       3: 3D COMPLETE
          WORK(150) = 1
C ----       LAG=0 LAGRANGIEN REACTUALISE (EPS=EPSLIN)
C ----       LAG=1 LAGRANGIEN TOTAL (EPS=EPSLIN+EPSNL)
          LAG = 0
          WORK(3) = LAG
          DO 250 I = 1,NBINCO
            DO 240 J = 1,NBINCO
              RE(I,J) = 0.D0
  240       CONTINUE
  250     CONTINUE
C ----   RIGIDITE ELASTIQUE
          SOPTIO = 'RIGI_MECA       '
          CALL SHB8PS(SOPTIO,ZR(IGEOM),D,WORK,WORK(100),RE,WORK(150))

          IF (ZK16(ICOMPO+2) (1:5).NE.'PETIT') THEN
C ----   RIGIDITE ELASTIQUE + RIGIDITE GEOMETRIQUE
            WORK(1) = 1
            SOPTIO = 'RIGI_MECA_GE    '
            IF (OPTION(1:16).EQ.'RIGI_MECA_TANG') THEN
              CALL SHBCSF(ZR(ICONTM),SIGMA,FSTAB)
              CALL SHB8PS(SOPTIO,ZR(IGEOM),WORK(20),WORK,SIGMA,RE,WORK)
            ELSE IF (OPTION(1:9).EQ.'FULL_MECA') THEN
C              CALL SHBCSF(ZR(ICONTP),SIGMA,FSTAB)
              CALL SHB8PS(SOPTIO,ZR(IGEOM),WORK(20),WORK,SIGMA,RE,WORK)
            END IF
          END IF

C ----   RECUPERATION ET AFFECTATION DU VECTEUR EN SORTIE
C ----   DEMI-MATRICE DE RIGIDITE
          CALL JEVECH('PMATUUR','E',IMATUU)
          K = 0
          DO 270 I = 1,NBINCO
            DO 260 J = 1,I
              K = K + 1
              ZR(IMATUU+K-1) = RE(I,J)
  260       CONTINUE
  270     CONTINUE
        END IF
C  ===============================================================
C  -  CALCUL DES FORCES INTERNES BT.SIGMA
C  ===============================================================
        IF (OPTION(1:9).EQ.'RAPH_MECA' .OR.
     &      OPTION(1:9).EQ.'FULL_MECA') THEN
C  -  ACTUALISATION : GEOM DEBUT PAS + INCR ITER
          IF (ZK16(ICOMPO+2) (1:5).NE.'PETIT') THEN
            DO 280 I = 1,3*NNO
              ZR(IGEOM+I-1) = ZR(IGEOM+I-1) + ZR(IDEPLP+I-1)
  280       CONTINUE
          END IF
C ----   PARAMETRES MATERIAU
          WORK(1) = E
          WORK(2) = NU
          YGOT = E
C ----       PARAMETRES MATERIAUX POUR LE CALCUL DE LA
C ----       MATRICE TANGENTE PLASTIQUE SI WORK(13)=1
          WORK(13) = 0
          WORK(14) = YGOT
C ----       WORK(150) : TYPE DE LOI DE COMPORTEMENT
C ----       1: SHB8PS PLEXUS
C ----       2: CONTRAINTES PLANES
C ----       3: 3D COMPLETE
          WORK(150) = 1
C ----       LAG=0 LAGRANGIEN REACTUALISE (EPS=EPSLIN)
C ----       LAG=1 LAGRANGIEN TOTAL (EPS=EPSLIN+EPSNL)
          LAG = 0
          WORK(3) = LAG
C           ON PASSE EN PARAMETRES
C           ZR(IGEOM) : GEOMETRIE + DEPL DEBUT PAS + INCR DEPL
C           WORK : PARAMETRES MATERIAU
C                  WORK(1)=E  WORK(2)=NU  WORK(3)=LAG
C           ZR(IDEPLP) : INCR DEPLACEMENT 
C                        (PAS UTILISE CAR LAGRANGIEN ACTUALISE)
C           ZR(ICONTP) : CONTRAINTE DE CAUCHY FIN DE PAS
C           ZR(IVARIM) (DE 2 A 14) : CONTRAINTES DE STABILISATION
C           ON RECUPERE :
C ----      ZR(IVECTU) : FORCES INTERNES FIN DE PAS
          ZR(IVECTU) = WORK(150)
          IF (ZK16(ICOMPO+2) (1:5).NE.'PETIT') THEN
            DO 290 I = 1,3*NNO
              UTOT(I) = ZR(IDEPLP+I-1)
  290       CONTINUE
          ELSE
            DO 300 I = 1,3*NNO
              UTOT(I) = ZR(IDEPLP+I-1)
  300       CONTINUE
          END IF
          SOPTIO = 'FORC_NODA       '
          CALL SHBCSF(ZR(ICONTM),SIGMM,FSTAB)
          CALL SHB8PS(SOPTIO,ZR(IGEOM),WORK,UTOT,SIGMP,FSTAB,
     &              ZR(IVECTU))
          CALL SHBSFC(SIGMP,FSTAB,ZR(ICONTP))
        END IF
      END IF
      END
