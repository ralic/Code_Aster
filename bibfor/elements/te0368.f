      SUBROUTINE TE0368(OPTION,NOMTE)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/01/2011   AUTEUR PELLET J.PELLET 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE DELMAS J.DELMAS
C TOLE CRP_20
C
C     BUT:
C       CALCUL DE L'INDICATEUR D'ERREUR EN MECANIQUE 3D AVEC LA
C       METHODE EN QUANTITE D'INTERET BASEE SUR LES RESIDUS EXPLICITES.
C       OPTION : 'QIRE_ELEM'
C
C REMARQUE : LES PROGRAMMES SUIVANTS DOIVENT RESTER TRES SIMILAIRES
C            TE0368, TE0375, TE0377, TE0378, TE0382, TE0497
C
C ----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE
C
C DECLARATION PARAMETRES D'APPELS
      CHARACTER*16 OPTION,NOMTE
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
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
      CHARACTER*32 JEXNUM
C
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
C DECLARATION VARIABLES LOCALES
C
      INTEGER IFM,NIV
      INTEGER IADZI,IAZK24
      INTEGER IBID,IAUX,IRET,ITAB(7)
      INTEGER NOE(9,6,4)
      INTEGER IGEOM,JTIME
      INTEGER IERR, IVOIS
      INTEGER IMATE
      INTEGER IADP
      INTEGER IADD
      INTEGER IFOVRP, IFOVFP
      INTEGER IFOVRD, IFOVFD
      INTEGER IPESP,IROTP
      INTEGER IPESD,IROTD
      INTEGER IREFP1,IREFP2
      INTEGER IREFD1,IREFD2
      INTEGER NDIM
      INTEGER NNO , NNOS , NPG , IPOIDS, IVF , IDFDE , JGANO
      INTEGER NDIMF
      INTEGER NNOF, NNOSF, NPGF, IPOIDF, IVFF, IDFDXF, JGANOF
      INTEGER NNO2, NNOS2, NPG2, IPOID2, IVF2, IDFDX2, JGANO2
      INTEGER NBCMP
      INTEGER IPG, IN
      INTEGER IPGF
      INTEGER NBF
      INTEGER TYMVOL,NDEGRE,IFA,TYV

      REAL*8 R8BID,R8BID2,R8BID3(3),R8BID4(3)
      REAL*8 DFDX(27),DFDY(27),DFDZ(27),HK,POIDS
      REAL*8 FPPX,FPPY,FPPZ
      REAL*8 FPDX,FPDY,FPDZ
      REAL*8 FRPX(27),FRPY(27),FRPZ(27)
      REAL*8 FRDX(27),FRDY(27),FRDZ(27)
      REAL*8 FOVOP(3)
      REAL*8 FOVOD(3)
      REAL*8 DSPX,DSPY,DSPZ
      REAL*8 DSDX,DSDY,DSDZ
      REAL*8 ERREST,COEFF
      REAL*8 TERPL1,TERMO1,TERPL2,TERMO2,TERPL3,TERMO3
      REAL*8 HF,INTPL,INTMO,INST
      REAL*8 S, UNSURS
      REAL*8 NX(9),NY(9),NZ(9),JACO(9)
      REAL*8 CHPX(9),CHPY(9),CHPZ(9),CHDX(9),CHDY(9),CHDZ(9)
      REAL*8 SGP11(9),SGP22(9),SGP33(9),SGP12(9),SGP13(9),SGP23(9)
      REAL*8 SGD11(9),SGD22(9),SGD33(9),SGD12(9),SGD13(9),SGD23(9)
      REAL*8 SIGP11(9),SIGP22(9),SIGP33(9),SIGP12(9),SIGP13(9),SIGP23(9)
      REAL*8 SIGD11(9),SIGD22(9),SIGD33(9),SIGD12(9),SIGD13(9),SIGD23(9)
      REAL*8 CHPLX(9),CHPLY(9),CHPLZ(9),CHMOX(9),CHMOY(9),CHMOZ(9)
      REAL*8 SOPL11(9),SOPL22(9),SOPL33(9),SOPL12(9),SOPL13(9),SOPL23(9)
      REAL*8 SOMO11(9),SOMO22(9),SOMO33(9),SOMO12(9),SOMO13(9),SOMO23(9)
      REAL*8 SIPL11(9),SIPL22(9),SIPL33(9),SIPL12(9),SIPL13(9),SIPL23(9)
      REAL*8 SIMO11(9),SIMO22(9),SIMO33(9),SIMO12(9),SIMO13(9),SIMO23(9)
      REAL*8 NUPLUS,NUMOIN
      REAL*8 RHO,VALRES(1)

      CHARACTER*2 CODRET(2)
      CHARACTER*8 TYPMAV, ELREFE
      CHARACTER*8 ELREFF, ELREFB
      CHARACTER*8 NOMPAR(1)
      CHARACTER*16 PHENOM
      CHARACTER*24 VALK(2)

      LOGICAL YAPRP, YAROP
      LOGICAL YAPRD, YAROD

C --- INITIALISATION DU TABLEAU DES NUMEROS DE NOEUDS FACE PAR FACE ----
C
C     NOE (IN,IFA,TYMVOL) : IN     : NUMERO DU NOEUD DANS LA FACE
C                           IFA    : NUMERO DE LA FACE
C                           TYMVOL : TYPE DE LA MAILLE VOLUMIQUE
C                                    1 : HEXAEDRE A 8,20 ET 27 NOEUDS
C                                    2 : PENTAEDRE A 6,15 ET 18 NOEUDS
C                                    3 : TETRAEDRE A 4 ET 10 NOEUDS
C                                    4 : PYRAMIDE A 5 ET 13 NOEUDS
C     VOIR TE003 POUR LES EXPLICATIONS DETAILLEES
C
      DATA NOE/1,4,3,2,12,11,10, 9,21, 1,2,6,5, 9,14,17,13,22,
     >         2,3,7,6,10,15,18,14,23, 3,4,8,7,11,16,19,15,24,
     >         4,1,5,8,12,13,20,16,25, 5,6,7,8,17,18,19,20,26,
     >         1,3,2,9,8,7, 3*0,       4,5,6,13,14,15, 3*0,
     >         1,2,5,4, 7,11,13,10,16, 2,3,6,5,8,12,14,11,17,
     >         1,4,6,3,10,15,12, 9,18, 9*0,
     >         1,3,2,7,6, 5, 3*0,      2,3,4,6,10,9, 3*0,
     >         3,1,4,7,8,10, 3*0,      1,2,4,5, 9,8, 3*0,
     >         9*0,                    9*0,
     >         1,2,5,6,11,10, 3*0,     2,3,5,7,12,11, 3*0,
     >         3,4,5,8,13,12, 3*0,     4,1,5,9,10,13, 3*0,
     >         1,4,3,2,9,8,7,6, 0,     9*0 /
C
C ----------------------------------------------------------------------
 1000 FORMAT(A,' :',(6(1X,1PE17.10)))
 2000 FORMAT(A,10I8)
C ----------------------------------------------------------------------
C 1 -------------- GESTION DES DONNEES ---------------------------------
C ----------------------------------------------------------------------
      CALL JEMARQ()

      CALL INFNIV(IFM,NIV)

C 1.1. --- LES INCONTOURNABLES
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PVOISIN','L',IVOIS)
C
      CALL JEVECH('PTEMPSR','L',JTIME)
      INST=ZR(JTIME-1+1)
C
      CALL JEVECH('PERREUR','E',IERR)

C 1.2. --- LES CARACTERISTIQUES DE LA MAILLE EN COURS

      CALL TECAEL(IADZI,IAZK24)
      VALK(1)=ZK24(IAZK24-1+3)
      VALK(2)=OPTION

      CALL ELREF1(ELREFE)

      IF ( NIV.GE.2 ) THEN
      WRITE(IFM,*) ' '
      WRITE(IFM,*) '================================================='
      WRITE(IFM,*) ' '
      WRITE(IFM,*) 'MAILLE NUMERO', ZI(IADZI),', DE TYPE ', ELREFE
      ENDIF

      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)

C 1.3. --- CHAMP DE CONTRAINTES

      CALL TECACH('OOO','PCONTNOP',3,ITAB,IRET)
      IADP=ITAB(1)
      NBCMP=ITAB(2)/NNO
      CALL TECACH('OOO','PCONTNOD',3,ITAB,IRET)
      IADD=ITAB(1)
C
C 1.4. --- CARTES DE PESANTEUR ET ROTATION
C
      CALL TECACH('ONN','PPESANRP',1,ITAB,IRET)
      IF ( ITAB(1).NE.0 ) THEN
        CALL JEVECH('PPESANRP','L',IPESP)
        YAPRP = .TRUE.
      ELSE
        YAPRP = .FALSE.
      ENDIF
      CALL TECACH('ONN','PROTATRP',1,ITAB,IRET)
      IF ( ITAB(1).NE.0 ) THEN
        CALL JEVECH('PROTATRP','L',IROTP)
        YAROP = .TRUE.
      ELSE
        YAROP = .FALSE.
      ENDIF
      CALL TECACH('ONN','PPESANRD',1,ITAB,IRET)
      IF ( ITAB(1).NE.0 ) THEN
        CALL JEVECH('PPESANRD','L',IPESD)
        YAPRD = .TRUE.
      ELSE
        YAPRD = .FALSE.
      ENDIF
      CALL TECACH('ONN','PROTATRD',1,ITAB,IRET)
      IF ( ITAB(1).NE.0 ) THEN
        CALL JEVECH('PROTATRD','L',IROTD)
        YAROD = .TRUE.
      ELSE
        YAROD = .FALSE.
      ENDIF
C
C 1.5. --- FORCES VOLUMIQUES EVENTUELLES
C          VALEURS REELLES ?
      CALL TECACH('ONN','PFRVOLUP',1,IFOVRP,IRET)
C          OU FONCTIONS ?
      IF ( IFOVRP.EQ.0 ) THEN
        CALL TECACH('ONN','PFFVOLUP',1,IFOVFP,IRET)
      ELSE
        IFOVFP = 0
      ENDIF
C          VALEURS REELLES ?
      CALL TECACH('ONN','PFRVOLUD',1,IFOVRD,IRET)
C          OU FONCTIONS ?
      IF ( IFOVRD.EQ.0 ) THEN
        CALL TECACH('ONN','PFFVOLUD',1,IFOVFD,IRET)
      ELSE
        IFOVFD = 0
      ENDIF
C
C 1.6. --- FORCES ET PRESSIONS AUX BORDS
C
      CALL JEVECH('PFORCEP','L',IREFP1)
      CALL JEVECH('PFORCED','L',IREFD1)
C
      CALL JEVECH('PPRESSP','L',IREFP2)
      CALL JEVECH('PPRESSD','L',IREFD2)
C
C 1.7. --- MATERIAU SI BESOIN
C
      IF ( YAPRP .OR. YAROP .OR. YAPRD .OR. YAROD ) THEN
C
        CALL JEVECH('PMATERC','L',IMATE)
        CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,CODRET)
        NOMPAR(1)='RHO'
        CALL RCVALA ( ZI(IMATE), ' ', PHENOM, 1, ' ', R8BID,
     >                1, NOMPAR, VALRES, CODRET, 'FM')
        RHO = VALRES(1)
CGN        WRITE(IFM,1000) 'RHO', RHO
C
      ENDIF
C
C 1.8. --- CALCUL DU COEFFICIENT S
C
      CALL JEVECH('PCONSTR','L',IBID)
      S = ZR(IBID-1+1)
      UNSURS = 1.D0/S

C ----------------------------------------------------------------------
C 2 -------------- CALCUL DU PREMIER TERME DE L'ERREUR -----------------
C ----------------------------------------------------------------------
C
C 2.1. --- CALCUL DU DIAMETRE HK DE LA MAILLE ----
C
      CALL UTHK(NOMTE,IGEOM,HK,NDIM,ITAB,IBID,IBID,IBID,NIV,IFM)
C
C 2.2. --- CALCUL DE LA FORCE DE PESANTEUR  ---
C
C ------ CALCUL DE LA FORCE DE PESANTEUR PB. PRIMAL --------------------
      IF ( YAPRP ) THEN
        FPPX=RHO*ZR(IPESP)*ZR(IPESP+1)
        FPPY=RHO*ZR(IPESP)*ZR(IPESP+2)
        FPPZ=RHO*ZR(IPESP)*ZR(IPESP+3)
      ELSE
        FPPX=0.D0
        FPPY=0.D0
        FPPZ=0.D0
      ENDIF
CGN      WRITE(IFM,1000) 'P PRIMAL',FPPX,FPPY,FPPZ
C ------ CALCUL DE LA FORCE DE PESANTEUR PB. DUAL ----------------------
      IF ( YAPRD ) THEN
        FPDX=RHO*ZR(IPESD)*ZR(IPESD+1)
        FPDY=RHO*ZR(IPESD)*ZR(IPESD+2)
        FPDZ=RHO*ZR(IPESD)*ZR(IPESD+3)
      ELSE
        FPDX=0.D0
        FPDY=0.D0
        FPDZ=0.D0
      ENDIF
CGN      WRITE(IFM,1000) 'P DUAL  ',FPDX,FPDY,FPDZ
C
C 2.3. --- CALCUL DE LA FORCE DE ROTATION ---
C ------ CALCUL DE LA FORCE DE ROTATION AUX POINTS DE GAUSS PB. PRIMAL -
C
      IF ( YAROP ) THEN
        CALL RESR3D (ZR(IROTP),ZR(IGEOM),ZR(IVF),RHO,NNO,NPG,
     >               FRPX,FRPY,FRPZ)
      ELSE
        DO 231 , IPG = 1 , NPG
          FRPX(IPG) = 0.D0
          FRPY(IPG) = 0.D0
          FRPZ(IPG) = 0.D0
  231   CONTINUE
      ENDIF
CGN      WRITE(IFM,1000) 'R PRIMAL X',(FRPX(IPG),IPG = 1 , NPG)
CGN      WRITE(IFM,1000) 'R PRIMAL Y',(FRPY(IPG),IPG = 1 , NPG)
CGN      WRITE(IFM,1000) 'R PRIMAL Z',(FRPZ(IPG),IPG = 1 , NPG)
C ------ CALCUL DE LA FORCE DE ROTATION AUX POINTS DE GAUSS PB. DUAL ---
      IF ( YAROD ) THEN
        CALL RESR3D (ZR(IROTD),ZR(IGEOM),ZR(IVF),RHO,NNO,NPG,
     >               FRDX,FRDY,FRDZ)
      ELSE
        DO 232 , IPG = 1 , NPG
          FRDX(IPG) = 0.D0
          FRDY(IPG) = 0.D0
          FRDZ(IPG) = 0.D0
  232   CONTINUE
      ENDIF
CGN      WRITE(IFM,1000) 'R DUAL X  ',(FRDX(IPG),IPG = 1 , NPG)
CGN      WRITE(IFM,1000) 'R DUAL Y  ',(FRDY(IPG),IPG = 1 , NPG)
CGN      WRITE(IFM,1000) 'R DUAL Z  ',(FRDZ(IPG),IPG = 1 , NPG)
C
C 2.4. --- CALCUL DE LA FORCE VOLUMIQUE EVENTUELLE ---
C
      IF ( IFOVRP.NE.0 ) THEN
        FOVOP(1) = ZR(IFOVRP  )
        FOVOP(2) = ZR(IFOVRP+1)
        FOVOP(3) = ZR(IFOVRP+2)
C
      ELSEIF ( IFOVFP.NE.0 ) THEN
        NOMPAR(1) = 'INST'
        R8BID3(1) = INST
C       SI UNE COMPOSANTE N'A PAS ETE DECRITE, ASTER AURA MIS PAR
C       DEFAUT LA FONCTION NULLE &FOZERO. ON LE REPERE POUR
C       IMPOSER LA VALEUR 0 SANS FAIRE DE CALCULS INUTILES
        DO 241 , IBID = 1 , NDIM
          IF ( ZK8(IFOVFP+IBID-1)(1:7).EQ.'&FOZERO' ) THEN
            FOVOP(IBID) = 0.D0
          ELSE
            CALL FOINTE('FM',ZK8(IFOVFP+IBID-1),1,NOMPAR,R8BID3,
     >                   FOVOP(IBID),IRET)
          ENDIF
  241   CONTINUE
CGN        WRITE(IFM,*) 'F PRIMAL X : ',ZK8(IFOVFP)
CGN        WRITE(IFM,*) 'F PRIMAL Y : ',ZK8(IFOVFP+1)
CGN        WRITE(IFM,*) 'F PRIMAL Z : ',ZK8(IFOVFP+2)
      ENDIF
CGN      WRITE(IFM,2000) 'IFOVRP', IFOVRP
CGN      WRITE(IFM,2000) 'IFOVFP', IFOVRP
C
      IF ( IFOVRD.NE.0 ) THEN
        FOVOD(1) = ZR(IFOVRD  )
        FOVOD(2) = ZR(IFOVRD+1)
        FOVOD(3) = ZR(IFOVRD+2)
C
      ELSEIF ( IFOVFD.NE.0 ) THEN
        NOMPAR(1) = 'INST'
        R8BID3(1) = INST
C       SI UNE COMPOSANTE N'A PAS ETE DECRITE, ASTER AURA MIS PAR
C       DEFAUT LA FONCTION NULLE &FOZERO. ON LE REPERE POUR
C       IMPOSER LA VALEUR 0 SANS FAIRE DE CALCULS INUTILES
        DO 242 , IBID = 1 , NDIM
          IF ( ZK8(IFOVFD+IBID-1)(1:7).EQ.'&FOZERO' ) THEN
            FOVOD(IBID) = 0.D0
          ELSE
            CALL FOINTE('FM',ZK8(IFOVFD+IBID-1),1,NOMPAR,R8BID3,
     >                   FOVOD(IBID),IRET)
          ENDIF
  242   CONTINUE
CGN        WRITE(IFM,*) 'F DUAL X   : ',ZK8(IFOVFD)
CGN        WRITE(IFM,*) 'F DUAL Y   : ',ZK8(IFOVFD+1)
CGN        WRITE(IFM,*) 'F DUAL Z   : ',ZK8(IFOVFD+2)
      ENDIF
CGN      WRITE(IFM,2000) 'IFOVRD', IFOVRD
CGN      WRITE(IFM,2000) 'IFOVFD', IFOVRD
C
C 2.5. --- CALCUL DU TERME D'ERREUR AVEC INTEGRATION DE GAUSS ---
C
      TERPL1 = 0.D0
      TERMO1 = 0.D0
C
      DO 25 , IPG = 1 , NPG
C
C ------- CALCUL DES DERIVEES DES FONCTIONS DE FORMES /X, /Y ET /Z -----
C
        IAUX = IPG
        CALL DFDM3D(NNO,IAUX,IPOIDS,IDFDE,ZR(IGEOM),
     >              DFDX,DFDY,DFDZ,POIDS)
C
C ------- CALCUL DE LA DIVERGENCE ET DE LA NORME DE SIGMA PB. PRIMAL ---
C
        CALL ERMEV3(NNO,IAUX,IVF,IADP,NBCMP,DFDX,DFDY,DFDZ,
     >              DSPX,DSPY,DSPZ,R8BID)
C
C ------- CALCUL DE LA DIVERGENCE ET DE LA NORME DE SIGMA PB. DUAL -----
C
        CALL ERMEV3(NNO,IAUX,IVF,IADD,NBCMP,DFDX,DFDY,DFDZ,
     >              DSDX,DSDY,DSDZ,R8BID)
C
C ------- CUMUL
C
        R8BID3(1) = FPPX + FRPX(IPG) + DSPX
        R8BID3(2) = FPPY + FRPY(IPG) + DSPY
        R8BID3(3) = FPPZ + FRPZ(IPG) + DSPZ
C
        R8BID4(1) = FPDX + FRDX(IPG) + DSDX
        R8BID4(2) = FPDY + FRDY(IPG) + DSDY
        R8BID4(3) = FPDZ + FRDZ(IPG) + DSDZ
C
C ------- PRISE EN COMPTE DE L'EFFORT VOLUMIQUE PRIMAL EVENTUEL --------
C
        IF ( IFOVRP.NE.0 .OR. IFOVFP.NE.0 ) THEN
C
CGN          WRITE(IFM,1000) 'F PRIMAL X', FOVOP(1)
CGN          WRITE(IFM,1000) 'F PRIMAL Y', FOVOP(2)
CGN          WRITE(IFM,1000) 'F PRIMAL Z', FOVOP(3)
          R8BID3(1) = R8BID3(1) + FOVOP(1)
          R8BID3(2) = R8BID3(2) + FOVOP(2)
          R8BID3(3) = R8BID3(3) + FOVOP(3)
C
        ENDIF
C
C ------- PRISE EN COMPTE DE L'EFFORT VOLUMIQUE DUAL EVENTUEL ----------
C
        IF ( IFOVRD.NE.0 .OR. IFOVFD.NE.0 ) THEN
C
CGN          WRITE(IFM,1000) 'F DUAL X  ',FOVOD(1)
CGN          WRITE(IFM,1000) 'F DUAL Y  ',FOVOD(2)
CGN          WRITE(IFM,1000) 'F DUAL Z  ',FOVOD(3)
          R8BID4(1) = R8BID4(1) + FOVOD(1)
          R8BID4(2) = R8BID4(2) + FOVOD(2)
          R8BID4(3) = R8BID4(3) + FOVOD(3)
C
        ENDIF
C
C ------- CUMUL DU TERME D'ERREUR
C
        TERPL1 = TERPL1
     >         + ( (S*R8BID3(1)+UNSURS*R8BID4(1))**2
     >         +   (S*R8BID3(2)+UNSURS*R8BID4(2))**2
     >         +   (S*R8BID3(3)+UNSURS*R8BID4(3))**2 ) * POIDS
C
        TERMO1 = TERMO1
     >         + ( (S*R8BID3(1)-UNSURS*R8BID4(1))**2
     >         +   (S*R8BID3(2)-UNSURS*R8BID4(2))**2
     >         +   (S*R8BID3(3)-UNSURS*R8BID4(3))**2 ) * POIDS
        IF ( NIV.GE.2 ) THEN
          WRITE(IFM,1000) 'POIDS', POIDS
          WRITE(IFM,1000) 'A2 + B2 + C2',
     >             (S*R8BID3(1)+UNSURS*R8BID4(1))**2
     >         +   (S*R8BID3(2)+UNSURS*R8BID4(2))**2
     >         +   (S*R8BID3(3)+UNSURS*R8BID4(3))**2
          WRITE(IFM,1000) '==> TERPL1    ', TERPL1
          WRITE(IFM,1000) 'A2 + B2 + C2',
     >             (S*R8BID3(1)-UNSURS*R8BID4(1))**2
     >         +   (S*R8BID3(2)-UNSURS*R8BID4(2))**2
     >         +   (S*R8BID3(3)-UNSURS*R8BID4(3))**2
          WRITE(IFM,1000) '==> TERMO1    ', TERMO1
        ENDIF
C
   25 CONTINUE
C
      TERPL1=(HK**2)*ABS(TERPL1)
      TERMO1=(HK**2)*ABS(TERMO1)
CGN            WRITE(IFM,*) TERPL1,TERMO1
C
C ----------------------------------------------------------------------
C ------------ FIN DU CALCUL DU PREMIER TERME DE L'ERREUR --------------
C ----------------------------------------------------------------------
C
C ----------------------------------------------------------------------
C 3. ------- CALCUL DES DEUXIEME ET TROISIEME TERMES DE L'ERREUR -------
C ----------------------------------------------------------------------
C
C 3.1. ---- INFORMATIONS SUR LA MAILLE COURANTE : ----------------------
C       TYMVOL : TYPE DE LA MAILLE VOLUMIQUE
C       NDEGRE : DEGRE DE L'ELEMENT
C       NBF    : NOMBRE DE FACES DE LA MAILLE VOLUMIQUE
C       ELREFF : DENOMINATION DE LA MAILLE FACE DE ELREFE - FAMILLE 1
C       ELREFB : DENOMINATION DE LA MAILLE FACE DE ELREFE - FAMILLE 2
C      --- REMARQUE : ON IMPOSE UNE FAMILLE DE POINTS DE GAUSS
C
      CALL ELREF7 ( ELREFE,
     >              TYMVOL, NDEGRE, NBF, ELREFF, ELREFB )
CGN      WRITE(6,*) 'TYPE MAILLE VOLUMIQUE COURANTE :',TYMVOL
C --- CARACTERISTIQUES DES FACES DE BORD DE LA FAMILLE 1 ---------------
      CALL ELREF4 ( ELREFF, 'NOEU',
     >              NDIMF, NNOF, NNOSF, NPGF, IPOIDF, IVFF,
     >              IDFDXF, JGANOF )
CGN      WRITE(IFM,2000) 'NDIMF',NDIMF
CGN      WRITE(IFM,2000) 'NNOSF,NNOF,NPGF',NNOSF,NNOF,NPGF
CGN      WRITE(IFM,1000) 'IPOIDF', (ZR(IPOIDF+IFA),IFA=0,NPGF-1)
C
C --- COMPLEMENT EVENTUEL POUR LES MAILLES QUI ONT 2 TYPES DE ---
C --- MAILLES DE BORD (PENTAEDRE, PYRAMIDE) ---
C
      IF ( ELREFB(1:1).NE.' ' ) THEN
         CALL ELREF4 ( ELREFB, 'NOEU',
     >                 NDIMF, NNO2, NNOS2, NPG2, IPOID2, IVF2,
     >                 IDFDX2, JGANO2 )
CGN       WRITE(IFM,2000) 'NDIMF,NNO2',NDIMF,NNO2
CGN       WRITE(IFM,2000) 'NNOS2,NPG2',NNOS2,NPG2
CGN       WRITE(IFM,1000) 'IPOID2', (ZR(IPOID2+IFA),IFA=0,NPG2-1)
      ENDIF
C
C 3.2. --- BOUCLE SUR LES FACES DE LA MAILLE VOLUMIQUE --------------
C
      TERPL2 = 0.D0
      TERMO2 = 0.D0
      TERPL3 = 0.D0
      TERMO3 = 0.D0
      DO 320 , IFA = 1 , NBF
C
C ------TEST DU TYPE DE VOISIN -----------------------------------------
C
        TYV=ZI(IVOIS+7+IFA)
C
        IF ( TYV.NE.0 ) THEN
C
C ------- RECUPERATION DU TYPE DE LA MAILLE VOISINE
C
          CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',TYV),TYPMAV)
          IF ( NIV.GE.2  ) THEN
            WRITE(IFM,1003)  IFA, ZI(IVOIS+IFA), TYPMAV
 1003 FORMAT (I2,'-EME FACE DE NUMERO',I10,' ==> TYPMAV = ', A)
          ENDIF
C
C --- QUAND ON ARRIVE AUX FACES QUAD DES PENTAEDRES OU DES PYRAMIDES ---
C --- IL FAUT REMPLACER LES CARACTERISTIQUES DE LA FAMILLE 1         ---
C --- PAR CELLES DE LA FAMILLE 2                                     ---
C
          IF ( ( TYMVOL.EQ.2 .AND. IFA.GE.3 ) .OR.
     >         ( TYMVOL.EQ.4 .AND. IFA.GE.5 ) ) THEN
C
            NNOF   = NNO2
            NPGF   = NPG2
            NNOSF  = NNOS2
            IPOIDF = IPOID2
            IDFDXF = IDFDX2
C
          ENDIF
CGN      WRITE(IFM,*) '. NPGF =', NPGF
C
C ----- CALCUL DU DIAMETRE HF DE LA FACE ----------
C
          IBID=0
          CALL UTHK(NOMTE,IGEOM,HF,IBID,NOE,NNOSF,TYMVOL,IFA,NIV,IFM)
C
C ------- CALCUL DE NORMALES ET JACOBIENS AUX POINTS DE GAUSS ----------
C
          IAUX = IFA
          CALL CALNOR ('3D', IGEOM,
     >                  IBID, IBID, IBID, R8BID,
     >                  NNOF, NPGF, NOE, IAUX, TYMVOL, IDFDXF,
     >                  JACO, NX, NY, NZ,
     >                  R8BID3, R8BID4, R8BID2 )
C
C ----------------------------------------------------------------------
C --------------- CALCUL DU DEUXIEME TERME DE L'ERREUR -----------------
C --------------- LE BORD VOISIN EST UN VOLUME -------------------------
C ----------------------------------------------------------------------
C
          IF ( TYPMAV(1:4).EQ.'HEXA' .OR.
     >         TYPMAV(1:4).EQ.'PENT' .OR.
     >         TYPMAV(1:4).EQ.'TETR' .OR.
     >         TYPMAV(1:4).EQ.'PYRA' ) THEN
C
C ------- CALCUL DU SAUT DE CONTRAINTE ENTRE ELEMENTS PB. PRIMAL -------
C ------- CE CHAMP SGPXX EST EXPRIME SUR LES NOEUDS DE LA FACE ---------
C
            CALL ERMES3(NOE,IFA,TYMVOL,NNOF,TYPMAV,
     >                  IREFP1,IVOIS,IADP,NBCMP,
     >                  SGP11,SGP22,SGP33,SGP12,SGP13,SGP23)
C
C ------- CALCUL DU SAUT DE CONTRAINTE ENTRE ELEMENTS PB. DUAL ---------
C
            CALL ERMES3(NOE,IFA,TYMVOL,NNOF,TYPMAV,
     >                  IREFD1,IVOIS,IADD,NBCMP,
     >                  SGD11,SGD22,SGD33,SGD12,SGD13,SGD23)
C
C ------- CALCUL DE L'INTEGRALE SUR LA FACE ----------------------------
C ------- ATTENTION : CELA MARCHE CAR ON A CHOISI LA FAMILLE -----------
C ------- AVEC LES POINTS DE GAUSS SUR LES NOEUDS ----------------------
C
            DO 321 , IN = 1 , NNOF
C
              SOPL11(IN)=S*SGP11(IN)+UNSURS*SGD11(IN)
              SOPL22(IN)=S*SGP22(IN)+UNSURS*SGD22(IN)
              SOPL33(IN)=S*SGP33(IN)+UNSURS*SGD33(IN)
              SOPL12(IN)=S*SGP12(IN)+UNSURS*SGD12(IN)
              SOPL13(IN)=S*SGP13(IN)+UNSURS*SGD13(IN)
              SOPL23(IN)=S*SGP23(IN)+UNSURS*SGD23(IN)
C
              SOMO11(IN)=S*SGP11(IN)-UNSURS*SGD11(IN)
              SOMO22(IN)=S*SGP22(IN)-UNSURS*SGD22(IN)
              SOMO33(IN)=S*SGP33(IN)-UNSURS*SGD33(IN)
              SOMO12(IN)=S*SGP12(IN)-UNSURS*SGD12(IN)
              SOMO13(IN)=S*SGP13(IN)-UNSURS*SGD13(IN)
              SOMO23(IN)=S*SGP23(IN)-UNSURS*SGD23(IN)
C
              CHPLX(IN) = 0.D0
              CHPLY(IN) = 0.D0
              CHPLZ(IN) = 0.D0
              CHMOX(IN) = 0.D0
              CHMOY(IN) = 0.D0
              CHMOZ(IN) = 0.D0
C
  321       CONTINUE
C
            CALL INTEGA(NPGF,
     >                  JACO,ZR(IPOIDF),
     >                  CHPLX,CHPLY,CHPLZ,
     >                  SOPL11,SOPL22,SOPL33,SOPL12,SOPL13,SOPL23,
     >                  NX,NY,NZ,
     >                  INTPL)
C
            CALL INTEGA(NPGF,
     >                  JACO,ZR(IPOIDF),
     >                  CHMOX,CHMOY,CHMOZ,
     >                  SOMO11,SOMO22,SOMO33,SOMO12,SOMO13,SOMO23,
     >                  NX,NY,NZ,
     >                  INTMO)
C
C ------- CALCUL DU TERME D'ERREUR -------------------------------------
C
            TERPL2=TERPL2+0.5D0*HF*ABS(INTPL)
            TERMO2=TERMO2+0.5D0*HF*ABS(INTMO)
            IF ( NIV.GE.2 ) THEN
              WRITE(IFM,1000) 'VOLU INTPL', INTPL
              WRITE(IFM,1000) '==> TERPL2', TERPL2
              WRITE(IFM,1000) 'VOLU INTMO', INTMO
              WRITE(IFM,1000) '==> TERMO2', TERMO2
            ENDIF
C
C ----------------------------------------------------------------------
C --------------- CALCUL DU TROISIEME TERME DE L'ERREUR ----------------
C --------------- LE BORD VOISIN EST UNE FACE --------------------------
C ----------------------------------------------------------------------
C
          ELSEIF ( TYPMAV(1:4).EQ.'QUAD' .OR.
     >             TYPMAV(1:4).EQ.'TRIA' ) THEN
C
C ------- CALCUL EFFORTS SURFACIQUES ET DES CONTRAINTES PB. PRIMAL -----
C
            CALL ERMEB3(NOE,IFA,TYMVOL,NNOF,
     >                  IREFP1,IREFP2,IVOIS,IGEOM,IADP,
     >                  NBCMP,INST,
     >                  NX,NY,NZ,
     >                  SIGP11,SIGP22,SIGP33,SIGP12,SIGP13,SIGP23,
     >                  CHPX,CHPY,CHPZ)
C
C ------- CALCUL EFFORTS SURFACIQUES ET DES CONTRAINTES PB. DUAL -------
C
            CALL ERMEB3(NOE,IFA,TYMVOL,NNOF,
     >                  IREFD1,IREFD2,IVOIS,IGEOM,IADD,
     >                  NBCMP,INST,
     >                  NX,NY,NZ,
     >                  SIGD11,SIGD22,SIGD33,SIGD12,SIGD13,SIGD23,
     >                  CHDX,CHDY,CHDZ)
C
C ------- CALCUL EFFORTS SURFACIQUES ET DES CONTRAINTES PB. GLOBAL -----
C
            DO 322 , IN = 1 , NNOF
C
              CHPLX(IN)=S*CHPX(IN)+UNSURS*CHDX(IN)
              CHPLY(IN)=S*CHPY(IN)+UNSURS*CHDY(IN)
              CHPLZ(IN)=S*CHPZ(IN)+UNSURS*CHDZ(IN)
              SIPL11(IN)=S*SIGP11(IN)+UNSURS*SIGD11(IN)
              SIPL22(IN)=S*SIGP22(IN)+UNSURS*SIGD22(IN)
              SIPL33(IN)=S*SIGP33(IN)+UNSURS*SIGD33(IN)
              SIPL12(IN)=S*SIGP12(IN)+UNSURS*SIGD12(IN)
              SIPL13(IN)=S*SIGP13(IN)+UNSURS*SIGD13(IN)
              SIPL23(IN)=S*SIGP23(IN)+UNSURS*SIGD23(IN)
C
              CHMOX(IN)=S*CHPX(IN)-UNSURS*CHDX(IN)
              CHMOY(IN)=S*CHPY(IN)-UNSURS*CHDY(IN)
              CHMOZ(IN)=S*CHPZ(IN)-UNSURS*CHDZ(IN)
              SIMO11(IN)=S*SIGP11(IN)-UNSURS*SIGD11(IN)
              SIMO22(IN)=S*SIGP22(IN)-UNSURS*SIGD22(IN)
              SIMO33(IN)=S*SIGP33(IN)-UNSURS*SIGD33(IN)
              SIMO12(IN)=S*SIGP12(IN)-UNSURS*SIGD12(IN)
              SIMO13(IN)=S*SIGP13(IN)-UNSURS*SIGD13(IN)
              SIMO23(IN)=S*SIGP23(IN)-UNSURS*SIGD23(IN)
C
  322       CONTINUE
C
C ------- CALCUL DE L'INTEGRALE SUR LA FACE ----------------------------
C ------- ATTENTION : CELA MARCHE CAR ON A CHOISI LA FAMILLE -----------
C ------- AVEC LES POINTS DE GAUSS SUR LES NOEUDS ----------------------
C
            CALL INTEGA(NPGF,
     >                  JACO,ZR(IPOIDF),
     >                  CHPLX,CHPLY,CHPLZ,
     >                  SIPL11,SIPL22,SIPL33,SIPL12,SIPL13,SIPL23,
     >                  NX,NY,NZ,
     >                  INTPL)
C
            CALL INTEGA(NPGF,
     >                  JACO,ZR(IPOIDF),
     >                  CHMOX,CHMOY,CHMOZ,
     >                  SIMO11,SIMO22,SIMO33,SIMO12,SIMO13,SIMO23,
     >                  NX,NY,NZ,
     >                  INTMO)
C
C ------- CALCUL DU TERME D'ERREUR -------------------------------------
C
            TERPL3=TERPL3+HF*ABS(INTPL)
            TERMO3=TERMO3+HF*ABS(INTMO)
            IF ( NIV.GE.2 ) THEN
              WRITE(IFM,1000) 'SURF INTPL', INTPL
              WRITE(IFM,1000) '==> TERPL3', TERPL3
              WRITE(IFM,1000) 'SURF INTMO', INTMO
              WRITE(IFM,1000) '==> TERMO3', TERMO3
            ENDIF
C
C ----------------------------------------------------------------------
C --------------- CURIEUX ----------------------------------------------
C ----------------------------------------------------------------------
C
          ELSE
C
            VALK(1)=TYPMAV(1:4)
            CALL U2MESK('F','INDICATEUR_10',1,VALK)
C
          ENDIF
C
        ENDIF
C
  320 CONTINUE
C
C ----------------------------------------------------------------------
C ------- FIN DU CALCUL DU DEUXIEME ET TROISIEME TERME DE L'ERREUR -----
C ----------------------------------------------------------------------
C
C ----------------------------------------------------------------------
C 4. ------- MISE EN MEMOIRE DES DIFFERENTS TERMES DE L'ERREUR ---------
C ----------------------------------------------------------------------
C
      IF (NDEGRE.EQ.2) THEN
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
C       DIAMETRE
      ZR(IERR+9)=HK
C
      CALL JEDEMA()
C
      END
