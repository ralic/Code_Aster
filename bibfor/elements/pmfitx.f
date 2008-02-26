      SUBROUTINE PMFITX(ICDMAT,ISW,CASECT,GTO)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 25/02/2008   AUTEUR FLEJOU J-L.FLEJOU 
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
      IMPLICIT NONE
      INTEGER ICDMAT,ISW,ITO
      REAL*8 CASECT(6),GTO
C     ------------------------------------------------------------------
C     CALCULE INTEGRALE(E.DS) ou INTEGRALE(RHO.DS)
C     AVEC E OU RHO CONSTANT PAR GROUPE
C     IN : ICDMAT : MATERIAU CODE
C     IN : ISW  1-> 'E' ou 2 -> 'RHO' ou 3 -> 'RHO' facultatif
C     OUT : CASECT
C     OUT : GTO : G DE MATER TORSION SI ISW=1
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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

      INTEGER NBFIB,NCARFI,JACF,IRET
      REAL*8 CASEC1(6)
      REAL*8 ZERO,UN,DEUX
      PARAMETER (ZERO=0.0D+0,UN=1.0D0,DEUX=2.D0)
      INTEGER NBGF,INBF,ICOMPO,ISDCOM,I,IPOS,ICP,NUGF,IG,NBFIG,NBGFMX
      INTEGER LCAGE,LABSC
      REAL*8  RHO,RHOS, RHOFI, RHOFE, CM, PHIE, PHII
      REAL*8      VAL,E,NU,VALRES(4),ABSMOY
      CHARACTER*8 MATERI,NOMRE4(4)
      CHARACTER*2 CODRES(4),NOMRES(2)
C     ------------------------------------------------------------------
      DATA NOMRE4/'RHO','RHO_F_IN','RHO_F_EX','CM'/
C     ------------------------------------------------------------------

C     --- RECUPERATION DES CARACTERISTIQUES DES FIBRES :
      CALL JEVECH('PNBSP_I','L',INBF)
      NBGF=ZI(INBF+1)
      CALL JEVECH('PFIBRES','L',JACF)
      NCARFI = 3

C --- RECUPERATION DES DIFFERENTS MATERIAUX DANS SDCOMP DANS COMPOR
      CALL JEVECH('PCOMPOR','L',ICOMPO)
      CALL JEVEUO(ZK16(ICOMPO-1+6),'L',ISDCOM)

      DO 10 I = 1,6
         CASECT(I) = ZERO
10    CONTINUE
C --- BOUCLE SUR LES GROUPES DE FIBRE
      IPOS=JACF
      DO 100 IG=1,NBGF
         NUGF=ZI(INBF+1+IG)
         ICP=ISDCOM-1+(NUGF-1)*6
         READ(ZK16(ICP+6),'(I16)')NBFIG
         MATERI=ZK16(ICP+2)(1:8)
C ---    CALCUL DES CARACTERISTIQUES DU GROUPE ---
         CALL PMFITG(NBFIG,NCARFI,ZR(IPOS),CASEC1)
C ---    ON MULTIPLIE PAR RHO OU E (CONSTANT SUR LE GROUPE)
         IF(ISW.EQ.1)THEN
            CALL RCVALB('RIGI',1,1,'+',ICDMAT,MATERI,'ELAS',
     &                  0,' ',ZERO,1,'E',VAL,CODRES,'  ')
            IF(CODRES(1).EQ.'NO') THEN
               CALL RCVALB('RIGI',1,1,'+',ICDMAT,MATERI,'ELAS_FLUI',
     &                     0,' ',ZERO,1,'E',VAL,CODRES,'FM')
            ENDIF
         ELSEIF(ISW.EQ.2)THEN
            CALL RCVALA(ICDMAT,MATERI,'ELAS',0,' ',ZERO,1,
     &                  'RHO',VAL,CODRES,'  ')
            IF(CODRES(1).EQ.'NO') THEN
               CALL JEVECH('PCAGEPO','L',LCAGE)
               CALL JEVECH('PABSCUR','L',LABSC)
               ABSMOY = (ZR(LABSC-1+1)+ZR(LABSC-1+2))/DEUX
               CALL RCVALA(ICDMAT,MATERI,'ELAS_FLUI',1,'ABSC',ABSMOY,4,
     &                     NOMRE4,VALRES,CODRES,'FM')
               RHOS  = VALRES(1)
               RHOFI = VALRES(2)
               RHOFE = VALRES(3)
               CM    = VALRES(4)
               PHIE = ZR(LCAGE-1+1)*DEUX
               IF (PHIE.EQ.0.D0) THEN
                  CALL U2MESS('F','ELEMENTS3_26')
               END IF
               PHII = (PHIE-DEUX*ZR(LCAGE-1+2))
               CALL RHOEQU(RHO,RHOS,RHOFI,RHOFE,CM,PHII,PHIE)
               VAL = RHO
            ENDIF
         ELSEIF(ISW.EQ.3)THEN
            CALL RCVALA(ICDMAT,MATERI,'ELAS',0,' ',ZERO,1,
     &                  'RHO',VAL,CODRES,'  ')
            IF (CODRES(1).NE.'OK' ) VAL = ZERO
         ENDIF
         DO 20 I = 1,6
            CASECT(I) = CASECT(I) + VAL*CASEC1(I)
20       CONTINUE
         IPOS=IPOS+NBFIG*NCARFI
100   CONTINUE

C ---  SI ITO=1 ON RECUPERE LE MATERIAU DE TORSION
      IF (ISW.EQ.1) THEN
         READ(ZK16(ICOMPO-1+7),'(I16)')NBGFMX
         MATERI=ZK16(ISDCOM-1+NBGFMX*6+1)
         NOMRES(1) = 'E'
         NOMRES(2) = 'NU'
         CALL RCVALB('RIGI',1,1,'+',ICDMAT,MATERI,'ELAS',
     &               0,' ',ZERO,2,NOMRES,VALRES,CODRES, '  ')

         IF (CODRES(1).EQ.'NO') THEN
            CALL RCVALB('RIGI',1,1,'+',ICDMAT,MATERI,'ELAS_FLUI',
     &                  0,' ',ZERO,2,NOMRES,VALRES,CODRES,'FM')
         ENDIF
         E = VALRES(1)
         NU = VALRES(2)
         GTO = E/ (DEUX* (UN+NU))
      ENDIF
      END
