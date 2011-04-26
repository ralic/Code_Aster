      SUBROUTINE NMZMAT (FAMI, KPG, KSP, NDIM,   TYPMOD, COMPOR,
     &                   INSTAM, INSTAP, EPSM,
     &                   DEPS,   SIGM,   VIM,    OPTION, ANGMAS,
     &                   SIGP,   VIP,    DSIDEP, CODRET)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR DELMAS J.DELMAS 
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
C RESPONSABLE PROIX J.M.PROIX
C
C ----------------------------------------------------------------------
C     INTEGRATION DU COMPORTEMENT EN UTILISANT LA BIBLIOTHEQUE ZMAT
C ----------------------------------------------------------------------
C     ROUTINE INTERFACE : Zaster.c (EN C++ DANS libzAster.so)
C
C ARGUMENTS DE ZASTER (SIMPLEMENT DEDUITS DE CEUX DE NMZMAT) :
C IN :
C  FAMI    (K) : LISTE DE NOMS DE FAMILLES DE POINTS DE GAUSS
C  KPG     (I) : NUMERO DE POINT DE GAUSS
C  KSP     (I) : NUMERO DE SOUS-POINT
C  NDIM    (I) : DIMENSION DE L'ESPACE (2 OU 3)
C  TYPMOD  (K) : TYPE DE MODELE (1:3D, 2:AXIS, 3:D_PLAN, 4:C_PLAN)
C  COMPOR  : (1) = TYPE DE RELATION COMPORTEMENT (INUTILE ICI)
C            (2) = NB VARIABLES INTERNES / PG (UTILISE ICI)
C            (3) = HYPOTHESE SUR LES DEFORMATIONS (UTILISE)
C            (6) NUNIT  : NUMERO DE L'UNITE LOGIQUE QUI CONTIENT
C                LES DONNEES POUR LE COMPORTEMENT ZMAT
C  INSTAM (R8) : INSTANT DU CALCUL PRECEDENT
C  INSTAP (R8) : INSTANT DU CALCUL
C  EPSM  (R8*) : DEFORMATIONS A L'INSTANT DU CALCUL PRECEDENT
C  DEPS  (R8*) : INCREMENT DE DEFORMATION TOTALE :
C                   DEPS(T) = DEPS(MECANIQUE(T)) + DEPS(DILATATION(T))
C  SIGM  (R8*) : CONTRAINTES A L'INSTANT DU CALCUL PRECEDENT
C  VIM   (R8*) : VARIABLES INTERNES A L'INSTANT DU CALCUL PRECEDENT
C  OPTION  (I) : OPTION (1:RIGI_MECA_TANG, 2:FULL_MECA , 3:RAPH_MECA)
C  ANGMAS  : LES TROIS ANGLES DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
C
C IN/OUT :
C  VIP   (R8*) : VARIABLES INTERNES
C                IN  : ESTIMATION (ITERATION PRECEDENTE OU LAG. AUGM.)
C                OUT : EN T+
C
C OUT :
C  SIGP   (R8*) : CONTRAINTES A L'INSTANT DU CALCUL
C  DSIDEP (R8*) : MATRICE CARREE
C ----------------------------------------------------------------------
C
      IMPLICIT NONE
      INTEGER    NDIM,CODRET,I,NUNIT,KPG,KSP,NVARCM,IRET2,IDBG,NUMA
      PARAMETER (NVARCM=5)
      CHARACTER*8        TYPMOD(*)
      CHARACTER*16       COMPOR(*), OPTION, NOMVAR(NVARCM)
      CHARACTER*(*)      FAMI
      REAL*8             INSTAM, INSTAP
      REAL*8             EPSM(*), DEPS(*), DSIDEP(*)
      REAL*8             SIGM(*), VIM(*), SIGP(*), VIP(*)
      REAL*8             ANGMAS(7),ANGEUL(3)
      REAL*8             VARPLU(NVARCM),VARMOI(NVARCM)
      REAL*8             VARREF(NVARCM)
C
C-------- DEBUT COMMUNS NORMALISES  JEVEUX  ----------------------------
C
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
C
C-----  FIN  COMMUNS NORMALISES  JEVEUX  -------------------------------
C
      INTEGER            IZI,IZK
      INTEGER MODELE,NVAR,NDEF,NOPT
C
C     MODELE=1 EN 3D,2 EN AXIS, 3 EN D_PLAN, 4 EN C_PLAN
      IF (TYPMOD(1).EQ.'3D') THEN
         MODELE=1
      ELSEIF (TYPMOD(1).EQ.'AXIS') THEN
        MODELE=2
      ELSEIF (TYPMOD(1).EQ.'D_PLAN') THEN
        MODELE=3
      ELSE
        MODELE=4
      ENDIF
      READ (COMPOR(7),'(I16)') NUNIT
      READ (COMPOR(2),'(I16)') NVAR
      IF (COMPOR(3).EQ.'GDEF_HYPO_ELAS') THEN
         NDEF=2
      ELSE
         NDEF=1
      ENDIF

      IF(OPTION.EQ.'RIGI_MECA_TANG') THEN
         NOPT=1
      ELSE IF(OPTION.EQ.'FULL_MECA') THEN
         NOPT=2
      ELSE
         NOPT=3
      ENDIF
C
C     VARIABLES DE COMMANDE
      NOMVAR(1)='temperature'
      NOMVAR(2)='fluence'
      NOMVAR(3)='corrosion'
      NOMVAR(4)='hydratation'
      NOMVAR(5)='sechage'

C     TEMPERATURE
      CALL RCVARC(' ','TEMP','-',FAMI,KPG,KSP,VARMOI(1),IRET2)
      IF (IRET2.GT.0) VARMOI(1)=0.D0
      CALL RCVARC(' ','TEMP','+',FAMI,KPG,KSP,VARPLU(1),IRET2)
      IF (IRET2.GT.0) VARPLU(1)=0.D0
      CALL RCVARC(' ','TEMP','REF',FAMI,KPG,KSP,VARREF(1),IRET2)
      IF (IRET2.GT.0) VARREF(1)=0.D0

C     IRRADIATION AU POINT CONSIDERE
C     FLUX NEUTRONIQUE
      CALL RCVARC(' ','IRRA','-',FAMI,KPG,KSP,VARMOI(2),IRET2)
      IF (IRET2.GT.0) VARMOI(2)=0.D0
      CALL RCVARC(' ','IRRA','+',FAMI,KPG,KSP,VARPLU(2),IRET2)
      IF (IRET2.GT.0) VARPLU(2)=0.D0
      VARREF(2)=0.D0

C     CORROSION
      CALL RCVARC(' ','CORR','-',FAMI,KPG,KSP,VARMOI(3),IRET2)
      IF (IRET2.GT.0) VARMOI(3)=0.D0
      CALL RCVARC(' ','CORR','+',FAMI,KPG,KSP,VARPLU(3),IRET2)
      IF (IRET2.GT.0) VARPLU(3)=0.D0
      VARREF(3)=0.D0

C     hydratation
      CALL RCVARC(' ','HYDR','-',FAMI,KPG,KSP,VARMOI(4),IRET2)
      IF (IRET2.GT.0) VARMOI(4)=0.D0
      CALL RCVARC(' ','CORR','+',FAMI,KPG,KSP,VARPLU(4),IRET2)
      IF (IRET2.GT.0) VARPLU(4)=0.D0
      VARREF(4)=0.D0

C     sechage
      CALL RCVARC(' ','SECH','-',FAMI,KPG,KSP,VARMOI(5),IRET2)
      IF (IRET2.GT.0) VARMOI(5)=0.D0
      CALL RCVARC(' ','SECH','+',FAMI,KPG,KSP,VARPLU(5),IRET2)
      IF (IRET2.GT.0) VARPLU(5)=0.D0
      CALL RCVARC(' ','SECH','REF',FAMI,KPG,KSP,VARREF(5),IRET2)
      IF ( IRET2.NE.0) VARREF(5)=0.D0

      IDBG=0
      IF (IDBG.EQ.1) THEN
         CALL TECAEL(IZI,IZK)
         NUMA=ZI(IZI)
      ELSE
         NUMA=0
      ENDIF
      IF (NDIM.EQ.3) THEN
         IF (ANGMAS(4).EQ.1.D0) THEN
             CALL NAUEUL(ANGMAS,ANGEUL)
         ELSE
            DO 1 I=1,3
               ANGEUL(I)=ANGMAS(4+I)
   1        CONTINUE
         ENDIF
      ELSE
         ANGEUL(1)=ANGMAS(5)
         ANGEUL(2)=0.D0
         ANGEUL(3)=0.D0
      ENDIF

      CALL ZASWRP(NUMA,MODELE,NVAR,NDEF,NUNIT,INSTAM,INSTAP,
     &       NVARCM,NOMVAR,VARPLU,VARMOI,VARREF,
     &       EPSM,DEPS,SIGM,VIM,NOPT,ANGEUL,SIGP,VIP,
     &       DSIDEP,CODRET)

      END
