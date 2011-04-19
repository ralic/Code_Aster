      SUBROUTINE LCDPNL(FAMI,KPG,KSP,TYPMOD,NDIM,
     & OPTION,COMPOR,IMATE,SIGM,EPSM,DEPS,VIM,
     & VIP,SIG,DSIDEP,PROJ,IRET)
C =====================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C =====================================================================
C =====================================================================
C TOLE CRP_20
C TOLE CRP_21
      IMPLICIT      NONE
      INTEGER       NDIM, IMATE,IRET,KSP,KPG,IRET2
      REAL*8        SIGM(6),DEPS(6,2),VIM(*),VIP(*),SIG(6),PROJ(6,6)
      REAL*8        DSIDEP(6,6,2),EPSM(6,2)
      CHARACTER*(*) FAMI
      CHARACTER*8   TYPMOD(*)
      CHARACTER*16  OPTION,COMPOR(*)
C =====================================================================
C --- APPLICATION DE LA LOI DE COMPORTEMENT DE TYPE DRUCKER PRAGER ----
C --- LINEAIRE AVEC PRISE EN COMPTE DES PHENOMENES DE NON LOCALISATION
C =====================================================================
C IN  NDIM    DIMENSION DE L'ESPACE
C IN  OPTION  OPTION DE CALCUL (RAPH_MECA, RIGI_MECA_TANG OU FULL_MECA)
C IN  IMATE   NATURE DU MATERIAU
C IN  EPSM    CHAMP DE DEFORMATION EN T-
C IN  DEPS    INCREMENT DU CHAMP DE DEFORMATION
C IN  VIM     VARIABLES INTERNES EN T-
C               1   : ENDOMMAGEMENT (D)
C               2   : INDICATEUR DISSIPATIF (1) OU ELASTIQUE (0)
C VAR VIP     VARIABLES INTERNES EN T+
C              IN  ESTIMATION (ITERATION PRECEDENTE)
C              OUT CALCULEES
C OUT SIGP    CONTRAINTES EN T+
C OUT DSIDEP  MATRICE TANGENTE
C OUT PROJ    PROJECTEUR DE COUPURE DU TERME DE REGULARISATION
C OUT IRET    CODE RETOUR (0 = OK)
C =====================================================================
      LOGICAL      RIGI,RESI,ELAS
      INTEGER      NDIMSI,I,J,NDT,NDI
      REAL*8       KRON(6),TRE,TRER,VALRES(2),DSDP2(6,6),TP,TM,TREF
      REAL*8       DEUXMU,LAMBDA,DSDP1B(6,6),YOUNG,NU
      INTEGER ICODRE(2)
      CHARACTER*8  NOMRES(2)
C =====================================================================
      COMMON /TDIM/   NDT, NDI
C =====================================================================
      DATA KRON   /1.D0, 1.D0, 1.D0, 0.D0, 0.D0, 0.D0/
      DATA NOMRES /'E','NU'/
C =====================================================================
C --- INITIALISATION --------------------------------------------------
C =====================================================================
      NDIMSI = 2*NDIM
      RIGI = OPTION(1:4).EQ.'FULL' .OR. OPTION(1:4).EQ.'RIGI'
      RESI = OPTION(1:4).EQ.'FULL' .OR. OPTION(1:4).EQ.'RAPH'
      ELAS = OPTION(11:14).EQ.'ELAS'
C =====================================================================
C --- CARACTERISTIQUES MATERIAU ---------------------------------------
C =====================================================================
      CALL RCVALB(FAMI,1,1,'+',IMATE,' ','ELAS',0,' ',0.D0,2,
     &            NOMRES,VALRES,ICODRE,2)
      YOUNG  = VALRES(1)
      NU     = VALRES(2)
      DEUXMU = YOUNG / ( 1.0D0 + NU )
      LAMBDA = YOUNG * NU / ( 1.0D0 + NU ) / ( 1.0D0 - 2.D0 * NU )
C =====================================================================
C --- COMPORTEMENT LOCAL ----------------------------------------------
C =====================================================================

C APPEL DE RCVARC POUR LE CALCUL DE LA TEMPERATURE
C RAISON: CETTE ROUTINE EST APPELEE POUR LE CALCUL THERMIQUE (CALCME)
      CALL RCVARC(' ','TEMP','-',FAMI,KPG,KSP,TM,IRET2)
      CALL RCVARC(' ','TEMP','+',FAMI,KPG,KSP,TP,IRET2)
      CALL RCVARC(' ','TEMP','REF',FAMI,KPG,KSP,TREF,IRET2)
      CALL LCDRPR(TYPMOD,OPTION,IMATE,COMPOR,SIGM,TM,TP,TREF,
     &                        DEPS(1,2),VIM,VIP,SIG,DSDP2,IRET)
C =====================================================================
C --- PROJECTEUR DE COUPURE POUR LA REGULARISATION : DEFAUT------------
C =====================================================================
      CALL R8INIR(36,0.D0,PROJ,1)
      DO 5 I = 1,6
        PROJ(I,I) = 1.D0
 5    CONTINUE
C =====================================================================
C --- CORRECTION NON LOCALE -------------------------------------------
C =====================================================================
      IF (RESI) THEN
         TRE  = DEPS(1,1)+DEPS(2,1)+DEPS(3,1)
         TRER = DEPS(1,2)+DEPS(2,2)+DEPS(3,2)

         DO 100 I = 1,NDIMSI
            SIG(I) = SIG(I) + LAMBDA*(TRE - TRER)*KRON(I)
     &                                  + DEUXMU*(DEPS(I,1)-DEPS(I,2))
 100     CONTINUE
      ENDIF

      IF (RIGI) THEN

         CALL LCINMA ( 0.0D0, DSIDEP(1,1,1) )
         CALL LCINMA ( 0.0D0, DSDP1B )
         CALL LCINMA ( 0.0D0, DSIDEP(1,1,2) )

         DO 10 I = 1,3
            DO 20 J = 1,3
               DSIDEP(I,J,1) = LAMBDA
 20         CONTINUE
 10      CONTINUE

         DO 30 I = 1,NDT
            DSIDEP(I,I,1) = DSIDEP(I,I,1) + DEUXMU
 30      CONTINUE

         IF (.NOT. ELAS) THEN
            CALL LCPRSM(-1.0D0,DSIDEP(1,1,1),DSDP1B)
            CALL LCSOMA(DSDP1B,DSDP2,DSIDEP(1,1,2))
         END IF
      ENDIF
C =====================================================================
      END
