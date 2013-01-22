      SUBROUTINE  EPSTMC(FAMI,NDIM, INSTAN, POUM, IGAU,ISGAU,
     &                   XYZGAU,REPERE,MATER,OPTION, EPSTH)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 21/01/2013   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C.======================================================================
      IMPLICIT NONE
C
C      EPSTMC :   CALCUL DES DEFORMATIONS THERMIQUES / HYDRIQUE (RETRAIT
C                 ENDOGENE) / DE SECHAGE (RETRAIT DE DESSICCATION)
C                 POUR LES ELEMENTS ISOPARAMETRIQUES
C
C   ARGUMENT        E/S  TYPE         ROLE
C    FAMI           IN     K4       FAMILLE DU POINT DE GAUSS
C    INSTAN         IN     R        INSTANT DE CALCUL (0 PAR DEFAUT)
C    POUM           IN     K1       T+ OU T-
C    IGAU           IN     I        POINT DE GAUSS
C    ISGAU          IN     I        SOUS-POINT DE GAUSS
C    XYZGAU         IN     R        COORDONNEES DU POINT DE GAUSS
C    REPERE(7)      IN     R        VALEURS DEFINISSANT LE REPERE
C                                   D'ORTHOTROPIE
C    MATER          IN     I        MATERIAU
C    OPTION         IN     K16      OPTION DE CALCUL
C    EPSTH(6)       IN     R        VECTEUR DES DEFORMATIONS THERMIQUES
C
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
           CHARACTER*(*)  FAMI,POUM
           CHARACTER*16 OPTION
           REAL*8       INSTAN,EPSTH(6), XYZGAU(3)
           REAL*8       REPERE(7)
           INTEGER      NDIM,IGAU,ISGAU
C -----  VARIABLES LOCALES
C-----------------------------------------------------------------------
      INTEGER MATER ,NBRES ,NBV
      REAL*8 BIOT ,E ,ZERO
C-----------------------------------------------------------------------
           PARAMETER (NBRES = 3)
C
      INTEGER ICODRE(NBRES)
           CHARACTER*8  NOMRES(NBRES), NOMPAR
           CHARACTER*16 PHENOM
C
           REAL*8 VALRES(NBRES),VALPAR,BENDOG,KDESSI,ANGL(3)
           REAL*8 DIRE(3),ORIG(3),P(3,3),EPSTHL(6),TROISK
           REAL*8 VEPST1(6),VEPST2(6),HYDR,SECH,SREF,PTOT,NU
           INTEGER I,K,IRET,IREPTO
      CHARACTER*6       EPSA(6)
      DATA EPSA   / 'EPSAXX','EPSAYY','EPSAZZ','EPSAXY','EPSAXZ',
     &              'EPSAYZ'/
C
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C ---- INITIALISATIONS
C      ---------------
      ZERO   = 0.0D0
      NOMPAR = 'INST'
      VALPAR = INSTAN
C
      DO 10 I = 1, 6
         EPSTH(I)  = ZERO
 10   CONTINUE

      CALL RCVARC(' ','HYDR',POUM,FAMI,IGAU,ISGAU,HYDR,IRET)
      IF (IRET.EQ.1) HYDR=0.D0

      CALL RCVARC(' ','SECH',POUM,FAMI,IGAU,ISGAU,SECH,IRET)
      IF (IRET.EQ.1) SECH=0.D0
      CALL RCVARC(' ','PTOT',POUM,FAMI,IGAU,ISGAU,PTOT,IREPTO)
      CALL RCVARC(' ','SECH','REF',FAMI,1,1,SREF,IRET)
      IF (IRET.EQ.1) SREF=0.D0

C
C ---- ------------------------------------------------------------
C ---- CALCUL DES DEFORMATIONS HYDRIQUES (OPTION CHAR_MECA_HYDR_R)
C ---- ------------------------------------------------------------
C
      IF (OPTION(11:14).EQ.'HYDR') THEN
         IF (HYDR.NE.0.D0) THEN
C
            PHENOM = 'ELAS'
            NOMRES(1) = 'B_ENDOGE'
            NBV = 1
C
C ----   RECUPERATION DES CARACTERISTIQUES MECANIQUES
C     ----------------------------------------------------------
            CALL RCVALB(FAMI,IGAU,ISGAU,POUM,MATER,' ',PHENOM,1,NOMPAR,
     &                  VALPAR,NBV,NOMRES,VALRES,  ICODRE, 0)
C
            IF (ICODRE(1).EQ.0) THEN
C
                BENDOG = VALRES(1)
C
                EPSTH(1) = - BENDOG*HYDR
                EPSTH(2) = - BENDOG*HYDR
                EPSTH(3) = - BENDOG*HYDR
C
            ELSE
C
               CALL U2MESK('I','ELEMENTS_58',1,PHENOM)
C
            ENDIF
         ENDIF
C
C ---- ---------------------------------------------------------------
C ---- CALCUL DES DEFORMATIONS DUES A LA PRESSION DE FLUIDE
C ---  (OPTION CHAR_MECA_PTOT_R)
C ---- ---------------------------------------------------------------
      ELSEIF (OPTION(11:14).EQ.'PTOT' ) THEN
C
        IF (IREPTO.EQ.0) THEN
C
C ----      RECUPERATION DU COEFFICIENT DE BIOT
C           ----------------------------------------------------------

            PHENOM = 'THM_DIFFU'
            NOMRES(1) = 'BIOT_COE'
            NBV = 1

            CALL RCVALB(FAMI,IGAU,ISGAU,POUM,MATER,' ',PHENOM,1,NOMPAR,
     &                  VALPAR,NBV,NOMRES,VALRES, ICODRE, 0)
C
            IF (ICODRE(1).NE.0) VALRES(1)=0.D0

            BIOT = VALRES(1)
C
C ----      RECUPERATION DU COEFFICIENT 3K
C           ----------------------------------------------------------

            PHENOM = 'ELAS'
            NOMRES(1)='E'
            NOMRES(2)='NU'
            NBV = 2

            CALL RCVALB(FAMI,IGAU,ISGAU,POUM,MATER,' ',PHENOM,1,NOMPAR,
     &                  VALPAR,NBV,NOMRES,VALRES, ICODRE, 0)
            E  = VALRES(1)
            NU = VALRES(2)
            TROISK = E/(1.D0-2.D0*NU)
C
            EPSTH(1) = BIOT/TROISK*PTOT
            EPSTH(2) = EPSTH(1)
            EPSTH(3) = EPSTH(1)
C
        ENDIF
C
C ---- ---------------------------------------------------------------
C ---- CALCUL DES DEFORMATIONS DU AU SECHAGE (OPTION CHAR_MECA_SECH_R)
C ---- ---------------------------------------------------------------
      ELSEIF (OPTION(11:14).EQ.'SECH' ) THEN

            PHENOM = 'ELAS'
            NOMRES(1) = 'K_DESSIC'
            NBV = 1
C
C ----      INTERPOLATION DE K_DESSICCA EN FONCTION DE LA TEMPERATURE
C           DE L HYDRATATION OU DU SECHAGE
C           ----------------------------------------------------------
            CALL RCVALB(FAMI,IGAU,ISGAU,POUM,MATER,' ',PHENOM,1,NOMPAR,
     &                  VALPAR,NBV,NOMRES,VALRES, ICODRE, 0)
C
            IF (ICODRE(1).NE.0) VALRES(1)=0.D0
C
            KDESSI = VALRES(1)
C
            EPSTH(1) = - KDESSI*(SREF-SECH)
            EPSTH(2) = - KDESSI*(SREF-SECH)
            EPSTH(3) = - KDESSI*(SREF-SECH)

C
C ---- ---------------------------------------------------------------
C ---- CALCUL DES DEFORMATIONS ANELASTIQUE (OPTION CHAR_MECA_EPSA_R)
C ---- ---------------------------------------------------------------
      ELSEIF (OPTION(11:14).EQ.'EPSA' ) THEN
      DO 20 K=1,6
        CALL RCVARC(' ',EPSA(K),POUM,FAMI,IGAU,ISGAU,EPSTH(K),IRET)
        IF (IRET.EQ.1) EPSTH(K)=0.D0
 20   CONTINUE


C ---- ------------------------------------------------------------
C ---- CALCUL DES DEFORMATIONS THERMIQUES (OPTION CHAR_MECA_TEMP_R)
C ---- ------------------------------------------------------------
      ELSE
C
C
C ---- RECUPERATION DU TYPE DU MATERIAU DANS PHENOM
C      --------------------------------------------
      CALL RCCOMA(MATER,'ELAS',1,PHENOM,ICODRE)
C
C      ------------
C ---- CAS ISOTROPE
C      ------------
      IF (PHENOM.EQ.'ELAS') THEN
C
          CALL VERIFT(FAMI,IGAU,ISGAU,POUM,MATER,'ELAS',1,EPSTH(1),IRET)
          EPSTH(2) = EPSTH(1)
          EPSTH(3) = EPSTH(1)

C      --------------
C ---- CAS ORTHOTROPE
C      --------------
      ELSEIF (PHENOM.EQ.'ELAS_ORTH') THEN
C
          IF (REPERE(1).GT.0.D0) THEN
            ANGL(1) = REPERE(2)
            ANGL(2) = REPERE(3)
            ANGL(3) = REPERE(4)
            CALL MATROT(ANGL, P)
         ELSE
            DIRE(1) = REPERE(2)
            DIRE(2) = REPERE(3)
            DIRE(3) = REPERE(4)
C
            ORIG(1) = REPERE(5)
            ORIG(2) = REPERE(6)
            ORIG(3) = REPERE(7)
            CALL UTRCYL(XYZGAU,DIRE,ORIG,P)
          ENDIF

          CALL VERIFT(FAMI,IGAU,ISGAU,POUM,MATER,PHENOM,3,
     &                EPSTHL(1),IRET)

          EPSTHL(4) = 0.D0
          EPSTHL(5) = 0.D0
          EPSTHL(6) = 0.D0



          VEPST1(1)=EPSTHL(1)
          VEPST1(2)=EPSTHL(4)
          VEPST1(3)=EPSTHL(2)
          VEPST1(4)=EPSTHL(5)
          VEPST1(5)=EPSTHL(6)
          VEPST1(6)=EPSTHL(3)


C        PASSAGE DES DEFORMATIONS DANS LE REPERE D ORTHOTROPIE
C        AU REPERE GLOBAL
          CALL UTPSLG(1,3,P,VEPST1,VEPST2)
          EPSTH(1)=VEPST2(1)
          EPSTH(2)=VEPST2(3)
          EPSTH(3)=VEPST2(6)
          EPSTH(4)=VEPST2(2)
          EPSTH(5)=VEPST2(4)
          EPSTH(6)=VEPST2(5)
          IF (NDIM.EQ.2) EPSTH(3)=EPSTHL(3)
C
C      -----------------------
C ---- CAS ISOTROPE-TRANSVERSE
C      -----------------------
      ELSEIF (PHENOM.EQ.'ELAS_ISTR') THEN
C
          IF (REPERE(1).GT.0.D0) THEN
            ANGL(1) = REPERE(2)
            ANGL(2) = REPERE(3)
            ANGL(3) = REPERE(4)
            CALL MATROT(ANGL, P)
          ELSE
            DIRE(1) = REPERE(2)
            DIRE(2) = REPERE(3)
            DIRE(3) = REPERE(4)
C
            ORIG(1) = REPERE(5)
            ORIG(2) = REPERE(6)
            ORIG(3) = REPERE(7)
            CALL UTRCYL(XYZGAU,DIRE,ORIG,P)
          ENDIF

          CALL VERIFT(FAMI,IGAU,ISGAU,POUM,MATER,PHENOM,2,
     &                EPSTHL(1),IRET)
C
          EPSTHL(3) = EPSTHL(2)
          EPSTHL(2) = EPSTHL(1)
          EPSTHL(4) = 0.D0
          EPSTHL(5) = 0.D0
          EPSTHL(6) = 0.D0


          VEPST1(1)=EPSTHL(1)
          VEPST1(2)=EPSTHL(4)
          VEPST1(3)=EPSTHL(2)
          VEPST1(4)=EPSTHL(5)
          VEPST1(5)=EPSTHL(6)
          VEPST1(6)=EPSTHL(3)


C        PASSAGE DES DEFORMATIONS DANS LE REPERE D ORTHOTROPIE
C        AU REPERE GLOBAL
          CALL UTPSLG(1,3,P,VEPST1,VEPST2)
          EPSTH(1)=VEPST2(1)
          EPSTH(2)=VEPST2(3)
          EPSTH(3)=VEPST2(6)
          EPSTH(4)=VEPST2(2)
          EPSTH(5)=VEPST2(4)
          EPSTH(6)=VEPST2(5)
          IF (NDIM.EQ.2) EPSTH(3)=EPSTHL(3)
      ELSEIF (PHENOM.EQ.'ELAS_HYPER') THEN
      ELSE
          CALL U2MESK('F','ELEMENTS_15',1,PHENOM)
      ENDIF
      ENDIF
C
C.============================ FIN DE LA ROUTINE ======================
      END
