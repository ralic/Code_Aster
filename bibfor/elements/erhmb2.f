      SUBROUTINE ERHMB2(PERMAN, INO   , NBS   , NBNA  ,
     &                  NDIM  , THETA , INSTPM, JAC   , NX,
     &                  NY    , TX    , TY    , NBCMP ,
     &                  GEOM  , IVOIS ,
     &                  SIELNP, SIELNM, ADSIP , IAGD  ,
     &                  TBREF2, IADE2 , IAVA2 , NCMPM2, IAPTM2,
     &                  IADE3 , IAVA3 , NCMPM3, IAPTM3, TM2H1B)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 11/03/2008   AUTEUR MEUNIER S.MEUNIER 
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
C TOLE CRP_20
C TOLE CRP_21
C =====================================================================
C  ERREUR EN HYDRO-MECANIQUE - TERME DE BORD - DIMENSION 2
C  **        *     *                    *                *
C =====================================================================
C  - FONCTION REALISEE :  CALCUL DE L'ERREUR DUE A LA NON-VERIFICATION
C                         DES CONDITIONS DE NEUMANN EN MECANIQUE ET
C                         EN HYDRAULIQUE DANS L'INDICATEUR
C                         HM PERMANENT.
C
C     ARGUMENTS:
C     ----------
C
C      ENTREE :
C-------------
C IN PERMAN : PERMANENT OU INSTATIONNAIRE ?
C IN INO    : NUMERO DU NOEUD X1
C IN NBS    : NOMBRE D'ARETES
C IN NBNA   : NOMBRE DE SOMMETS SUR L'ARETE
C IN NDIM   : DIMENSION DE L'ESPACE
C IN THETA  : PARAMETRE THETA DE LA DISCRETISATION TEMPORELLE
C IN INSTPM : TABLEAU DES INSTANTS (ACTUEL ET PRECEDENT)
C IN JAC    : VECTEUR DES JACOBIENS DE LA TRANSFORMATION AUX NOEUDS
C IN NX     : VECTEUR DES ABSCISSES DES NORMALES AUX NOEUDS
C IN NY     : VECTEUR DES ORDONNEES DES NORMALES AUX NOEUDS
C IN TX     : VECTEUR DES ABSCISSES DES TANGENTES AUX NOEUDS
C IN TY     : VECTEUR DES ORDONNEES DES TANGENTES AUX NOEUDS
C IN NBCMP  : NOMBRE DE COMPOSANTES DES CONTRAINTES GENERALISEES
C             AUX NOEUDS
C IN GEOM   : TABLEAU DES COORDONNEES
C IN IVOIS  : ADRESSE DES VOISINS
C IN SIELNP : CONTRAINTES AUX NOEUDS PAR ELEMENT A L'INSTANT ACTUEL
C IN SIELNM : CONTRAINTES AUX NOEUDS PAR ELEMENT A L'INSTANT PRECEDENT
C IN ADSIP  : ADRESSE DANS ZR DU TABLEAU DES CONTRAINTES DE PRESSION
C             DE LA MECANIQUE
C IN IAGD   : ADRESSE DU VECTEUR GRANDEUR
C IN TBREF2 : TABLEAU DES CHARGEMENTS DE TYPE PRES_REP
C IN IADE2  : ADRESSE DE L'OBJET .DESC DE LA CARTE2
C IN IAVA2  : ADRESSE DE L'OBJET .VALE DE LA CARTE2
C IN NCMPM2 : NOMBRE DE COMPOSANTES DE LA GRANDEUR ASSOCIEE A LA
C             CARTE 2
C IN IAPTM2 : ADRESSE DU .PTMA DE LA CARTE ETENDUE 2
C IN IADE3  : ADRESSE DE L'OBJET .DESC DE LA CARTE3
C IN IAVA3  : ADRESSE DE L'OBJET .VALE DE LA CARTE3
C IN NCMPM3 : NOMBRE DE COMPOSANTES DE LA GRANDEUR ASSOCIEE A LA
C             CARTE 3
C IN IAPTM3 : ADRESSE DU .PTMA DE LA CARTE ETENDUE 3
C
C      SORTIE :
C-------------
C OUT TM2H1B : TABLEAU CONTENANT LES TERMES DE BORD DES TERMES DIFFUSIFS
C              (2 POUR LA MECANIQUE, 1 POUR L'HYDRAULIQUE)
C  1 : MECANIQUE
C  2 : DERIVEE TEMPORELLE DE LA MECA
C  3 : HYDRAULIQUE
C ......................................................................
C
C   -------------------------------------------------------------------
C     SUBROUTINES APPELLEES :
C       MESSAGE     : U2MESK
C       UTILITAIRES : FOINTE
C     FONCTION INTRINSEQUE : SQRT
C   -------------------------------------------------------------------
C
      IMPLICIT NONE
C
C DECLARATION PARAMETRES D'APPEL
C
      LOGICAL  PERMAN
      INTEGER  INO,NBS,NBNA,NDIM
      REAL*8   JAC(3),NX(3),NY(3),TX(3),TY(3)
      INTEGER  NBCMP,IVOIS,IAGD
      INTEGER  ADSIP
      REAL*8   THETA,SIELNP(90),SIELNM(90),INSTPM(2)
      INTEGER  TBREF2(12),IADE2,IAVA2,NCMPM2,IAPTM2,
     &         IADE3,IAVA3,NCMPM3,IAPTM3
      REAL*8   TM2H1B(3)
      REAL*8   GEOM(NDIM,*)

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
      INTEGER      IMAV,IENT2,NUMGD2,IENT3,NUMGD3
      INTEGER      JNO, MNO,IBID
      INTEGER      IDEC1,IDEC2,IDEC3
      INTEGER      IER1,IER2,IER3,IER4,IER5,IER6
      INTEGER      IER11,IER21,IER31,IER41,IER51,IER61
      REAL*8       VALPAR(3),PRP(3)   ,PRM(3),CIP(3),CIM(3),
     &             FLUXHP(3),FLUXHM(3),INTE1 ,INTE2 ,INTE3,
     &             INTED1,INTED2,INTED3
      REAL*8       TA1
      CHARACTER*4  NOMPAR(3)
      CHARACTER*8  PRF,CIF,FLUXHF
      CHARACTER*19 NOMGD2,NOMGD3
C =====================================================================
      IF ( .NOT. PERMAN ) THEN
        IBID = 1
      ELSE
        IBID  = 0
        THETA = 1.D0
      ENDIF
      TA1   = 1.D0 - THETA
C =====================================================================
C 1. RECUPERATION SUR LA MAILLE COURANTE AUX NOEUDS INO ET JNO DE :
C     . CONTRAINTES EFFECTIVES (SIGMA MECANIQUE : SIXX, SIYY, SIXY)
C     . CONTRAINTES DE PRESSION (BIOT*PRESSION  : SIP)
C     . FLUX DE PRESSION (LAMBDA*GRAD(P))
C
C              X1          X2          X3
C               O-----------O-----------O
C              INO         MNO         JNO
C
C         POINTS  1 --> INO PREMIER POINT DE L'ARETE COURANTE
C                 2 --> JNO DEUXIEME POINT  DE L'ARETE COURANTE
C                 3 --> MNO NOEUD MILIEU S'IL EXISTE
C =====================================================================
C
      IF (INO.EQ.NBS) THEN
        JNO = 1
      ELSE
        JNO = INO+1
      ENDIF
C
      IF (NBNA.EQ.3) THEN
        MNO = NBS+INO
      ENDIF
C
      IMAV = ZI(IVOIS+INO)
C
C =====================================================================
C 2. ON TRAITE LA PARTIE MECANIQUE
C =====================================================================
C
C --------------------------------------------------------------------
C ** 1ER CAS : ON A IMPOSE EXPLICITEMENT LES CONDITIONS AUX LIMITES
C --------------------------------------------------------------------
C
      IF (IADE2.NE.0) THEN
C
C --------------------------------------------------------------------
C 2.1. RECHERCHE DES ADRESSES POUR LA CONDITION LIMITE MECANIQUE
C --------------------------------------------------------------------
C
        IF (IAPTM2.EQ.0) THEN
C CARTE CONSTANTE
          IENT2 = 1
        ELSE
C LA CARTE A ETE ETENDUE
          IENT2 = ZI(IAPTM2 -1 +IMAV)
        ENDIF
        NUMGD2 = TBREF2(8)
        NOMGD2 = ZK8(IAGD-1+NUMGD2)
C
C --------------------------------------------------------------------
C 2.2. LA CONDITION DE NEUMANN MECANIQUE EST DE TYPE PRES_
C --------------------------------------------------------------------
C
        IF (NOMGD2(1:5).EQ.'PRES_') THEN
C
C 2.2.1. DETERMINATION DES VALEURS SI CONSTANTE
C
          IF ( NOMGD2(1:6).EQ.'PRES_R' ) THEN
C
            PRP(1) = ZR(IAVA2-1+(IENT2-1)*NCMPM2+1)
            PRP(2) = PRP(1)
            PRP(3) = PRP(1)
            CIP(1) = ZR(IAVA2-1+(IENT2-1)*NCMPM2+2)
            CIP(2) = CIP(1)
            CIP(3) = CIP(1)
C
C 2.2.2. DETERMINATION DES VALEURS SI FONCTION
C
          ELSE IF ( NOMGD2(1:6).EQ.'PRES_F' ) THEN
C
            PRF = ZK8(IAVA2-1+(IENT2-1)*NCMPM2+1)
            CIF = ZK8(IAVA2-1+(IENT2-1)*NCMPM2+2)
C
C CE QUI SUIT EST LE SEUL MOYEN AUJOURD'HUI DE PIEGER LES BORDS A
C DIRICHLET. EN EFFET ASTER LES AFFECTE AUTOMATIQUEMENT D'UN NEUMANN
C VALANT &FOZERO. CELA SUPPOSE QUE LES VRAIS BORDS A NEUMANN NUL
C ONT ETE DECLARES EXPLICITEMENT DANS LES COMMANDES
C
            IF (PRF.EQ.'&FOZERO'.AND. CIF.EQ.'&FOZERO') THEN
              GOTO 3333
            ENDIF
C
            NOMPAR(1) = 'X'
            NOMPAR(2) = 'Y'
            NOMPAR(3) = 'INST'
C
            VALPAR(1) = GEOM(1,INO)
            VALPAR(2) = GEOM(2,INO)
            VALPAR(3) = INSTPM(1)
C
            CALL FOINTE('FM',PRF,3,NOMPAR,VALPAR,PRP(1),IER1)
            CALL FOINTE('FM',CIF,3,NOMPAR,VALPAR,CIP(1),IER2)
C
            IF (.NOT. PERMAN) THEN
              VALPAR(3) = INSTPM(2)
              CALL FOINTE('FM',PRF,3,NOMPAR,VALPAR,PRM(1),IER11)
              CALL FOINTE('FM',CIF,3,NOMPAR,VALPAR,CIM(1),IER21)
            ENDIF
C
            VALPAR(1) = GEOM(1,JNO)
            VALPAR(2) = GEOM(2,JNO)
            VALPAR(3) = INSTPM(1)
C
            CALL FOINTE('FM',PRF,3,NOMPAR,VALPAR,PRP(2),IER3)
            CALL FOINTE('FM',CIF,3,NOMPAR,VALPAR,CIP(2),IER4)
C
            IF (.NOT. PERMAN) THEN
              VALPAR(3) = INSTPM(2)
              CALL FOINTE('FM',PRF,3,NOMPAR,VALPAR,PRM(2),IER31)
              CALL FOINTE('FM',CIF,3,NOMPAR,VALPAR,CIM(2),IER41)
            ENDIF
C
            IF (NBNA.EQ.3) THEN
              VALPAR(1) = GEOM(1,MNO)
              VALPAR(2) = GEOM(2,MNO)
              VALPAR(3) = INSTPM(1)
C
              CALL FOINTE('FM',PRF,3,NOMPAR,VALPAR,PRP(3),IER5)
              CALL FOINTE('FM',CIF,3,NOMPAR,VALPAR,CIP(3),IER6)
C
              IF (.NOT. PERMAN) THEN
                VALPAR(3) = INSTPM(2)
                CALL FOINTE('FM',PRF,3,NOMPAR,VALPAR,PRM(3),IER51)
                CALL FOINTE('FM',CIF,3,NOMPAR,VALPAR,CIM(3),IER61)
              ENDIF
C
            ENDIF
C
C 2.2.3. ERREUR
C
          ELSE
            CALL U2MESK('F','INDICATEUR_90',1,NOMGD2)
          ENDIF
C
C 2.2.4. ERREUR
C
        ELSE
          CALL U2MESK('F','INDICATEUR_91',1,NOMGD2)
        ENDIF
C
C --------------------------------------------------------------------
C ** 2EME CAS : AUCUNE CONDITION DE CONTRAINTE N'A ETE FIXEE SUR LES
C               BORDS DU MAILLAGE : ON A IMPOSE IMPLICITEMENT SIGMA = 0
C               ATTENTION : NE MARCHE PAS AUJOURD'HUI CAR ON NE SAIT
C               PAS TRIER AVEC LES BORDS DIRICHLET
C --------------------------------------------------------------------
C
      ELSE
C
        PRP(1) = 0.D0
        PRP(2) = PRP(1)
        PRP(3) = PRP(1)
        CIP(1) = 0.D0
        CIP(2) = CIP(1)
        CIP(3) = CIP(1)
C
      ENDIF
C
C 2.3. TERME DE BORD
C
      IDEC1 = NBCMP*(INO-1)
      INTE1 = JAC(1)*(
     &      ( - PRP(1) * NX(1) + CIP(1) * TX(1)
     &        - ( SIELNP(IDEC1+1) + SIELNP(IDEC1+ADSIP)) * NX(1)
     &        - SIELNP(IDEC1+4) * NY(1) )**2
     &    + ( - PRP(1) * NY(1) + CIP(1) * TY(1)
     &        - SIELNP(IDEC1+4)*NX(1)
     &        - ( SIELNP(IDEC1+2) + SIELNP(IDEC1+ADSIP) ) * NY(1) )**2 )
C
      IDEC2 = NBCMP*(JNO-1)
      INTE2 = JAC(2)*(
     &      ( - PRP(2) * NX(2) + CIP(2) * TX(2)
     &        - ( SIELNP(IDEC2+1) + SIELNP(IDEC2+ADSIP)) * NX(2)
     &        - SIELNP(IDEC2+4) * NY(2) )**2
     &    + ( - PRP(2) * NY(2) + CIP(2) * TY(2)
     &        - SIELNP(IDEC2+4) * NX(2)
     &        - ( SIELNP(IDEC2+2) + SIELNP(IDEC2+ADSIP) ) * NY(2) )**2 )
C
      IF (NBNA.EQ.3) THEN
C
        IDEC3 = NBCMP*(MNO-1)
        INTE3 = JAC(3)*(
     &    ( - PRP(3) * NX(3) + CIP(3) * TX(3)
     &      - ( SIELNP(IDEC3+1) + SIELNP(IDEC3+ADSIP)) * NX(3)
     &      - SIELNP(IDEC3+4) * NY(3) )**2
     &  + ( - PRP(3) * NY(3) + CIP(3) * TY(3)
     &      - SIELNP(IDEC3+4) * NX(3)
     &      - ( SIELNP(IDEC3+2) + SIELNP(IDEC3+ADSIP)) * NY(3) )**2 )
C
        TM2H1B(1) = TM2H1B(1) + (INTE1+4.D0*INTE3+INTE2)/3.D0
      ELSE
        TM2H1B(1) = TM2H1B(1) + (INTE1+INTE2)
      ENDIF
C
      IF (.NOT. PERMAN) THEN
C
        IDEC1 = NBCMP*(INO-1)
        INTED1 = JAC(1)*(
     &  (- (PRP(1)-PRM(1)) * NX(1) + (CIP(1)-CIM(1)) * TX(1)
     &   - ( (SIELNP(IDEC1+1)-SIELNM(IDEC1+1) +
     &          SIELNP(IDEC1+ADSIP)-SIELNM(IDEC1+ADSIP)) * NX(1)
     &      +  (SIELNP(IDEC1+4)-SIELNM(IDEC1+4)) * NY(1) ))**2
     &+ (- (PRP(1)-PRM(1)) * NY(1) + (CIP(1)-CIM(1)) * TY(1)
     &   - ( (SIELNP(IDEC1+4)-SIELNM(IDEC1+4)) * NX(1) +
     &       (  SIELNP(IDEC1+2)-SIELNM(IDEC1+2)
     &        + SIELNP(IDEC1+ADSIP)-SIELNM(IDEC1+ADSIP) ) * NY(1)))**2)
C
        IDEC2 = NBCMP*(JNO-1)
        INTED2 = JAC(2)*(
     &  (- (PRP(2)-PRM(2)) * NX(2) + (CIP(2)-CIM(2)) * TX(2)
     &   - ( (SIELNP(IDEC2+1)-SIELNM(IDEC2+1) +
     &          SIELNP(IDEC2+ADSIP)-SIELNM(IDEC2+ADSIP)) * NX(2)
     &      +  (SIELNP(IDEC2+4)-SIELNM(IDEC2+4)) * NY(2) ))**2
     &+ (- (PRP(2)-PRM(2)) * NY(2) + (CIP(2)-CIM(2)) * TY(2)
     &   - ( (SIELNP(IDEC2+4)-SIELNM(IDEC2+4)) * NX(2) +
     &       (  SIELNP(IDEC2+2)-SIELNM(IDEC2+2)
     &        + SIELNP(IDEC2+ADSIP)-SIELNM(IDEC2+ADSIP) ) * NY(2)))**2)
C
        IF (NBNA.EQ.3) THEN
C
          IDEC3 = NBCMP*(MNO-1)
          INTED3 = JAC(3)*(
     &  (- (PRP(3)-PRM(3)) * NX(3) + (CIP(3)-CIM(3)) * TX(3)
     &   - ( (SIELNP(IDEC3+1)-SIELNM(IDEC3+1) +
     &          SIELNP(IDEC3+ADSIP)-SIELNM(IDEC3+ADSIP)) * NX(3)
     &      +  (SIELNP(IDEC3+4)-SIELNM(IDEC3+4)) * NY(3) ))**2
     &+ (- (PRP(3)-PRM(3)) * NY(3) + (CIP(3)-CIM(3)) * TY(3)
     &   - ( (SIELNP(IDEC3+4)-SIELNM(IDEC3+4)) * NX(3) +
     &       (  SIELNP(IDEC3+2)-SIELNM(IDEC3+2)
     &        + SIELNP(IDEC3+ADSIP)-SIELNM(IDEC3+ADSIP) ) * NY(3)))**2)
C
          TM2H1B(2) = TM2H1B(2) + (INTED1+4.D0*INTED3+INTED2)/3.D0
        ELSE
          TM2H1B(2) = TM2H1B(2) + (INTED1+INTED2)
        ENDIF
C
      ENDIF
C
 3333 CONTINUE
C
C =====================================================================
C 3. ON TRAITE LA PARTIE HYDRAULIQUE
C =====================================================================
C
C --------------------------------------------------------------------
C ** 1ER CAS : ON A IMPOSE EXPLICITEMENT LES CONDITIONS AUX LIMITES
C --------------------------------------------------------------------
C
      IF (IADE3.NE.0) THEN
C
C --------------------------------------------------------------------
C 3.1. RECHERCHE DES ADRESSES POUR LA CONDITION LIMITE HYDRAULIQUE
C --------------------------------------------------------------------
C
        IF (IAPTM3.EQ.0) THEN
C CARTE CONSTANTE
          IENT3 = 1
        ELSE
C LA CARTE A ETE ETENDUE
          IENT3 = ZI(IAPTM3 -1 +IMAV)
        ENDIF
C
        NUMGD3 = TBREF2(12)
        NOMGD3 = ZK8(IAGD-1+NUMGD3)
C
C --------------------------------------------------------------------
C 3.2. LA CONDITION DE NEUMANN HYDRAULIQUE EST DE TYPE FTHM_
C --------------------------------------------------------------------
C
        IF (NOMGD3(1:5).EQ.'FTHM_') THEN
C
C 3.2.1. DETERMINATION DES VALEURS SI CONSTANTE
C
          IF (NOMGD3(1:6).EQ.'FTHM_R') THEN
C
            FLUXHP(1) = ZR(IAVA3-1+(IENT3-1)*NCMPM3+1)
            FLUXHP(2) = FLUXHP(1)
            FLUXHP(3) = FLUXHP(1)
C
C 3.2.2. DETERMINATION DES VALEURS SI FONCTION
C
          ELSE IF (NOMGD3(1:6).EQ.'FTHM_F') THEN
            FLUXHF = ZK8(IAVA3-1+(IENT3-1)*NCMPM3+1)
C
C CE QUI SUIT EST LE SEUL MOYEN AUJOURD'HUI DE PIEGER LES BORDS
C A DIRICHLET. EN EFFET ASTER LES AFFECTE AUTOMATIQUEMENT D'UN NEUMANN
C VALANT &FOZERO. CELA SUPPOSE QUE LES VRAIS BORDS A NEUMANN NUL
C ONT ETE DECLARES EXPLICITEMENT DANS LES COMMANDES.
C
            IF (FLUXHF.EQ.'&FOZERO') THEN
              GOTO 9999
            ENDIF
C
            NOMPAR(1) = 'X'
            NOMPAR(2) = 'Y'
            NOMPAR(3) = 'INST'
            VALPAR(1) = GEOM(1,INO)
            VALPAR(2) = GEOM(2,INO)
            VALPAR(3) = INSTPM(1)
            CALL FOINTE('FM',FLUXHF,3,NOMPAR,VALPAR,FLUXHP(1),IER1)
C
            IF (.NOT. PERMAN) THEN
              VALPAR(3) = INSTPM(2)
              CALL FOINTE('FM',FLUXHF,3,NOMPAR,VALPAR,FLUXHM(1),IER11)
            ENDIF
C
            VALPAR(1) = GEOM(1,JNO)
            VALPAR(2) = GEOM(2,JNO)
            VALPAR(3) = INSTPM(1)
            CALL FOINTE('FM',FLUXHF,3,NOMPAR,VALPAR,FLUXHP(2),IER2)
C
            IF (.NOT. PERMAN) THEN
              VALPAR(3) = INSTPM(2)
              CALL FOINTE('FM',FLUXHF,3,NOMPAR,VALPAR,FLUXHM(2),IER21)
            ENDIF
C
            IF (NBNA.EQ.3) THEN
              VALPAR(1) = GEOM(1,MNO)
              VALPAR(2) = GEOM(2,MNO)
              VALPAR(3) = INSTPM(1)
              CALL FOINTE('FM',FLUXHF,3,NOMPAR,VALPAR,FLUXHP(3),IER3)
C
              IF (.NOT. PERMAN) THEN
                VALPAR(3) = INSTPM(2)
                CALL FOINTE('FM',FLUXHF,3,NOMPAR,VALPAR,FLUXHM(3),IER31)
              ENDIF
C
            ENDIF
C
C 3.2.3. ERREUR
C
          ELSE
            CALL U2MESK('F','INDICATEUR_90',1,NOMGD3)
          ENDIF
C
C 3.2.4. ERREUR
C
        ELSE
          CALL U2MESK('F','INDICATEUR_91',1,NOMGD3)
        ENDIF
C
C --------------------------------------------------------------------
C ** 2EME CAS : AUCUNE CONDITION DE FLUX N'A ETE FIXEE SUR LES BORDS
C               DU MAILLAGE : ON A IMPOSE IMPLICITEMENT LE FLUX = 0
C               ATTENTION : NE MARCHE PAS AUJOURD'HUI CAR ON NE SAIT
C               PAS TRIER AVEC LES BORDS DIRICHLET
C --------------------------------------------------------------------
C
      ELSE
C
        FLUXHP(1) = 0.D0
        FLUXHP(2) = FLUXHP(1)
        FLUXHP(3) = FLUXHP(1)
C
        IF (.NOT. PERMAN) THEN
          FLUXHM(1) = 0.D0
          FLUXHM(2) = FLUXHM(1)
          FLUXHM(3) = FLUXHM(1)
        ENDIF
C
      ENDIF
C
C 3.3. TERME DE BORD
C
      IDEC1 = NBCMP*(INO-1)
C
      INTE1 = JAC(1)* ( THETA * FLUXHP(1)+ TA1 * FLUXHM(1)
     &                - THETA *
     &                ( SIELNP(IDEC1+ADSIP+IBID+1) * NX(1)
     &                + SIELNP(IDEC1+ADSIP+IBID+2) * NY(1) )
     &                - TA1 *
     &                ( SIELNM(IDEC1+ADSIP+IBID+1) * NX(1)
     &                + SIELNM(IDEC1+ADSIP+IBID+2) * NY(1) ) )**2
C
      IDEC2 = NBCMP*(JNO-1)
C
      INTE2 = JAC(2)* ( THETA * FLUXHP(2)+ TA1 * FLUXHM(2)
     &                - THETA *
     &                ( SIELNP(IDEC2+ADSIP+IBID+1) * NX(2)
     &                + SIELNP(IDEC2+ADSIP+IBID+2) * NY(2) )
     &                - TA1 *
     &                ( SIELNM(IDEC2+ADSIP+IBID+1) * NX(2)
     &                + SIELNM(IDEC2+ADSIP+IBID+2) * NY(2) ) )**2
C
      IF (NBNA.EQ.3) THEN
C
        IDEC3 = NBCMP*(MNO-1)
C
        INTE3 = JAC(3)* ( THETA * FLUXHP(3)+ TA1 * FLUXHM(3)
     &                  - THETA *
     &                  ( SIELNP(IDEC3+ADSIP+IBID+1) * NX(3)
     &                  + SIELNP(IDEC3+ADSIP+IBID+2) * NY(3) )
     &                  - TA1 *
     &                  ( SIELNM(IDEC3+ADSIP+IBID+1) * NX(3)
     &                  + SIELNM(IDEC3+ADSIP+IBID+2) * NY(3) ) )**2
C
        TM2H1B(3) = TM2H1B(3) + ( INTE1+4.D0*INTE3+INTE2 )/3.D0
C
      ELSE
C
        TM2H1B(3) = TM2H1B(3) + ( INTE1+INTE2 )
C
      ENDIF
C
 9999 CONTINUE
C
      END
