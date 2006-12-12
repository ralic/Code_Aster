      SUBROUTINE ERHMB2(INO,NBS,NBNA,HF,NDIM,
     &                  JAC,NX,NY,TX,TY,
     &                  NBCMP,GEOM,IVOIS,
     &                  SIELNO,ADSIP,
     &                  IAGD,TBREF2,IADE2,IAVA2,NCMPM2,IAPTM2,
     &                  IADE3,IAVA3,NCMPM3,IAPTM3,
     &                  TERCLM,TERCLH)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/12/2006   AUTEUR PELLET J.PELLET 
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
C IN INO    : NUMERO DU NOEUD X1
C IN NBS    : NOMBRE D'ARETES
C IN NBNA   : NOMBRE DE SOMMETS SUR L'ARETE
C IN HF     : LONGUEUR DE L'ARETE
C IN NDIM   : DIMENSION DE L'ESPACE
C IN JAC    : VECTEUR DES JACOBIENS DE LA TRANSFORMATION AUX NOEUDS
C IN NX     : VECTEUR DES ABSCISSES DES NORMALES AUX NOEUDS
C IN NY     : VECTEUR DES ORDONNEES DES NORMALES AUX NOEUDS
C IN TX     : VECTEUR DES ABSCISSES DES TANGENTES AUX NOEUDS
C IN TY     : VECTEUR DES ORDONNEES DES TANGENTES AUX NOEUDS
C IN NBCMP  : NOMBRE DE COMPOSANTES DES CONTRAINTES GENERALISEES
C             AUX NOEUDS
C IN GEOM   : TABLEAU DES COORDONNEES
C IN IVOIS  : ADRESSE DES VOISINS
C IN SIELNO : CONTRAINTES AUX NOEUDS PAR ELEMENT
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
C OUT  TERCLM : TERME DE L'INDICATEUR D'ERREUR DU A LA NON-VERIFICATION
C               DES CONDITIONS DE NEUMANN EN MECANIQUE
C OUT  TERCLH : TERME DE L'INDICATEUR D'ERREUR DU A LA NON-VERIFICATION
C               DES CONDITIONS DE NEUMANN EN HYDRAULIQUE
C ......................................................................
C
C   -------------------------------------------------------------------
C     SUBROUTINES APPELLEES :
C       UTILITAIRES : FOINTE
C     FONCTION INTRINSEQUE :
C       SQRT.
C   -------------------------------------------------------------------
C
      IMPLICIT NONE
C
C DECLARATION PARAMETRES D'APPEL
C
      INTEGER  INO,NBS,NBNA,NDIM
      REAL*8   JAC(3),NX(3),NY(3),TX(3),TY(3),HF
      INTEGER  NBCMP,IVOIS,IAGD
      INTEGER  ADSIP
      REAL*8   SIELNO(81)
      INTEGER  TBREF2(12),IADE2,IAVA2,NCMPM2,IAPTM2,
     &         IADE3,IAVA3,NCMPM3,IAPTM3
      REAL*8   TERCLM,TERCLH
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
      INTEGER      JNO, MNO
      INTEGER      IDEC1,IDEC2,IDEC3
      INTEGER      IER1,IER2,IER3,IER4,IER5,IER6
      REAL*8       VALPAR(2),PR(3),CI(3),FLUXHY(3),
     &             INTE1,INTE2,INTE3
      CHARACTER*4  NOMPAR(2)
      CHARACTER*8  PRF,CIF,FLUXHF
      CHARACTER*19 NOMGD2,NOMGD3
C
C =====================================================================
C 1. RECUPERATION SUR LA MAILLE COURANTE AUX NOEUDS INO ET JNO DE :
C     . CONTRAINTES EFFECTIVES (SIGMA MECANIQUE : SIXX, SIYY, SIXY)
C     . CONTRAINTES DE PRESSION (BIOT*PRESSION : SIP)
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
        JNO=1
      ELSE
        JNO=INO+1
      ENDIF
C
      IF (NBNA.EQ.3) THEN
        MNO=NBS+INO
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
            PR(1) = ZR(IAVA2-1+(IENT2-1)*NCMPM2+1)
            PR(2) = PR(1)
            PR(3) = PR(1)
            CI(1) = ZR(IAVA2-1+(IENT2-1)*NCMPM2+2)
            CI(2) = CI(1)
            CI(3) = CI(1)
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
            IF (PRF.EQ.'&FOZERO'.AND.
     &          CIF.EQ.'&FOZERO') THEN
              GOTO 3333
            ENDIF
C
            NOMPAR(1) = 'X'
            NOMPAR(2) = 'Y'
            VALPAR(1) = GEOM(1,INO)
            VALPAR(2) = GEOM(2,INO)
            CALL FOINTE('FM',PRF,2,NOMPAR,VALPAR,PR(1),IER1)
            CALL FOINTE('FM',CIF,2,NOMPAR,VALPAR,CI(1),IER2)
C
            VALPAR(1) = GEOM(1,JNO)
            VALPAR(2) = GEOM(2,JNO)
            CALL FOINTE('FM',PRF,2,NOMPAR,VALPAR,PR(2),IER3)
            CALL FOINTE('FM',CIF,2,NOMPAR,VALPAR,CI(2),IER4)
C
            IF (NBNA.EQ.3) THEN
              VALPAR(1) = GEOM(1,MNO)
              VALPAR(2) = GEOM(2,MNO)
              CALL FOINTE('FM',PRF,2,NOMPAR,VALPAR,PR(3),IER5)
              CALL FOINTE('FM',CIF,2,NOMPAR,VALPAR,CI(3),IER6)
            ENDIF
C
C 2.2.3. ERREUR
C
          ELSE
            CALL U2MESK('F','ELEMENTS_59',1,NOMGD2)
          ENDIF
C
C 2.2.4. ERREUR
C
        ELSE
          CALL U2MESK('F','ELEMENTS_60',1,NOMGD2)
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
        PR(1) = 0.D0
        PR(2) = PR(1)
        PR(3) = PR(1)
        CI(1) = 0.D0
        CI(2) = CI(1)
        CI(3) = CI(1)
C
      ENDIF
C
C 2.3. TERME DE SAUT
C
      IDEC1 = NBCMP*(INO-1)
      INTE1 = JAC(1)*(
     &          (-PR(1)*NX(1)+CI(1)*TX(1)
     &          -(SIELNO(IDEC1+1)+SIELNO(IDEC1+ADSIP))*NX(1)
     &          -SIELNO(IDEC1+4)*NY(1))**2
     &          +(-PR(1)*NY(1)+CI(1)*TY(1)-SIELNO(IDEC1+4)*NX(1)
     &          -(SIELNO(IDEC1+2)+SIELNO(IDEC1+ADSIP))*NY(1))**2)
C
      IDEC2 = NBCMP*(JNO-1)
      INTE2 = JAC(2)*(
     &          (-PR(2)*NX(2)+CI(2)*TX(2)
     &          -(SIELNO(IDEC2+1)+SIELNO(IDEC2+ADSIP))*NX(2)
     &          -SIELNO(IDEC2+4)*NY(2))**2
     &          +(-PR(2)*NY(2)+CI(2)*TY(2)-SIELNO(IDEC2+4)*NX(2)
     &           -(SIELNO(IDEC2+2)+SIELNO(IDEC2+ADSIP))*NY(2))**2)
C
      IF (NBNA.EQ.3) THEN
C
        IDEC3 = NBCMP*(MNO-1)
        INTE3 = JAC(3)*(
     &            (-PR(3)*NX(3)+CI(3)*TX(3)
     &                -(SIELNO(IDEC3+1)+SIELNO(IDEC3+ADSIP))*NX(3)
     &                -SIELNO(IDEC3+4)*NY(3))**2
     &                +(-PR(3)*NY(3)+CI(3)*TY(3)-SIELNO(IDEC3+4)*NX(3)
     &                -(SIELNO(IDEC3+2)+SIELNO(IDEC3+ADSIP))*NY(3))**2 )
C
        TERCLM = TERCLM +
     &               SQRT(HF)*SQRT((INTE1+4.D0*INTE3+INTE2)/3.D0)
      ELSE
        TERCLM = TERCLM + SQRT(HF)*SQRT(INTE1+INTE2)
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
            FLUXHY(1) = ZR(IAVA3-1+(IENT3-1)*NCMPM3+2)
            FLUXHY(2) = FLUXHY(1)
            FLUXHY(3) = FLUXHY(1)
C
C 3.2.2. DETERMINATION DES VALEURS SI FONCTION
C
          ELSE IF (NOMGD3(1:6).EQ.'FTHM_F') THEN
            FLUXHF = ZK8(IAVA3-1+(IENT3-1)*NCMPM3+2)
C
C CE QUI SUIT EST LE SEUL MOYEN AUJOURD'HUI DE PIEGER LES BORDS
C A DIRICHLET. EN EFFET ASTER LES AFFECTE AUTOMATIQUEMENT D'UN NEUMANN
C VALANT &FOZERO. CELA SUPPOSE QUE LES VRAIS BORDS A NEUMANN NUL
C CONT ETE DECLARES EXPLICITEMENT DANS LES COMMANDES.
C
            IF (FLUXHF.EQ.'&FOZERO') THEN
              GOTO 9999
            ENDIF
C
            NOMPAR(1) = 'X'
            NOMPAR(2) = 'Y'
            VALPAR(1) = GEOM(1,INO)
            VALPAR(2) = GEOM(2,INO)
            CALL FOINTE('FM',FLUXHF,2,NOMPAR,VALPAR,FLUXHY(1),IER1)
C
            VALPAR(1) = GEOM(1,JNO)
            VALPAR(2) = GEOM(2,JNO)
            CALL FOINTE('FM',FLUXHF,2,NOMPAR,VALPAR,FLUXHY(2),IER3)
C
            IF (NBNA.EQ.3) THEN
              VALPAR(1) = GEOM(1,MNO)
              VALPAR(2) = GEOM(2,MNO)
              CALL FOINTE('FM',FLUXHF,2,NOMPAR,VALPAR,FLUXHY(3),IER3)
            ENDIF
C
C 3.2.3. ERREUR
C
          ELSE
            CALL U2MESK('F','ELEMENTS_59',1,NOMGD3)
          ENDIF
C
C 3.2.4. ERREUR
C
        ELSE
          CALL U2MESK('F','ELEMENTS_60',1,NOMGD3)
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
        FLUXHY(1) = 0.D0
        FLUXHY(2) = FLUXHY(1)
        FLUXHY(3) = FLUXHY(1)
C
      ENDIF
C
C 3.3. TERME DE SAUT
C
      IDEC1 = NBCMP*(INO-1)
C
      INTE1 = JAC(1)*(FLUXHY(1)-SIELNO(IDEC1+ADSIP+1)*NX(1)
     &         -SIELNO(IDEC1+ADSIP+2)*NY(1))**2
C
      IDEC2 = NBCMP*(JNO-1)
C
      INTE2 = JAC(2)*(FLUXHY(2)-SIELNO(IDEC2+ADSIP+1)*NX(2)
     &         -SIELNO(IDEC2+ADSIP+2)*NY(2))**2
C
      IF (NBNA.EQ.3) THEN
C
      IDEC3 = NBCMP*(MNO-1)
C
         INTE3 = JAC(3)*(FLUXHY(3)-SIELNO(IDEC3+ADSIP+1)*NX(3)
     &              -SIELNO(IDEC3+ADSIP+2)*NY(3))**2
C
        TERCLH = TERCLH +
     &               SQRT(HF)*SQRT((INTE1+4.D0*INTE3+INTE2)/3.D0)
      ELSE
C
        TERCLH = TERCLH + SQRT(HF)*SQRT(INTE1+INTE2)
C
      ENDIF
C
 9999 CONTINUE
C
      END
