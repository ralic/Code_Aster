      SUBROUTINE TE0511(OPTION,NOMTE)
      IMPLICIT   NONE
      CHARACTER*16 OPTION,NOMTE
C.......................................................................
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 18/09/2007   AUTEUR PELLET J.PELLET 
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

C     BUT: -- CALCUL
C               - DU TAUX DE TRIAXIALITE DES CONTRAINTES (TRIAX)
C               - DE LA CONTRAINTES EQUIVALENTE D'ENDOMAGEMENT (SIGMA*)

C        TAUX DE TRIAXIALITE : TRIAX
C        ---------------------------
C           TRIAX    = SIGMA_H / SIGMA_EQ

C    OU     S        = SIGMA - 1/3 TRACE(SIGMA) I
C           SIGMA_EQ = ( 3/2 S : S ) ** 1/2
C           SIGMA_H  = 1/3 TRACE(SIGMA)
C           SIGMA    = TENSEUR DES CONTRAINTES
C           S        = DEVIATEUR DE SIGMA
C           I        = TENSEUR IDENTITE

C        CONTRAINTES EQUIVALENTE D'ENDOMAGEMENT : SIGMA*
C        -----------------------------------------------
C           SI_ENDO  = SIGMA_EQ (2/3(1+NU) + 3(1-2 NU) TRIAX**2 )**1/2

C     OU    NU       = COEFFICIENT DE POISSON
C.......................................................................
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

      INTEGER NBSIGM,NBPAR
      INTEGER NPG1,IPOIDS,IVF,IDFDE,JGANO
      INTEGER MXCMEL,NBPGMX,NBRES,I,K,NNO,NNOS,NPG
      INTEGER NBSIG,IGAU,INDIC,INO,NDIM1,IADZI,JTAB(3)
      INTEGER IMATE,IRET,IDTRIA,ICONPG,ICONNO,IAZK24
      PARAMETER (MXCMEL=162)
      PARAMETER (NBPGMX=27)
      PARAMETER (NBRES=2)
      REAL*8 SIGMA(MXCMEL),SIGEQ(NBPGMX),TRSIG(NBPGMX)
      REAL*8 SENDO(NBPGMX),SENDON(NBPGMX),R8PREM
      REAL*8 TRIAX(NBPGMX),TRIAXN(NBPGMX),VALRES(NBRES)
      REAL*8 ZERO,UN,DEUX,TROIS,UNTIER,DETIER,TRDEMI
      REAL*8 VALPAR,XNU,COE1,COE2,R8VIDE
      CHARACTER*2 CODRES(NBRES)
      CHARACTER*4 FAMI
      CHARACTER*24 VALK
      CHARACTER*8 NOMPAR,NOMRES(NBRES),NOMAIL
      CHARACTER*16 PHENOM
C.......................................................................
      DATA NOMRES/'E','NU'/
C.......................................................................

      ZERO = 0.0D0
      UN = 1.0D0
      DEUX = 2.0D0
      TROIS = 3.0D0
      UNTIER = 1.0D0/3.0D0
      DETIER = 2.0D0/3.0D0
      TRDEMI = 3.0D0/2.0D0

      DO 10 I = 1,MXCMEL
        SIGMA(I) = ZERO
   10 CONTINUE

      DO 20 I = 1,NBPGMX
        SIGEQ(I) = ZERO
        TRSIG(I) = ZERO
        SENDO(I) = ZERO
        SENDON(I) = ZERO
        TRIAX(I) = ZERO
        TRIAXN(I) = ZERO
   20 CONTINUE


C --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---
C     -----------------------------------------------

      CALL JEVECH('PMATERC','L',IMATE)


C --- RECUPERATION DU TYPE DU MATERIAU DANS PHENOM
C     --------------------------------------------

      FAMI = 'RIGI'
      CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,CODRES)

      CALL RCVALB(FAMI,1,1,'+',ZI(IMATE),' ',PHENOM,0,' ',ZERO,
     &            NBRES,NOMRES,VALRES,CODRES,'FM')

      XNU = VALRES(2)
      COE1 = DETIER* (UN+XNU)
      COE2 = TROIS* (UN-DEUX*XNU)


C ----     DIMENSION DE L'ELEMENT :
      CALL ELREF4(' ',FAMI,NDIM1,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)

C ----     NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT :
      NBSIG = NBSIGM()

C --- RECUPERATION  ET DES L'ADRESSES DU TAUX DE TRIAXIALITE DES
C --- CONTRAINTES (TRIAX) ET DE LA CONTRAINTES EQUIVALENTE
C --- D'ENDOMAGEMENT (SENDO)
C     -----------------------------------------------------------
      CALL JEVECH('PTRIAXS','E',IDTRIA)


C ---- RECHERCHE DU TYPE DU CHAMP D'ENTREE, DEFINI AUX NOEUDS OU
C ---- AUX POINTS DE GAUSS
C      -----------------------------------------------------------
      CALL TECACH('ONN','PCONTPG',1,ICONPG,IRET)
      CALL TECACH('ONN','PCONTNO',1,ICONNO,IRET)


C     ------------------------------------------------------------------
C              CHAMP DE CONTRAINTES DEFINI AUX POINTS DE GAUSS
C     ------------------------------------------------------------------

      IF (ICONPG.NE.0) THEN
        CALL TECACH('OOO','PCONTPG',3,JTAB,IRET)
        NPG1 = JTAB(3)
        CALL ASSERT(NPG.EQ.NPG1)


C ---    AFFECTATION DU VECTEUR DE TRAVAIL SIGMA REPRESENTANT
C ---    LE TENSEUR DE CONTRAINTES
C        -------------------------
        K = 0
        DO 40 IGAU = 1,NPG
          DO 30 I = 1,NBSIG
            K = K + 1
            SIGMA(I+ (IGAU-1)*NBSIG) = ZR(ICONPG+K-1)
   30     CONTINUE
   40   CONTINUE

C ---    CALCUL DU DEVIATEUR DES CONTRAINTES
C        -----------------------------------

        DO 50 IGAU = 1,NPG

          INDIC = (IGAU-1)*NBSIG
          TRSIG(IGAU) = UNTIER* (SIGMA(INDIC+1)+SIGMA(INDIC+2)+
     &                  SIGMA(INDIC+3))

          SIGMA(INDIC+1) = SIGMA(INDIC+1) - TRSIG(IGAU)
          SIGMA(INDIC+2) = SIGMA(INDIC+2) - TRSIG(IGAU)
          SIGMA(INDIC+3) = SIGMA(INDIC+3) - TRSIG(IGAU)

   50   CONTINUE

C ---    CALCUL DE SIGEQ
C        ---------------
        DO 60 IGAU = 1,NPG

          INDIC = (IGAU-1)*NBSIG
          SIGEQ(IGAU) = SIGMA(INDIC+1)*SIGMA(INDIC+1) +
     &                  SIGMA(INDIC+2)*SIGMA(INDIC+2) +
     &                  SIGMA(INDIC+3)*SIGMA(INDIC+3) +
     &                  SIGMA(INDIC+4)*SIGMA(INDIC+4)*DEUX
          IF (NBSIG.EQ.6) SIGEQ(IGAU) = SIGEQ(IGAU) +
     &                                 SIGMA(INDIC+5)*SIGMA(INDIC+5)*
     &                                 DEUX + SIGMA(INDIC+6)*
     &                                 SIGMA(INDIC+6)*DEUX
          SIGEQ(IGAU) = (SIGEQ(IGAU)*TRDEMI)**0.5D0

   60   CONTINUE

C ---    CALCUL DU TAUX DE TRIAXIALITE DES CONTRAINTES (TRIAX)
C        -----------------------------------------------------

        DO 80 IGAU = 1,NPG
          IF (ABS(SIGEQ(IGAU)).LE.R8PREM()) THEN
            CALL TECAEL(IADZI,IAZK24)
            NOMAIL = ZK24(IAZK24-1+3) (1:8)
            VALK = NOMAIL
            CALL U2MESG('A', 'ELEMENTS4_98',1,VALK,0,0,0,0.D0)
            DO 70 INO = 1,NNO
              TRIAXN(INO) = R8VIDE()
   70       CONTINUE
            GO TO 170
          END IF
          TRIAX(IGAU) = TRSIG(IGAU)/SIGEQ(IGAU)
   80   CONTINUE

C ---    CALCUL DE LA CONTRAINTES EQUIVALENTE D'ENDOMAGEMENT (SENDO)
C        -------------------------------------------------------------

        DO 90 IGAU = 1,NPG
          SENDO(IGAU) = (COE1*SIGEQ(IGAU)**2+COE2*TRSIG(IGAU)**2)**0.5D0
   90   CONTINUE

C ---    CALCUL DES VALEURS AUX NOEUDS :
C        -------------------------------
        CALL PPGAN2(JGANO,1,TRIAX,TRIAXN)
        CALL PPGAN2(JGANO,1,SENDO,SENDON)


C     ------------------------------------------------------------------
C                 CHAMP DE CONTRAINTES DEFINI AUX NOEUDS
C     ------------------------------------------------------------------

      ELSE IF (ICONNO.NE.0) THEN

C ---    AFFECTATION DU VECTEUR DE TRAVAIL SIGMA REPRESENTANT
C ---    LE TENSEUR DE CONTRAINTES
C         -------------------------
        K = 0
        DO 110 INO = 1,NNO
          DO 100 I = 1,NBSIG
            K = K + 1
            SIGMA(I+ (INO-1)*NBSIG) = ZR(ICONNO+K-1)
  100     CONTINUE
  110   CONTINUE

C ---    CALCUL DU DEVIATEUR DES CONTRAINTES
C        -----------------------------------

        DO 120 INO = 1,NNO

          INDIC = (INO-1)*NBSIG
          TRSIG(INO) = UNTIER* (SIGMA(INDIC+1)+SIGMA(INDIC+2)+
     &                 SIGMA(INDIC+3))

          SIGMA(INDIC+1) = SIGMA(INDIC+1) - TRSIG(INO)
          SIGMA(INDIC+2) = SIGMA(INDIC+2) - TRSIG(INO)
          SIGMA(INDIC+3) = SIGMA(INDIC+3) - TRSIG(INO)

  120   CONTINUE

C ---    CALCUL DE SIGEQ
C        ---------------

        DO 130 INO = 1,NNO

          INDIC = (INO-1)*NBSIG
          SIGEQ(INO) = SIGMA(INDIC+1)*SIGMA(INDIC+1) +
     &                 SIGMA(INDIC+2)*SIGMA(INDIC+2) +
     &                 SIGMA(INDIC+3)*SIGMA(INDIC+3) +
     &                 SIGMA(INDIC+4)*SIGMA(INDIC+4)*DEUX
          IF (NBSIG.EQ.6) SIGEQ(INO) = SIGEQ(INO) +
     &                                SIGMA(INDIC+5)*SIGMA(INDIC+5)*
     &                                DEUX + SIGMA(INDIC+6)*
     &                                SIGMA(INDIC+6)*DEUX
          SIGEQ(INO) = (SIGEQ(INO)*TRDEMI)**0.5D0

  130   CONTINUE

C ---    CALCUL DU TAUX DE TRIAXIALITE DES CONTRAINTES (TRIAX)
C        -----------------------------------------------------

        DO 150 INO = 1,NNO
          IF (ABS(SIGEQ(INO)).LE.R8PREM()) THEN
            CALL TECAEL(IADZI,IAZK24)
            NOMAIL = ZK24(IAZK24-1+3) (1:8)
            VALK = NOMAIL
            CALL U2MESG('A', 'ELEMENTS4_98',1,VALK,0,0,0,0.D0)
            DO 140 INDIC = 1,NNO
              TRIAXN(INDIC) = R8VIDE()
  140       CONTINUE
            GO TO 170
          END IF
          TRIAXN(INO) = TRSIG(INO)/SIGEQ(INO)
  150   CONTINUE

C ---    CALCUL DE LA CONTRAINTES EQUIVALENTE D'ENDOMAGEMENT (SENDO)
C        -------------------------------------------------------------

        DO 160 INO = 1,NNO
          SENDON(INO) = (COE1*SIGEQ(INO)**2+COE2*TRSIG(INO)**2)**0.5D0
  160   CONTINUE

C     ------------------------------------------------------------------
C ---    CHAMP DE CONTRAINTES MAL DEFINI
      ELSE
        CALL U2MESK('F','ELEMENTS4_6',1,OPTION)
      END IF


C --- RECUPERATION  ET AFFECTATION DU DU TAUX DE TRIAXIALITE DES
C --- CONTRAINTES (TRIAX) ET DE LA CONTRAINTES EQUIVALENTE
C --- D'ENDOMAGEMENT (SENDO)

  170 CONTINUE

      DO 180 INO = 1,NNO
        ZR(IDTRIA+2* (INO-1)) = TRIAXN(INO)
        ZR(IDTRIA+2* (INO-1)+1) = SENDON(INO)
  180 CONTINUE

      END
