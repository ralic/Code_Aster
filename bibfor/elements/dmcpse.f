      SUBROUTINE DMCPSE(FAMI,MATER,INSTAN,POUM,IGAU,ISGAU,REPERE,D)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 20/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C.======================================================================
C      DMCPSE --   CALCUL DE LA MATRICE DE HOOKE POUR LES ELEMENTS
C                  MASSIFS 2D EN CONTRAINTES PLANES
C                  POUR DES MATERIAUX ISOTROPE, ORTHOTROPE
C                  ET ISOTROPE TRANSVERSE
C
C                  CALCUL DE SENSIBILITE
C
C   ARGUMENT        E/S  TYPE         ROLE
C    FAMI           IN     K4       FAMILLE DU POINT DE GAUSS
C    MATER          IN     I        MATERIAU
C    INSTAN         IN     R        INSTANT DE CALCUL (0 PAR DEFAUT)
C    POUM           IN     K1       + OU -
C    IGAU           IN     I        POINT DE GAUSS
C    ISGAU          IN     I        SOUS-POINT DE GAUSS
C    REPERE(3)      IN     R        VALEURS DEFINISSANT LE REPERE
C                                   D'ORTHOTROPIE
C    D(4,4)         OUT    R        MATRICE DE HOOKE
C.======================================================================
      IMPLICIT NONE
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

C -----  ARGUMENTS
      CHARACTER*(*)  FAMI,POUM
      REAL*8   REPERE(7),D(4,4),INSTAN
      INTEGER  MATER,IGAU,ISGAU
C -----  VARIABLES LOCALES
      INTEGER NBRES,I,J,IMATSE,NBV,IREP,TETYPS
      PARAMETER (NBRES=7)

      INTEGER ICODRE(NBRES)
      CHARACTER*8 NOMRES(NBRES),NOMPAR
      CHARACTER*16 PHENOM,PHESEN,VALK(2)

      REAL*8 VALRES(NBRES),PREC,ZERO,UNDEMI,UN,VALPAR
      REAL*8 PASSAG(4,4),DORTH(4,4),WORK(4,4),E,ES,NU,NUS
      REAL*8 E1,E2,NU12,R8PREM,E1S,E2S,NU12S,G12S
C.========================= DEBUT DU CODE EXECUTABLE ==================

C ---- INITIALISATIONS
C      ---------------
      ZERO   = 0.0D0
      UNDEMI = 0.5D0
      UN     = 1.0D0
      PREC   = R8PREM()
      NOMPAR = 'INST'
      VALPAR = INSTAN

      DO 20 I = 1,4
        DO 10 J = 1,4
          D(I,J) = ZERO
          DORTH(I,J) = ZERO
          WORK(I,J) = ZERO
   10   CONTINUE
   20 CONTINUE

C ---- RECUPERATION DU TYPE DU MATERIAU DANS PHENOM
C      --------------------------------------------
      CALL RCCOMA(MATER,'ELAS',PHENOM,ICODRE)

C ---- RECUPERATION DU MATERIAU DERIVE SI SENSIBILITE
C      ----------------------------------------------
      CALL JEVECH('PMATSEN','L',IMATSE)
      CALL RCCOMA(ZI(IMATSE),'ELAS',PHESEN,ICODRE)
      IF (PHESEN.NE.PHENOM) THEN
          VALK(1)=PHESEN
          VALK(2)=PHENOM
          CALL U2MESK('F','ELEMENTS_38',2,VALK)
      ENDIF
C      ------------
C ---- CAS ISOTROPE
C      ------------
      IF (PHENOM.EQ.'ELAS') THEN

         NOMRES(1) = 'E'
         NOMRES(2) = 'NU'
         NBV = 2

C ----   RECUPERATION DES CARACTERISTIQUES MECANIQUES
C        -----------
         CALL RCVALB(FAMI,IGAU,ISGAU,POUM,MATER,' ',PHENOM,1,NOMPAR,
     &               VALPAR,NBV,NOMRES,VALRES,ICODRE,1)

         E  = VALRES(1)
         NU = VALRES(2)

         CALL RCVALB(FAMI,IGAU,ISGAU,POUM,ZI(IMATSE),' ',PHENOM,1,
     &               NOMPAR,VALPAR,NBV,NOMRES,VALRES,ICODRE,1)
         ES  = VALRES(1)
         NUS = VALRES(2)

         IF ((ABS(ES).LT.PREC) .AND. (ABS(NUS).LT.PREC)) THEN
C --------- CALCUL INSENSIBLE
            GOTO 9999

         ELSE IF (ABS(NUS).LT.PREC) THEN
C --------- SENSIBILITE PAR RAPPORT A E
            TETYPS = 1

         ELSE IF (ABS(ES).LT.PREC) THEN
C --------- SENSIBILITE PAR RAPPORT A NU
            TETYPS = 2

         END IF

         IF (TETYPS.EQ.1) THEN
            D(1,1) = UN/ (UN-NU*NU)
            D(1,2) = D(1,1)*NU

            D(2,1) = D(1,2)
            D(2,2) = D(1,1)

            D(4,4) = UNDEMI/ (UN+NU)
         ELSE IF (TETYPS.EQ.2) THEN
            D(1,1) = 2.D0*E*NU/ ((UN-NU*NU)* (UN-NU*NU))
            D(1,2) = E* (UN+NU*NU)/ ((UN-NU*NU)* (UN-NU*NU))

            D(2,1) = D(1,2)
            D(2,2) = D(1,1)

            D(4,4) = -1.D0*UNDEMI*E/ ((UN+NU)* (UN+NU))
         END IF

C      --------------
C ---- CAS ORTHOTROPE
C      --------------
      ELSE IF (PHENOM.EQ.'ELAS_ORTH') THEN

         NOMRES(1) = 'E_L'
         NOMRES(2) = 'E_T'
         NOMRES(3) = 'NU_LT'
         NOMRES(4) = 'G_LT'
         NBV = 4

C ----   RECUPERATION DES CARACTERISTIQUES MECANIQUES
C        -----------
         CALL RCVALB(FAMI,IGAU,ISGAU,POUM,MATER,' ',PHENOM,1,NOMPAR,
     &               VALPAR,NBV,NOMRES,VALRES,ICODRE,1)

         E1   = VALRES(1)
         E2   = VALRES(2)
         NU12 = VALRES(3)

         CALL RCVALB(FAMI,IGAU,ISGAU,POUM,ZI(IMATSE),' ',PHENOM,1,
     &               NOMPAR,VALPAR,NBV,NOMRES,VALRES,ICODRE,1)

         E1S   = VALRES(1)
         E2S   = VALRES(2)
         NU12S = VALRES(3)
         G12S  = VALRES(4)

         IF ((ABS(E1S).LT.PREC) .AND. (ABS(E2S).LT.PREC) .AND.
     &        (ABS(NU12S).LT.PREC) .AND. (ABS(G12S).LT.PREC)) THEN
C --------- CALCUL INSENSIBLE
            GOTO 9999

         ELSE IF ((ABS(E2S).LT.PREC) .AND. (ABS(NU12S).LT.PREC) .AND.
     &             (ABS(G12S).LT.PREC)) THEN
C --------- SENSIBILITE PAR RAPPORT A E1
            TETYPS = 1

         ELSE IF ((ABS(E1S).LT.PREC) .AND. (ABS(NU12S).LT.PREC) .AND.
     &             (ABS(G12S).LT.PREC)) THEN
C --------- SENSIBILITE PAR RAPPORT A E2
            TETYPS = 2

         ELSE IF ((ABS(E1S).LT.PREC) .AND. (ABS(E2S).LT.PREC) .AND.
     &             (ABS(G12S).LT.PREC)) THEN
C --------- SENSIBILITE PAR RAPPORT A NU12
            TETYPS = 3

         ELSE IF ((ABS(E1S).LT.PREC) .AND. (ABS(E2S).LT.PREC) .AND.
     &             (ABS(NU12S).LT.PREC)) THEN
C --------- SENSIBILITE PAR RAPPORT A G12
            TETYPS = 4

         END IF

        IF (TETYPS.EQ.1) THEN
          DORTH(1,1) =  -(E1**2/(E1 - E2*NU12**2)**2) +
     &                   (2*E1)/(E1 - E2*NU12**2)

          DORTH(1,2) = -((E1*E2*NU12)/(E1 - E2*NU12**2)**2) +
     &                   (E2*NU12)/(E1 - E2*NU12**2)

          DORTH(2,2) = -((E1*E2)/(E1 - E2*NU12**2)**2) +
     &                    E2/(E1 - E2*NU12**2)

          DORTH(2,1) = DORTH(1,2)

          DORTH(4,4) = ZERO

        ELSE IF (TETYPS.EQ.2) THEN
          DORTH(1,1) = (E1**2*NU12**2)/(E1 - E2*NU12**2)**2

          DORTH(1,2) = (E1*E2*NU12**3)/(E1 - E2*NU12**2)**2 +
     &                 (E1*NU12)/(E1 - E2*NU12**2)

          DORTH(2,2) = (E1*E2*NU12**2)/(E1 - E2*NU12**2)**2 +
     &                  E1/(E1 - E2*NU12**2)

          DORTH(2,1) = DORTH(1,2)

          DORTH(4,4) = ZERO

        ELSE IF (TETYPS.EQ.3) THEN
          DORTH(1,1) = (2*E1**2*E2*NU12)/(E1 - E2*NU12**2)**2

          DORTH(1,2) = (2*E1*E2**2*NU12**2)/(E1 - E2*NU12**2)**2 +
     &                 (E1*E2)/(E1 - E2*NU12**2)

          DORTH(2,2) = (2*E1*E2**2*NU12)/(E1 - E2*NU12**2)**2

          DORTH(2,1) = DORTH(1,2)

          DORTH(4,4) = ZERO

        ELSE IF (TETYPS.EQ.4) THEN
          DORTH(1,1) = ZERO
          DORTH(1,2) = ZERO
          DORTH(2,2) = ZERO
          DORTH(2,1) = ZERO
          DORTH(4,4) = UN
        END IF

C ----   CALCUL DE LA MATRICE DE PASSAGE DU REPERE D'ORTHOTROPIE AU
C ----   REPERE GLOBAL POUR LE TENSEUR D'ELASTICITE
C        ------------------------------------------
        CALL DPAO2D(REPERE,IREP,PASSAG)

C ----   TENSEUR D'ELASTICITE DANS LE REPERE GLOBAL :
C ----    D_GLOB = PASSAG_T * D_ORTH * PASSAG
C ----    (ON NE FAIT REELLEMENT LE PRODUIT QUE SI LA MATRICE
C ----     DE PASSAGE N'EST PAS L'IDENTITE)
C        ----------------------------------
        CALL ASSERT((IREP.EQ.1).OR.(IREP.EQ.0))
        IF (IREP.EQ.1) THEN
          CALL UTBTAB('ZERO',4,4,DORTH,PASSAG,WORK,D)
        ELSE IF (IREP.EQ.0) THEN
          DO 40 I = 1,4
            DO 30 J = 1,4
              D(I,J) = DORTH(I,J)
   30       CONTINUE
   40     CONTINUE
        END IF


C      -----------------------
C ---- CAS ISOTROPE-TRANSVERSE
C      -----------------------
      ELSE IF (PHENOM.EQ.'ELAS_ISTR') THEN

         NOMRES(1) = 'E_L'
         NOMRES(2) = 'NU_LT'
         NBV = 2

C ----   RECUPERATION DES CARACTERISTIQUES MECANIQUES
C        -----------
         CALL RCVALB(FAMI,IGAU,ISGAU,POUM,MATER,' ',PHENOM,1,NOMPAR,
     &               VALPAR,NBV,NOMRES,VALRES,ICODRE,1)

         E  = VALRES(1)
         NU = VALRES(2)

         CALL RCVALB(FAMI,IGAU,ISGAU,POUM,ZI(IMATSE),' ',PHENOM,1,
     &               NOMPAR,VALPAR,NBV,NOMRES,VALRES,ICODRE,1)

         ES  = VALRES(1)
         NUS = VALRES(2)

         IF ((ABS(ES).LT.PREC) .AND. (ABS(NUS).LT.PREC)) THEN
C --------- CALCUL INSENSIBLE
            GOTO 9999

         ELSE IF (ABS(NUS).LT.PREC) THEN
C --------- SENSIBILITE PAR RAPPORT A E
            TETYPS = 1

         ELSE IF (ABS(ES).LT.PREC) THEN
C --------- SENSIBILITE PAR RAPPORT A NU
            TETYPS = 2
         END IF

        IF (TETYPS.EQ.1) THEN
          D(1,1) = 1/ (-NU**2+UN)

          D(1,2) = NU/ (-NU**2+UN)

          D(2,2) = 1/ (-NU**2+UN)

          D(2,1) = NU/ (-NU**2+UN)

          D(4,4) = UNDEMI/ (NU+UN)
        ELSE IF (TETYPS.EQ.2) THEN
          D(1,1) = (2*E*NU)/ (-NU**2+UN)**2

          D(1,2) = (2*E*NU**2)/ (-NU**2+UN)**2 + E/ (-NU**2+UN)

          D(2,2) = (2*E*NU)/ (-NU**2+UN)**2

          D(2,1) = (2*E*NU**2)/ (-NU**2+UN)**2 + E/ (-NU**2+UN)

          D(4,4) = - ((E*UNDEMI)/ (NU+UN)**2)
        END IF

      ELSE
        CALL U2MESK('F','ELEMENTS_15',1,PHENOM)
      END IF
C.============================ FIN DE LA ROUTINE ======================
 9999 CONTINUE
      END
