      SUBROUTINE DMCPSE(MATER,TEMPE,HYDR,SECH,INSTAN,REPERE,D)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 02/10/2002   AUTEUR ASSIRE A.ASSIRE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    MATER          IN     I        MATERIAU
C    TEMPE          IN     R        TEMPERATURE AU POINT D'INTEGRATION
C    HYDR           IN     R        HYDRATATION AU POINT D'INTEGRATION
C    SECH           IN     R        SECHAGE AU POINT D'INTEGRATION
C    INSTAN         IN     R        INSTANT DE CALCUL (0 PAR DEFAUT)
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
      REAL*8   TEMPE,REPERE(7),D(4,4),INSTAN,HYDR,SECH
      INTEGER  MATER
C -----  VARIABLES LOCALES
      INTEGER NBRES,I,J,IMATSE,NBV,IREP,TETYPS
      PARAMETER (NBRES=7)

      CHARACTER*2 CODRET(NBRES)
      CHARACTER*8 NOMRES(NBRES),NOMPAR(4)
      CHARACTER*16 PHENOM,PHESEN

      REAL*8 VALRES(NBRES),VALPAR(4),PREC,ZERO,UNDEMI,UN
      REAL*8 PASSAG(4,4),DORTH(4,4),WORK(4,4),E,ES,NU,NUS
      REAL*8 E1,E2,NU12,R8PREM,E1S,E2S,NU12S,G12S
C.========================= DEBUT DU CODE EXECUTABLE ==================

C ---- INITIALISATIONS
C      ---------------
      ZERO   = 0.0D0
      UNDEMI = 0.5D0
      UN     = 1.0D0
      PREC   = R8PREM()

      NOMPAR(1) = 'TEMP'
      NOMPAR(2) = 'INST'
      NOMPAR(3) = 'HYDR'
      NOMPAR(4) = 'SECH'
      VALPAR(1) = TEMPE
      VALPAR(2) = INSTAN
      VALPAR(3) = HYDR
      VALPAR(4) = SECH

      DO 20 I = 1,4
        DO 10 J = 1,4
          D(I,J) = ZERO
          DORTH(I,J) = ZERO
          WORK(I,J) = ZERO
   10   CONTINUE
   20 CONTINUE

C ---- RECUPERATION DU TYPE DU MATERIAU DANS PHENOM
C      --------------------------------------------
      CALL RCCOMA(MATER,'ELAS',PHENOM,CODRET)

C ---- RECUPERATION DU MATERIAU DERIVE SI SENSIBILITE
C      ----------------------------------------------
      CALL JEVECH('PMATSEN','L',IMATSE)
      CALL RCCOMA(ZI(IMATSE),'ELAS',PHESEN,CODRET)
      IF (PHESEN.NE.PHENOM) CALL UTMESS('F','DMCPSE',
     &                             '! PB PHESEN.NE.PHENOM !')

C      ------------
C ---- CAS ISOTROPE
C      ------------
      IF (PHENOM.EQ.'ELAS') THEN

         NOMRES(1) = 'E'
         NOMRES(2) = 'NU'
         NBV = 2

C ----   INTERPOLATION DES COEFFICIENTS EN FONCTION DE LA TEMPERATURE
C ----   ET DU TEMPS
C        -----------
         CALL RCVALA(MATER,PHENOM,4,NOMPAR,VALPAR,NBV,NOMRES,VALRES,
     &              CODRET,'FM')

         E  = VALRES(1)
         NU = VALRES(2)

         CALL RCVALA(ZI(IMATSE),PHENOM,4,NOMPAR,VALPAR,NBV,NOMRES,
     &                VALRES,CODRET,'FM')
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

C ----   INTERPOLATION DES COEFFICIENTS EN FONCTION DE LA TEMPERATURE
C ----   ET DU TEMPS
C        -----------
         CALL RCVALA(MATER,PHENOM,4,NOMPAR,VALPAR,NBV,NOMRES,VALRES,
     &              CODRET,'FM')

         E1   = VALRES(1)
         E2   = VALRES(2)
         NU12 = VALRES(3)

         CALL RCVALA(ZI(IMATSE),PHENOM,4,NOMPAR,VALPAR,NBV,NOMRES,
     &                VALRES,CODRET,'FM')

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
     -                   (2*E1)/(E1 - E2*NU12**2)

          DORTH(1,2) = -((E1*E2*NU12)/(E1 - E2*NU12**2)**2) + 
     -                   (E2*NU12)/(E1 - E2*NU12**2)

          DORTH(2,2) = -((E1*E2)/(E1 - E2*NU12**2)**2) + 
     -                    E2/(E1 - E2*NU12**2)

          DORTH(2,1) = DORTH(1,2)

          DORTH(4,4) = ZERO

        ELSE IF (TETYPS.EQ.2) THEN
          DORTH(1,1) = (E1**2*NU12**2)/(E1 - E2*NU12**2)**2

          DORTH(1,2) = (E1*E2*NU12**3)/(E1 - E2*NU12**2)**2 + 
     -                 (E1*NU12)/(E1 - E2*NU12**2)

          DORTH(2,2) = (E1*E2*NU12**2)/(E1 - E2*NU12**2)**2 + 
     -                  E1/(E1 - E2*NU12**2)

          DORTH(2,1) = DORTH(1,2)

          DORTH(4,4) = ZERO

        ELSE IF (TETYPS.EQ.3) THEN
          DORTH(1,1) = (2*E1**2*E2*NU12)/(E1 - E2*NU12**2)**2

          DORTH(1,2) = (2*E1*E2**2*NU12**2)/(E1 - E2*NU12**2)**2 + 
     -                 (E1*E2)/(E1 - E2*NU12**2)

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
        IF (IREP.EQ.1) THEN
          CALL UTBTAB('ZERO',4,4,DORTH,PASSAG,WORK,D)
        ELSE IF (IREP.EQ.0) THEN
          DO 40 I = 1,4
            DO 30 J = 1,4
              D(I,J) = DORTH(I,J)
   30       CONTINUE
   40     CONTINUE
        ELSE
          CALL UTMESS('F','DMCPSE','IREP (INDICATEUR DE CHANGEMENT'//
     &                ' DE REPERE) DOIT ETRE EGAL A 0 OU 1 ')
        END IF


C      -----------------------
C ---- CAS ISOTROPE-TRANSVERSE
C      -----------------------
      ELSE IF (PHENOM.EQ.'ELAS_ISTR') THEN

         NOMRES(1) = 'E_L'
         NOMRES(2) = 'NU_LT'
         NBV = 2

C ----   INTERPOLATION DES COEFFICIENTS EN FONCTION DE LA TEMPERATURE
C ----   ET DU TEMPS
C        -----------
         CALL RCVALA(MATER,PHENOM,4,NOMPAR,VALPAR,NBV,NOMRES,VALRES,
     &              CODRET,'FM')

         E  = VALRES(1)
         NU = VALRES(2)

         CALL RCVALA(ZI(IMATSE),PHENOM,4,NOMPAR,VALPAR,NBV,NOMRES,
     &                VALRES,CODRET,'FM')

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
        CALL UTMESS('F','DMCPSE','LA NATURE DU MATERIAU '//PHENOM//
     &              ' N''EST PAS TRAITEE, SEULES SONT CONSIDEREES '//
     &              'LES NATURES : ELAS, ELAS_ISTR, ELAS_ORTH .')
      END IF
C.============================ FIN DE LA ROUTINE ======================
 9999 CONTINUE
      END
