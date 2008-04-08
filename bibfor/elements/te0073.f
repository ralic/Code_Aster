      SUBROUTINE TE0073(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 08/04/2008   AUTEUR MEUNIER S.MEUNIER 
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
C    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
C                          OPTION : 'CHAR_THER_TEXT_F/R'
C                                   'CHAR_THER_RAYO_F/R'
C                                   'CHAR_SENS_TEXT_F'
C                                   'CHAR_SENS_RAYO_F'

C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       18/01/02 (OB): MODIFICATIONS POUR INSERER LES ARGUMENTS OPTION
C       NELS PERMETTANT D'UTILISER CETTE ROUTINE POUR CALCULER LA
C       SENSIBILITE PAR RAPPORT A H.
C       + MODIFS FORMELLES: IMPLICIT NONE, IDENTATION...
C       08/03/02 (OB): CORRECTION BUG EN STATIONNAIRE ET SENSIBILITE
C       11/03/02 (OB): INTRO. SENSIBILITE PAR RAPPORT AU RAYONNEMENT
C       + MODIFS FORMELLES: CALL FOINTE....
C----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

      CHARACTER*16 OPTION,NOMTE
      INTEGER NBRES
      PARAMETER (NBRES=3)
      CHARACTER*8 NOMPAR(NBRES),ELREFE
      REAL*8 VALPAR(NBRES),POIDS,R,Z,NX,NY,TPG,PREC,R8PREM,COEN,COENP1,
     &       TEXN,TEXNP1,VAPRIN,VAPRMO,EPSNS,COORSE(18),VECTT(9),THETA,
     &       SIGM1S,SIGMNS,EPS1S,SIGM1,SIGMN,EPS1,EPSN,TPF1,TPFN,TZ0,
     &       R8T0,TPF1S,TPFNS
      INTEGER NNO,NNOS,JGANO,NDIM,KP,NPG,IPOIDS,IVF,IDFDE,IGEOM,ITEMPS,
     &        IVECTT,I,L,LI,ITEX,ICOEFH,IRAY,ITEMP,NNOP2,C(6,9),ISE,
     &        NSE,IVAPRI,IVAPRM,J,IER,ICODE,IRAYS,IRET
      LOGICAL LSENS,LSTAT,LAXI,LTEXT,LTEATT

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

      CALL ELREF1(ELREFE)
      IF (NOMTE(5:7).EQ.'SL3') ELREFE = 'SE2'
      CALL ELREF4(ELREFE,'RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,
     &            JGANO)

C====
C 1.1 PREALABLES: RECUPERATION ADRESSES FONCTIONS DE FORMES...
C====
      TZ0 = R8T0()
      PREC = R8PREM()
      LAXI = .FALSE.
      IF (LTEATT(' ','AXIS','OUI')) LAXI = .TRUE.

      IF (OPTION(11:14).EQ.'TEXT') THEN
        LTEXT = .TRUE.
      ELSE IF (OPTION(11:14).EQ.'RAYO') THEN
        LTEXT = .FALSE.
      ELSE
        CALL U2MESS('F','ELEMENTS2_95')
      END IF
      IF (OPTION(6:9).EQ.'SENS') THEN
        LSENS = .TRUE.
      ELSE IF (OPTION(6:9).EQ.'THER') THEN
        LSENS = .FALSE.
      ELSE
        CALL U2MESS('F','ELEMENTS2_95')
      END IF

C====
C 1.2 PREALABLES LIES AUX RECHERCHES DE DONNEES GENERALES
C====

C CALCUL DE SENSIBILITE PART 0
      LSTAT = .FALSE.
      IF (LTEXT) THEN
C CHAR_.._TEXT : 2 TYPES DE CALCUL

        CALL JEVECH('PTEMPER','L',ITEMP)
        CALL JEVECH('PCOEFHF','L',ICOEFH)
        CALL JEVECH('PT_EXTF','L',ITEX)

C CALCUL DE SENSIBILITE PART I: SENSIBILITE / H ECHANGE EXT
        IF (LSENS) THEN
          CALL JEVECH('PVAPRIN','L',IVAPRI)
          CALL TECACH('ONN','PVAPRMO',1,IVAPRM,IRET)
C L'ABSENCE DE CE CHAMP DETERMINE LE CRITERE STATIONNAIRE OU PAS
          IF (IVAPRM.EQ.0) LSTAT = .TRUE.
        END IF

      ELSE
C CHAR_..._RAYO: 4 TYPES DE CALCUL

C CHAMP DE RAYONNEMENT STD OU DERIVE
        CALL JEVECH('PRAYONF','L',IRAY)

C CALCUL DE SENSIBILITE PART I BIS
        IF (LSENS) THEN
C RECUPERATION DE (DT/DS)-. LA PRESENCE DE CE PARAMETRE, OPTIONNEL EN
C CALCUL DE SENSIBILITE, SIGNIFIE QU'IL FAUT ASSEMBLER LE TERME COMPLEN
C TAIRE. SINON, IL FAUT ASSEMBLER LE TERME CORRESPONDANT A LA DERIVEE
C PAR RAPPORT A UNE DES CARACTERISTIQUES DU RAYONNEMENT
          CALL TECACH('ONN','PTEMPER',1,ITEMP,IRET)
          IF (ITEMP.EQ.0) THEN
C SENSIBILITE EN RAYONNEMENT: CHAMP DE RAYONNEMENT STD ET T+/T-
            CALL JEVECH('PRAYONS','L',IRAYS)
            CALL JEVECH('PVAPRIN','L',IVAPRI)
            CALL TECACH('ONN','PVAPRMO',1,IVAPRM,IRET)
            IF (IVAPRM.EQ.0) LSTAT = .TRUE.
          ELSE
C TERME COMPLEMENTAIRE EN RAYONNEMENT: CHAMP CORRESPONDANT A T-
            CALL JEVECH('PVAPRMO','L',IVAPRM)
            IVAPRI = 0
            IRAYS = 0
            LSTAT = .FALSE.
          END IF
        ELSE
C CALCUL STD: LECTURE DE T-
          CALL JEVECH('PTEMPER','L',ITEMP)
          LSTAT = .FALSE.
        END IF
C FIN DU IF LTEXT
      END IF

C TRONC COMMUN
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PTEMPSR','L',ITEMPS)
      CALL JEVECH('PVECTTR','E',IVECTT)

C====
C 1.3 PREALABLES LIES AUX CALCULS
C====
      THETA = ZR(ITEMPS+2)
      CALL CONNEC(NOMTE,NSE,NNOP2,C)
      DO 10 I = 1,NNOP2
        VECTT(I) = 0.D0
   10 CONTINUE
      NOMPAR(1) = 'X'
      NOMPAR(2) = 'Y'
      NOMPAR(3) = 'INST'

C====
C 2. CALCULS TERMES DE MASSE (STD ET/OU SENSIBLE)
C====

C BOUCLE SUR LES SOUS-ELEMENTS

      DO 160 ISE = 1,NSE

        DO 30 I = 1,NNO
          DO 20 J = 1,2
            COORSE(2* (I-1)+J) = ZR(IGEOM-1+2* (C(ISE,I)-1)+J)
   20     CONTINUE
   30   CONTINUE

        DO 150 KP = 1,NPG
          CALL VFF2DN(NDIM,NNO,KP,IPOIDS,IDFDE,COORSE,NX,NY,
     &                POIDS)
          R = 0.D0
          Z = 0.D0
          TPG = 0.D0
          IF (ITEMP.NE.0) THEN
            DO 40 I = 1,NNO
C CALCUL DE T- (OU (DT/DS)- EN SENSIBILITE)
              L = (KP-1)*NNO + I
              TPG = TPG + ZR(ITEMP-1+C(ISE,I))*ZR(IVF+L-1)
   40       CONTINUE
          END IF

          DO 50 I = 1,NNO
            L = (KP-1)*NNO + I
            R = R + COORSE(2* (I-1)+1)*ZR(IVF+L-1)
            Z = Z + COORSE(2* (I-1)+2)*ZR(IVF+L-1)
   50     CONTINUE
          IF (LAXI) POIDS = POIDS*R

C CALCUL DE SENSIBILITE PART II
C DETERMINATION DE T+ (VAPRIN) ET DE T- (VAPRMO) SI NECESSAIRE
          IF (LSENS) THEN
            VAPRIN = 0.D0
            VAPRMO = 0.D0
C CALCUL T+ SAUF TERME COMPLEMENTAIRE EN RAYONNEMENT
            IF (IVAPRI.NE.0) THEN
              DO 60 I = 1,NNO
                L = (KP-1)*NNO + I
                VAPRIN = VAPRIN + ZR(IVAPRI-1+C(ISE,I))*ZR(IVF+L-1)
   60         CONTINUE
            END IF
C CALCUL DE T- EN TRANSITOIRE
            IF (.NOT.LSTAT) THEN
              DO 70 I = 1,NNO
                L = (KP-1)*NNO + I
                VAPRMO = VAPRMO + ZR(IVAPRM-1+C(ISE,I))*ZR(IVF+L-1)
   70         CONTINUE
            END IF
          END IF

          VALPAR(1) = R
          VALPAR(2) = Z
          VALPAR(3) = ZR(ITEMPS)

C====
C 2.1 OPTION CHAR_THER_TEXT_F/R OU CHAR_SENS_TEXT_F
C====
          IF (LTEXT) THEN

            CALL FOINTE('FM',ZK8(ICOEFH),3,NOMPAR,VALPAR,COENP1,ICODE)
            CALL ASSERT (ICODE.EQ.0)
            IF (THETA.NE.1.0D0) THEN
              VALPAR(3) = ZR(ITEMPS) - ZR(ITEMPS+1)
              CALL FOINTE('FM',ZK8(ICOEFH),3,NOMPAR,VALPAR,COEN,ICODE)
              CALL ASSERT (ICODE.EQ.0)
            ELSE
              COEN = 0.D0
            END IF

            VALPAR(3) = ZR(ITEMPS)
            CALL FOINTE('FM',ZK8(ITEX),3,NOMPAR,VALPAR,TEXNP1,ICODE)
            CALL ASSERT (ICODE.EQ.0)
            IF (THETA.NE.1.0D0) THEN
              VALPAR(3) = ZR(ITEMPS) - ZR(ITEMPS+1)
              CALL FOINTE('FM',ZK8(ITEX),3,NOMPAR,VALPAR,TEXN,ICODE)
              CALL ASSERT (ICODE.EQ.0)
            ELSE
              TEXN = 0.D0
            END IF

            IF (.NOT.LSENS) THEN
C CALCUL STD
              DO 80 I = 1,NNO
                LI = IVF + (KP-1)*NNO + I - 1
                VECTT(C(ISE,I)) = VECTT(C(ISE,I)) +
     &                            POIDS*ZR(LI)* (THETA*COENP1*TEXNP1+
     &                            (1.0D0-THETA)*COEN* (TEXN-TPG))
   80         CONTINUE
            ELSE
C CALCUL DE SENSIBILITE PART III
              DO 90 I = 1,NNO
                LI = IVF + (KP-1)*NNO + I - 1
                VECTT(C(ISE,I)) = VECTT(C(ISE,I)) +
     &                            POIDS*ZR(LI)* (THETA*COENP1*
     &                            (TEXNP1-VAPRIN)+ (1.0D0-THETA)*COEN*
     &                            (TEXN-VAPRMO))
   90         CONTINUE
            END IF

C====
C 2.2 OPTION CHAR_THER_RAYO_F/R OU CHAR_SENS_RAYO_F
C====
          ELSE

C PB STD ET TERME COMPLEMENTAIRE EN SENSIBILITE
C                         ===> DONNEES DU RAYONNEMENT INITIAL
C SENSIBILITE / UNE DES CARACTERISTIQUES DU RAYONNEMENT
C                         ===> DONNEES DU RAYONNEMENT DERIVE
            CALL FOINTE('FM',ZK8(IRAY),3,NOMPAR,VALPAR,SIGM1,IER)
            CALL ASSERT (IER.EQ.0)
            IF (THETA.NE.1.0D0) THEN
              VALPAR(3) = ZR(ITEMPS) - ZR(ITEMPS+1)
              CALL FOINTE('FM',ZK8(IRAY),3,NOMPAR,VALPAR,SIGMN,IER)
              CALL ASSERT (IER.EQ.0)
            ELSE
              SIGMN = 0.D0
            END IF

            VALPAR(3) = ZR(ITEMPS)
            CALL FOINTE('FM',ZK8(IRAY+1),3,NOMPAR,VALPAR,EPS1,IER)
            CALL ASSERT (IER.EQ.0)
            IF (THETA.NE.1.0D0) THEN
              VALPAR(3) = ZR(ITEMPS) - ZR(ITEMPS+1)
              CALL FOINTE('FM',ZK8(IRAY+1),3,NOMPAR,VALPAR,EPSN,IER)
              CALL ASSERT (IER.EQ.0)
            ELSE
              EPSN = 0.D0
            END IF

            VALPAR(3) = ZR(ITEMPS)
            CALL FOINTE('FM',ZK8(IRAY+2),3,NOMPAR,VALPAR,TPF1,IER)
            CALL ASSERT (IER.EQ.0)
            IF (THETA.NE.1.0D0) THEN
              VALPAR(3) = ZR(ITEMPS) - ZR(ITEMPS+1)
              CALL FOINTE('FM',ZK8(IRAY+2),3,NOMPAR,VALPAR,TPFN,IER)
              CALL ASSERT (IER.EQ.0)
            ELSE
              TPFN = 0.D0
            END IF

C CALCUL DE SENSIBILITE PART IV
C SENSIBILITE / UNE DES CARACTERISTIQUES DU RAYONNEMENT
C                         ===> DONNEES DU RAYONNEMENT INITIAL
            IF (LSENS .AND. (IRAYS.NE.0)) THEN
              CALL FOINTE('FM',ZK8(IRAYS),3,NOMPAR,VALPAR,SIGM1S,IER)
              CALL ASSERT (IER.EQ.0)
              IF (THETA.NE.1.0D0) THEN
                VALPAR(3) = ZR(ITEMPS) - ZR(ITEMPS+1)
                CALL FOINTE('FM',ZK8(IRAYS),3,NOMPAR,VALPAR,SIGMNS,IER)
                CALL ASSERT (IER.EQ.0)
              ELSE
                SIGMNS = 0.D0
              END IF

              VALPAR(3) = ZR(ITEMPS)
              CALL FOINTE('FM',ZK8(IRAYS+1),3,NOMPAR,VALPAR,EPS1S,IER)
              CALL ASSERT (IER.EQ.0)
              IF (THETA.NE.1.0D0) THEN
                VALPAR(3) = ZR(ITEMPS) - ZR(ITEMPS+1)
                CALL FOINTE('FM',ZK8(IRAYS+1),3,NOMPAR,VALPAR,EPSNS,IER)
                CALL ASSERT (IER.EQ.0)
              ELSE
                EPSNS = 0.D0
              END IF

              VALPAR(3) = ZR(ITEMPS)
              CALL FOINTE('FM',ZK8(IRAYS+2),3,NOMPAR,VALPAR,TPF1S,IER)
              CALL ASSERT (IER.EQ.0)
              IF (THETA.NE.1.0D0) THEN
                VALPAR(3) = ZR(ITEMPS) - ZR(ITEMPS+1)
                CALL FOINTE('FM',ZK8(IRAYS+2),3,NOMPAR,VALPAR,TPFNS,IER)
                CALL ASSERT (IER.EQ.0)
              ELSE
                TPFNS = 0.D0
              END IF
            END IF

            IF (.NOT.LSENS) THEN
C CALCUL STD
              DO 100 I = 1,NNO
                LI = IVF + (KP-1)*NNO + I - 1
                VECTT(C(ISE,I)) = VECTT(C(ISE,I)) +
     &                            POIDS*ZR(LI)* (THETA*SIGM1*EPS1*
     &                            (TPF1+TZ0)**4+ (1.0D0-THETA)*SIGMN*
     &                            EPSN* ((TPFN+TZ0)**4- (TPG+TZ0)**4))
  100         CONTINUE
            ELSE

C CALCUL DE SENSIBILITE PART V
              IF ((ITEMP.NE.0) .AND. (.NOT.LSTAT)) THEN
C TERME COMPLEMENTAIRE EN SENSIBILITE

                DO 110 I = 1,NNO
                  LI = IVF + (KP-1)*NNO + I - 1
                  VECTT(C(ISE,I)) = VECTT(C(ISE,I)) +
     &                              4*POIDS* (THETA-1.D0)*ZR(LI)*TPG*
     &                              SIGMN*EPSN* ((VAPRMO+TZ0)**3)
  110           CONTINUE
              ELSE


C ON RAJOUTE CES TESTS POUR DISTINGUER ENTRE LA DERIVATION PAR RAPPORT
C A L'EMISSIVITE, LA CONSTANTE DE STEFAN OU LA TEMPERATURE INFINI
                IF ((ABS(SIGM1).LT.PREC) .AND. (ABS(EPS1).LT.PREC) .AND.
     &              (ABS(TPF1).LT.PREC)) THEN
C PAS DE TERME DE SENSIBILITE SUPPLEMENTAIRE, CALCUL INSENSIBLE

                ELSE IF ((ABS(SIGM1).LT.PREC) .AND.
     &                   (ABS(TPF1).LT.PREC)) THEN
C SENSIBILITE PAR RAPPORT A L'EMISSIVITE
                  DO 120 I = 1,NNO
                    LI = IVF + (KP-1)*NNO + I - 1
                    VECTT(C(ISE,I)) = VECTT(C(ISE,I)) +
     &                                POIDS*EPS1*ZR(LI)*
     &                                (EPS1*SIGM1S*THETA*
     &                                ((TPF1S+TZ0)**4- (VAPRIN+TZ0)**4)+
     &                                EPSN*SIGMNS* (1.D0-THETA)*
     &                                ((TPFNS+TZ0)**4- (VAPRMO+TZ0)**4))
  120             CONTINUE

                ELSE IF ((ABS(EPS1).LT.PREC) .AND.
     &                   (ABS(TPF1).LT.PREC)) THEN
C SENSIBILITE PAR RAPPORT A LA CONSTANTE DE STEFAN
                  DO 130 I = 1,NNO
                    LI = IVF + (KP-1)*NNO + I - 1
                    VECTT(C(ISE,I)) = VECTT(C(ISE,I)) +
     &                                POIDS*SIGM1*ZR(LI)*
     &                                (EPS1S*SIGM1*THETA*
     &                                ((TPF1S+TZ0)**4- (VAPRIN+TZ0)**4)+
     &                                EPSNS*SIGMN* (1.D0-THETA)*
     &                                ((TPFNS+TZ0)**4- (VAPRMO+TZ0)**4))
  130             CONTINUE

                ELSE IF ((ABS(EPS1).LT.PREC) .AND.
     &                   (ABS(SIGM1).LT.PREC)) THEN
C SENSIBILITE PAR RAPPORT A TEMPERATURE A L'INFINI
                  DO 140 I = 1,NNO
                    LI = IVF + (KP-1)*NNO + I - 1
                    VECTT(C(ISE,I)) = VECTT(C(ISE,I)) +
     &                                4*POIDS*ZR(LI)* (THETA*EPS1S*
     &                                SIGM1S*TPF1* (TPF1S+TZ0)**3+
     &                                (1.D0-THETA)*EPSNS*SIGMNS*TPFN*
     &                                (TPFNS+TZ0)**3)
  140             CONTINUE

                ELSE
                  CALL U2MESS('F','SENSIBILITE_38')
                END IF
C FIN DU IF ITEMP
              END IF
C FIN DU IF LSENS
            END IF
C FIN DU IF LTEXT
          END IF

C FIN DE BOUCLE SUR LES PTS DE GAUSS
  150   CONTINUE
C FIN DE BOUCLE SUR LES SOUS-ELEMENTS
  160 CONTINUE

      DO 170 I = 1,NNOP2
        ZR(IVECTT-1+I) = VECTT(I)
  170 CONTINUE
      END
