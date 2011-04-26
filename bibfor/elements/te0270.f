      SUBROUTINE TE0270(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                          OPTION : 'CHAR_THER_TEXT_F'
C                          OPTION : 'CHAR_SENS_TEXT_F'
C                          ELEMENTS FOURIER
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
C----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

      CHARACTER*16 OPTION,NOMTE
      INTEGER NBRES
      PARAMETER (NBRES=3)
      CHARACTER*8 NOMPAR(NBRES)
      REAL*8 VALPAR(NBRES),POIDS,R,Z,NX,NY,TPG,THETA,COEN,COENP1,TEXN,
     &       TEXNP1,VAPRIN,VAPRMO
      INTEGER NNO,NNOS,JGANO,NDIM,KP,NPG,IPOIDS,IVF,IDFDE,IGEOM,ITEMPS,
     &        IVECTT,K,I,ITEX,ICOEFH,IVAPRI,IVAPRM,ITEMP,ICODE,IRET
      LOGICAL LSENS,LSTAT

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

C====
C 1.1 PREALABLES: RECUPERATION ADRESSES FONCTIONS DE FORMES...
C====
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)

C====
C 1.2 PREALABLES LIES AUX RECHERCHES DE DONNEES GENERALES
C====
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PTEMPSR','L',ITEMPS)
      CALL JEVECH('PTEMPER','L',ITEMP)
      CALL JEVECH('PCOEFHF','L',ICOEFH)
      CALL JEVECH('PT_EXTF','L',ITEX)
      CALL JEVECH('PVECTTR','E',IVECTT)

C CALCUL DE SENSIBILITE PART I
      LSENS = .FALSE.
      LSTAT = .FALSE.
      IF (OPTION(6:9).EQ.'SENS') THEN
        LSENS = .TRUE.
        CALL JEVECH('PVAPRIN','L',IVAPRI)
        CALL TECACH('ONN','PVAPRMO',1,IVAPRM,IRET)
C L'ABSENCE DE CE CHAMP DETERMINE LE CRITERE STATIONNAIRE OU PAS
        IF (IVAPRM.EQ.0) LSTAT = .TRUE.
      END IF

C====
C 2. CALCULS TERMES DE MASSE (STD ET/OU SENSIBLE)
C====
      THETA = ZR(ITEMPS+2)
      DO 50 KP = 1,NPG
        K = (KP-1)*NNO
        CALL VFF2DN(NDIM,NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),NX,NY,POIDS)
        R = 0.D0
        Z = 0.D0
        TPG = 0.D0
        DO 10 I = 1,NNO
          R   =   R + ZR(IGEOM+2*I-2)*ZR(IVF+K+I-1)
          Z   =   Z + ZR(IGEOM+2*I-1)*ZR(IVF+K+I-1)
          TPG = TPG +   ZR(ITEMP+I-1)*ZR(IVF+K+I-1)
   10   CONTINUE
        POIDS = POIDS*R

C CALCUL DE SENSIBILITE PART II
C DETERMINATION DE T+ (VAPRIN) ET DE T- (VAPRMO)
        IF (LSENS) THEN
          VAPRIN = 0.D0
          VAPRMO = 0.D0
          DO 20 I = 1,NNO
            VAPRIN = VAPRIN + ZR(IVAPRI+I-1)*ZR(IVF+K+I-1)
            IF (.NOT.LSTAT) VAPRMO = VAPRMO +
     &                               ZR(IVAPRM+I-1)*ZR(IVF+K+I-1)
   20     CONTINUE
        END IF

        VALPAR(1) = R
        NOMPAR(1) = 'X'
        VALPAR(2) = Z
        NOMPAR(2) = 'Y'
        NOMPAR(3) = 'INST'
        VALPAR(3) = ZR(ITEMPS)
        CALL FOINTE('FM',ZK8(ICOEFH),3,NOMPAR,VALPAR,COENP1,ICODE)
        VALPAR(3) = ZR(ITEMPS) - ZR(ITEMPS+1)
        CALL FOINTE('FM',ZK8(ICOEFH),3,NOMPAR,VALPAR,COEN,ICODE)
        VALPAR(3) = ZR(ITEMPS)
        CALL FOINTE('FM',ZK8(ITEX),3,NOMPAR,VALPAR,TEXNP1,ICODE)
        VALPAR(3) = ZR(ITEMPS) - ZR(ITEMPS+1)
        CALL FOINTE('FM',ZK8(ITEX),3,NOMPAR,VALPAR,TEXN,ICODE)
CCDIR$ IVDEP
        IF (.NOT.LSENS) THEN
          DO 30 I = 1,NNO
            ZR(IVECTT+I-1) = ZR(IVECTT+I-1) +
     &                       POIDS*ZR(IVF+K+I-1)* (THETA*COENP1*TEXNP1+
     &                        (1.0D0-THETA)*COEN* (TEXN-TPG))
   30     CONTINUE
        ELSE
C CALCUL DE SENSIBILITE PART III
          DO 40 I = 1,NNO
            ZR(IVECTT+I-1) = ZR(IVECTT+I-1) +
     &                       POIDS*ZR(IVF+K+I-1)* (THETA*COENP1*
     &                       (TEXNP1-VAPRIN)+ (1.0D0-THETA)*COEN*
     &                       (TEXN-VAPRMO))
   40     CONTINUE
        END IF
   50 CONTINUE
      END
