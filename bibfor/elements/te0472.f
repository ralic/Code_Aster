      SUBROUTINE TE0472(OPTION,NOMTE)
      IMPLICIT   NONE
      CHARACTER*16 OPTION,NOMTE
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C ======================================================================
C MODIF ELEMENTS  DATE 31/01/2005   AUTEUR ROMEO R.FERNANDES 
C RESPONSABLE UFBHHLL C.CHAVANT
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
C TOLE CRP_20

C     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN MECANIQUE
C          CORRESPONDANT A UN FLUX THM (THH, THHM, THH, THH2,HHM,HM)
C          SUR DES FACES D'ELEMENTS ISOPARAMETRIQUES 2D

C          OPTION : 'CHAR_MECA_FLUX_R'
C          OPTION : 'CHAR_MECA_FLUX_F'

C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ======================================================================
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
C ======================================================================
      CHARACTER*8 NOMPAR(3)
      REAL*8 POIDS,R,Z,TX,TY,NX,NY,VALPAR(3),DELTAT,TPLUS
      REAL*8 PRES,PRESF

      REAL*8 FLU1,FLU2,FLUTH
      INTEGER NAPRE1,NAPRE2,NATEMP

      INTEGER NNO,NNOS,KP,NPG,NDIM,JGANO
      INTEGER IPOIDS,IVF,IDFDE,IGEOM
      INTEGER IPRES,K,I,L,IRES,IFLUX,ITEMPS,IOPT,IPRESF
      INTEGER IS,IM,LS,LM
      INTEGER IDLTHM,NDLTHM,DEBTHM
      INTEGER IFLUXF,IRET,NDLNO
      LOGICAL AXI,P2P1

      INTEGER NNOMAX,NVOMAX,NSOMAX
      PARAMETER (NNOMAX=20,NVOMAX=4,NSOMAX=8)
      INTEGER VOISIN(NVOMAX,NNOMAX)
      INTEGER NBVOS(NSOMAX)
      CHARACTER*8 TYPMOD(2)
C     ------------------------------------------------------------------

      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)

      CALL CAETHM(NOMTE,AXI,TYPMOD,NNOS,NNOMAX,NVOMAX,NSOMAX,VOISIN,
     +                                                    NBVOS,P2P1)

C     ------------------------------------------------------------------
C  CETTE ROUTINE FAIT UN CALCUL EN THHM ,THH2M , HM , HHM , THH THH2 THM
C     ------------------------------------------------------------------
C        SI MODELISATION = THHM
      IF (NOMTE(1:4).EQ.'THHM') THEN
        NDLNO = 5
C        SI MODELISATION = THH2M
      ELSE IF (NOMTE(1:5).EQ.'THH2M') THEN
        NDLNO = 5

C        SI MODELISATION = HM
      ELSE IF (NOMTE(1:2).EQ.'HM') THEN
        NDLNO = 3

C SI MODELISATION = HHM
      ELSE IF (NOMTE(1:3).EQ.'HHM') THEN
        NDLNO = 4

C SI MODELISATION = HH2M
      ELSE IF (NOMTE(1:4).EQ.'HH2M') THEN
        NDLNO = 4

C SI MODELISATION = THH
      ELSE IF (NOMTE(1:4).EQ.'THH_') THEN
        NDLNO = 3

      ELSE IF (NOMTE(1:5).EQ.'THH2_') THEN
        NDLNO = 3

C SI MODELISATION = THV
      ELSE IF (NOMTE(1:4).EQ.'THV_') THEN
        NDLNO = 2

C SI MODELISATION = THM
      ELSE IF (NOMTE(1:4).EQ.'THM_') THEN
        NDLNO = 4

      ELSE
        CALL UTMESS('F','TE0472','ELEMENT '//'NON TRAITE')
      END IF


      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PVECTUR','E',IRES)


      IF (OPTION.EQ.'CHAR_MECA_FLUX_R') THEN
        IOPT = 1
        CALL JEVECH('PFLUXR','L',IFLUX)
        CALL JEVECH('PTEMPSR','L',ITEMPS)
        DELTAT = ZR(ITEMPS+1)

      ELSE IF (OPTION.EQ.'CHAR_MECA_FLUX_F') THEN
        IOPT = 2
        CALL JEVECH('PFLUXF','L',IFLUXF)
        CALL JEVECH('PTEMPSR','L',ITEMPS)
        TPLUS = ZR(ITEMPS)
        DELTAT = ZR(ITEMPS+1)
        NOMPAR(1) = 'X'
        NOMPAR(2) = 'Y'
        NOMPAR(3) = 'INST'
        VALPAR(3) = TPLUS

      ELSE IF (OPTION.EQ.'CHAR_MECA_PRES_R') THEN
        IOPT = 3
        CALL JEVECH('PPRESSR','L',IPRES)

      ELSE IF (OPTION.EQ.'CHAR_MECA_PRES_F') THEN
        IOPT = 4
        CALL JEVECH('PPRESSF','L',IPRESF)
        CALL JEVECH('PTEMPSR','L',ITEMPS)
        NOMPAR(1) = 'X'
        NOMPAR(2) = 'Y'
        NOMPAR(3) = 'INST'
        VALPAR(3) = ZR(ITEMPS)

      END IF

C    BOUCLE SUR LES POINTS DE GAUSS

      DO 190 KP = 1,NPG
        K = (KP-1)*NNO
        CALL VFF2DN(NDIM,NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),NX,NY,POIDS)

C        --- OPTION CHAR_MECA_FLUX_F ---

        IF (AXI) THEN
          R = 0.D0
          Z = 0.D0
          DO 10 I = 1,NNO
            L = (KP-1)*NNO + I
            R = R + ZR(IGEOM+2*I-2)*ZR(IVF+L-1)
   10     CONTINUE
          POIDS = POIDS*R
        END IF


C        --- OPTION CHAR_MECA_FLUX_R OU CHAR_MECA_FLUX_F ---

        IF (IOPT.EQ.1 .OR. IOPT.EQ.2) THEN

C SI MODELISATION = THHM OU THH

          IF (NOMTE(1:4).EQ.'THHM' .OR. NOMTE(1:4).EQ.'THH_' .OR.
     &        NOMTE(1:4).EQ.'THH2') THEN

C --- NAPRE1,NAPRE2,NATEMP SONT MIS EN PLACE
C --- POUR UNE EVENTUELLE MODIFICATION DE L'ORDRE DES DDL :
C     PRE1, PRE2, TEMP DANS LES CATALOGUES D'ELEMENTS

            NAPRE1 = 0
            NAPRE2 = 1
            NATEMP = 2

            IF (IOPT.EQ.1) THEN

C ---   FLUTH REPRESENTE LE FLUX THERMIQUE
C ---   FLU1 REPRESENTE LE FLUX ASSOCIE A PRE1
C ---   FLU2 REPRESENTE LE FLUX ASSOCIE A PRE2

              FLU1 = ZR((IFLUX)+ (KP-1)*3+NAPRE1)
              FLU2 = ZR((IFLUX)+ (KP-1)*3+NAPRE2)
              FLUTH = ZR((IFLUX)+ (KP-1)*3+NATEMP)

            ELSE IF (IOPT.EQ.2) THEN
              R = 0.D0
              Z = 0.D0
              DO 20 I = 1,NNO
                L = (KP-1)*NNO + I
                R = R + ZR(IGEOM+2*I-2)*ZR(IVF+L-1)
                Z = Z + ZR(IGEOM+2*I-1)*ZR(IVF+L-1)
   20         CONTINUE
              VALPAR(1) = R
              VALPAR(2) = Z
              CALL FOINTE('FM',ZK8(IFLUXF+NAPRE1),3,NOMPAR,VALPAR,FLU1,
     &                    IRET)
              CALL FOINTE('FM',ZK8(IFLUXF+NAPRE2),3,NOMPAR,VALPAR,FLU2,
     &                    IRET)
              CALL FOINTE('FM',ZK8(IFLUXF+NATEMP),3,NOMPAR,VALPAR,FLUTH,
     &                    IRET)
            END IF
            IF ((NOMTE(1:4).EQ.'THHM') .OR.
     &          (NOMTE(1:5).EQ.'THH2M')) THEN
              DO 30 I = 1,NNO
                L = 5* (I-1) - 1
                ZR(IRES+L+3) = ZR(IRES+L+3) -
     &                         POIDS*DELTAT*FLU1*ZR(IVF+K+I-1)
                ZR(IRES+L+4) = ZR(IRES+L+4) -
     &                         POIDS*DELTAT*FLU2*ZR(IVF+K+I-1)
                ZR(IRES+L+5) = ZR(IRES+L+5) -
     &                         POIDS*DELTAT*FLUTH*ZR(IVF+K+I-1)
   30         CONTINUE
              NDLTHM = 3
              DEBTHM = 3
            ELSE
              DO 40 I = 1,NNO
                L = 3* (I-1) - 1
                ZR(IRES+L+1) = ZR(IRES+L+1) -
     &                         POIDS*DELTAT*FLU1*ZR(IVF+K+I-1)
                ZR(IRES+L+2) = ZR(IRES+L+2) -
     &                         POIDS*DELTAT*FLU2*ZR(IVF+K+I-1)
                ZR(IRES+L+3) = ZR(IRES+L+3) -
     &                         POIDS*DELTAT*FLUTH*ZR(IVF+K+I-1)
   40         CONTINUE
              NDLTHM = 3
              DEBTHM = 1
            END IF

          END IF

C SI MODELISATION = THV

          IF (NOMTE(1:4).EQ.'THV_') THEN

C --- NAPRE1,NAPRE2,NATEMP SONT MIS EN PLACE
C --- POUR UNE EVENTUELLE MODIFICATION DE L'ORDRE DES DDL :
C     PRE1, PRE2, TEMP DANS LES CATALOGUES D'ELEMENTS

            NAPRE1 = 0
            NATEMP = 1

            IF (IOPT.EQ.1) THEN

C ---   FLUTH REPRESENTE LE FLUX THERMIQUE
C ---   FLU1 REPRESENTE LE FLUX ASSOCIE A PRE1

              FLU1 = ZR((IFLUX)+ (KP-1)*2+NAPRE1)
              FLUTH = ZR((IFLUX)+ (KP-1)*2+NATEMP)

            ELSE IF (IOPT.EQ.2) THEN
              R = 0.D0
              Z = 0.D0
              DO 50 I = 1,NNO
                L = (KP-1)*NNO + I
                R = R + ZR(IGEOM+2*I-2)*ZR(IVF+L-1)
                Z = Z + ZR(IGEOM+2*I-1)*ZR(IVF+L-1)
   50         CONTINUE
              VALPAR(1) = R
              VALPAR(2) = Z
              CALL FOINTE('FM',ZK8(IFLUXF+NAPRE1),3,NOMPAR,VALPAR,FLU1,
     &                    IRET)
              CALL FOINTE('FM',ZK8(IFLUXF+NAPRE2),3,NOMPAR,VALPAR,FLU2,
     &                    IRET)
              CALL FOINTE('FM',ZK8(IFLUXF+NATEMP),3,NOMPAR,VALPAR,FLUTH,
     &                    IRET)
            END IF
            DO 60 I = 1,NNO
              L = 2* (I-1) - 1
              ZR(IRES+L+1) = ZR(IRES+L+1) -
     &                       POIDS*DELTAT*FLU1*ZR(IVF+K+I-1)
              ZR(IRES+L+2) = ZR(IRES+L+2) -
     &                       POIDS*DELTAT*FLUTH*ZR(IVF+K+I-1)
   60       CONTINUE
            NDLTHM = 2
            DEBTHM = 1

          END IF

C SI MODELISATION = HM
          IF (NOMTE(1:2).EQ.'HM') THEN

            NAPRE1 = 0

            IF (IOPT.EQ.1) THEN

C ---   FLU1 REPRESENTE LE FLUX ASSOCIE A PRE1

              FLU1 = ZR((IFLUX)+ (KP-1)+NAPRE1)

            ELSE IF (IOPT.EQ.2) THEN
              R = 0.D0
              Z = 0.D0
              DO 70 I = 1,NNO
                L = (KP-1)*NNO + I
                R = R + ZR(IGEOM+2*I-2)*ZR(IVF+L-1)
                Z = Z + ZR(IGEOM+2*I-1)*ZR(IVF+L-1)
   70         CONTINUE
              VALPAR(1) = R
              VALPAR(2) = Z
              CALL FOINTE('FM',ZK8(IFLUXF+NAPRE1),3,NOMPAR,VALPAR,FLU1,
     &                    IRET)
            END IF
            DO 80 I = 1,NNO
              L = 3* (I-1) - 1
              ZR(IRES+L+3) = ZR(IRES+L+3) -
     &                       POIDS*DELTAT*FLU1*ZR(IVF+K+I-1)
   80       CONTINUE
            NDLTHM = 1
            DEBTHM = 3

          END IF

C SI MODELISATION = HHM

          IF ((NOMTE(1:3).EQ.'HHM') .OR. (NOMTE(1:4).EQ.'HH2M')) THEN

            NAPRE1 = 0
            NAPRE2 = 1

            IF (IOPT.EQ.1) THEN

C ---   FLU1 REPRESENTE LE FLUX ASSOCIE A PRE1
C ---   FLU2 REPRESENTE LE FLUX ASSOCIE A PRE2

              FLU1 = ZR((IFLUX)+ (KP-1)*2+NAPRE1)
              FLU2 = ZR((IFLUX)+ (KP-1)*2+NAPRE2)

            ELSE IF (IOPT.EQ.2) THEN
              R = 0.D0
              Z = 0.D0
              DO 90 I = 1,NNO
                L = (KP-1)*NNO + I
                R = R + ZR(IGEOM+2*I-2)*ZR(IVF+L-1)
                Z = Z + ZR(IGEOM+2*I-1)*ZR(IVF+L-1)
   90         CONTINUE
              VALPAR(1) = R
              VALPAR(2) = Z
              CALL FOINTE('FM',ZK8(IFLUXF+NAPRE1),3,NOMPAR,VALPAR,FLU1,
     &                    IRET)
              CALL FOINTE('FM',ZK8(IFLUXF+NAPRE2),3,NOMPAR,VALPAR,FLU2,
     &                    IRET)
            END IF
            DO 100 I = 1,NNO
              L = 4* (I-1) - 1
              ZR(IRES+L+3) = ZR(IRES+L+3) -
     &                       POIDS*DELTAT*FLU1*ZR(IVF+K+I-1)
              ZR(IRES+L+4) = ZR(IRES+L+4) -
     &                       POIDS*DELTAT*FLU2*ZR(IVF+K+I-1)
  100       CONTINUE
            NDLTHM = 2
            DEBTHM = 3

          END IF


C SI MODELISATION = THM

          IF (NOMTE(1:3).EQ.'THM') THEN

C --- NAPRE1,NAPRE2,NATEMP SONT MIS EN PLACE
C --- POUR UNE EVENTUELLE MODIFICATION DE L'ORDRE DES DDL :
C     PRE1, PRE2, TEMP DANS LES CATALOGUES D'ELEMENTS

            NAPRE1 = 0
            NATEMP = 1

            IF (IOPT.EQ.1) THEN

C ---   FLUTH REPRESENTE LE FLUX THERMIQUE
C ---   FLU1 REPRESENTE LE FLUX ASSOCIE A PRE1
C ---   FLU2 REPRESENTE LE FLUX ASSOCIE A PRE2

              FLU1 = ZR((IFLUX)+ (KP-1)*2+NAPRE1)
              FLUTH = ZR((IFLUX)+ (KP-1)*2+NATEMP)

            ELSE IF (IOPT.EQ.2) THEN
              R = 0.D0
              Z = 0.D0
              DO 110 I = 1,NNO
                L = (KP-1)*NNO + I
                R = R + ZR(IGEOM+2*I-2)*ZR(IVF+L-1)
                Z = Z + ZR(IGEOM+2*I-1)*ZR(IVF+L-1)
  110         CONTINUE
              VALPAR(1) = R
              VALPAR(2) = Z
              CALL FOINTE('FM',ZK8(IFLUXF+NAPRE1),3,NOMPAR,VALPAR,FLU1,
     &                    IRET)
              CALL FOINTE('FM',ZK8(IFLUXF+NATEMP),3,NOMPAR,VALPAR,FLUTH,
     &                    IRET)
            END IF
            DO 120 I = 1,NNO
              L = 4* (I-1) - 1
              ZR(IRES+L+3) = ZR(IRES+L+3) -
     &                       POIDS*DELTAT*FLU1*ZR(IVF+K+I-1)
              ZR(IRES+L+4) = ZR(IRES+L+4) -
     &                       POIDS*DELTAT*FLUTH*ZR(IVF+K+I-1)
  120       CONTINUE

            NDLTHM = 2
            DEBTHM = 3

          END IF

C TRAITEMENT P2P1

          IF (P2P1) THEN
            DO 150 IM = NNOS + 1,NNO
              IS = VOISIN(1,IM)
              LS = NDLNO* (IS-1) - 1
              LM = NDLNO* (IM-1) - 1
              DO 130 IDLTHM = 1,NDLTHM
                ZR(IRES+LS+DEBTHM+IDLTHM-1) = ZR(IRES+LS+DEBTHM+IDLTHM-
     &            1) + ZR(IRES+LM+DEBTHM+IDLTHM-1)/2.D0
  130         CONTINUE
              IS = VOISIN(2,IM)
              LS = NDLNO* (IS-1) - 1
              DO 140 IDLTHM = 1,NDLTHM
                ZR(IRES+LS+DEBTHM+IDLTHM-1) = ZR(IRES+LS+DEBTHM+IDLTHM-
     &            1) + ZR(IRES+LM+DEBTHM+IDLTHM-1)/2.D0
                ZR(IRES+LM+DEBTHM+IDLTHM-1) = 0.D0
  140         CONTINUE
  150       CONTINUE
          END IF

C        --- OPTION CHAR_MECA_PRES_R OU CHAR_MECA_PRES_F ---

        ELSE IF ((IOPT.EQ.3) .OR. (IOPT.EQ.4)) THEN
          IF (IOPT.EQ.3) THEN
            PRES = 0.D0
            DO 160 I = 1,NNO
              L = (KP-1)*NNO + I
              PRES = PRES + ZR(IPRES+I-1)*ZR(IVF+L-1)

  160       CONTINUE
          ELSE IF (IOPT.EQ.4) THEN
            PRES = 0.D0
            DO 170 I = 1,NNO
              VALPAR(1) = ZR(IGEOM+2*I-2)
              VALPAR(2) = ZR(IGEOM+2*I-1)
              CALL FOINTE('FM',ZK8(IPRESF),3,NOMPAR,VALPAR,PRESF,IRET)
              L = (KP-1)*NNO + I
              PRES = PRES + PRESF*ZR(IVF+L-1)
  170       CONTINUE
          END IF

          TX = -NX*PRES
          TY = -NY*PRES

          DO 180 I = 1,NNO

            L = NDLNO* (I-1) - 1
            ZR(IRES+L+1) = ZR(IRES+L+1) + TX*ZR(IVF+K+I-1)*POIDS
            ZR(IRES+L+2) = ZR(IRES+L+2) + TY*ZR(IVF+K+I-1)*POIDS

  180     CONTINUE
        END IF

  190 CONTINUE

      END
