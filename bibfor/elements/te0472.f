      SUBROUTINE TE0472(OPTION,NOMTE)
      IMPLICIT     NONE
      CHARACTER*16 OPTION,NOMTE
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C ======================================================================
C MODIF ELEMENTS  DATE 23/03/2010   AUTEUR ANGELINI O.ANGELINI 
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
C          CORRESPONDANT A UN FLUX THM (THH, THHM, THH, THH2,HHM,HM,HH)
C          SUR DES FACES D'ELEMENTS ISOPARAMETRIQUES 2D

C          OPTION : 'CHAR_MECA_FLUX_R'
C          OPTION : 'CHAR_MECA_FLUX_F'

C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ======================================================================
C NNO      NB DE NOEUDS DE L'ELEMENT DE BORD QUADRATIQUE
C NNO2     NB DE NOEUDS DE L'ELEMENT DE BORD LINEAIRE
C NNOS     NB DE NOEUDS EXTREMITE
C NDLNO    NB DE DDL DES NOEUDS EXTREMITE
C NDLNM    NB DE DDL DES NOEUDS MILIEUX
C NPG      NB DE POINTS DE GAUSS DE L'ELEMENT DE BORD
C ======================================================================
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
C ======================================================================
      LOGICAL     AXI,PERMAN,VF
      INTEGER     NNO,NNO2,NNOS,KP,NPG,NDIM,JGANO,JGANO2,NAPRE1,NAPRE2
      INTEGER     IPOIDS,IPOID2,IVF,IVF2,IDFDE,IDFDE2,IGEOM,NATEMP
      INTEGER     IPRES,K,KK,I,L,IRES,IFLUX,ITEMPS,IOPT,IPRESF,NDLNM
      INTEGER     IFLUXF,IRET,NDLNO
      REAL*8      POIDS,R,Z,TX,TY,NX,NY,VALPAR(3),DELTAT,TPLUS
      REAL*8      PRES,PRESF,POIDS2,NX2,NY2,FLU1,FLU2,FLUTH
      CHARACTER*8 NOMPAR(3),TYPMOD(2)
      INTEGER     TYPVF 
C ======================================================================
C --- CARACTERISTIQUES DE LA MODELISATION ------------------------------
C ======================================================================
      CALL BORTHM(NOMTE,AXI,VF,PERMAN,TYPVF,TYPMOD,NDIM,NDLNO,NDLNM)
C ======================================================================
C --- DEFINITION DE L'ELEMENT (NOEUDS, SOMMETS, POINTS DE GAUSS) -------
C ======================================================================
C --- LES DDLS THERMO-HYDRAULIQUES NE SONT PLUS EXPRIMES AUX NOEUDS ----
C --- MILIEUX ----------------------------------------------------------
C ======================================================================
C --- INTERPOLATION (QUADRATIQUE) POUR LA MECANIQUE --------------------
C ======================================================================
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
C ======================================================================
C --- INTERPOLATION (LINEAIRE) POUR LA THERMO-HYDRAULIQUE --------------
C ======================================================================
      CALL ELREF4('SE2','RIGI',NDIM,NNO2,NNOS,NPG,IPOID2,IVF2,IDFDE2,
     +               JGANO2)
C ======================================================================
C --- RECUPERATION DES CHAMPS IN ET DES CHAMPS OUT ---------------------
C ======================================================================
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PVECTUR','E',IRES)
C ======================================================================
C --- CAS DES FLUX -----------------------------------------------------
C ======================================================================
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
C ======================================================================
C --- CAS DES PRESSIONS MECANIQUES -------------------------------------
C ======================================================================
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
C ======================================================================
C --- CAS DU PERMANENT POUR LA PARTIE H OU T : LE SYSTEME A ETE --------
C --- CONSTRUIT EN SIMPLIFIANT PAR LE PAS DE TEMPS. ON DOIT DONC -------
C --- LE PRENDRE EGAL A 1 DANS LE CALCUL DU SECOND MEMBRE --------------
C ======================================================================
C ======================================================================
C --- BOUCLE SUR LES POINTS DE GAUSS DE L'ELEMENT DE BORD --------------
C ======================================================================
      DO 190 KP = 1,NPG
         K = (KP-1)*NNO
         KK = (KP-1)*NNO2
C ======================================================================
C --- RECUPERATION DES DERIVEES DES FONCTONS DE FORMES -----------------
C ======================================================================
        CALL VFF2DN(NDIM,NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),NX,NY,POIDS)
        CALL VFF2DN(NDIM,NNO2,KP,IPOID2,IDFDE2,ZR(IGEOM),NX2,NY2,POIDS2)
C ======================================================================
C --- MODIFICATION DU POIDS POUR LES CAS AXI ---------------------------
C ======================================================================
        IF (AXI) THEN
          R = 0.D0
          Z = 0.D0
          DO 10 I = 1,NNO
            L = (KP-1)*NNO + I
            R = R + ZR(IGEOM+2*I-2)*ZR(IVF+L-1)
   10     CONTINUE
          POIDS = POIDS*R
        END IF
C ======================================================================
C --- OPTION CHAR_MECA_FLUX_R OU CHAR_MECA_FLUX_F ----------------------
C ======================================================================
C --- DANS CES CAS LES INTERPOLATIONS SONT LINEAIRES -------------------
C --- CAR FLUX = GRANDEURS TH (ON UTILISE IVF2 ET NNO2) ----------------
C ======================================================================
C --- NAPRE1,NAPRE2,NATEMP SONT MIS EN PLACE ---------------------------
C --- POUR UNE EVENTUELLE MODIFICATION DE L'ORDRE DES DDL --------------
C --- PRE1, PRE2, TEMP DANS LES CATALOGUES D'ELEMENTS ------------------
C ======================================================================
C --- FLUTH REPRESENTE LE FLUX THERMIQUE -------------------------------
C --- FLU1 REPRESENTE LE FLUX ASSOCIE A PRE1 ---------------------------
C --- FLU2 REPRESENTE LE FLUX ASSOCIE A PRE2 ---------------------------
C ======================================================================
        IF (IOPT.EQ.1 .OR. IOPT.EQ.2) THEN
C ======================================================================
C --- SI MODELISATION = THHM, THH OU THH2 ------------------------------
C ======================================================================
           IF (NOMTE(1:4).EQ.'THHM' .OR.
     +         NOMTE(1:4).EQ.'THH_' .OR.
     +         NOMTE(1:4).EQ.'THH2'      ) THEN
              NAPRE1 = 0
              NAPRE2 = 1
              NATEMP = 2
              IF (IOPT.EQ.1) THEN
                 FLU1 = ZR(IFLUX+(KP-1)*3+NAPRE1)
                 FLU2 = ZR(IFLUX+(KP-1)*3+NAPRE2)
                 FLUTH = ZR(IFLUX+(KP-1)*3+NATEMP)
              ELSE IF (IOPT.EQ.2) THEN
                 R = 0.D0
                 Z = 0.D0
                 DO 20 I = 1,NNO2
                    L = (KP-1)*NNO2 + I
                    R = R + ZR(IGEOM+2*I-2)*ZR(IVF2+L-1)
                    Z = Z + ZR(IGEOM+2*I-1)*ZR(IVF2+L-1)
 20              CONTINUE
                 VALPAR(1) = R
                 VALPAR(2) = Z
                 CALL FOINTE('FM',ZK8(IFLUXF+NAPRE1),3,NOMPAR,VALPAR,
     +                                                        FLU1,IRET)
                 CALL FOINTE('FM',ZK8(IFLUXF+NAPRE2),3,NOMPAR,VALPAR,
     +                                                        FLU2,IRET)
                 CALL FOINTE('FM',ZK8(IFLUXF+NATEMP),3,NOMPAR,VALPAR,
     +                                                       FLUTH,IRET)
              END IF
C ======================================================================
C --- SI MODELISATION = THHM, OU THH2M ---------------------------------
C ======================================================================
              IF (NOMTE(1:4).EQ.'THHM' .OR.
     +            NOMTE(1:5).EQ.'THH2M'     ) THEN
                 DO 30 I = 1,NNO2
                    L = 5* (I-1) - 1
                    ZR(IRES+L+3) = ZR(IRES+L+3) -
     +                                 POIDS*DELTAT*FLU1*ZR(IVF2+KK+I-1)
                    ZR(IRES+L+4) = ZR(IRES+L+4) -
     +                                 POIDS*DELTAT*FLU2*ZR(IVF2+KK+I-1)
                    ZR(IRES+L+5) = ZR(IRES+L+5) -
     +                                POIDS*DELTAT*FLUTH*ZR(IVF2+KK+I-1)
 30              CONTINUE
              ELSE
                 DO 40 I = 1,NNO2
                    L = 3* (I-1) - 1
                    ZR(IRES+L+1) = ZR(IRES+L+1) -
     +                                 POIDS*DELTAT*FLU1*ZR(IVF2+KK+I-1)
                    ZR(IRES+L+2) = ZR(IRES+L+2) -
     +                                 POIDS*DELTAT*FLU2*ZR(IVF2+KK+I-1)
                    ZR(IRES+L+3) = ZR(IRES+L+3) -
     +                                POIDS*DELTAT*FLUTH*ZR(IVF2+KK+I-1)
 40              CONTINUE
              END IF
           END IF
C ======================================================================
C --- SI MODELISATION = HH, OU HH2 -------------------------------------
C ======================================================================
           IF ((NOMTE(1:3).EQ.'HH_' ).OR. 
     >         (NOMTE(1:4).EQ.'HH2_').OR.
     >         (NOMTE(1:9).EQ.'DHH2S3_SU').OR.
     >         (NOMTE(1:9).EQ.'DHH2S3_VF')) THEN
              NAPRE1 = 0
              NAPRE2 = 1
              IF (IOPT.EQ.1) THEN
                 FLU1 = ZR(IFLUX+(KP-1)*2+NAPRE1)
                 FLU2 = ZR(IFLUX+(KP-1)*2+NAPRE2)
              ELSE IF (IOPT.EQ.2) THEN
                 R = 0.D0
                 Z = 0.D0
                 DO 201 I = 1,NNO2
                    L = (KP-1)*NNO2 + I
                    R = R + ZR(IGEOM+2*I-2)*ZR(IVF2+L-1)
                    Z = Z + ZR(IGEOM+2*I-1)*ZR(IVF2+L-1)
 201             CONTINUE
                 VALPAR(1) = R
                 VALPAR(2) = Z
                 CALL FOINTE('FM',ZK8(IFLUXF+NAPRE1),2,NOMPAR,VALPAR,
     +                                                        FLU1,IRET)
                 CALL FOINTE('FM',ZK8(IFLUXF+NAPRE2),2,NOMPAR,VALPAR,
     +                                                        FLU2,IRET)
              END IF

              IF(
     >         (NOMTE(1:9).EQ.'DHH2S3_SU').OR.
     >         (NOMTE(1:9).EQ.'DHH2S3_VF') ) THEN
C ======================================================================
C --- SI TE = DHH2S3_SU ------------------------------------------------
C ======================================================================
                DO 401 I = 1,NNO2
                    ZR(IRES) = ZR(IRES) -
     +                                 POIDS*FLU1*ZR(IVF2+KK+I-1)
                    ZR(IRES+1) = ZR(IRES+1) -
     +                                 POIDS*FLU2*ZR(IVF2+KK+I-1)
 401            CONTINUE
              ELSE
C ======================================================================
C --- SI MODELISATION = HH*, OU HH2*------------------------------------
C ======================================================================
                DO 402 I = 1,NNO2
                    L = 2* (I-1) - 1
                    ZR(IRES+L+1) = ZR(IRES+L+1) -
     +                                 POIDS*DELTAT*FLU1*ZR(IVF2+KK+I-1)
                    ZR(IRES+L+2) = ZR(IRES+L+2) -
     +                                 POIDS*DELTAT*FLU2*ZR(IVF2+KK+I-1)
 402            CONTINUE
              END IF
           END IF  
C ======================================================================
C --- SI MODELISATION = THV --------------------------------------------
C ======================================================================
           IF (NOMTE(1:4).EQ.'THV_') THEN
              NAPRE1 = 0
              NATEMP = 1
              IF (IOPT.EQ.1) THEN
                 FLU1 = ZR(IFLUX+ (KP-1)*2+NAPRE1)
                 FLUTH = ZR(IFLUX+ (KP-1)*2+NATEMP)
              ELSE IF (IOPT.EQ.2) THEN
                 R = 0.D0
                 Z = 0.D0
                 DO 50 I = 1,NNO2
                    L = (KP-1)*NNO2 + I
                    R = R + ZR(IGEOM+2*I-2)*ZR(IVF2+L-1)
                    Z = Z + ZR(IGEOM+2*I-1)*ZR(IVF2+L-1)
 50              CONTINUE
                 VALPAR(1) = R
                 VALPAR(2) = Z
                 CALL FOINTE('FM',ZK8(IFLUXF+NAPRE1),3,NOMPAR,VALPAR,
     +                                                        FLU1,IRET)
                 CALL FOINTE('FM',ZK8(IFLUXF+NAPRE2),3,NOMPAR,VALPAR,
     +                                                        FLU2,IRET)
                 CALL FOINTE('FM',ZK8(IFLUXF+NATEMP),3,NOMPAR,VALPAR,
     +                                                       FLUTH,IRET)
              END IF
              DO 60 I = 1,NNO2
                 L = 2* (I-1) - 1
                 ZR(IRES+L+1) = ZR(IRES+L+1) -
     +                                 POIDS*DELTAT*FLU1*ZR(IVF2+KK+I-1)
                 ZR(IRES+L+2) = ZR(IRES+L+2) -
     +                                POIDS*DELTAT*FLUTH*ZR(IVF2+KK+I-1)
 60           CONTINUE
          END IF
C ======================================================================
C --- SI MODELISATION = HM ---------------------------------------------
C ======================================================================
          IF (NOMTE(1:2).EQ.'HM') THEN
             NAPRE1 = 0
             IF (IOPT.EQ.1) THEN
                FLU1 = ZR(IFLUX+ (KP-1)+NAPRE1)
             ELSE IF (IOPT.EQ.2) THEN
                R = 0.D0
                Z = 0.D0
                DO 70 I = 1,NNO2
                   L = (KP-1)*NNO2 + I
                   R = R + ZR(IGEOM+2*I-2)*ZR(IVF2+L-1)
                   Z = Z + ZR(IGEOM+2*I-1)*ZR(IVF2+L-1)
 70             CONTINUE
                VALPAR(1) = R
                VALPAR(2) = Z
                CALL FOINTE('FM',ZK8(IFLUXF+NAPRE1),3,NOMPAR,VALPAR,
     +                                                        FLU1,IRET)
             END IF
             DO 80 I = 1,NNO2
                L = 3* (I-1) - 1
             IF ( .NOT.PERMAN ) THEN
                ZR(IRES+L+3) = ZR(IRES+L+3) -
     +                                POIDS2*DELTAT*FLU1*ZR(IVF2+KK+I-1)
             ELSE
                ZR(IRES+L+3) = ZR(IRES+L+3) -
     +                                POIDS2*FLU1*ZR(IVF2+KK+I-1)
             END IF
 80          CONTINUE
          END IF
C ======================================================================
C --- SI MODELISATION = HHM OU HH2M ------------------------------------
C ======================================================================
          IF ((NOMTE(1:3).EQ.'HHM') .OR. (NOMTE(1:4).EQ.'HH2M')) THEN
             NAPRE1 = 0
             NAPRE2 = 1
             IF (IOPT.EQ.1) THEN
                FLU1 = ZR(IFLUX+ (KP-1)*2+NAPRE1)
                FLU2 = ZR(IFLUX+ (KP-1)*2+NAPRE2)
             ELSE IF (IOPT.EQ.2) THEN
                R = 0.D0
                Z = 0.D0
                DO 90 I = 1,NNO2
                   L = (KP-1)*NNO2 + I
                   R = R + ZR(IGEOM+2*I-2)*ZR(IVF2+L-1)
                   Z = Z + ZR(IGEOM+2*I-1)*ZR(IVF2+L-1)
 90             CONTINUE
                VALPAR(1) = R
                VALPAR(2) = Z
                CALL FOINTE('FM',ZK8(IFLUXF+NAPRE1),3,NOMPAR,VALPAR,
     +                                                        FLU1,IRET)
                CALL FOINTE('FM',ZK8(IFLUXF+NAPRE2),3,NOMPAR,VALPAR,
     +                                                        FLU2,IRET)
             END IF
             DO 100 I = 1,NNO2
                L = 4* (I-1) - 1
                ZR(IRES+L+3) = ZR(IRES+L+3) -
     +                                 POIDS*DELTAT*FLU1*ZR(IVF2+KK+I-1)
                ZR(IRES+L+4) = ZR(IRES+L+4) -
     +                                 POIDS*DELTAT*FLU2*ZR(IVF2+KK+I-1)
 100         CONTINUE
          END IF
C ======================================================================
C --- SI MODELISATION = THM --------------------------------------------
C ======================================================================
          IF (NOMTE(1:3).EQ.'THM') THEN
             NAPRE1 = 0
             NATEMP = 1
             IF (IOPT.EQ.1) THEN
                FLU1 = ZR(IFLUX+ (KP-1)*2+NAPRE1)
                FLUTH = ZR(IFLUX+ (KP-1)*2+NATEMP)
             ELSE IF (IOPT.EQ.2) THEN
                R = 0.D0
                Z = 0.D0
                DO 110 I = 1,NNO2
                   L = (KP-1)*NNO2 + I
                   R = R + ZR(IGEOM+2*I-2)*ZR(IVF2+L-1)
                   Z = Z + ZR(IGEOM+2*I-1)*ZR(IVF2+L-1)
 110            CONTINUE
                VALPAR(1) = R
                VALPAR(2) = Z
                CALL FOINTE('FM',ZK8(IFLUXF+NAPRE1),3,NOMPAR,VALPAR,
     +                                                        FLU1,IRET)
                CALL FOINTE('FM',ZK8(IFLUXF+NATEMP),3,NOMPAR,VALPAR,
     +                                                       FLUTH,IRET)
             END IF
             DO 120 I = 1,NNO2
                L = 4* (I-1) - 1
                ZR(IRES+L+3) = ZR(IRES+L+3) -
     +                                 POIDS*DELTAT*FLU1*ZR(IVF2+KK+I-1)
                ZR(IRES+L+4) = ZR(IRES+L+4) -
     +                                POIDS*DELTAT*FLUTH*ZR(IVF2+KK+I-1)
 120         CONTINUE
          END IF
C ======================================================================
C --- OPTION CHAR_MECA_PRES_R OU CHAR_MECA_PRES_F ----------------------
C ======================================================================
C --- ICI, LES INTERPOLATIONS SONT QUADRATIQUES ------------------------
C ======================================================================
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
          DO 180 I = 1,NNOS
            L = NDLNO* (I-1) - 1
            ZR(IRES+L+1) = ZR(IRES+L+1) + TX*ZR(IVF+K+I-1)*POIDS
            ZR(IRES+L+2) = ZR(IRES+L+2) + TY*ZR(IVF+K+I-1)*POIDS   
  180     CONTINUE
          DO 181 I = 1,(NNO - NNOS)
            L = NDLNO*NNOS+NDLNM* (I-1) -1
            ZR(IRES+L+1) = ZR(IRES+L+1) + TX*ZR(IVF+K+I+NNOS-1)*POIDS
            ZR(IRES+L+2) = ZR(IRES+L+2) + TY*ZR(IVF+K+I+NNOS-1)*POIDS
  181     CONTINUE
        END IF
  190 CONTINUE
C ======================================================================
      END
