      SUBROUTINE TE0466 ( OPTION , NOMTE )
      IMPLICIT   NONE
      CHARACTER*16        OPTION , NOMTE
C =====================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C =====================================================================
C MODIF ELEMENTS  DATE 21/05/2007   AUTEUR FERNANDES R.FERNANDES 
C RESPONSABLE UFBHHLL C.CHAVANT
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C TOLE CRP_20
C
C     BUT: CALCUL DES VECTEURS ELEMENTA EN MECANIQUE
C          CORRESPONDANT A UN CHARGEMENT EN FLUX NORMAUX HYDRAULIQUES
C          ET THERMIQUES SUR DES FACES D'ELEMENTS ISOPARAMETRIQUES
C          3D_THHM, 3D_THM,3D_THH,3D_HHM,3D_HM, 3D_HH
C          ACTUELLEMENT TRAITES : FACE8 ET FACE6
C          OPTIONS : 'CHAR_MECA_FLUX_R' ET 'CHAR_MECA_FLUX_F'
C
C     ENTREES  ---> OPTION : OPTION DE CALCUL
C              ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER            IPOIDS,IVF,IDFDX,IDFDY,IGEOM,I,J,L,IFLUXF
      INTEGER            NDIM,NNO,IPG,NPI,IRES,IFLUX,ITEMPS,JGANO
      INTEGER            IDEC,JDEC,KDEC,LDEC,LDEC2,JVAL,INO,JNO,IBID
      INTEGER            NBPG(10),IOPT,IRET,IFORC,NBFPG,JIN
      INTEGER            NNO2,NNOS2,IPOID2,IDFDX2,IVF2
      REAL*8             NX,NY,NZ,SX(9,9),SY(9,9),SZ(9,9),JAC,VALPAR(4)
      REAL*8             DELTAT,FLU1,FLU2,FLUTH,X,Y,Z,FX,FY,FZ
      INTEGER            NAPRE1,NAPRE2,NATEMP,NDIM2
      CHARACTER*8        ALIAS,NOMPAR(4),ELREFE
      CHARACTER*24       CHVAL,CHCTE,ELREF2
      INTEGER            NDLNO,NDLNM,IPRES,IPRESF
      REAL*8             PRES,PRESF
C
      INTEGER NNOS,NPI2
C     ------------------------------------------------------------------
C  CETTE ROUTINE FAIT UN CALCUL EN THHM , HM , HHM , THH ,THM,
C                           THH2M, HH2M,  THH2, HH, HH2
C     ------------------------------------------------------------------
C ======================================================================
C --- INITIALISATIONS --------------------------------------------------
C ======================================================================
         NDIM2 = 3
         CALL ELREF1(ELREFE)
         IF ( ELREFE.EQ.'TR6') THEN
            ELREF2 = 'TR3'
         ELSEIF ( ELREFE.EQ.'QU8') THEN
            ELREF2 = 'QU4'
         ELSE
           CALL U2MESK('F','ALGORITH4_46',1,ELREFE)
         ENDIF

C FONCTIONS DE FORMES QUADRATIQUES
      CALL ELREF4(ELREFE,'RIGI',NDIM,NNO,NNOS,NPI,
     &                          IPOIDS,IVF,IDFDX,JGANO)
C FONCTIONS DE FORMES LINEAIRES
      CALL ELREF4(ELREF2,'RIGI',NDIM,NNO2,NNOS2,NPI2,
     &                          IPOID2,IVF2,IDFDX2,IBID)
C
C NB DE DDL A CHAQUE NOEUD
      CALL DIMTHM(NOMTE,NDLNO,NDLNM,NDIM2)

      CALL JEVECH ( 'PGEOMER', 'L', IGEOM )
      CALL JEVECH ( 'PVECTUR', 'E', IRES  )
C
      IF (OPTION.EQ.'CHAR_MECA_FLUX_R') THEN
         IOPT = 1
         CALL JEVECH ( 'PFLUXR' , 'L', IFLUX  )
         CALL JEVECH ( 'PTEMPSR', 'L', ITEMPS )
         DELTAT = ZR(ITEMPS+1)
C
      ELSE IF (OPTION.EQ.'CHAR_MECA_FLUX_F') THEN
         IOPT = 2
         CALL JEVECH ( 'PFLUXF' , 'L', IFLUXF )
         CALL JEVECH ( 'PTEMPSR', 'L', ITEMPS )
         DELTAT = ZR(ITEMPS+1)
         NOMPAR(1) = 'X'
         NOMPAR(2) = 'Y'
         NOMPAR(3) = 'Z'
         NOMPAR(4) = 'INST'
         VALPAR(4) = ZR(ITEMPS)
      ELSE IF (OPTION.EQ.'CHAR_MECA_PRES_R') THEN
         IOPT = 3
         CALL JEVECH ( 'PPRESSR', 'L', IPRES )
C
      ELSE IF (OPTION.EQ.'CHAR_MECA_PRES_F') THEN
         IOPT = 4
         CALL JEVECH ( 'PPRESSF', 'L', IPRESF )
         CALL JEVECH ( 'PTEMPSR', 'L', ITEMPS )
         NOMPAR(1) = 'X'
         NOMPAR(2) = 'Y'
         NOMPAR(3) = 'Z'
         NOMPAR(4) = 'INST'
         VALPAR(4) = ZR(ITEMPS)
      ELSE IF (OPTION.EQ.'CHAR_MECA_FR2D3D') THEN
         IOPT = 5
         CALL JEVECH ( 'PFR2D3D', 'L', IFORC )
      END IF
C
      IDFDX  = IVF    + NPI * NNO
      IDFDY  = IDFDX  + 1
C
C --- CALCUL DES PRODUITS VECTORIELS OMI X OMJ ---
C
      DO 20 INO = 1,NNO
         I = IGEOM + 3*(INO-1) -1
         DO 22 JNO = 1,NNO
            J = IGEOM + 3*(JNO-1) -1
            SX(INO,JNO) = ZR(I+2) * ZR(J+3) - ZR(I+3) * ZR(J+2)
            SY(INO,JNO) = ZR(I+3) * ZR(J+1) - ZR(I+1) * ZR(J+3)
            SZ(INO,JNO) = ZR(I+1) * ZR(J+2) - ZR(I+2) * ZR(J+1)
 22      CONTINUE
 20   CONTINUE

C ======================================================================
C     --- BOUCLE SUR LES POINTS DE GAUSS ---
C ======================================================================
      DO 100 IPG = 1 , NPI
         KDEC = (IPG-1)*NNO*NDIM
         LDEC = (IPG-1)*NNO
         LDEC2 = (IPG-1)*NNO2
C
         NX = 0.0D0
         NY = 0.0D0
         NZ = 0.0D0
C
C --- CALCUL DE LA NORMALE AU POINT DE GAUSS IPG ---
C
         DO 102 I=1,NNO
           IDEC = (I-1)*NDIM
           DO 102 J=1,NNO
             JDEC = (J-1)*NDIM
             NX = NX + ZR(IDFDX+KDEC+IDEC)*ZR(IDFDY+KDEC+JDEC)*SX(I,J)
             NY = NY + ZR(IDFDX+KDEC+IDEC)*ZR(IDFDY+KDEC+JDEC)*SY(I,J)
             NZ = NZ + ZR(IDFDX+KDEC+IDEC)*ZR(IDFDY+KDEC+JDEC)*SZ(I,J)
 102       CONTINUE
C
           JAC = SQRT(NX*NX+NY*NY+NZ*NZ)

C ======================================================================
C OPTIONS CHAR_MECA_FLUX_R ET CHAR_MECA_FLUX_F (INTERPOLATION LINEAIRE)
C ======================================================================

         IF(IOPT.EQ.1.OR.IOPT.EQ.2) THEN
C
C ======================================================================
C --- SI MODELISATION = THHM OU THH
C
           IF (NOMTE(1:4).EQ.'THHM'.OR.NOMTE(1:4).EQ.'THH_'
     &      .OR.NOMTE(1:4).EQ.'THH2'.OR.NOMTE(1:4).EQ.'HH2M')THEN
C
C --- NAPRE1,NAPRE2,NATEMP SONT MIS EN PLACE
C --- POUR UNE EVENTUELLE MODIFICATION DE L'ORDRE DES DDL :
C     PRE1, PRE2, TEMP DANS LES CATALOGUES D'ELEMENTS
C
             NAPRE1=0
             NAPRE2=1
             NATEMP=2
C
             IF (IOPT.EQ.1) THEN
C
C ---   FLU1 REPRESENTE LE FLUX ASSOCIE A PRE1
C ---   FLU2 REPRESENTE LE FLUX ASSOCIE A PRE2
C ---   ET FLUTH LE FLUX THERMIQUE
C
               FLU1  = ZR((IFLUX)+(IPG-1)*3+NAPRE1 )
               FLU2  = ZR((IFLUX)+(IPG-1)*3+NAPRE2 )
               FLUTH = ZR((IFLUX)+(IPG-1)*3+NATEMP  )
C
             ELSE IF (IOPT.EQ.2) THEN
               X = 0.D0
               Y = 0.D0
               Z = 0.D0
               DO 104 I=1,NNO2
                 X = X + ZR(IGEOM+3*I-3) * ZR(IVF2+LDEC+I-1)
                 Y = Y + ZR(IGEOM+3*I-2) * ZR(IVF2+LDEC+I-1)
                 Z = Z + ZR(IGEOM+3*I-1) * ZR(IVF2+LDEC+I-1)
 104           CONTINUE
               VALPAR(1) = X
               VALPAR(2) = Y
               VALPAR(3) = Z
C
               CALL FOINTE('FM',ZK8(IFLUXF+NAPRE1),4,NOMPAR,VALPAR,
     &                     FLU1,IRET)
               CALL FOINTE('FM',ZK8(IFLUXF+NAPRE2),4,NOMPAR,VALPAR,
     &                     FLU2,IRET)
               CALL FOINTE('FM',ZK8(IFLUXF+NATEMP),4,NOMPAR,VALPAR,
     &                     FLUTH,IRET)
C
             ENDIF
C
             IF (NOMTE(1:4).EQ.'THHM'.OR.NOMTE(1:5).EQ.'THH2M')THEN
               DO 120 I=1,NNO2
                 L = 6 * (I-1) -1
C
                 ZR(IRES+L+4) = ZR(IRES+L+4) - ZR(IPOIDS+IPG-1) *
     &                   DELTAT * FLU1 * ZR(IVF2+LDEC2+I-1) * JAC
                 ZR(IRES+L+5) = ZR(IRES+L+5) - ZR(IPOIDS+IPG-1) *
     &                   DELTAT * FLU2 * ZR(IVF2+LDEC2+I-1) * JAC
                 ZR(IRES+L+6) = ZR(IRES+L+6) - ZR(IPOIDS+IPG-1) *
     &                  DELTAT * FLUTH * ZR(IVF2+LDEC2+I-1) * JAC
 120           CONTINUE

             ELSE
               DO 127 I=1,NNO2
                 L = 3 * (I-1) -1
C
                 ZR(IRES+L+1) = ZR(IRES+L+1) - ZR(IPOIDS+IPG-1) *
     &                DELTAT * FLU1 * ZR(IVF2+LDEC2+I-1) * JAC
                 ZR(IRES+L+2) = ZR(IRES+L+2) - ZR(IPOIDS+IPG-1) *
     &                DELTAT * FLU2 * ZR(IVF2+LDEC2+I-1) * JAC
                 ZR(IRES+L+3) = ZR(IRES+L+3) - ZR(IPOIDS+IPG-1) *
     &                DELTAT * FLUTH * ZR(IVF2+LDEC2+I-1) * JAC
 127           CONTINUE
             ENDIF
C
           ENDIF
CFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

           IF (NOMTE(1:3).EQ.'HH2'.OR.NOMTE(1:3).EQ.'HH_')THEN
C
C --- NAPRE1,NAPRE2,NATEMP SONT MIS EN PLACE
C --- POUR UNE EVENTUELLE MODIFICATION DE L'ORDRE DES DDL :
C     PRE1, PRE2, TEMP DANS LES CATALOGUES D'ELEMENTS
C
             NAPRE1=0
             NAPRE2=1
C
             IF (IOPT.EQ.1) THEN
C
C ---   FLU1 REPRESENTE LE FLUX ASSOCIE A PRE1
C ---   FLU2 REPRESENTE LE FLUX ASSOCIE A PRE2
C
               FLU1  = ZR((IFLUX)+(IPG-1)*2+NAPRE1 )
               FLU2  = ZR((IFLUX)+(IPG-1)*2+NAPRE2 )
C
             ELSE IF (IOPT.EQ.2) THEN
               X = 0.D0
               Y = 0.D0
               Z = 0.D0
               DO 116 I=1,NNO2
                 X = X + ZR(IGEOM+3*I-3) * ZR(IVF2+LDEC+I-1)
                 Y = Y + ZR(IGEOM+3*I-2) * ZR(IVF2+LDEC+I-1)
                 Z = Z + ZR(IGEOM+3*I-1) * ZR(IVF2+LDEC+I-1)
 116           CONTINUE
               VALPAR(1) = X
               VALPAR(2) = Y
               VALPAR(3) = Z
C
               CALL FOINTE('FM',ZK8(IFLUXF+NAPRE1),4,NOMPAR,VALPAR,
     &                     FLU1,IRET)
               CALL FOINTE('FM',ZK8(IFLUXF+NAPRE2),4,NOMPAR,VALPAR,
     &                     FLU2,IRET)
C
             ENDIF
C
               DO 128 I=1,NNO2
                 L = 2 * (I-1) -1
C
                 ZR(IRES+L+1) = ZR(IRES+L+1) - ZR(IPOIDS+IPG-1) *
     &                DELTAT * FLU1 * ZR(IVF2+LDEC2+I-1) * JAC
                 ZR(IRES+L+2) = ZR(IRES+L+2) - ZR(IPOIDS+IPG-1) *
     &                DELTAT * FLU2 * ZR(IVF2+LDEC2+I-1) * JAC
 128           CONTINUE
C
         ENDIF


CFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF



C
C ======================================================================
C --- SI MODELISATION = THV
C
           IF (NOMTE(1:4).EQ.'THV_')THEN
C
C --- NAPRE1,NATEMP SONT MIS EN PLACE
C --- POUR UNE EVENTUELLE MODIFICATION DE L'ORDRE DES DDL :
C     PRE1, PRE2, TEMP DANS LES CATALOGUES D'ELEMENTS
C
             NAPRE1=0
             NATEMP=1
C
             IF (IOPT.EQ.1) THEN
C
C ---   FLU1 REPRESENTE LE FLUX ASSOCIE A PRE1
C ---   FLU2 REPRESENTE LE FLUX ASSOCIE A PRE2
C ---   ET FLUTH LE FLUX THERMIQUE
C
               FLU1  = ZR((IFLUX)+(IPG-1)*2+NAPRE1 )
               FLUTH = ZR((IFLUX)+(IPG-1)*2+NATEMP  )
C
             ELSE IF (IOPT.EQ.2) THEN
               X = 0.D0
               Y = 0.D0
               Z = 0.D0
               DO 204 I=1,NNO2
                X = X + ZR(IGEOM+3*I-3) * ZR(IVF2+LDEC2+I-1)
                Y = Y + ZR(IGEOM+3*I-2) * ZR(IVF2+LDEC2+I-1)
                Z = Z + ZR(IGEOM+3*I-1) * ZR(IVF2+LDEC2+I-1)
 204           CONTINUE
               VALPAR(1) = X
               VALPAR(2) = Y
               VALPAR(3) = Z
C
               CALL FOINTE('FM',ZK8(IFLUXF+NAPRE1),4,NOMPAR,VALPAR,
     &                     FLU1,IRET)
               CALL FOINTE('FM',ZK8(IFLUXF+NAPRE2),4,NOMPAR,VALPAR,
     &                     FLU2,IRET)
               CALL FOINTE('FM',ZK8(IFLUXF+NATEMP),4,NOMPAR,VALPAR,
     &                     FLUTH,IRET)
C
             ENDIF
C
               DO 205 I=1,NNO2
                 L = 2 * (I-1) -1
C
                 ZR(IRES+L+1) = ZR(IRES+L+1) - ZR(IPOIDS+IPG-1) *
     &                  DELTAT * FLU1 * ZR(IVF2+LDEC2+I-1) * JAC
                 ZR(IRES+L+2) = ZR(IRES+L+2) - ZR(IPOIDS+IPG-1) *
     &                  DELTAT * FLUTH * ZR(IVF2+LDEC2+I-1) * JAC
 205           CONTINUE
           ENDIF
C
C ======================================================================
C SI MODELISATION = HM
C
           IF (NOMTE(1:2).EQ.'HM') THEN
C
             NAPRE1=0
C
             IF (IOPT.EQ.1) THEN
C
C ---   FLU1 REPRESENTE LE FLUX ASSOCIE A PRE1
C
               FLU1  = ZR((IFLUX)+(IPG-1)+NAPRE1 )
C
             ELSE IF (IOPT.EQ.2) THEN
               X = 0.D0
               Y = 0.D0
               Z = 0.D0
               DO 106 I=1,NNO2
               X = X + ZR(IGEOM+3*I-3) * ZR(IVF2+LDEC2+I-1)
               Y = Y + ZR(IGEOM+3*I-2) * ZR(IVF2+LDEC2+I-1)
               Z = Z + ZR(IGEOM+3*I-1) * ZR(IVF2+LDEC2+I-1)
 106           CONTINUE
               VALPAR(1) = X
               VALPAR(2) = Y
               VALPAR(3) = Z
C
               CALL FOINTE('FM',ZK8(IFLUXF+NAPRE1),4,
     &                     NOMPAR,VALPAR,FLU1,IRET)
C
             ENDIF
             DO 122 I=1,NNO2
               L = 4 * (I-1) -1
               ZR(IRES+L+4) = ZR(IRES+L+4) - ZR(IPOIDS+IPG-1) *
     &                DELTAT * FLU1 * ZR(IVF2+LDEC2+I-1) * JAC

 122         CONTINUE
C
           ENDIF
C ======================================================================
C SI MODELISATION = HHM
C
           IF (NOMTE(1:3).EQ.'HHM'.OR.NOMTE(1:4).EQ.'HH2M') THEN
C
             NAPRE1=0
             NAPRE2=1
C
             IF (IOPT.EQ.1) THEN
C
C ---    FLU1 REPRESENTE LE FLUX ASSOCIE A PRE1
C ---    FLU2 REPRESENTE LE FLUX ASSOCIE A PRE2
C
               FLU1  = ZR((IFLUX)+(IPG-1)*2+NAPRE1 )
               FLU2  = ZR((IFLUX)+(IPG-1)*2+NAPRE2 )
C
              ELSE IF (IOPT.EQ.2) THEN
                X = 0.D0
                Y = 0.D0
                Z = 0.D0
                DO 105 I=1,NNO2
                X = X + ZR(IGEOM+3*I-3) * ZR(IVF2+LDEC2+I-1)
                Y = Y + ZR(IGEOM+3*I-2) * ZR(IVF2+LDEC2+I-1)
                Z = Z + ZR(IGEOM+3*I-1) * ZR(IVF2+LDEC2+I-1)
 105            CONTINUE
                VALPAR(1) = X
                VALPAR(2) = Y
                VALPAR(3) = Z
C
               CALL FOINTE('FM',ZK8(IFLUXF+NAPRE1),4,
     &                      NOMPAR,VALPAR,FLU1,IRET)
               CALL FOINTE('FM',ZK8(IFLUXF+NAPRE2),4,
     &                     NOMPAR,VALPAR,FLU2,IRET)
C
             ENDIF
C
             DO 121 I=1,NNO2
               L = 5 * (I-1) -1
C
               ZR(IRES+L+4) = ZR(IRES+L+4) - ZR(IPOIDS+IPG-1) *
     &               DELTAT * FLU1 * ZR(IVF2+LDEC2+I-1) * JAC
               ZR(IRES+L+5) = ZR(IRES+L+5) - ZR(IPOIDS+IPG-1) *
     &               DELTAT * FLU2 * ZR(IVF2+LDEC2+I-1) * JAC
 121         CONTINUE
C
           ENDIF
C ======================================================================
C SI MODELISATION = THM
C
           IF (NOMTE(1:3).EQ.'THM') THEN
C
             NAPRE1=0
             NATEMP=1
C
             IF (IOPT.EQ.1) THEN
C
C ---    FLU1 REPRESENTE LE FLUX ASSOCIE A PRE1
C ---    ET FLUTH LA THERMIQUE
C
               FLU1  = ZR((IFLUX)+(IPG-1)*2+NAPRE1 )
               FLUTH = ZR((IFLUX)+(IPG-1)*2+NATEMP )
C
              ELSE IF (IOPT.EQ.2) THEN
                X = 0.D0
                Y = 0.D0
                Z = 0.D0
                DO 115 I=1,NNO2
                 X = X + ZR(IGEOM+3*I-3) * ZR(IVF2+LDEC2+I-1)
                 Y = Y + ZR(IGEOM+3*I-2) * ZR(IVF2+LDEC2+I-1)
                 Z = Z + ZR(IGEOM+3*I-1) * ZR(IVF2+LDEC2+I-1)
 115            CONTINUE
                VALPAR(1) = X
                VALPAR(2) = Y
                VALPAR(3) = Z
C
               CALL FOINTE('FM',ZK8(IFLUXF+NAPRE1),4,
     &                     NOMPAR,VALPAR,FLU1,IRET)
               CALL FOINTE('FM',ZK8(IFLUXF+NATEMP),4,
     &                     NOMPAR,VALPAR,FLUTH,IRET)
C
             ENDIF
C
             DO 131 I=1,NNO2
               L = 5 * (I-1) -1
C
               ZR(IRES+L+4) = ZR(IRES+L+4) - ZR(IPOIDS+IPG-1) *
     &                DELTAT * FLU1 * ZR(IVF2+LDEC2+I-1) * JAC
               ZR(IRES+L+5) = ZR(IRES+L+5) - ZR(IPOIDS+IPG-1) *
     &               DELTAT * FLUTH * ZR(IVF2+LDEC2+I-1) * JAC
 131         CONTINUE
               ENDIF
C
C ======================================================================
C        --- OPTION CHAR_MECA_PRES_R OU CHAR_MECA_PRES_F ---
C INTERPOLATION QUADRATIQUE
C ======================================================================
          ELSE IF ( (IOPT.EQ.3) .OR. (IOPT.EQ.4) ) THEN
            IF (IOPT.EQ.3) THEN
               PRES = 0.D0
               DO 107 I = 1 , NNO
                  PRES = PRES + ZR(IPRES+I-1)*ZR(IVF+LDEC+I-1)
 107           CONTINUE
            ELSE IF (IOPT.EQ.4) THEN
               PRES = 0.D0
               DO 108 I = 1 , NNO
                  VALPAR(1) = ZR(IGEOM+3*I-3)
                  VALPAR(2) = ZR(IGEOM+3*I-2)
                  VALPAR(3) = ZR(IGEOM+3*I-1)
             CALL FOINTE('FM',ZK8(IPRESF),4,NOMPAR,VALPAR,PRESF,IRET)
                  PRES = PRES + PRESF*ZR(IVF+LDEC+I-1)
 108           CONTINUE
            ENDIF
C
            DO 110 I = 1 , NNOS
               L = NDLNO * (I-1) -1
               ZR(IRES+L+1) = ZR(IRES+L+1) - ZR(IPOIDS+IPG-1) *
     &                                   PRES * ZR(IVF+LDEC+I-1) * NX
               ZR(IRES+L+2) = ZR(IRES+L+2) - ZR(IPOIDS+IPG-1) *
     &                                   PRES * ZR(IVF+LDEC+I-1) * NY
               ZR(IRES+L+3) = ZR(IRES+L+3) - ZR(IPOIDS+IPG-1) *
     &                                   PRES * ZR(IVF+LDEC+I-1) * NZ
 110        CONTINUE
            DO 111 I = 1 ,(NNO-NNOS)
               L = NDLNO*NNOS + NDLNM*(I-1) -1
               ZR(IRES+L+1) = ZR(IRES+L+1) - ZR(IPOIDS+IPG-1) *
     &                              PRES * ZR(IVF+LDEC+I+NNOS-1) * NX
               ZR(IRES+L+2) = ZR(IRES+L+2) - ZR(IPOIDS+IPG-1) *
     &                              PRES * ZR(IVF+LDEC+I+NNOS-1) * NY
               ZR(IRES+L+3) = ZR(IRES+L+3) - ZR(IPOIDS+IPG-1) *
     &                              PRES * ZR(IVF+LDEC+I+NNOS-1) * NZ
 111        CONTINUE

C ======================================================================
C        --- OPTION CHAR_MECA_FR2D3D : FORCE_FACE ---
C ======================================================================

         ELSEIF (IOPT.EQ.5) THEN
           FX = 0.0D0
           FY = 0.0D0
           FZ = 0.0D0
           DO 124 I = 1 , NNO
              FX = FX + ZR(IFORC-1+3*(I-1)+1)*ZR(IVF+LDEC+I-1)
              FY = FY + ZR(IFORC-1+3*(I-1)+2)*ZR(IVF+LDEC+I-1)
              FZ = FZ + ZR(IFORC-1+3*(I-1)+3)*ZR(IVF+LDEC+I-1)
 124       CONTINUE
           DO 123 I = 1 , NNO
              L = NDLNO * (I-1) -1
              ZR(IRES+L+1) = ZR(IRES+L+1) +
     &                     ZR(IPOIDS+IPG-1)*FX*ZR(IVF+LDEC+I-1)*JAC
              ZR(IRES+L+2) = ZR(IRES+L+2) +
     &                     ZR(IPOIDS+IPG-1)*FY*ZR(IVF+LDEC+I-1)*JAC
              ZR(IRES+L+3) = ZR(IRES+L+3) +
     &                     ZR(IPOIDS+IPG-1)*FZ*ZR(IVF+LDEC+I-1)*JAC
 123       CONTINUE
            DO 125 I = 1 , NNO
              L = NDLNO*NNOS+NDLNM * (I-1) -1
              ZR(IRES+L+1) = ZR(IRES+L+1) +
     &                ZR(IPOIDS+IPG-1)*FX*ZR(IVF+LDEC+I+NNOS-1)*JAC
              ZR(IRES+L+2) = ZR(IRES+L+2) +
     &                ZR(IPOIDS+IPG-1)*FY*ZR(IVF+LDEC+I+NNOS-1)*JAC
              ZR(IRES+L+3) = ZR(IRES+L+3) +
     &                ZR(IPOIDS+IPG-1)*FZ*ZR(IVF+LDEC+I+NNOS-1)*JAC
 125       CONTINUE
         ENDIF
C
 100  CONTINUE
C-----------------------------------------------------------------------
      END
