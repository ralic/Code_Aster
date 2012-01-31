      SUBROUTINE TE0498 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 31/01/2012   AUTEUR IDOUX L.IDOUX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT   NONE
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C
C     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN MECANIQUE
C          CORRESPONDANT A UN CHARGEMENT PAR ONDE PLANE
C          SUR DES FACES D'ELEMENTS ISOPARAMETRIQUES 3D
C
C          OPTION : 'IMPE_ABSO'
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
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
      INTEGER            IPOIDS,IVF,IDFDX,IDFDY,IGEOM,I,J
      INTEGER            NDIM,NNO,IPG,NPG1,INO,JNO
      INTEGER            IDEC,JDEC,KDEC,LDEC,IRES,IMATE
      INTEGER            II,MATER,JINST,INDIC1,INDIC2
      INTEGER            IONDE,IER,NNOS,JGANO
      REAL*8             JAC,NX,NY,NZ,SX(9,9),SY(9,9),SZ(9,9)
      REAL*8             VALRES(3),E,NU,LAMBDA,MU,CP,CS,RHO,TYPER
      REAL*8             TAUX,TAUY,TAUZ,DIRX,DIRY,DIRZ
      REAL*8             NORM,TANX,TANY,NORX,NORY,NORZ
      REAL*8             TAONDX,TAONDY,TAONDZ
      REAL*8             NUX,NUY,NUZ,SCAL,COEDIR
      REAL*8             R8B
      REAL*8             NORTAN,CELE,TRACE,PARAM,DIST
      REAL*8             SIGMA(3,3),EPSI(3,3),GRAD(3,3),VALFON
      REAL*8             VONDN(3),VONDT(3)
      INTEGER ICODRE(3),KPG,SPT,JMA,JNB,JMAIL,JNBMA,IADZI,IAZK24
      CHARACTER*2 TYPE
      CHARACTER*8        NOMRES(3),FAMI,POUM,NOMAIL
C     ------------------------------------------------------------------
C
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,IDFDX,JGANO)
      IDFDY  = IDFDX  + 1
C
      CALL JEVECH ( 'PGEOMER', 'L', IGEOM )
      CALL JEVECH ( 'PMATERC', 'L', IMATE )
      CALL JEVECH ( 'PONDPLA', 'L', IONDE )
      CALL JEVECH ( 'PTEMPSR', 'L', JINST )
      CALL JEVECH ( 'PVECTUR', 'E', IRES  )
      
      CALL JEEXIN(ZK24(IONDE+5)(1:19)//'.NBMA',JNB)
      IF (JNB.EQ.0) GOTO 200
      
      CALL JEVEUO(ZK24(IONDE+5)(1:19)//'.NBMA','L',JNB)
      CALL JEVEUO(ZK24(IONDE+5)(1:19)//'.VALE','L',JMA)
      CALL TECAEL(IADZI,IAZK24)
      NOMAIL = ZK24(IAZK24-1+3) (1:8)

      DO 20 I = 1,3*NNO
        ZR(IRES+I-1) = 0.0D0
 20     CONTINUE
 
      DO 11 I=1,ZI(JNB)
        IF (NOMAIL.EQ.ZK8(JMA+I-1)) GOTO 200
  11    CONTINUE
      GOTO 140
 200  CONTINUE
C
C     --- INITIALISATION DE SIGMA
C
      DO 21 I = 1,3
        DO 21 J = 1,3
          SIGMA(I,J) = 0.D0
21        CONTINUE
C
      MATER=ZI(IMATE)
      NOMRES(1)='E'
      NOMRES(2)='NU'
      NOMRES(3)='RHO'
      FAMI='FPG1'
      KPG=1
      SPT=1
      POUM='+'
      CALL RCVALB(FAMI,KPG,SPT,POUM,MATER,' ','ELAS',0,' ',R8B,3,
     &            NOMRES,VALRES,ICODRE,1)
      E = VALRES(1)
      NU = VALRES(2)
      RHO = VALRES(3)

      LAMBDA = E*NU/(1.D0+NU)/(1.D0-2.D0*NU)
      MU = E/2.D0/(1.D0+NU)

      CP = SQRT((LAMBDA+2.D0*MU)/RHO)
      CS = SQRT(MU/RHO)
C
C     --- CARACTERISTIQUES DE L'ONDE PLANE
C
      CALL FOINTE('FM',ZK24(IONDE),1,'X',1.D0,DIRX,IER)
      CALL FOINTE('FM',ZK24(IONDE+1),1,'X',1.D0,DIRY,IER)
      CALL FOINTE('FM',ZK24(IONDE+2),1,'X',1.D0,DIRZ,IER)
      CALL FOINTE('FM',ZK24(IONDE+3),1,'X',1.D0,TYPER,IER)
      IF (TYPER.EQ.0.D0) TYPE = 'P'
      IF (TYPER.EQ.1.D0) TYPE = 'SV'
      IF (TYPER.EQ.2.D0) TYPE = 'SH'
C
C     --- CALCUL DU VECTEUR DIRECTEUR UNITAIRE DE L'ONDE PLANE
C
      NORM = SQRT(DIRX**2.D0+DIRY**2.D0+DIRZ**2.D0)
      DIRX = DIRX/NORM
      DIRY = DIRY/NORM
      DIRZ = DIRZ/NORM
      
C     CALCUL DU REPERE ASSOCIE A L'ONDE
      TANX = DIRY
      TANY = -DIRX

      NORTAN = SQRT(TANX**2.D0+TANY**2.D0)

      IF (NORTAN.NE.0.D0) THEN
        TANX = TANX/NORTAN
        TANY = TANY/NORTAN

        NORX = TANY*DIRZ
        NORY = -TANX*DIRZ
        NORZ = TANX*DIRY - TANY*DIRX
      ELSE
        NORX = DIRZ
        NORY = 0.D0
        NORZ = 0.D0

        TANX = 0.D0
        TANY = DIRZ
      ENDIF
      IF (TYPE.EQ.'P') THEN
        CELE = CP
      ELSE
        CELE = CS
      ENDIF
C
C     --- CALCUL DES PRODUITS VECTORIELS OMI X OMJ ---
C
      DO 30 INO = 1 , NNO
        I = IGEOM + 3*(INO-1) -1
        DO 30 JNO = 1,NNO
          J = IGEOM + 3*(JNO-1) -1
          SX(INO,JNO) = ZR(I+2)*ZR(J+3) - ZR(I+3)*ZR(J+2)
          SY(INO,JNO) = ZR(I+3)*ZR(J+1) - ZR(I+1)*ZR(J+3)
          SZ(INO,JNO) = ZR(I+1)*ZR(J+2) - ZR(I+2)*ZR(J+1)
 30       CONTINUE
C
C     --- BOUCLE SUR LES POINTS DE GAUSS ---
C
      DO 100 IPG = 1,NPG1

         KDEC = (IPG-1)*NNO*NDIM
         LDEC = (IPG-1)*NNO
C
C        --- CALCUL DU CHARGEMENT PAR ONDE PLANE
CKH          ON SUPPOSE QU'ON RECUPERE UNE VITESSE
         CALL FOINTE('FM',ZK24(IONDE+4),1,'INST',ZR(JINST),VALFON,IER)
         VALFON = -VALFON/CELE
C         VALFON = +VALFON/CELE
         
C        CALCUL DES CONTRAINTES ASSOCIEES A L'ONDE PLANE
C        CALCUL DU GRADIENT DU DEPLACEMENT
         IF (TYPE.EQ.'P') THEN
         
            GRAD(1,1) = DIRX*VALFON*DIRX
            GRAD(1,2) = DIRY*VALFON*DIRX
            GRAD(1,3) = DIRZ*VALFON*DIRX

            GRAD(2,1) = DIRX*VALFON*DIRY
            GRAD(2,2) = DIRY*VALFON*DIRY
            GRAD(2,3) = DIRZ*VALFON*DIRY

            GRAD(3,1) = DIRX*VALFON*DIRZ
            GRAD(3,2) = DIRY*VALFON*DIRZ
            GRAD(3,3) = DIRZ*VALFON*DIRZ
            
         ELSEIF (TYPE.EQ.'SV') THEN
         
            GRAD(1,1) = DIRX*VALFON*NORX
            GRAD(1,2) = DIRY*VALFON*NORX
            GRAD(1,3) = DIRZ*VALFON*NORX

            GRAD(2,1) = DIRX*VALFON*NORY
            GRAD(2,2) = DIRY*VALFON*NORY
            GRAD(2,3) = DIRZ*VALFON*NORY

            GRAD(3,1) = DIRX*VALFON*NORZ
            GRAD(3,2) = DIRY*VALFON*NORZ
            GRAD(3,3) = DIRZ*VALFON*NORZ
            
         ELSEIF (TYPE.EQ.'SH') THEN
         
            GRAD(1,1) = DIRX*VALFON*TANX
            GRAD(1,2) = DIRY*VALFON*TANX
            GRAD(1,3) = DIRZ*VALFON*TANX

            GRAD(2,1) = DIRX*VALFON*TANY
            GRAD(2,2) = DIRY*VALFON*TANY
            GRAD(2,3) = DIRZ*VALFON*TANY

            GRAD(3,1) = 0.D0
            GRAD(3,2) = 0.D0
            GRAD(3,3) = 0.D0
            
         ENDIF

C        CALCUL DES DEFORMATIONS
         DO 201 INDIC1 = 1,3
           DO 201 INDIC2 = 1,3
             EPSI(INDIC1,INDIC2) = .5D0*(GRAD(INDIC1,INDIC2)
     &                             + GRAD(INDIC2,INDIC1))
201          CONTINUE

C        CALCUL DES CONTRAINTES
         TRACE = 0.D0
         DO 203 INDIC1 = 1,3
           TRACE = TRACE + EPSI(INDIC1,INDIC1)
203        CONTINUE

         DO 204 INDIC1 = 1,3
           DO 204 INDIC2 = 1,3
             IF (INDIC1.EQ.INDIC2) THEN
                SIGMA(INDIC1,INDIC2) = LAMBDA*TRACE
     &                                 +2.D0*MU*EPSI(INDIC1,INDIC2)
             ELSE
                SIGMA(INDIC1,INDIC2) = 2.D0*MU*EPSI(INDIC1,INDIC2)
             ENDIF
204          CONTINUE
C
         NX = 0.0D0
         NY = 0.0D0
         NZ = 0.0D0
C
C        --- CALCUL DE LA NORMALE AU POINT DE GAUSS IPG ---
C
         DO 102 I = 1,NNO
           IDEC = (I-1)*NDIM
           DO 102 J = 1,NNO
              JDEC = (J-1)*NDIM
           NX = NX + ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) * SX(I,J)
           NY = NY + ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) * SY(I,J)
           NZ = NZ + ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) * SZ(I,J)
 102       CONTINUE
C
C        --- LE JACOBIEN EST EGAL A LA NORME DE LA NORMALE ---
C
         JAC = SQRT(NX*NX + NY*NY + NZ*NZ)
C
C        --- CALCUL DE LA NORMALE UNITAIRE ---
C
         NUX = NX /JAC
         NUY = NY /JAC
         NUZ = NZ /JAC
C
C        --- TEST DU SENS DE LA NORMALE PAR RAPPORT A LA DIRECTION
C            DE L'ONDE

        SCAL = NUX*DIRX + NUY*DIRY + NUZ*DIRZ
        IF (SCAL.GT.0.D0) THEN
          COEDIR = 1.D0
        ELSE
          COEDIR = -1.D0
        END IF
C
C        --- CALCUL DE V.N ---
C
          VONDT(1) = 0.D0
          VONDT(2) = 0.D0
          VONDT(3) = 0.D0

          IF (TYPE.EQ.'P') THEN
             VONDT(1) = -CELE*VALFON*DIRX
             VONDT(2) = -CELE*VALFON*DIRY
             VONDT(3) = -CELE*VALFON*DIRZ
          ELSEIF (TYPE.EQ.'SV') THEN
             VONDT(1) = -CELE*VALFON*NORX
             VONDT(2) = -CELE*VALFON*NORY
             VONDT(3) = -CELE*VALFON*NORZ
          ELSEIF (TYPE.EQ.'SH') THEN
             VONDT(1) = -CELE*VALFON*TANX
             VONDT(2) = -CELE*VALFON*TANY
             VONDT(3) = 0.D0
          ENDIF
          
C        --- CALCUL DE LA VITESSE NORMALE ET DE LA VITESSE TANGENCIELLE
          CALL PRONOR(NUX,NUY,NUZ,VONDT,VONDN)
C
C        --- CALCUL DU VECTEUR CONTRAINTE
C
          TAUX = - RHO*(CP*VONDN(1) + CS*VONDT(1))
          TAUY = - RHO*(CP*VONDN(2) + CS*VONDT(2))
          TAUZ = - RHO*(CP*VONDN(3) + CS*VONDT(3))
C
C        --- CALCUL DU VECTEUR CONTRAINTE DU A UNE ONDE PLANE
C
          TAONDX = SIGMA(1,1)*NUX
          TAONDX = TAONDX + SIGMA(1,2)*NUY
          TAONDX = TAONDX + SIGMA(1,3)*NUZ

          TAONDY = SIGMA(2,1)*NUX
          TAONDY = TAONDY + SIGMA(2,2)*NUY
          TAONDY = TAONDY + SIGMA(2,3)*NUZ

          TAONDZ = SIGMA(3,1)*NUX
          TAONDZ = TAONDZ + SIGMA(3,2)*NUY
          TAONDZ = TAONDZ + SIGMA(3,3)*NUZ
C
C        --- CALCUL DU VECTEUR ELEMENTAIRE
C
          DO 100 I = 1,NNO
             II = 3*I-2
             ZR(IRES+II-1) = ZR(IRES+II-1) +
     &     (TAUX+COEDIR*TAONDX)*ZR(IVF+LDEC+I-1)*JAC*ZR(IPOIDS+IPG-1)
             ZR(IRES+II+1-1) = ZR(IRES+II+1-1) +
     &     (TAUY+COEDIR*TAONDY)*ZR(IVF+LDEC+I-1)*JAC*ZR(IPOIDS+IPG-1)
             ZR(IRES+II+2-1) = ZR(IRES+II+2-1) +
     &     (TAUZ+COEDIR*TAONDZ)*ZR(IVF+LDEC+I-1)*JAC*ZR(IPOIDS+IPG-1)
100       CONTINUE

140    CONTINUE

       END
