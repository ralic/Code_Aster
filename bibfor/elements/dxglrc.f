      SUBROUTINE DXGLRC ( NOMTE, OPT, COMPOR, XYZL, UL, DUL, BTSIG,
     &                    KTAN, EFFINT, PGL, CODRET )

      IMPLICIT NONE

      INTEGER      CODRET
      REAL*8       XYZL(3,4), KTAN((6*4)*(6*4+1)/2), BTSIG(6,4)
      REAL*8       UL(6,4), DUL(6,4), PGL(3,3)
      CHARACTER*16 OPT, NOMTE, COMPOR(*)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 18/09/2007   AUTEUR DURAND C.DURAND 
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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

      REAL*8   MG(3), ROT(9), DH(9), R, R8B
      INTEGER  NDIM,NNO,NNOS,NPG,IPOIDS,ICOOPG,IVF,IDFDX,IDFD2,JGANO
      INTEGER  MULTIC
      INTEGER  IMATE,  IRET,   ICONTM, IVARIM
      INTEGER  ICOMPO, ICACOQ, ICONTP, IVARIP, INO,   NBCON
      INTEGER  NBVAR,  NDIMV,  IVARIX, L,      IPG,    JVARI, IVARI
      INTEGER  K

      REAL*8 DELAS(6,6)
      REAL*8 ZERO
      REAL*8 DFE(9),   DME(9),    DMFE(9)
      REAL*8 RBIB8,    RBIB1(4),  RBIB2(4),  RBIB3(6), RBIB4(6)
      REAL*8 XAB(3,3), DEFO(2,2) ,DSIDEP(6,6)
      INTEGER I,J
      LOGICAL LBID,ELASCQ
C     ------------------------------------------------------------------
C     CALCUL LES OPTIONS NON LINEAIRES POUR L'ELEMENT DE PLAQUE DKTG
C     TOUTES LES ENTREES ET LES SORTIES SONT DANS LE REPERE LOCAL.
C     ------------------------------------------------------------------
C     IN  OPT  : OPTION NON LINEAIRE A CALCULER
C                'RAPH_MECA' ,'FULL_MECA', OU 'RIGI_MECA_TANG'
C     IN  XYZL : COORDONNEES DES NOEUDS DANS LE REPERE LOCAL
C     IN  UL   : DEPLACEMENT A L'INSTANT T "-"
C     IN  DUL  : INCREMENT DE DEPLACEMENT
C     IN  PGL  : MATRICE DE PASSAGE
C                DU REPERE GLOBAL AU REPERE LOCAL ELEMENT
C     OUT KTAN : MATRICE DE RIGIDITE TANGENTE
C                    SI 'FULL_MECA' OU 'RIGI_MECA_TANG'
C     OUT BTSIG: DIV (SIGMA)
C                    SI 'FULL_MECA' OU 'RAPH_MECA'
C     ------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C     CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL,VECTEU,MATRIC,TEMPNO
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*24 VALK
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C --------- VARIABLES LOCALES :
C  -- GENERALITES :
C  ----------------
C  CMPS DE DEPLACEMENT :
C   - MEMBRANE : DX(N1), DY(N1), DX(N2), ..., DY(NNO)
C   - FLEXION  : DZ(N1), BETAX(N1), BETAY(N1), DZ(N2), ..., BETAY(NNO)
C  CMPS DE DEFORMATION COQUE :
C   - MEMBRANE : EPSIXX,EPSIYY,2*EPSIXY
C   - FLEXION  : KHIXX,KHIYY,2*KHIXY
C  CMPS D' EFFORTS COQUE :
C   - MEMBRANE : NXX,NYY,NXY
C   - FLEXION  : MXX,MYY,MXY
C --------------------------------------------------------------------
C            NPG:    NOMBRE DE POINTS DE GAUSS PAR ELEMENT
C            NC :    NOMBRE DE COTES DE L'ELEMENT
      REAL*8 POIDS,ZIC,ZMIN,TP,TM,VALPU(2)
C            POIDS:  POIDS DE GAUSS (Y COMPRIS LE JACOBIEN)
C            AIRE:   SURFACE DE L'ELEMENT
      REAL*8 UM(2,4),UF(3,4),DUM(2,4),DUF(3,4)
C            UM:     DEPLACEMENT (MEMBRANE) "-"
C            UF:     DEPLACEMENT (FLEXION)  "-"
C           DUM:     INCREMENT DEPLACEMENT (MEMBRANE)
C           DUF:     INCREMENT DEPLACEMENT (FLEXION)
      REAL*8 EPS(3),KHI(3),DEPS(6),DKHI(3),N(3),M(3)
C            EPS:    DEFORMATION DE MEMBRANE "-"
C            DEPS:   INCREMENT DE DEFORMATION DE MEMBRANE
C            KHI:    DEFORMATION DE FLEXION  "-" (COURBURE)
C            DKHI:   INCREMENT DE DEFORMATION DE FLEXION (COURBURE)
C            N  :    EFFORT NORMAL "+"
C            M  :    MOMENT FLECHISSANT "+"
C
      REAL*8 EFFINT(32)
C            EFFINT : EFFORTS DANS LE REPERE DE L'ELEMENT
      REAL*8 DF(9),DM(9),DMF(9)
C            DF :    MATRICE DE RIGIDITE TANGENTE MATERIELLE (FLEXION)
C            DM :    MATRICE DE RIGIDITE TANGENTE MATERIELLE (MEMBRANE)
C            DMF:    MATRICE DE RIGIDITE TANGENTE MATERIELLE (COUPLAGE)
      REAL*8 BF(3,3*4),BM(3,2*4)
C            BF :    MATRICE "B" (FLEXION)
C            BM :    MATRICE "B" (MEMBRANE)
      REAL*8 FLEX(3*4,3*4),MEMB(2*4,2*4)
      REAL*8 MEFL(2*4,3*4),WORK(3,3*4)
C           MEMB:    MATRICE DE RIGIDITE DE MEMBRANE
C           FLEX:    MATRICE DE RIGIDITE DE FLEXION
C           WORK:    TABLEAU DE TRAVAIL
C           MEFL:    MATRICE DE COUPLAGE MEMBRANE-FLEXION
C             LE MATERIAU EST SUPPOSE HOMOGENE
C     ------------------
      INTEGER  JTAB(7), COD, ITABP(8),ITABM(8)
      REAL*8   DEUX, CTOR, LC
      REAL*8   T2EV(4),T2VE(4),T1VE(9),CARAT3(21),JACOB(5),CARAQ4(25)

      REAL*8   MATR(50),SIGM(6)
      INTEGER  MATI(50)
      INTEGER  TSHEAR, ICARA
      REAL*8   EPST(6), EP, SURFGP, SIG(6),DSIG(8),ECR(21),ECRP(21)
      REAL*8   EPSM(6),MVAL(50),QSI,ETA
      INTEGER  MENT(50),ICPG,ICPV,NVINT,T(2,2),IBID
      REAL*8   LAMBDA,DEUXMU,DEUMUF,LAMF,GT,GC,GF,SEUIL,ALPHA
      CHARACTER*8 K8BID
      REAL*8      R8BID
C     ------------------------------------------------------------------

      CALL ELREF5(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,ICOOPG,
     &                                         IVF,IDFDX,IDFD2,JGANO)

      CODRET=0

      VECTEU = ((OPT.EQ.'FULL_MECA') .OR. (OPT.EQ.'RAPH_MECA'))
      MATRIC = ((OPT.EQ.'FULL_MECA') .OR. (OPT.EQ.'RIGI_MECA_TANG'))

      IF ( NOMTE(1:8).NE.'MEDKTG3 ' .AND.
     &     NOMTE(1:8).NE.'MEDKQG4 ' ) THEN
        CALL U2MESK('F','ELEMENTS_14',1,NOMTE(1:8))
      ENDIF
C
      CALL JEVECH('PMATERC','L',IMATE)

      CALL TECACH('OON','PCONTMR',7,JTAB,IRET)
      ICONTM=JTAB(1)
      CALL ASSERT(NPG.EQ.JTAB(3))
      CALL JEVECH('PVARIMR','L',IVARIM)
      CALL JEVECH('PCOMPOR','L',ICOMPO)
      CALL JEVECH('PCACOQU','L',ICACOQ)

      IF (VECTEU) THEN
        CALL JEVECH('PCONTPR','E',ICONTP)
        CALL JEVECH('PVARIPR','E',IVARIP)
      ELSE
        ICONTP = ICONTM
        IVARIP = IVARIM
      END IF
C
C     -- GRANDEURS GEOMETRIQUES :
C     ---------------------------
      IF(NNO.EQ.3) THEN
        CALL GTRIA3(XYZL,CARAT3)
      ELSEIF(NNO.EQ.4) THEN
        CALL GQUAD4(XYZL,CARAQ4)
      ENDIF
      CTOR = ZR(ICACOQ+3)
C
C     -- MISES A ZERO :
C     ------------------
      IF (MATRIC) THEN
        CALL R8INIR((3*NNO)* (3*NNO),0.D0,FLEX,1)
        CALL R8INIR((2*NNO)* (2*NNO),0.D0,MEMB,1)
        CALL R8INIR((2*NNO)* (3*NNO),0.D0,MEFL,1)
      END IF
      IF (VECTEU) THEN
        CALL R8INIR(6*NNO,0.D0,BTSIG,1)
        CALL R8INIR(32,0.D0,EFFINT,1)
      END IF
C
      CALL R8INIR(36,0.D0,DELAS,1)
C
C     -- PARTITION DU DEPLACEMENT EN MEMBRANE/FLEXION :
C     -------------------------------------------------
      DO 10,INO = 1,NNO
        UM(1,INO) = UL(1,INO)
        UM(2,INO) = UL(2,INO)
        UF(1,INO) = UL(3,INO)
        UF(2,INO) = UL(5,INO)
        UF(3,INO) = -UL(4,INO)
        DUM(1,INO) = DUL(1,INO)
        DUM(2,INO) = DUL(2,INO)
        DUF(1,INO) = DUL(3,INO)
        DUF(2,INO) = DUL(5,INO)
        DUF(3,INO) = -DUL(4,INO)
   10 CONTINUE
C
C     -- INTEGRATION SUR LA SURFACE DE L'ELEMENT:
C     -------------------------------------------
C     CONTRAINTE 2D : NXX,NYY,NXY,MXX,MYY,MXY
      NBCON = 8
C     NBVAR: NOMBRE DE VARIABLES INTERNES (2D) LOI COMPORTEMENT
      READ (ZK16(ICOMPO-1+2),'(I16)') NBVAR
      CALL TECACH('OON','PVARIMR',7,JTAB,IRET)

C===============================================================
C     -- BOUCLE SUR LES POINTS DE GAUSS DE LA SURFACE:
C     -------------------------------------------------
      DO 130,IPG = 1,NPG
C
        CALL R8INIR(21,0.D0,ECRP,1)
        
        CALL R8INIR(3,0.D0,N,1)
        CALL R8INIR(3,0.D0,M,1)
        CALL R8INIR(9,0.D0,DF,1)
        CALL R8INIR(9,0.D0,DM,1)
        CALL R8INIR(9,0.D0,DMF,1)


        QSI = ZR(ICOOPG-1+NDIM*(IPG-1)+1)
        ETA = ZR(ICOOPG-1+NDIM*(IPG-1)+2)
C
        ICPG = (IPG-1)*NBCON
        ICPV = (IPG-1)*NBVAR

        IF(NOMTE(1:8).EQ.'MEDKTG3 ') THEN
          CALL DXTBM(CARAT3(9),BM)
          CALL DKTBF(QSI,ETA,CARAT3,BF)
          POIDS = ZR(IPOIDS+IPG-1)*CARAT3(7)
        ELSE IF(NOMTE(1:8).EQ.'MEDKQG4 ') THEN
          CALL JQUAD4(XYZL,QSI,ETA,JACOB)
          CALL DXQBM(QSI,ETA,JACOB(2),BM)
          CALL DKQBF(QSI,ETA,JACOB(2),CARAQ4,BF)
          POIDS = ZR(IPOIDS+IPG-1)*JACOB(1)
        ENDIF

        CALL PMRVEC('ZERO',3,2*NNO,BM,UM,EPS)
        CALL PMRVEC('ZERO',3,2*NNO,BM,DUM,DEPS)
        CALL PMRVEC('ZERO',3,3*NNO,BF,UF,KHI)
        CALL PMRVEC('ZERO',3,3*NNO,BF,DUF,DKHI)

        CALL JEVECH('PCACOQU','L',ICARA)
C ---   EPAISSEUR TOTALE :
        EP = ZR(ICARA)

        DO 60, I = 1,50
          MATR(I) = 0.0D0
          MATI(I) = 0
 60     CONTINUE

        TSHEAR = 0

        DO 70, I = 1,3
          EPST(I)     = EPS(I) + DEPS(I)
          EPST(3 + I) = KHI(I) + DKHI(I)
          DEPS(3 + I) = DKHI(I)
 70     CONTINUE
        
        DO 73, I = 1,3
          EPSM(I)   = EPS(I)        
          EPSM(I+3) = KHI(I)        
 73     CONTINUE

        DO 77, I = 1,6
          SIG(I)  = ZR(ICONTM-1 + ICPG + I)
          SIGM(I) = SIG(I)
 77     CONTINUE

        IF (COMPOR(1)(1:11).EQ. 'GLRC_DAMAGE') THEN

          NVINT = 21
          
          DO 75, I = 1,NVINT
            ECR(I)     = ZR(IVARIM-1 + ICPV + I)
 75       CONTINUE

          CALL MAGLRC (NNO,COMPOR,PGL,MATR,MATI,DELAS,ECR)

C   AIRE DE SURFACE APPARTENANT AU POINT DE G.
          SURFGP = POIDS
          CALL GLRCMM(MATR,MATI,EP,SURFGP,PGL,SIG
     &               ,EPST,DEPS,DSIG,ECR,TSHEAR,DELAS,DSIDEP)

          DO 78, I = 1,3
            DSIG(I)     = DSIG(I) * EP
            DSIG(3 + I) = DSIG(3 + I) * EP*EP / 6.D0
 78       CONTINUE

          DO 80, I = 1,NVINT
            ECRP(I) = ECR(I)
 80       CONTINUE

          DO 85, I = 1,6
            SIG(I) = SIG(I) + DSIG(I)
 85       CONTINUE

        ELSEIF (COMPOR(1)(1:7).EQ. 'GLRC_DM') THEN

          NVINT = 4 

          DO 8510, I = 1,NVINT
            ECR(I)     = ZR(IVARIM-1 + ICPV + I)        
 8510     CONTINUE

          CALL CRGDM(ZI(IMATE),'GLRC_DM         ',T,LAMBDA,DEUXMU,
     &               LAMF,DEUMUF,GT,GC,GF,SEUIL,ALPHA,EP)
     
C         ENDOMMAGEMENT SEULEMENT      
     
            CALL R8INIR(36,0.D0,DSIDEP,1)
            CALL LCGLDM(EPSM,DEPS,ECR,OPT,SIG,ECRP,DSIDEP,T,
     &                LAMBDA,DEUXMU,LAMF,DEUMUF,GT,GC,GF,SEUIL,ALPHA)

        ELSEIF (COMPOR(1)(1:7).EQ. 'KIT_DDI') THEN
C         ENDOMMAGEMENT PLUS PLASTICITE

          NVINT = 21 

          DO 8515, I = 1,NVINT
            ECR(I)     = ZR(IVARIM-1 + ICPV + I)        
 8515     CONTINUE

            CALL NMCOUP('RIGI',IPG,1,3,K8BID,ZI(IMATE),COMPOR,LBID,
     &                   R8BID,R8BID,R8BID,EPSM,DEPS,SIGM,ECR,OPT,R8BID,
     &                   SIG,ECRP,DSIDEP,IBID)

        ELSE
           VALK = COMPOR(1)
           CALL U2MESG('F', 'ELEMENTS4_79',1,VALK,0,0,0,0.D0)
        ENDIF

        DO 8520, I = 1,NVINT
          ZR(IVARIP-1 + ICPV + I) = ECRP(I)        
 8520   CONTINUE
 
        DO 8530, I = 1,6
          ZR(ICONTP-1 + ICPG + I) = SIG(I)        
 8530   CONTINUE

C
C         EFFORTS RESULTANTS (N ET M)
C         --------------------------
            IF (VECTEU) THEN
              DO 90 I= 1,3
                N(I) = ZR(ICONTP-1+ICPG+I)
                M(I) = ZR(ICONTP-1+ICPG+I+3)
   90            CONTINUE
            END IF

C         -- CALCUL DES MATRICES TANGENTES MATERIELLES (DM,DF,DMF):
C         ---------------------------------------------------------
            IF (MATRIC) THEN
                L=0
                DO 96 I=1,3
                  DO 97 J=1,3
                  L=L+1
                  DM(L) = DM(L)  + POIDS*DSIDEP(J,I)
                  DMF(L)= DMF(L) + POIDS*DSIDEP(J,I+3)
                  DF(L) = DF(L)  + POIDS*DSIDEP(J+3,I+3)
   97           CONTINUE
   96         CONTINUE
            END IF
C
C       -- CALCUL DE DIV(SIGMA) ET RECOPIE DE N ET M DANS 'PCONTPR':
C       ----------------------------------------------------------
C       BTSIG = BTSIG + BFT*M + BMT*N
        IF (VECTEU) THEN
           DO 100,K = 1,3
              EFFINT((IPG-1)*8+K)   = N(K)
              EFFINT((IPG-1)*8+3+K) = M(K)
  100       CONTINUE
          DO 120,INO = 1,NNO
            DO 110,K = 1,3
              BTSIG(1,INO) = BTSIG(1,INO) +
     &                       BM(K,2* (INO-1)+1)*N(K)*POIDS
              BTSIG(2,INO) = BTSIG(2,INO) +
     &                       BM(K,2* (INO-1)+2)*N(K)*POIDS
              BTSIG(3,INO) = BTSIG(3,INO) +
     &                       BF(K,3* (INO-1)+1)*M(K)*POIDS
              BTSIG(5,INO) = BTSIG(5,INO) +
     &                       BF(K,3* (INO-1)+2)*M(K)*POIDS
              BTSIG(4,INO) = BTSIG(4,INO) -
     &                       BF(K,3* (INO-1)+3)*M(K)*POIDS
  110       CONTINUE
  120     CONTINUE
        END IF
C       -- CALCUL DE LA MATRICE TANGENTE :
C       ----------------------------------
C       KTANG = KTANG + BFT*DF*BF + BMT*DM*BM + BMT*DMF*BF
        IF (MATRIC) THEN
C         -- MEMBRANE :
C         -------------
          CALL UTBTAB('CUMU',3,2*NNO,DM,BM,WORK,MEMB)
C         -- FLEXION :
C         ------------
          CALL UTBTAB('CUMU',3,3*NNO,DF,BF,WORK,FLEX)
C         -- COUPLAGE:
C         ------------
          CALL UTCTAB('CUMU',3,3*NNO,2*NNO,DMF,BF,BM,WORK,MEFL)
        END IF
C
C       -- FIN BOUCLE SUR LES POINTS DE GAUSS
C

  130 CONTINUE

C
C     -- ACCUMULATION DES SOUS MATRICES DANS KTAN :
C     -----------------------------------------------
      IF (MATRIC) THEN
        IF(NOMTE(1:8).EQ.'MEDKTG3 ') THEN
          CALL DXTLOC(FLEX,MEMB,MEFL,CTOR,KTAN)
        ELSE IF(NOMTE(1:8).EQ.'MEDKQG4 ') THEN
          CALL DXQLOC(FLEX,MEMB,MEFL,CTOR,KTAN)
        ENDIF
      END IF
  140 CONTINUE

      END
