      SUBROUTINE DXGLRC(NOMTE,OPT,COMPOR,XYZL,UL,DUL,BTSIG,KTAN,
     +                  EFFINT,PGL,CODRET)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 18/10/2004   AUTEUR LEBOUVIE F.LEBOUVIER 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16 OPT
      CHARACTER*16 NOMTE,COMPOR(*)
      INTEGER NNO, CODRET
C
      REAL*8 XYZL(3,4),KTAN((6*4)* (6*4+1)/2),BTSIG(6,4)
      REAL*8 UL(6,4),  DUL(6,4)
      REAL*8 MG(3),    PGL(3,3), ROT(9), DH(9)
      REAL*8     R,        R8B
      INTEGER MULTIC
      INTEGER LZR,    IMATE,  IRET,   ICONTM, IVARIM 
      INTEGER ICOMPO, ICACOQ, ICONTP, IVARIP, INO,   NBCON
      INTEGER NBVAR,  NDIMV,  IVARIX, L,      IPG,    JVARI, IVARI
      INTEGER K
C GLRC
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
      INTEGER NPG,NC,ITABP(8),ITABM(8)
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
      REAL*8 EPS(3),KHI(3),DEPS(3),DKHI(3),N(3),M(3)
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
C     ------------------ PARAMETRAGE TRIANGLE --------------------------
      INTEGER   LDETJ,LJACO,LTOR,LQSI,LETA,LWGT,JTAB(7),COD
      INTEGER   NDIM,NNOS,IPOIDS,IVF,IDFDX,JGANO
C
      PARAMETER (LDETJ=1)
      PARAMETER (LJACO=2)
      PARAMETER (LTOR=LJACO+4)
      PARAMETER (LQSI=LTOR+1)
      REAL*8 DEUX
      REAL*8 CTOR
      REAL*8 LC
C     ------------------------------------------------------------------
C --DEB

      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDX,JGANO)

      CODRET=0
C
C     2 BOOLEENS COMMODES :
C     ---------------------
      VECTEU = ((OPT.EQ.'FULL_MECA') .OR. (OPT.EQ.'RAPH_MECA'))
      MATRIC = ((OPT.EQ.'FULL_MECA') .OR. (OPT.EQ.'RIGI_MECA_TANG'))
C
C     RECUPERATION DES OBJETS &INEL ET DES CHAMPS PARAMETRES :
C     --------------------------------------------------------
      IF(NOMTE(1:8).EQ.'MEDKTG3 ') THEN   
        NC  = 0
      ELSE IF(NOMTE(1:8).EQ.'MEDKQG4 ') THEN
        NC  = 4
      ELSE
        CALL UTMESS('F','DXGLRC','LE TYPE D''ELEMENT : '//NOMTE(1:8)//
     +              'N''EST PAS PREVU.')
      ENDIF
C
      CALL JEVETE('&INEL.'//NOMTE(1:8)//'.DESR',' ',LZR)     
C
      LETA=LQSI+NPG+NNO+2*NC
      LWGT=LETA+NPG+NNO+2*NC

      CALL JEVECH('PMATERC','L',IMATE)

      CALL TECACH('OON','PCONTMR',7,JTAB,IRET)
      ICONTM=JTAB(1)
      IF (NPG.NE.JTAB(3)) CALL UTMESS('F','DXGLRC','STOP')
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
        CALL GTRIA3(XYZL,ZR(LZR))
      ELSE IF(NNO.EQ.4) THEN
        CALL GQUAD4(XYZL,ZR(LZR))
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
C
C- REMPLISSAGE DE LA MATRICE ELASTIQUE
C
      CALL DXMATE(DFE,DME,DMFE,RBIB1,RBIB2,RBIB3,RBIB4,NNO,PGL,ZR(LZR),
     +            MULTIC,LBID,ELASCQ)
C  
      L=0
      DO 20 I = 1,3
        DO 30 J = 1,3
          L = L + 1
          DELAS(J,I)     = DME(L)
          DELAS(J+3,I+3) = DFE(L)
          DELAS(J,I+3)   = DMFE(L)
          DELAS(I+3,J)   = DMFE(L)
 30     CONTINUE
 20   CONTINUE
C
      IF (NNO.EQ.3) THEN
C        8 + 3 * NPG + 2 * NNO + 5 * NC ( NNO = NPG = NC = 3 )
        LT1VE = 38
      ELSE IF (NNO.EQ.4) THEN
C        8 + 3 * NPG + 2 * NNO + 9 * NC + 4 ( NNO = NPG = NC = 4 )
        LT1VE = 68
      END IF
      LT2VE = LT1VE + 9
      LT2EV = LT2VE + 4
C
C===============================================================
C     -- BOUCLE SUR LES POINTS DE GAUSS DE LA SURFACE:
C     -------------------------------------------------
      DO 130,IPG = 1,NPG
C
        ICPG = (IPG-1)*NBCON
        ICPV = (IPG-1)*NBVAR
C
        CALL R8INIR(3,0.D0,N,1)
        CALL R8INIR(3,0.D0,M,1)
        CALL R8INIR(9,0.D0,DF,1)
        CALL R8INIR(9,0.D0,DM,1)
        CALL R8INIR(9,0.D0,DMF,1)
C
        IF(NOMTE(1:8).EQ.'MEDKTG3 ') THEN
          CALL DXTBM(ZR(LZR),BM)
          CALL DKTBF(IPG,ZR(LZR),BF)
        ELSE IF(NOMTE(1:8).EQ.'MEDKQG4 ') THEN
          CALL JQUAD4(IPG,XYZL,ZR(LZR))
          CALL DXQBM(IPG,ZR(LZR),BM)
          CALL DKQBF(IPG,ZR(LZR),BF)
        ENDIF

        POIDS = ZR(LZR-1+LWGT+IPG-1)*ZR(LZR-1+LDETJ)
C
C       -- CALCUL DE EPS, DEPS, KHI, DKHI : 
C          DANS LE REPERE DE L'ELEMENT
C       -----------------------------------
        CALL PMRVEC('ZERO',3,2*NNO,BM,UM,EPS)
        CALL PMRVEC('ZERO',3,2*NNO,BM,DUM,DEPS)
        CALL PMRVEC('ZERO',3,3*NNO,BF,UF,KHI)
        CALL PMRVEC('ZERO',3,3*NNO,BF,DUF,DKHI)
C
        IF ( COMPOR(1)(1:4).EQ.'GLRC') THEN
            CALL GLRCST(OPT,DELAS,EPS,DEPS,KHI,DKHI,ZR(LZR+LT1VE-1),
     &                  ZR(LZR+LT2VE-1),ZR(LZR+LT2EV-1),
     &                  ZR(ICONTM+ICPG),ZR(IVARIM+ICPV),
     &                  ZR(ICONTP+ICPG),ZR(IVARIP+ICPV),
     &                  DSIDEP)
C
        ELSE
           CALL UTDEBM ('F','DXGLRC','LA LOI DE COMPORTEMENT SUIVANTE')
           CALL UTIMPK ('S',' N''EXISTE PAS POUR LA '//
     &                  'MODELISATION DKTG : ',1,COMPOR(1))
           CALL UTFINM ()
        ENDIF
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
     +                       BM(K,2* (INO-1)+1)*N(K)*POIDS
              BTSIG(2,INO) = BTSIG(2,INO) +
     +                       BM(K,2* (INO-1)+2)*N(K)*POIDS
              BTSIG(3,INO) = BTSIG(3,INO) +
     +                       BF(K,3* (INO-1)+1)*M(K)*POIDS
              BTSIG(5,INO) = BTSIG(5,INO) +
     +                       BF(K,3* (INO-1)+2)*M(K)*POIDS
              BTSIG(4,INO) = BTSIG(4,INO) -
     +                       BF(K,3* (INO-1)+3)*M(K)*POIDS
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
C
      END
