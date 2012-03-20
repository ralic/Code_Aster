      SUBROUTINE DXEFFI ( OPTION, NOMTE, PGL, CONT, IND, EFFINT )
      IMPLICIT  NONE
      REAL*8              PGL(3,3), CONT(*), EFFINT(*)
      CHARACTER*16        OPTION, NOMTE
      INTEGER             IND
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 19/03/2012   AUTEUR LEBOUVIER F.LEBOUVIER 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     ------------------------------------------------------------------
C     IN  NOMTE  : NOM DE L'ELEMENT TRAITE
C     IN  XYZL   : COORDONNEES DES NOEUDS
C     IN  UL     : DEPLACEMENT A L'INSTANT T
C     IN  IND    : =6 : 6 CMP D'EFFORT PAR NOEUD
C     IN  IND    : =8 : 8 CMP D'EFFORT PAR NOEUD
C     OUT EFFINT : EFFORTS INTERNES
C     ------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER         ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8          ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16      ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL         ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8     ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                     ZK24
      CHARACTER*32                              ZK32
      CHARACTER*80                                       ZK80
      COMMON /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER  NDIM,NNO,NNOS,NPG,IPOIDS,ICOOPG,IVF,IDFDX,IDFD2,JGANO
      INTEGER  NBCON, NBCOU, NPGH, K, IPG, ICOU, IGAUH, ICPG,
     &         ICACOQ, JNBSPI
      REAL*8   HIC, H, ZIC, ZMIN, COEF, ZERO, DEUX, DISTN, COEHSD
      REAL*8   N(3), M(3), T(2)
C
      INTEGER MULTIC,INIV
      REAL*8 DF(3,3),DM(3,3),DMF(3,3),DC(2,2),DCI(2,2),DMC(3,2),DFC(3,2)
      REAL*8 T2EV(2,2),T2VE(2,2),T1VE(3,3)
      REAL*8 HM(3,3)
      REAL*8 D1I(2,2),D2I(2,4)
      LOGICAL COUPMF
C     ------------------------------------------------------------------
C
      CALL ELREF5(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,ICOOPG,
     &                                         IVF,IDFDX,IDFD2,JGANO)
C
      ZERO = 0.0D0
      DEUX = 2.0D0
C
C     RECUPERATION DES OBJETS &INEL ET DES CHAMPS PARAMETRES :
C     --------------------------------------------------------
      IF (NOMTE.NE.'MEDKTR3 ' .AND.
     &        NOMTE.NE.'MEDSTR3 ' .AND.
     &        NOMTE.NE.'MEDKQU4 ' .AND.
     &        NOMTE.NE.'MEDSQU4 ' .AND.
     &        NOMTE.NE.'MEQ4QU4 ' .AND.
     &        NOMTE.NE.'MET3TR3 ' ) THEN
         CALL U2MESK('F','ELEMENTS_34',1,NOMTE)
      END IF

      CALL JEVECH ( 'PNBSP_I', 'L', JNBSPI )
      NBCON = 6
      NBCOU = ZI(JNBSPI-1+1)
      IF (NBCOU.LE.0) CALL U2MESS('F','ELEMENTS_46')
C
C
      MULTIC = 0
      IF (OPTION.EQ.'FORC_NODA') THEN
C     ----- CARACTERISTIQUES DES MATERIAUX --------
        CALL DXMATE('RIGI',DF,DM,DMF,DC,DCI,DMC,DFC,NNO,PGL,MULTIC,
     &                                            COUPMF,T2EV,T2VE,T1VE)
      ENDIF

C     -- GRANDEURS GEOMETRIQUES :
C     ---------------------------
      NPGH = 3
      IF (MULTIC.EQ.0) THEN
        CALL JEVECH ( 'PCACOQU', 'L', ICACOQ )
        H = ZR(ICACOQ)
        HIC = H/NBCOU
        DISTN = ZR(ICACOQ+4)
        ZMIN = -H/DEUX + DISTN
      ENDIF

      CALL R8INIR ( 32, ZERO, EFFINT, 1 )

C===============================================================
C     -- BOUCLE SUR LES POINTS DE GAUSS DE LA SURFACE:
C     -------------------------------------------------
      DO 100, IPG = 1,NPG
         CALL R8INIR ( 3, ZERO, N, 1 )
         CALL R8INIR ( 3, ZERO, M, 1 )
         CALL R8INIR ( 2, ZERO, T, 1 )

         DO 110, ICOU = 1,NBCOU
            DO 120, IGAUH = 1,NPGH
               ICPG = NBCON*NPGH*NBCOU*(IPG-1) + NBCON*NPGH*(ICOU-1) +
     &                                           NBCON*(IGAUH-1)

               IF (IGAUH.EQ.1) THEN
                  ZIC = ZMIN + (ICOU-1)*HIC
                  COEF = 1.D0/3.D0
               ELSE IF (IGAUH.EQ.2) THEN
                  ZIC = ZMIN + HIC/2.D0 + (ICOU-1)*HIC
                  COEF = 4.D0/3.D0
               ELSE
                  ZIC = ZMIN + HIC + (ICOU-1)*HIC
                  COEF = 1.D0/3.D0
               END IF
               IF (MULTIC.GT.0) THEN
                 INIV = IGAUH - 2
                 CALL DXDMUL(.FALSE.,ICOU,INIV,T1VE,T2VE,HM,D1I,D2I,
     &                       ZIC,HIC)
               ENDIF
C
C         -- CALCUL DES EFFORTS GENERALISES DANS L'EPAISSEUR (N, M ET T)
C         --------------------------------------------------------------
               COEHSD = COEF*HIC/2.D0
               N(1) = N(1) + COEHSD*CONT(ICPG+1)
               N(2) = N(2) + COEHSD*CONT(ICPG+2)
               N(3) = N(3) + COEHSD*CONT(ICPG+4)
               M(1) = M(1) + COEHSD*ZIC*CONT(ICPG+1)
               M(2) = M(2) + COEHSD*ZIC*CONT(ICPG+2)
               M(3) = M(3) + COEHSD*ZIC*CONT(ICPG+4)
               T(1) = T(1) + COEHSD*CONT(ICPG+5)
               T(2) = T(2) + COEHSD*CONT(ICPG+6)
               
 120        CONTINUE
 110     CONTINUE
C
            DO 140,K = 1,3
               EFFINT((IPG-1)*IND+K)   = N(K)
               EFFINT((IPG-1)*IND+K+3) = M(K)
 140        CONTINUE
            IF (IND.GT.6) THEN
               EFFINT((IPG-1)*IND+7) = T(1)
               EFFINT((IPG-1)*IND+8) = T(2)
            ENDIF
C
 100  CONTINUE
C
      END
