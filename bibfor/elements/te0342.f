      SUBROUTINE TE0342(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 16/10/2007   AUTEUR SALMONA L.SALMONA 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*) OPTION,NOMTE
C     ------------------------------------------------------------------
C     CALCUL
C       - DU VECTEUR ELEMENTAIRE EFFORT GENERALISE,
C     POUR LES ELEMENTS DE POUTRE DE TIMOSHENKO AVEC GAUCHISSEMENT.
C     ------------------------------------------------------------------
C IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
C        'SIEF_ELGA_DEPL'
C IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
C        'MECA_POU_D_TG': POUTRE DROITE DE TIMOSHENKO AVEC GAUCHISSEMENT
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
      PARAMETER   (       NBRES=2)
      INTEGER      IRET
      REAL*8       VALRES(NBRES)
      CHARACTER*2  CODRES(NBRES)
      CHARACTER*8  NOMPAR,NOMRES(NBRES)
      CHARACTER*16 CH16
      REAL*8       NU, TPG
      REAL*8       B(7,14)
      REAL*8       PGL(14,14), DEPL(14), DEPGLO(14)
      REAL*8       EPSGEN(7), SIGGEN(3,7)
C     ------------------------------------------------------------------
      DATA NOMRES / 'E' , 'NU' /
C     ------------------------------------------------------------------
C

C
C --- INITIALISATIONS :
C     ---------------
      ZERO  =  0.0D0
      UN    =  1.0D0
      DEUX  =  2.0D0
      DOUZE = 12.0D0
C
      CALL R8INIR(7*14,ZERO,B,1)
      CALL R8INIR(7*3,ZERO,SIGGEN,1)
C
      NBPAR = 0
      NOMPAR = '  '
      VALPAR = 0.D0
C
      DO 10 I = 1,NBRES
          VALRES(I) = ZERO
   10 CONTINUE
C
C --- RECUPERATION DE LA TEMPERATURE :
C     -----------------------------------------------
      NPG = 3
      CALL MOYTEM('RIGI',NPG,1,'+',VALPAR,IRET)

      NBPAR  = 1
      NOMPAR = 'TEMP'
C
C --- RECUPERATION ET INTERPOLATION DES CARACTERISTIQUES MATERIAUX :
C     ------------------------------------------------------------
      CALL JEVECH('PMATERC','L',LMATER)
C
      CALL RCVALB('RIGI',NPG,1,'+',ZI(LMATER),' ', 'ELAS', 
     &             NBPAR, NOMPAR, VALPAR, 
     &             NBRES, NOMRES, VALRES, CODRES, 'FM' )
C
      E     = VALRES(1)
      NU    = VALRES(2)
      G = E / ( DEUX * ( UN + NU ) )
C
C --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS :
C     --------------------------------------------------------
      CALL JEVECH('PCAGNPO','L',LSECT)
C
      LSECT = LSECT - 1
      A     = ZR(LSECT+1)
      XIY   = ZR(LSECT+2)
      XIZ   = ZR(LSECT+3)
      ALFAY = ZR(LSECT+4)
      ALFAZ = ZR(LSECT+5)
      XJX   =  ZR(LSECT+8)
      XJG   =  ZR(LSECT+12)
      NNO   = 2
      NC    = 7
C
C --- RECUPERATION DES COORDONNEES DES NOEUDS
C --- ET CALCUL DE LA LONGUEUR DE LA POUTRE :
C     -------------------------------------
      CALL JEVECH('PGEOMER','L',LX)
C
      LX = LX - 1
      XL = SQRT((ZR(LX+4)-ZR(LX+1))**2+ (ZR(LX+5)-ZR(LX+2))**2+
     &     (ZR(LX+6)-ZR(LX+3))**2)
C
      IF (XL.EQ.0.D0) THEN
          CH16 = ' ?????????'
          CALL U2MESK('F','ELEMENTS2_43',1,CH16(:8))
      END IF
C
      XL2 = XL*XL
C
C --- CALCUL DES COEFFICIENTS D'INFLUENCE DU CISAILLEMENT TRANSVERSE :
C     --------------------------------------------------------------
      PHIY = E*XIZ*DOUZE*ALFAY/ (XL2*G*A)
      PHIZ = E*XIY*DOUZE*ALFAZ/ (XL2*G*A)
C
C --- RECUPERATION DES ORIENTATIONS ALPHA,BETA,GAMMA  :
C     ----------------------------------------------
      CALL JEVECH('PCAORIE','L',LORIEN)
C
C --- CONSTRUCTION DE LA MATRICE DE PASSAGE PGL DU REPERE GLOBAL
C --- AU REPERE LOCAL  :
C     ---------------
      CALL MATROT ( ZR(LORIEN) , PGL )
C
C --- RECUPERATION DU CHAMP DE DEPLACEMENT SUR L'ELEMENT :
C     --------------------------------------------------
      CALL JEVECH('PDEPLAR','L',JDEPL)
C
      DO 20 I = 1,14
         DEPGLO(I) = ZR(JDEPL+I-1)
 20   CONTINUE
C
C --- PASSAGE DES DEPLACEMENTS DU REPERE GLOBAL AU REPERE LOCAL :
C     ---------------------------------------------------------
      CALL UTPVGL ( NNO, NC, PGL, DEPGLO, DEPL )
C
C --- BOUCLE SUR LES POINTS DE GAUSS :
C     ------------------------------
      DO 30 IGAU = 1,3
C
C --- INITIALISATION :
C     ---------------
        CALL R8INIR(7,ZERO,EPSGEN,1)
C
C --- CALCUL DE LA MATRICE (B) RELIANT LES DEFORMATIONS GENERALISEES
C --- (DU/DX,GAMAXY,GAMAXZ,D(TETAX)/DX,D(TETAY)/DX,D(TETAZ/DX,D(GRX)/DX)
C --- AUX DEPLACEMENTS :
C     ----------------
         CALL JSD1FF(IGAU,XL,PHIY,PHIZ,B)
C
C --- CALCUL DES DEFORMATIONS GENERALISEES AU POINT D'INTEGRATION
C --- COURANT :
C     -------
         DO 40 I = 1, 7
            DO 50 J = 1, 14
               EPSGEN(I) = EPSGEN(I) + B(I,J)*DEPL(J)
 50         CONTINUE
 40     CONTINUE
C
C --- CALCUL DES EFFORTS GENERALISES AU POINT D'INTEGRATION
C --- COURANT :
C     -------
         SIGGEN(IGAU,1) =       E*A*EPSGEN(1)
         SIGGEN(IGAU,2) = ALFAY*G*A*EPSGEN(2)
         SIGGEN(IGAU,3) = ALFAZ*G*A*EPSGEN(3)
         SIGGEN(IGAU,4) =     XJX*G*EPSGEN(4)
         SIGGEN(IGAU,5) =     E*XIY*EPSGEN(5)
         SIGGEN(IGAU,6) =     E*XIZ*EPSGEN(6)
         SIGGEN(IGAU,7) =     E*XJG*EPSGEN(7)
C
 30   CONTINUE
C
C --- RECUPERATION ET AFFECTATION DU VECTEUR DES EFFORTS
C --- GENERALISES EN SORTIE :
C     ---------------------
      IF (OPTION.EQ.'SIEF_ELGA_DEPL') THEN
          CALL JEVECH('PCONTRR','E',JEFFO)
      ELSE
          CH16 = OPTION
          CALL U2MESK('F','ELEMENTS3_27',1,CH16)
      ENDIF
C
      K = 0
      DO 60 IGAU = 1, 3
         DO 70 I = 1, 7
            K = K + 1
            ZR(JEFFO+K-1) = SIGGEN(IGAU,I)
 70     CONTINUE
 60   CONTINUE
C
      END
