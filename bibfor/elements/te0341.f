      SUBROUTINE TE0341 ( OPTION , NOMTE )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)       OPTION , NOMTE
C     ------------------------------------------------------------------
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
C     ------------------------------------------------------------------
C     CALCULE POUR LES ELEMENTS DE POUTRE A 7 DDLS
C             - LA MATRICE DE MASSE ELEMENTAIRE
C             - LE VECTEUR ELEMENTAIRE M_GAMMA
C     ------------------------------------------------------------------
C IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
C        'MASS_MECA'     : CALCUL DE LA MATRICE DE MASSE
C        'M_GAMMA'       : CALCUL DU VECTEUR M_GAMMA
C IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
C        'MECA_POU_D_TG' : POUTRE DROITE SIMPLIFIEE A 7DDLS.
C     ------------------------------------------------------------------
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      PARAMETER                 (NBRES=3)
      INTEGER      IRET
      REAL*8       VALPAR,VALRES(NBRES)
      CHARACTER*2         CODRES(NBRES)
      CHARACTER*8  NOMPAR,NOMRES(NBRES)
      CHARACTER*16 CH16
      REAL*8       ZERO,UN,DEUX,E,G, PGL(3,3), MAT(105), MATP(78)
      REAL*8       A,XIY,XIZ,ALFAY,ALFAZ,EZ,EY,XL,TPG
      REAL*8       MATGV(105), MATG(14,14)
C     ------------------------------------------------------------------
      DATA NOMRES / 'E' , 'NU' , 'RHO' /
C     ------------------------------------------------------------------
      ZERO = 0.D0
      UN = 1.D0
      DEUX = 2.D0
C     ------------------------------------------------------------------
C
C     --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---
      CALL JEVECH('PMATERC','L',IMATE)
      DO 10 I = 1,NBRES
          VALRES(I) = ZERO
   10 CONTINUE
C
      NPG = 3
      CALL MOYTEM('RIGI',NPG,1,'+',VALPAR,IRET)
      
      NBPAR  = 1
      NOMPAR = 'TEMP'
C
      CALL RCVALA(ZI(IMATE),' ','ELAS',NBPAR,NOMPAR,VALPAR,NBRES,NOMRES,
     &              VALRES, CODRES, 'FM' )
C
      E   = VALRES(1)
      XNU = VALRES(2)
      RHO = VALRES(3)
      G   = E / ( DEUX * ( UN + XNU ) )
C
C     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
      CALL JEVECH('PCAGNPO','L',LSECT)
      LSECT = LSECT - 1
      A = ZR(LSECT+1)
      XIY = ZR(LSECT+2)
      XIZ = ZR(LSECT+3)
      ALFAY = ZR(LSECT+4)
      ALFAZ = ZR(LSECT+5)
      EY = -ZR(LSECT+6)
      EZ = -ZR(LSECT+7)
      ITYPE = 0
      ISTRUC = 1
      NNO = 2
      NC  = 7
C
C     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
      CALL JEVECH('PGEOMER','L',LX)
      LX = LX - 1
      XL = SQRT((ZR(LX+4)-ZR(LX+1))**2+ (ZR(LX+5)-ZR(LX+2))**2+
     &     (ZR(LX+6)-ZR(LX+3))**2)
      IF (XL.EQ.ZERO) THEN
          CH16 = ' ?????????'
          CALL U2MESK('F','ELEMENTS2_43',1,CH16(:8))
      ENDIF
C
C     --- RECUPERATION DES ORIENTATIONS ALPHA,BETA,GAMMA ---
      CALL JEVECH('PCAORIE','L',LORIEN)
C
C     --- CALCUL DES MATRICES ELEMENTAIRES ----
      IF ( OPTION .EQ. 'MASS_MECA' .OR.
     &     OPTION .EQ. 'M_GAMMA'   ) THEN
         KANL = 1
         DO 20 I = 1 , 78
            MATP(I) = 0.0D0
 20      CONTINUE
         CALL PTMA01 ( KANL,ITYPE,MATP,ISTRUC,RHO,E,A,A,XL,XIY,XIY,XIZ,
     &                 XIZ,G,ALFAY,ALFAY,ALFAZ,ALFAZ,EY,EZ )
         DO 100 I = 1 , 21
           MAT(I) = MATP(I)
 100     CONTINUE
         DO 102 I = 22 , 28
           MAT(I) = 0.D0
 102     CONTINUE
         DO 104 I = 29 , 34
           MAT(I) = MATP(I-7)
 104     CONTINUE
         MAT(35) = 0.D0
         DO 106 I = 36 , 42
           MAT(I) = MATP(I-8)
 106     CONTINUE
         MAT(43) = 0.D0
         DO 108 I = 44 , 51
           MAT(I) = MATP(I-9)
 108     CONTINUE
         MAT(52) = 0.D0
         DO 110 I = 53 , 61
           MAT(I) = MATP(I-10)
 110     CONTINUE
         MAT(62) = 0.D0
         DO 112 I = 63 , 72
           MAT(I) = MATP(I-11)
 112     CONTINUE
         MAT(73) = 0.D0
         DO 114 I = 74 , 84
           MAT(I) = MATP(I-12)
 114     CONTINUE
         MAT(85) = 0.D0
         DO 116 I = 86 , 91
           MAT(I) = MATP(I-13)
 116     CONTINUE
         DO 118 I = 92 , 105
           MAT(I) = 0.D0
 118     CONTINUE
C
C        --- PASSAGE DU REPERE LOCAL AU REPERE GLOBAL ---
         CALL MATROT ( ZR(LORIEN) , PGL )
C
         IF ( OPTION .EQ. 'MASS_MECA' ) THEN
            CALL JEVECH ( 'PMATUUR', 'E', LMAT )
            CALL UTPSLG ( NNO, NC, PGL, MAT, ZR(LMAT) )
C
         ELSEIF ( OPTION .EQ. 'M_GAMMA' ) THEN
            CALL JEVECH('PDEPLAR','L',IACCE)
            CALL JEVECH('PVECTUR','E',IVECT)
            CALL UTPSLG ( NNO, NC, PGL, MAT, MATGV  )
            CALL VECMA(MATGV,105,MATG,14)
            CALL PMAVEC('ZERO',14,MATG,ZR(IACCE),ZR(IVECT))
         ENDIF
C
      ELSE
          CH16 = OPTION
          CALL U2MESK('F','ELEMENTS2_47',1,CH16)
      ENDIF
C
      END
