      SUBROUTINE TE0340(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 09/03/99   AUTEUR VABHHTS J.PELLET 
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
C INSPI TE0140
C     CALCULE LA MATRICE DE RIGIDITE ELEMENTAIRE DES ELEMENTS DE POUTRE
C     A 7 DDLS
C     ------------------------------------------------------------------
C IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
C        'RIGI_MECA '     : CALCUL DE LA MATRICE DE RIGIDITE
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
      PARAMETER                 (NBRES=2)
      REAL*8       VALPAR,VALRES(NBRES)
      CHARACTER*2         CODRES(NBRES)
      CHARACTER*8  NOMPAR,NOMRES(NBRES)
      CHARACTER*16 CH16
      REAL*8       ZERO,UN,DEUX,E,NU,G, PGL(3,3), MAT(105)
      REAL*8       A,XIY,XIZ,ALFAY,ALFAZ,XJX,XJG,EZ,EY,XL
C     ------------------------------------------------------------------
      DATA NOMRES/'E','NU'/
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
      CALL TECACH(.TRUE.,.FALSE.,'PTEMPER',1,ITEMPE)
      IF ( ITEMPE .EQ. 0 ) THEN
         NBPAR  = 0
         NOMPAR = ' '
         VALPAR = ZERO
      ELSE
         NBPAR  = 1
         NOMPAR = 'TEMP'
         VALPAR = ZR(ITEMPE)
      ENDIF
C
      CALL RCVALA ( ZI(IMATE),'ELAS',NBPAR,NOMPAR,VALPAR,NBRES,NOMRES,
     +              VALRES, CODRES, 'FM' )
C
      E  = VALRES(1)
      NU = VALRES(2)
      G = E/ (DEUX* (UN+NU))
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
      XJX = ZR(LSECT+8)
      XJG = ZR(LSECT+12)
      NNO = 2
      NC  = 7
C
C     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
      CALL JEVECH('PGEOMER','L',LX)
      LX = LX - 1
      XL = SQRT((ZR(LX+4)-ZR(LX+1))**2+ (ZR(LX+5)-ZR(LX+2))**2+
     +     (ZR(LX+6)-ZR(LX+3))**2)
      IF (XL.EQ.ZERO) THEN
          CH16 = ' ?????????'
          CALL UTMESS('F','ELEMENTS DE POUTRE (TE0340)',
     +                'NOEUDS CONFONDUS POUR UN ELEMENT: '//CH16(:8))
      ENDIF
C
      DO 20 I = 1 , 105
         MAT(I) = 0.D0
 20   CONTINUE
C
C     --- CALCUL DES MATRICES ELEMENTAIRES ----
      IF ( OPTION(1:9) .EQ. 'RIGI_MECA' ) THEN
         CALL PTKA21(MAT,E,A,XL,XIY,XIZ,XJX,XJG,G,ALFAY,ALFAZ,EY,EZ)
C
C        --- RECUPERATION DES ORIENTATIONS ALPHA,BETA,GAMMA ---
         CALL JEVECH('PCAORIE','L',LORIEN)
C
C        --- PASSAGE DU REPERE LOCAL AU REPERE GLOBAL ---
         CALL MATROT ( ZR(LORIEN) , PGL )
         CALL JEVECH ( 'PMATUUR', 'E', LMAT )
         CALL UTPSLG ( NNO, NC, PGL, MAT, ZR(LMAT) )
C
      ELSE
          CH16 = OPTION
          CALL UTMESS('F','ELEMENTS DE POUTRE (TE0340)',
     +                'L''OPTION "'//CH16//'" EST INCONNUE')
      ENDIF
C
      END
