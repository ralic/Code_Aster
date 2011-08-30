      SUBROUTINE TE0235(OPTION,NOMTE)
      IMPLICIT NONE
      CHARACTER*(*) OPTION,NOMTE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/08/2011   AUTEUR DESROCHE X.DESROCHES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C     CALCULE LA MATRICE DE RAIDEUR CENTRIFUGE DES ELEMENTS DE POUTRE
C     AVEC GAUCHISSEMENT (MULTIFIBRE ON NON)
C     ------------------------------------------------------------------
C IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
C       'RIGI_MECA_RO'      : CALCUL DE LA MATRICE DE RAIDEUR CENTRIFUGE
C IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
C       'MECA_POU_D_TG' : POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
C       'MECA_POU_D_TGM': POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
C                         MULTI-FIBRES SECTION CONSTANTE
C     ------------------------------------------------------------------
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

      INTEGER NBRES
      PARAMETER (NBRES=6)
      REAL*8 VALRES(NBRES),VALPAR,R8B,A2,XIY2,XIZ2,ALFAY2,ALFAZ2
      INTEGER CODRES(NBRES)
      CHARACTER*8 NOMPAR,NOMRES(NBRES),NOMAIL
      CHARACTER*16 CH16
      INTEGER I, LMATER, IRET, NBPAR,J
      INTEGER LORIEN, LMAT
      INTEGER NNO, NC
      INTEGER ITYPE, LSECT, LX,IADZI,IAZK24,IROTA,LSECT2
      REAL*8  OMEGA(3),OMEGL(3),S
      REAL*8  XL, RAD, ANGS2
      REAL*8  ZERO, UN, DEUX, ABSMOY, ANGARC, TRIGOM
      REAL*8  E, G, XNU, RHO, RHOS, RHOFI, RHOFE, CM, PHIE, PHII
      REAL*8  A, XIY, XIZ, ALFAY, ALFAZ
      REAL*8  PGL(3,3),MLV(105),MATP1(105)
C     ------------------------------------------------------------------
      DATA NOMRES/'E','NU','RHO','RHO_F_IN','RHO_F_EX','CM'/
C     ------------------------------------------------------------------
      ZERO = 0.D0
      UN   = 1.D0
      DEUX = 2.D0
C     ------------------------------------------------------------------

C     --- CARACTERISTIQUES DES ELEMENTS
C
      NNO    = 2
      NC     = 7
      ITYPE  = 0
C
C     --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---

      DO 10 I = 1,NBRES
         VALRES(I) = ZERO
10    CONTINUE

      CALL JEVECH('PMATERC','L',LMATER)

      CALL RCVALA(ZI(LMATER),' ','ELAS',0,' ',R8B,3,
     &            NOMRES,VALRES,CODRES,1)
      E = VALRES(1)
      XNU = VALRES(2)
      RHO = VALRES(3)
      G   = E / ( DEUX * ( UN + XNU ) )
C     --- CARACTERISTIQUES GENERALES DES SECTIONS ---
      CALL JEVECH('PCAGNPO','L',LSECT)
      LSECT = LSECT - 1
      A = ZR(LSECT+1)
      XIY = ZR(LSECT+2)
      XIZ = ZR(LSECT+3)
      ALFAY = ZR(LSECT+4)
      ALFAZ = ZR(LSECT+5)
C     --- COORDONNEES DES NOEUDS ---
      CALL JEVECH('PGEOMER','L',LX)
      LX = LX - 1
      XL = SQRT( (ZR(LX+4)-ZR(LX+1))**2 +
     &           (ZR(LX+5)-ZR(LX+2))**2 +
     &           (ZR(LX+6)-ZR(LX+3))**2)
      IF (XL.EQ.ZERO) THEN
        CALL TECAEL(IADZI,IAZK24)
        NOMAIL = ZK24(IAZK24-1+3)(1:8)
        CALL U2MESK('F','ELEMENTS2_43',1,NOMAIL)
      ENDIF

C     --- RECUPERATION DES ORIENTATIONS ---
      CALL JEVECH('PCAORIE','L',LORIEN)
C     --- RECUPERATION DU VECTEUR ROTATION ---
      CALL JEVECH('PROTATR','L',IROTA)
      OMEGA(1) = ZR(IROTA+1)*ZR(IROTA)
      OMEGA(2) = ZR(IROTA+2)*ZR(IROTA)
      OMEGA(3) = ZR(IROTA+3)*ZR(IROTA)
      CALL MATROT(ZR(LORIEN),PGL)
      DO 1 I=1,3
      S=0.D0
        DO 2 J=1,3
          S=S+PGL(I,J)*OMEGA(J)
2       CONTINUE
      OMEGL(I)=S
1     CONTINUE

CCC     --- CALCUL DE LA MATRICE DE MASSE LOCALE ---
         DO 20 I = 1 , 105
            MATP1(I) = 0.0D0
20       CONTINUE
C        --- POUTRE DROITE SECTION CONSTANTE OU VARIABLE (1 OU 2)
        CALL PORIRO(ITYPE,MATP1,RHO,OMEGL,E,A,A,XL,XIY,XIY,XIZ,
     &              XIZ,G,ALFAY,ALFAY,ALFAZ,ALFAZ)

         DO 100 I = 1 , 21
            MLV(I) = MATP1(I)
100      CONTINUE
         DO 102 I = 22 , 28
            MLV(I) = 0.D0
102      CONTINUE
         DO 104 I = 29 , 34
            MLV(I) = MATP1(I-7)
104      CONTINUE
         MLV(35) = 0.D0
         DO 106 I = 36 , 42
            MLV(I) = MATP1(I-8)
106      CONTINUE
         MLV(43) = 0.D0
         DO 108 I = 44 , 51
            MLV(I) = MATP1(I-9)
108      CONTINUE
         MLV(52) = 0.D0
         DO 110 I = 53 , 61
            MLV(I) = MATP1(I-10)
110      CONTINUE
         MLV(62) = 0.D0
         DO 112 I = 63 , 72
            MLV(I) = MATP1(I-11)
112      CONTINUE
         MLV(73) = 0.D0
         DO 114 I = 74 , 84
            MLV(I) = MATP1(I-12)
114      CONTINUE
         MLV(85) = 0.D0
         DO 116 I = 86 , 91
            MLV(I) = MATP1(I-13)
116      CONTINUE
         DO 118 I = 92 , 105
            MLV(I) = 0.D0
118      CONTINUE


      CALL JEVECH('PMATUUR','E',LMAT)

      CALL UTPSLG(NNO,NC,PGL,MLV,ZR(LMAT))

      END
