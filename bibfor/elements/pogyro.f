      SUBROUTINE POGYRO(NOMTE,E,RHO,XNU,KLV,NL)
      IMPLICIT NONE
      CHARACTER*(*) NOMTE
      REAL*8 E, RHO, XNU,KLV(*)
C ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 23/10/2007   AUTEUR BOYERE E.BOYERE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     CALCULE LA MATRICE GYROSCOPIQUE DES ELEMENTS DE POUTRE

C IN  NOMTE : NOM DU TYPE ELEMENT
C             'MECA_POU_D_E'  'MECA_POU_D_T'  'MECA_POU_C_T'
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER ZI,NL
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

      CHARACTER*16 CH16
      INTEGER LSECT,LSECT2, LX, LRCOU, ISTRUC, ITYPE
      REAL*8 ZERO, UN, DEUX, TRIGOM
      REAL*8 G, EY, EZ, XL, RAD, ANG, ANGS2, XFL, XFLY, XFLZ
      REAL*8 A, XIY, XIZ, XJX, ALFAY, ALFAZ
C     ------------------------------------------------------------------

      ZERO = 0.D0
      UN = 1.D0
      DEUX = 2.D0
      G = E/ (DEUX* (UN+XNU))


C     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---

      CALL JEVECH('PCAGNPO','L',LSECT)
      LSECT = LSECT - 1
      ITYPE = NINT(ZR(LSECT+23))

C     --- SECTION INITIALE ---
      A = ZR(LSECT+1)
      XIY = ZR(LSECT+2)
      XIZ = ZR(LSECT+3)
      ALFAY = ZR(LSECT+4)
      ALFAZ = ZR(LSECT+5)
      XJX = ZR(LSECT+8)

C     --- SECTION FINALE ---
      LSECT2 = LSECT + 11
      EY = - (ZR(LSECT+6)+ZR(LSECT2+6))/DEUX
      EZ = - (ZR(LSECT+7)+ZR(LSECT2+7))/DEUX

C     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
      CALL JEVECH('PGEOMER','L',LX)
      LX = LX - 1
      XL = SQRT((ZR(LX+4)-ZR(LX+1))**2+ (ZR(LX+5)-ZR(LX+2))**2+
     &     (ZR(LX+6)-ZR(LX+3))**2)
      IF (XL.EQ.ZERO) THEN
        CH16 = ' ?????????'
        CALL U2MESK('F','ELEMENTS2_43',1,CH16(:8))
      END IF

      IF (NOMTE(1:12).EQ.'MECA_POU_D_E') THEN
C        --- POUTRE DROITE D'EULER A 6 DDL ---
        ISTRUC = 1
        ALFAY = ZERO
        ALFAZ = ZERO
      ELSE IF (NOMTE(1:12).EQ.'MECA_POU_D_T') THEN
C        --- POUTRE DROITE DE TIMOSKENKO A 6 DDL ---
        ISTRUC = 1
      ELSE IF (NOMTE(1:12).EQ.'MECA_POU_C_T') THEN
C        --- POUTRE COURBE DE TIMOSKENKO A 6 DDL ---
        ISTRUC = 1
        CALL JEVECH('PCAARPO','L',LRCOU)
        RAD = ZR(LRCOU)
        XFL = ZR(LRCOU+2)
        XFLY = XFL
        XFLZ = XFL
        IF (XFL.EQ.ZERO) THEN
          XFLY = ZR(LRCOU+4)
          XFLZ = ZR(LRCOU+6)
        END IF
        ANGS2 = TRIGOM('ASIN',XL/ (DEUX*RAD))
        ANG = ANGS2*DEUX
        XL = RAD*ANG
        XIY = XIY/XFLY
        XIZ = XIZ/XFLZ
      ELSE
        CH16 = NOMTE
        CALL U2MESK('F','ELEMENTS2_42',1,CH16)
      END IF


      IF (ITYPE.EQ.0) THEN
C        --- POUTRE DROITE A SECTION CONSTANTE ---
C RAJOUT NL POUR APPEL A PTGY01
        CALL PTGY01(KLV,NL,E,RHO,A,XL,XIY,XIZ,XJX,G,ALFAY,
     &                     ALFAZ,EY,EZ,ISTRUC)

      ELSE IF (ITYPE.EQ.1 .OR. ITYPE.EQ.2) THEN
C        --- POUTRE DROITE A SECTION VARIABLE (TYPE 1 OU 2) ---
        CH16 = NOMTE
        CALL U2MESK('F','ELEMENTS2_67',1,CH16)
      ELSE IF (ITYPE.EQ.10) THEN
C        --- POUTRE COURBE A SECTION CONSTANTE ---
        CH16 = NOMTE
        CALL U2MESK('F','ELEMENTS2_67',1,CH16)
      END IF
        
      END
