      SUBROUTINE DIMAX1 ( JVEC1, JVEC2, NBP1, NBP2, DISMAX, CU1MAX,
     &                    CV1MAX, CU2MAX, CV2MAX)
      IMPLICIT   NONE
      INTEGER             JVEC1, JVEC2, NBP1, NBP2
      REAL*8              DISMAX, CU1MAX, CV1MAX, CU2MAX, CV2MAX
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 24/06/2002   AUTEUR F1BHHAJ J.ANGLES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE F1BHHAJ J.ANGLES
C ---------------------------------------------------------------------
C BUT: ENTRE DEUX LISTES DE POINTS, DETERMINER LES DEUX POINTS QUI SONT
C     LE PLUS ELOIGNES.
C ---------------------------------------------------------------------
C ARGUMENTS :
C     JVEC1   : IN  : ADRESSE DU VECTEUR CONTENANT LES POINTS DU 
C                     PREMIER GROUPE DE POINTS.
C     JVEC2   : IN  : ADRESSE DU VECTEUR CONTENANT LES POINTS DU 
C                     SECOND GROUPE DE POINTS.
C     NBP1    : IN  : NOMBRE DE POINTS DU PREMIER GROUPE DE POINTS.
C     NBP2    : IN  : NOMBRE DE POINTS DU SECOND GROUPE DE POINTS.
C     DISMAX  : OUT : DISTANCE ENTRE LES DEUX POINTS LES PLUS ELOIGNES.
C     CU1MAX  : OUT : COMPOSANTE U DU POINT LE PLUS ELOIGNE APPARTENANT
C                     AU PREMIER GROUPE.
C     CU1MAX  : OUT : COMPOSANTE V DU POINT LE PLUS ELOIGNE APPARTENANT
C                     AU PREMIER GROUPE.
C     CU2MAX  : OUT : COMPOSANTE U DU POINT LE PLUS ELOIGNE APPARTENANT
C                     AU SECOND GROUPE.
C     CU2MAX  : OUT : COMPOSANTE V DU POINT LE PLUS ELOIGNE APPARTENANT
C                     AU SECOND GROUPE.
C     -----------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ------------------------------------------------------------------
      INTEGER    I, J
      REAL*8     CU1, CV1, CU2, CV2, DIST
C     ------------------------------------------------------------------
C
C234567                                                              012
      CALL JEMARQ()

      DISMAX = 0.0D0

      DO 10 I=1, NBP1
         CU1 = ZR(JVEC1 + (I-1)*2)
         CV1 = ZR(JVEC1 + (I-1)*2 + 1)

         DO 20 J=1, NBP2
            CU2 = ZR(JVEC2 + (J-1)*2)
            CV2 = ZR(JVEC2 + (J-1)*2 + 1)
            DIST = SQRT((CU1 - CU2)**2 + (CV1 - CV2)**2)

            IF ( DIST .GT. DISMAX ) THEN
               DISMAX = DIST
               CU1MAX = CU1
               CV1MAX = CV1
               CU2MAX = CU2
               CV2MAX = CV2
            ENDIF

 20      CONTINUE
 10   CONTINUE

      CALL JEDEMA()
      END
