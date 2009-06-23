      SUBROUTINE INFDIS(CARA,IVALE,RVALE)
      IMPLICIT NONE
      CHARACTER*4 CARA
      REAL*8      RVALE
      INTEGER     IVALE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 22/06/2009   AUTEUR FLEJOU J-L.FLEJOU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE FLEJOU J-L.FLEJOU
C --- ------------------------------------------------------------------
C
C              INTERROGE LA CARTE DES 'CINFDI' DES DISCRETS
C
C  IN
C     CARA : CLEF
C              REPK, REPM, REPA, SYMK, SYMM, SYMA, ETAK
C              SKMA
C  OUT
C     IVALE : SI REPK, REPM, REPA :
C              REPERE GLOBAL(=1) OU LOCAL(=2)
C           : SI SYMK, SYMM, SYMA :
C              MATRICE SYMETRIQUE(=1) OU NON-SYSMETRE(=2)
C           : SI SKMA
C              SI TOUTES LES MATRICES SONT SYMETRIQUE=3 SINON >3
C     RVALE : SI ETAK
C              COEFFICIENT AMORTISSEMENT HYSTERETIQUE
C
C --- ------------------------------------------------------------------
C     ELEMENTS CONCERNES :
C        MECA_DIS_TR_L    : SUR UNE MAILLE A 2 NOEUDS EN 3D
C        MECA_DIS_T_L     : SUR UNE MAILLE A 2 NOEUDS EN 3D
C        MECA_DIS_TR_N    : SUR UNE MAILLE A 1 NOEUD  EN 3D
C        MECA_DIS_T_N     : SUR UNE MAILLE A 1 NOEUD  EN 3D
C        MECA_2D_DIS_TR_L : SUR UNE MAILLE A 2 NOEUDS EN 2D
C        MECA_2D_DIS_T_L  : SUR UNE MAILLE A 2 NOEUDS EN 2D
C        MECA_2D_DIS_TR_N : SUR UNE MAILLE A 1 NOEUD  EN 2D
C        MECA_2D_DIS_T_N  : SUR UNE MAILLE A 1 NOEUD  EN 2D
C --- ------------------------------------------------------------------
C
C *** ************ DEBUT COMMUNS NORMALISES JEVEUX *********************
      INTEGER        ZI
      COMMON /IVARJE/ZI(1)
      REAL*8         ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16     ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL        ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8    ZK8
      CHARACTER*16          ZK16
      CHARACTER*24                  ZK24
      CHARACTER*32                          ZK32
      CHARACTER*80                                  ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C *** ************* FIN COMMUNS NORMALISES JEVEUX **********************
      INTEGER JDC

C     ORDRE DE STOCKAGE DANS LA CARTE
C                    0     1     2     3     4     5     6
C     CINFDI   = R   REPK  REPM  REPA  SYMK  SYMM  SYMA  ETAK
C
C     TYPE DU REPERE     ==> MASSES, AMORTISSEMENTS, RAIDEURS
C        REPM, REPA, REPK  : (=1 REPERE GLOBAL,  =2 REPERE LOCAL)
C     TYPE DE LA MATRICE ==> MASSES, AMORTISSEMENTS, RAIDEURS
C        SYMM, SYMA, SYMK  : (=1 SYMETRIQUE, =2 NON-SYMETRIQUE)
C     COEFFICIENT AMORTISSEMENT HYSTERETIQUE ==> RAIDEURS
C        ETAK
      CALL JEVECH('PCINFDI','L',JDC)
      IF      ( CARA .EQ. 'REPK' ) THEN
         IVALE = NINT(ZR(JDC))
      ELSE IF ( CARA .EQ. 'REPM' ) THEN
         IVALE = NINT(ZR(JDC+1))
      ELSE IF ( CARA .EQ. 'REPA' ) THEN
         IVALE = NINT(ZR(JDC+2))
      ELSE IF ( CARA .EQ. 'SYMK' ) THEN
         IVALE = NINT(ZR(JDC+3))
      ELSE IF ( CARA .EQ. 'SYMM' ) THEN
         IVALE = NINT(ZR(JDC+4))
      ELSE IF ( CARA .EQ. 'SYMA' ) THEN
         IVALE = NINT(ZR(JDC+5))
      ELSE IF ( CARA .EQ. 'ETAK' ) THEN
         RVALE = ZR(JDC+6)
      ELSE IF ( CARA .EQ. 'SKMA' ) THEN
         IVALE = NINT(ZR(JDC+3)+ZR(JDC+4)+ZR(JDC+5))
      ELSE
         CALL ASSERT( .FALSE. )
      ENDIF
      END
