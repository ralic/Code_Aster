      SUBROUTINE VEFCUR(VEC1,NBN,KNOM,VEC2,NBVALE,NOMNOE)
      IMPLICIT REAL*8 (A-H,O-Z)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 17/01/97   AUTEUR VABHHTS J.PELLET 
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
C   VERIFICATION DE LA DEFINITION DE LA FONCTION : EXISTENCE DES NOEUDS
C    ET ORDRE D'APPARITION DANS LA LISTE. LECTURE DU NOMBRE DE VALEURS
C                POUR LE DIMENSIONNEMENT DU .VALE
C ----------------------------------------------------------------------
C  IN : VEC1    : I  LISTE DES NUMEROS DE NOEUDS (ABS_CURV)
C  IN : NBN     :    DIMENSION DE VEC1
C  IN : KNOM    : K8 NOM DES NOEUDS 
C  OUT: VEC2    : I  POINTEURS D INDICE DE NOEUDS
C  IN : NBVALE  :    DIMENSION DES VECTEURS KNOM ET VEC2
C ----------------------------------------------------------------------
C     ----------- COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER       ZI
      COMMON/IVARJE/ZI(1)
      REAL*8        ZR
      COMMON/RVARJE/ZR(1)
      COMPLEX*16    ZC
      COMMON/CVARJE/ZC(1)
      LOGICAL       ZL
      COMMON/LVARJE/ZL(1)
      CHARACTER*8   ZK8
      CHARACTER*16         ZK16
      CHARACTER*24                 ZK24
      CHARACTER*32                         ZK32
      CHARACTER*80                                 ZK80
      COMMON/KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     ----------- FIN COMMUNS NORMALISES  JEVEUX  ----------------------
      INTEGER      NBN,VEC1(NBN),NBVALE,VEC2(NBVALE)
      CHARACTER*8  KNOM(NBVALE), NOMND
      CHARACTER*19 CMD
      CHARACTER*24 NOMNOE
      CHARACTER*32 JEXNOM
C     ------------------------------------------------------------------
C
      CMD = 'DEFI_FONCTION'
C
      DO 10 I = 1,NBVALE
        NOMND = KNOM(I)
        CALL JENONU(JEXNOM(NOMNOE,NOMND),NUMN)
C
        DO 20 JJ = 1,NBN
          IF (VEC1(JJ) .EQ. NUMN) THEN
            VEC2(I) = JJ
            IT = 1
          ENDIF
  20    CONTINUE
        IF (IT .NE. 1) THEN
          CALL UTMESS('F',CMD,'IL EXISTE AU MOINS UN NOEUD QUI'//
     +       ' N APPARTIENT PAS AU GROUPE DE MAILLES.')
        ENDIF
        IT = 0
  10  CONTINUE
      DO 30 I = 1,NBVALE
        JP = VEC2(I)
        JI = I   
        DO 40 JJ = I,NBVALE
          IF (VEC2(JJ) .LT. JP) THEN
            JI = JJ
            JP = VEC2(JJ)
          ENDIF
   40   CONTINUE
        VEC2(JI) = VEC2(I) 
        VEC2(I) = JP
   30 CONTINUE
      END
