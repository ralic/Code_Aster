      INTEGER FUNCTION ENTCOD ( NEC, IADM, M1, K, L )
      IMPLICIT REAL*8 (A-H,O-Z)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 11/01/2005   AUTEUR CIBHHLV L.VIVAN 
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
C-----------------------------------------------------------------------
C     IN:
C     MODE: MODE_LOCAL DE TYPE CHNO,VECT,OU MATR.
C     NEC : NBRE D ENTIERS POUR LA GRANDEUR
C     K : NUMERO DE NOEUD ( LOCAL ) ; L : NUMERO D ENTIER CODE
C     OUT:
C     ENTCOD: KEME ENTIER CODE.
C
C-----------------------------------------------------------------------
C ----- COMMUNS NORMALISES  JEVEUX
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C-----------------------------------------------------------------------
C
      CHARACTER*8  K8B1, K8B2, K8B3
C
      N1 = ZI(IADM+3)
      IF (N1.GT.10000) THEN
         N2 = N1 - 10000
         IF (K.GT.N2) THEN
            CALL CODENT ( M1, 'D', K8B1 )
            CALL CODENT ( N2, 'D', K8B2 )
            CALL CODENT ( K , 'D', K8B3 )
            CALL UTMESS('F',' ENTCOD',' POUR LE MODE '//K8B1//
     +                 ' NOMBRE DE POINTS '//K8B2//' < ARGUMENT K : '//
     +                  K8B3)
         END IF
         IAD = 4 + NEC* (K-1) + L
      ELSE
         IAD = 4 + L
      END IF
      ENTCOD = ZI(IADM+IAD-1)
C
      END
