      SUBROUTINE       CONFAC(TYPMA,FT,NBFT)
      IMPLICIT NONE
      INTEGER          FT(12,3),NBFT
      CHARACTER*8      TYPMA
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 30/03/2004   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                       RENVOIE LA MATRICE DE CONNECTIVITÉ DES 
C                 FACES TRIANGULAIRES D'UNE MAILLE DE TYPE TYPMA
C
C    ENTREE :
C              TYPMA : TYPE DE LA MAILLE
C
C    SORTIE : 
C              FT   : MATRICE DE CONNECTIVITÉ DES FACES TRIANGULAIES
C              NBFT : NOMBRE DE FACES TRIANGULAIRES
C     ------------------------------------------------------------------
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C     
      INTEGER       I,J  
C ----------------------------------------------------------------------

      CALL JEMARQ()
     
      DO 100 I=1,12
        DO 110 J=1,3
          FT(I,J)=0
 110    CONTINUE
 100  CONTINUE    

      IF (TYPMA.EQ.'HEXA8') THEN
        NBFT=12
C       CONNECTIVITÉ DES FACES TRIANGLES POUR UNE MAILLE HEXA8
        FT(1,1)=1
        FT(1,2)=2
        FT(1,3)=3
        FT(2,1)=3
        FT(2,2)=4
        FT(2,3)=1
        FT(3,1)=1
        FT(3,2)=2
        FT(3,3)=6
        FT(4,1)=6
        FT(4,2)=5
        FT(4,3)=1
        FT(5,1)=3
        FT(5,2)=4
        FT(5,3)=8
        FT(6,1)=8
        FT(6,2)=7
        FT(6,3)=3
        FT(7,1)=5
        FT(7,2)=6
        FT(7,3)=7
        FT(8,1)=7
        FT(8,2)=8
        FT(8,3)=5
        FT(9,1)=1
        FT(9,2)=4
        FT(9,3)=8
        FT(10,1)=8
        FT(10,2)=5
        FT(10,3)=1
        FT(11,1)=2
        FT(11,2)=3
        FT(11,3)=7
        FT(12,1)=7
        FT(12,2)=6
        FT(12,3)=2         
      ELSEIF (TYPMA.EQ.'PENTA6') THEN
        NBFT=8
C       CONNECTIVITÉ DES FACES TRIANGLES POUR UNE MAILLE PENTA6
        FT(1,1)=1
        FT(1,2)=2
        FT(1,3)=3
        FT(2,1)=4
        FT(2,2)=5
        FT(2,3)=6
        FT(3,1)=1
        FT(3,2)=3
        FT(3,3)=6
        FT(4,1)=6
        FT(4,2)=4
        FT(4,3)=1
        FT(5,1)=2
        FT(5,2)=3
        FT(5,3)=6
        FT(6,1)=6
        FT(6,2)=5
        FT(6,3)=2
        FT(7,1)=1
        FT(7,2)=2
        FT(7,3)=5
        FT(8,1)=5
        FT(8,2)=4
        FT(8,3)=1
      ELSEIF (TYPMA.EQ.'TETRA4') THEN
        NBFT=4
C       CONNECTIVITÉ DES FACES TRIANGLES POUR UNE MAILLE TETRA4
        FT(1,1)=1
        FT(1,2)=3
        FT(1,3)=2
        FT(2,1)=2
        FT(2,2)=3
        FT(2,3)=4
        FT(3,1)=1
        FT(3,2)=4
        FT(3,3)=3
        FT(4,1)=1
        FT(4,2)=2
        FT(4,3)=4
      ELSE
        CALL UTMESS('F','CONFAC','MAILLE NI HEXA8 NI PENTA6 '//
     &      'NI TETRA4. LA MAILLE EST DE TYPE :'//TYPMA//'.') 
      ENDIF

      CALL JEDEMA()
      END
