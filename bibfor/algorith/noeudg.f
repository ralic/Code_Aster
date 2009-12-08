      FUNCTION NOEUDG ( ICOOR, NBNO, JVNO, XG, YG, ZG )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 25/10/2004   AUTEUR CIBHHLV L.VIVAN 
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
      IMPLICIT   NONE
      INTEGER             ICOOR, NBNO,JVNO, I, NOEUDG
      REAL*8              TAB(NBNO),TAB2(NBNO), XG, YG, ZG
      REAL*8              PPTITE, X, Y, Z, MIN, PPT
C     ----- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------
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
      CHARACTER*32       JEXNOM
C     ----- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

      DO 40 I = 0 , NBNO-1
         X = ZR(ICOOR+3*(ZI(JVNO+I)-1)+1-1)
         Y = ZR(ICOOR+3*(ZI(JVNO+I)-1)+2-1)
         Z = ZR(ICOOR+3*(ZI(JVNO+I)-1)+3-1)
         TAB(I+1) = SQRT((X-XG)*(X-XG)+(Y-YG)*(Y-YG)+(Z-ZG)*(Z-ZG))
         TAB2(I+1) = SQRT((X-XG)*(X-XG)+(Y-YG)*(Y-YG)+(Z-ZG)*(Z-ZG))
 40   CONTINUE
      PPT = PPTITE(1,NBNO,TAB)
 
      DO 50 I = 1 , NBNO
         IF (TAB2(I).EQ.PPT)  NOEUDG = I
 50   CONTINUE

      END
