      SUBROUTINE RCANGM ( NDIM, ANGMAS )
      IMPLICIT NONE
      INTEGER  NDIM
      REAL*8   ANGMAS(3)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 23/05/2005   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ......................................................................
C    - ORIENTATION DU MASSIF
C      
C      
C ......................................................................
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
      INTEGER  ICAMAS, IRET
      REAL*8   R8NNEM, R8DGRD
C     ------------------------------------------------------------------

      CALL TECACH ( 'NNO', 'PCAMASS', 1, ICAMAS, IRET )

      IF (IRET.EQ.0) THEN
         CALL R8INIR ( 3, 0.D0, ANGMAS ,1 )
         IF (ZR(ICAMAS).GT.0.D0) THEN
            ANGMAS(1) = ZR(ICAMAS+1)*R8DGRD()
            IF ( NDIM .EQ. 3 ) THEN
               ANGMAS(2) = ZR(ICAMAS+2)*R8DGRD()
               ANGMAS(3) = ZR(ICAMAS+3)*R8DGRD()
            ENDIF
         ENDIF
C
      ELSEIF (IRET.EQ.1) THEN
         CALL R8INIR ( 3, R8NNEM(), ANGMAS ,1 )
C
      ELSEIF (IRET.EQ.2) THEN
         CALL R8INIR ( 3, 0.D0, ANGMAS ,1 )
C
      ENDIF
C
      END
