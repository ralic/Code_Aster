      SUBROUTINE DKTVIT ( DEFG ,NORMAL, VIN )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 17/01/97   AUTEUR VABHHTS J.PELLET 
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
      COMPLEX*16   DEFG(*) , VIN
      REAL*8       NORMAL(3)
C     ------------------------------------------------------------------
C     VITESSES NORMALES MODALES DE L'ELEMENT DE PLAQUE DKT
C     ------------------------------------------------------------------
C     IN  DEFG   : DEFORMEE
C     IN  NORMAL : NORMALE  NORMEE A L'ELEMENT
C     OUT VIT    : DEFORMEE NORMALE AUX NOEUDS DANS LE REPERE DE 
C                  L ELEMENT
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      INTEGER    IE
      REAL*8     ZERO
      COMPLEX*16 CZERO,VIT(18),TMP(3)
C     --- INITIALISATIONS DIVERSES ---
      ZERO  = 0.D0
      CZERO = DCMPLX(ZERO,ZERO)
C
      DO 10 IE = 1 , 3
C        -----------------------------------------------------------
         VIT(1+6*(IE-1)) = NORMAL(1) * DEFG(1+6*(IE-1))
         VIT(2+6*(IE-1)) = NORMAL(2) * DEFG(2+6*(IE-1))
         VIT(3+6*(IE-1)) = NORMAL(3) * DEFG(3+6*(IE-1))
C        ------------------------------------------------------------
         VIT(4+6*(IE-1)) = CZERO
         VIT(5+6*(IE-1)) = CZERO
         VIT(6+6*(IE-1)) = CZERO
C        ------------------------------------------------------------
         TMP(IE) = VIT(1+6*(IE-1)) + VIT(2+6*(IE-1)) + VIT(3+6*(IE-1))
  10  CONTINUE
      VIN = (TMP(1) + TMP(2) + TMP(3))/3
      END
