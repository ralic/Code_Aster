      SUBROUTINE EXCENT ( OPTION, NOMTE, NNO, TENS , ICOMPX )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 15/06/2004   AUTEUR MABBAS M.ABBAS 
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
      CHARACTER*16 OPTION,NOMTE
      INTEGER  NNO, ICOMPX
      REAL*8   TENS(32)
C     ------------------------------------------------------------------
C ---    PRISE EN COMPTE DE L'EXCENTREMENT SI ON CALCULE LES 
C ---    EFFORTS GENERALISES SUR UN FEUILLET DE REFERENCE DIFFERENT
C ---    DU FEUILLET DU MAILLAGE (I.E. EN PEAU SUP, INF OU MOY)
C     ------------------------------------------------------------------
C
C     ------------------------------------------------------------------
C     ----- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
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
C
      INTEGER  IPLAN,JCARA,JPLAN
      LOGICAL GRILLE
      REAL*8   DIST,EPAIS,EXCEN,UNDEMI,ZERO
C
      ZERO   = 0.0D0
      UNDEMI = 0.50D0
      EPAIS  = ZERO
      EXCEN  = ZERO
      DIST   = ZERO
      IPLAN  = 0
C
      IF (NOMTE(1:8).EQ.'MEGRDKT ') THEN
        GRILLE = .TRUE.
      ELSE
        GRILLE = .FALSE.
      END IF
C
      IF (OPTION(1:9).EQ.'EFGE_ELNO') THEN
         IF (ICOMPX.EQ.0) THEN
            CALL JEVECH('PCACOQU','L',JCARA)
            CALL JEVECH('PFREQR ','L',JPLAN)
            EPAIS  = ZR(JCARA)
            IF ((.NOT.GRILLE)) THEN
               EXCEN = ZR(JCARA+5-1)
            ELSE
               EXCEN = ZR(JCARA+4-1)
            ENDIF
            IPLAN  = NINT(ZR(JPLAN))
         ENDIF
      ENDIF
C
      IF (IPLAN.EQ.1) THEN
         DIST = EXCEN + UNDEMI*EPAIS
      ELSEIF (IPLAN.EQ.-1) THEN
         DIST = EXCEN - UNDEMI*EPAIS
      ELSEIF (IPLAN.EQ.2) THEN
         DIST = EXCEN 
      ENDIF
C
      DO 10 I = 1, NNO
        TENS(8*(I-1)+4) =  TENS(8*(I-1)+4) 
     +                         - DIST * TENS(8*(I-1)+1) 
        TENS(8*(I-1)+5) =  TENS(8*(I-1)+5) 
     +                         - DIST * TENS(8*(I-1)+2) 
        TENS(8*(I-1)+6) =  TENS(8*(I-1)+6) 
     +                         - DIST * TENS(8*(I-1)+3) 
   10 CONTINUE
C
      END
