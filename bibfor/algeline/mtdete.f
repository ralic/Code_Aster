      SUBROUTINE MTDETE ( LMAT  , MANTIS , EXPO )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             LMAT ,           EXPO
      REAL*8                      MANTIS
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 11/04/97   AUTEUR VABHHTS J.PELLET 
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
C     CALCUL DU DETERMINANT D'UNE MATRICE DECOMPOSEE SOUS FORME L*D*LT
C     ------------------------------------------------------------------
C
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
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C     ------------------------------------------------------------------
      CHARACTER*24     NOMDIA
      DATA  NOMDIA/'                   .&VDI'/
C     ------------------------------------------------------------------
C
C
      CALL JEMARQ()
      NOMDIA(1:19) = ZK24(ZI(LMAT+1))
      NEQ          = ZI(LMAT+2 )
      CALL JEEXIN(NOMDIA, IRET )
      IF ( IRET .EQ. 0 ) THEN
         CALL UTMESS('F', 'MTDETE',NOMDIA//' N''EXISTE PAS')
      ENDIF
      CALL JEVEUO( NOMDIA , 'L', LDIAG )
C
C        --- CALCUL DU DETERMINANT --
      IF ( ZI(LMAT+3) .EQ. 1 ) THEN
C
C        --- DIAGONALE A COEFFICIENTS REELS ---
         CALL ALMULR( 'ZERO', ZR(LDIAG), NEQ , MANTIS, EXPO )
         NBNEG  = 0
         DO 10 I=0, NEQ-1
            IF ( ZR(LDIAG+I) .LE. 0.D0 ) NBNEG = NBNEG + 1
  10     CONTINUE
      ELSE
C
C        --- DIAGONALE A COEFFICIENTS COMPLEXES ---
CCCC     CALL ALDETC( ZC(LDIAG), NEQ , MANTIS, EXPO )
      ENDIF
CCC   IMS = IUNIFI('MESSAGES')
CCC   WRITE(IMS,'(10X,A,F20.15,A,I8)') 'DETERMINANT = ',MANTIS,' E',EXPO
C
      CALL JEDEMA()
      END
