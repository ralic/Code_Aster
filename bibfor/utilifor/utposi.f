      SUBROUTINE UTPOSI ( IPOS )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 06/01/95   AUTEUR G8BHHAC A.Y.PORTABILITE 
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
      INTEGER             IPOS
C     ==================================================================
      INTEGER          MXCOLS , ITABU , LIGCOU , COLCOU , IDF
      COMMON /UTINIP/  MXCOLS , ITABU , LIGCOU , COLCOU , IDF
      PARAMETER      ( NT = 10 )
      CHARACTER*132    TAMPON
      COMMON /UTTAMP/  TAMPON(NT)
      INTEGER          LDEB
      COMMON /UTDEB /  LDEB
C     ------------------------------------------------------------------
      INTEGER          CDEB , CFIN, IPOS1
C     ------------------------------------------------------------------
C
      IPOS1 = IPOS + LDEB - 1
      IF ( IPOS1 .GT. MXCOLS .OR. IPOS1 .LT. LDEB ) THEN
        TAMPON(1) (1:LDEB+22) = '<F> ERREUR DE PROGRAMMATION'
        TAMPON(2) (1:LDEB+22)   = '    APPEL A UTPOSI ERRONE  '
        IDF    = 6
        LIGCOU = 2
        CALL UTVTAM
      ENDIF
      IF ( IPOS1 .LT. COLCOU ) THEN
         LIGCOU = LIGCOU + 1
         IF ( LIGCOU .GT. NT ) THEN
            CALL UTVTAM
            LIGCOU = 1
         ENDIF
      ENDIF
      COLCOU = IPOS1
C
      END
