      SUBROUTINE JVRTAM ( TEXTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 02/10/95   AUTEUR GIBHHAY A.Y.PORTABILITE 
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
      CHARACTER *(*)      TEXTE
C     ==================================================================
      INTEGER          MXCOLS , ITABU , LIGCOU , COLCOU , IDF
      COMMON /JVINIP/  MXCOLS , ITABU , LIGCOU , COLCOU , IDF
      PARAMETER      ( NT = 10 )
      CHARACTER*132    TAMPON
      COMMON /JVTAMP/  TAMPON(NT)
      INTEGER          LDEB
      COMMON /JVDEB /  LDEB
C     ------------------------------------------------------------------
      INTEGER          LONG,CDEB,CFIN,LTOT,NBCHAR,NL,NC
C     ------------------------------------------------------------------
      NC = COLCOU
      NL = LIGCOU
C
C     --- ON RETIRE LES BLANCS FINAUX , SAUF LE DERNIER ---
C
      LONG   =  LEN(TEXTE)
      LONGIN =  LONG
      IF ( LONG .EQ. 0 ) GOTO 100
 10   CONTINUE
      IF ( TEXTE(LONG:LONG) .EQ. ' ' ) THEN
         LONG = LONG - 1
         IF ( LONG .GT. 0 )  GOTO 10
      ENDIF
      IF ( LONG .EQ. 0 ) GOTO 100
      IF ( LONG .LT. LONGIN ) LONG = LONG + 1
C
      CDEB = 1
 11   CONTINUE
      LTOT = MXCOLS - LONG - NC + 1
      IF ( LTOT .LE. 0 ) THEN
         NBCHAR = MXCOLS - NC + 1
         CFIN   = CDEB + NBCHAR - 1
 12      CONTINUE
         IF ( TEXTE(CFIN:CFIN) .NE. ' ' ) THEN
            CFIN = CFIN - 1
            IF ( CFIN .LE. CDEB ) THEN
               CFIN = CDEB+NBCHAR-1
            ELSE
               GOTO 12
            ENDIF
         ENDIF
         TAMPON (NL) (NC : NC+CFIN-CDEB) = TEXTE(CDEB:CFIN)
         IF ( NL .EQ. NT ) THEN
            CALL JVVTAM
            NL = 0
         ENDIF
         NL   = NL + 1
         NC   = LDEB
         LONG = LONG - CFIN + CDEB - 1
         CDEB = CFIN + 1
         GO TO 11
      ELSE
         CFIN = CDEB + LONG - 1
         TAMPON (NL) (NC : NC+LONG-1) = TEXTE(CDEB:CFIN)
         NC   = NC + LONG
      ENDIF
      LIGCOU = NL
      COLCOU = NC
 100  CONTINUE
C
      END
