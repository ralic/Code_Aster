      SUBROUTINE LXCAPS(CHAINE)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     CHAINE
C
C     ------------------------------------------------------------------
C      TRANSFORME LES MINUSCULES EN MAJUSCULES
C     ------------------------------------------------------------------
C VAR CHAINE CHAINE A TRANSFORMER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR DATE 21/02/96 AUTEUR VABHHTS J.PELLET
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
C TOLE CRP_6
C     ------------------------------------------------------------------
C     ROUTINE(S) UTILISEE(S) :
C         -
C     ROUTINE(S) FORTRAN     :
C         CHAR    ICHAR   LEN
C     ------------------------------------------------------------------
C FIN LXCAPS
C     ------------------------------------------------------------------
C
      INTEGER     MXCHAR
      PARAMETER ( MXCHAR=255 )
      CHARACTER*1  CLASS(0:MXCHAR)
      CHARACTER*26 MINUS, MAJOR
C
      INTEGER      LONG , FIRST
      SAVE         CLASS, FIRST
C
C     ------------------------------------------------------------------
      DATA FIRST/0/
      DATA MINUS/'abcdefghijklmnopqrstuvwxyz'/
      DATA MAJOR/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
C     ------------------------------------------------------------------
C
      IF ( FIRST.EQ.0 ) THEN
C
C        INITIALISATION DES TABLES DE CONVERSION
C
         FIRST = 1
         DO 10 I = 0, MXCHAR
            CLASS(I) = CHAR(I)
 10      CONTINUE
C
         DO 20 I = 1 , 26
            CLASS(ICHAR(MINUS(I:I))) = CLASS(ICHAR(MAJOR(I:I)))
 20      CONTINUE
C        ---------------------------------------------------------------
C-DBG    WRITE(6,'(25X,A)')' *** CONTROLE DE LA TABLE DE CONVERSION ***'
C-DBG    WRITE(6,'(10(1X,4A))') (' * ',CHAR(I),'= ',CLASS(I),I=0,255)
C-DBG    WRITE(6,'(1X,79(''-''))')
C        ---------------------------------------------------------------
      ENDIF
C
      LONG = LEN(CHAINE)
      DO 100 ILONG = 1, LONG
         CHAINE(ILONG:ILONG) = CLASS(ICHAR(CHAINE(ILONG:ILONG)))
 100  CONTINUE
C
C     ------------------------------------------------------------------
      END
