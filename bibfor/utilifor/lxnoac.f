      SUBROUTINE LXNOAC(CHIN, CHOUT)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 30/05/2007   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
      CHARACTER*(*)     CHIN, CHOUT
C
C TOLE CRP_6
C ----------------------------------------------------------------------
C --- REMPLACE TOUS LES CARACTERES NON AUTORISES D'UNE CHAINE
C     DE CARACTERES PAR DES '_' (UNDERSCORE).
C      IN : CHIN  = CHAINE EN ENTREE
C     OUT : CHOUT = CHAINE AVEC UNIQUEMENT DES CARACTERES LICITES
C ----------------------------------------------------------------------
C
      INTEGER     LXLGUT
      INTEGER     MXCHAR
      PARAMETER ( MXCHAR=255 )
      CHARACTER*1 CLASS(0:MXCHAR)
      CHARACTER*255 KEEP
      INTEGER     I, LONG, LONG2
C
      INTEGER      FIRST
      SAVE         CLASS, FIRST
C
C     ------------------------------------------------------------------
      DATA FIRST/0/
C                123456789.123456789.123456789.123456789.123456789.12
      DATA KEEP/'ABCDEFGHIJKLMONPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz
     +01234567890'/
C     ------------------------------------------------------------------
C
      IF ( FIRST.EQ.0 ) THEN
C
C        INITIALISATION DES TABLES DE CONVERSION
C
         FIRST = 1
         DO 10 I = 0, MXCHAR
            CLASS(I) = '_'
 10      CONTINUE
C
         DO 20 I = 1 , LXLGUT(KEEP)
            CLASS(ICHAR(KEEP(I:I))) = KEEP(I:I)
 20      CONTINUE
C        ---------------------------------------------------------------
C        WRITE(6,'(25X,A)')' *** CONTROLE DE LA TABLE DE CONVERSION ***'
C        WRITE(6,'(10(1X,4A))') (' * ',CHAR(I),'= ',CLASS(I),I=0,255)
C        WRITE(6,'(1X,79(''-''))')
C        ---------------------------------------------------------------
      ENDIF
C
C       LONG = LEN(CHIN)
      LONG = LXLGUT(CHIN)
      LONG2 = LEN(CHOUT)
      DO 100 I = 1, MIN(LONG, LONG2)
         CHOUT(I:I) = CLASS(ICHAR(CHIN(I:I)))
 100  CONTINUE
C
C     MISE A BLANC DE LA FIN DE LA CHAINE
      DO 110, I = MIN(LONG, LONG2)+1, LONG2
         CHOUT(I:I) = ' '
 110  CONTINUE
C
C     ------------------------------------------------------------------
      END
