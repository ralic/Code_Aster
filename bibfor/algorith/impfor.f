      SUBROUTINE IMPFOR(UNITE,LONG,PREC,VALR,CHAINE)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 24/08/2005   AUTEUR MABBAS M.ABBAS 
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
C
      IMPLICIT NONE
      INTEGER       UNITE
      INTEGER       LONG
      INTEGER       PREC
      REAL*8        VALR
      CHARACTER*(*) CHAINE
C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : 
C ----------------------------------------------------------------------
C
C IMPRESSION D'UN REEL SUR UNE UNITE LOGICIELLE
C 
C IN  UNITE  : UNITE LOGICIELLE D'IMPRESSION
C IN  LONG   : LONGUEUR D'AFFICHAGE DU NOMBRE
C IN  PRECI  : PRECISION EVENTUELLE D'UN NOMBRE REEL
C IN  VALR   : VALEUR REELLE A AFFICHER
C OUT CHAINE : CHAINE DE SORTIE SI UNITE = 0
C
C ----------------------------------------------------------------------
C
      CHARACTER*8  FOR8
      CHARACTER*9  FOR9
      CHARACTER*1  FOR1
      INTEGER      LONFOR
      REAL*8       R8VIDE
C
C ----------------------------------------------------------------------
C

      IF ((PREC.LT.1).OR.(PREC.GT.9)) THEN
         PREC = 5
      ENDIF    
   
      IF (VALR.EQ.R8VIDE()) THEN
        IF (UNITE.NE.0) THEN 
          WRITE(UNITE,'(A)') ' '
        ELSE
          WRITE(CHAINE,'(A)') ' '
        ENDIF
        GOTO 99
      ENDIF

      IF (LONG.LE.9) THEN
        LONFOR = 8
        FOR8(1:4) = '(1PE'
        WRITE(FOR1,'(I1)') LONG
        FOR8(5:5) = FOR1
        FOR8(6:6) = '.'
        WRITE(FOR1,'(I1)') PREC
        FOR8(7:7) = FOR1
        FOR8(8:8) = ')'       
      ELSE IF (LONG.LE.19) THEN      
        LONFOR = 9
        FOR9(1:4) = '(1PE'
        FOR9(5:5) = '1'
        WRITE(FOR1,'(I1)') LONG-10
        FOR9(6:6) = FOR1
        FOR9(7:7) = '.'
        WRITE(FOR1,'(I1)') PREC
        FOR9(8:8) = FOR1
        FOR9(9:9) = ')' 
      ELSE
        CALL UTMESS('F','IMPFOR','FORMAT D''AFFICHAGE TROP GRAND')
      ENDIF


      IF (UNITE.NE.0) THEN
        IF (LONFOR.EQ.8) THEN
          WRITE(UNITE,FOR8) VALR
        ELSE IF (LONFOR.EQ.9) THEN
          WRITE(UNITE,FOR9) VALR
        ELSE
          CALL UTMESS('F','IMPFOR','LONGUEUR FORMAT EXCESSIF (DVLP)')
        ENDIF
      ELSE
        IF (LONFOR.EQ.8) THEN
          WRITE(CHAINE,FOR8) VALR
        ELSE IF (LONFOR.EQ.9) THEN
          WRITE(CHAINE,FOR9) VALR
        ELSE
          CALL UTMESS('F','IMPFOR','LONGUEUR FORMAT EXCESSIF (DVLP)')
        ENDIF
      ENDIF

  99  CONTINUE  

      END
