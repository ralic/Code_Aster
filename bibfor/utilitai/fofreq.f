      SUBROUTINE FOFREQ ( F1, LFREQ, NBDEB, NBFRI, CRIT )
      IMPLICIT NONE
      CHARACTER*3         CRIT          
      INTEGER             NBDEB, NBFRI
      REAL*8              F1, LFREQ(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 11/09/2002   AUTEUR CIBHHPD D.NUNEZ 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
      INTEGER       I
      REAL*8        FRMIN, FRMAX
C
      FRMIN = LFREQ(NBDEB)
      FRMAX = LFREQ(NBFRI)
C
      IF ( (F1.GE.FRMIN) .AND. (F1.LE.FRMAX) ) THEN
         DO 12 I = NBDEB , NBFRI
            IF ( F1 .LT. LFREQ(I) ) THEN
               FRMAX = LFREQ(I)
               GOTO 13
            ELSE IF (F1.GT.LFREQ(I)) THEN
               FRMIN = LFREQ(I)
            ENDIF
 12      CONTINUE
 13      CONTINUE
         IF ( CRIT .EQ. 'PRO' ) THEN
            IF ( ABS(F1-FRMIN) .LT. ABS(F1-FRMAX) ) THEN
               F1 = FRMIN
            ELSE 
               F1 = FRMAX
            ENDIF
         ELSEIF (CRIT.EQ.'INF') THEN
            F1 = FRMIN
         ELSEIF (CRIT.EQ.'SUP') THEN
            F1 = FRMAX
         ENDIF
C
      ENDIF
C
      END
