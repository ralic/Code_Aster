      SUBROUTINE USUBIS ( TYPE, PARA, CRIT, EPSI, X1, X2, X, IRET )
      IMPLICIT   REAL*8 ( A-H , O-Z )
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 17/01/97   AUTEUR VABHHTS J.PELLET 
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
C ----------------------------------------------------------------------
      REAL*8        PARA(*)
      CHARACTER*(*) TYPE, CRIT
      PARAMETER     ( MAXIT = 100 )
C     ------------------------------------------------------------------
C
      IRET = 0
C
      RESU = PARA(5)
      CALL USUFON ( TYPE , PARA , X1 , F1 , DF1 )
      CALL USUFON ( TYPE , PARA , X2 , F2 , DF2 )
      IF ( CRIT(1:4) .EQ. 'RELA' ) THEN
         IF ( ABS(F1-RESU) .LE. EPSI * ABS(RESU) ) GOTO 9999
      ELSE
         IF ( ABS(F1 - RESU) .LE. EPSI )  GOTO 9999
      ENDIF
      IF ( CRIT(1:4) .EQ. 'RELA' ) THEN
         IF ( ABS(F2-RESU) .LE. EPSI * ABS(RESU) ) GOTO 9999
      ELSE
         IF ( ABS(F2 - RESU) .LE. EPSI )  GOTO 9999
      ENDIF
      XG = X1
      XD = X2
      DO 10 I = 1 , MAXIT
         X = ( XG + XD ) * 0.5D0
         CALL USUFON ( TYPE , PARA , X , F , DF )
         IF ( CRIT(1:4) .EQ. 'RELA' ) THEN
            IF ( ABS(F-RESU) .LE. EPSI * ABS(RESU) ) GOTO 9999
         ELSE
            IF ( ABS(F-RESU) .LE. EPSI )  GOTO 9999
         ENDIF
         IF ( F .LT. RESU ) THEN
            XG = X
            XD = XD
         ELSE
            XG = XG
            XD = X
         ENDIF
 10   CONTINUE
      IRET = 10
      GOTO 9999
C
9999  CONTINUE
      END
