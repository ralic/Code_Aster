      SUBROUTINE USUNEW ( TYPE, PARA, CRIT, EPSI, X1, X2, RESU, IRET )
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C-----------------------------------------------------------------------
      INTEGER I ,IRET ,MAXIT 
      REAL*8 DA ,DFR ,DL ,DX ,DXOLD ,EPSI ,FR 
      REAL*8 RESU ,TEMP ,X1 ,X2 ,XH ,XL ,ZERO 

C-----------------------------------------------------------------------
      PARAMETER     ( MAXIT = 100 )
C     ------------------------------------------------------------------
C
      IRET = 0
      ZERO = 0.D0
C
      IF ( TYPE(1:8) .EQ. 'TUBE_BAV' ) THEN
         DL = PARA(2)
         DA = PARA(3)
      ENDIF
      XL = X1
      XH = X2
      RESU = ( X1 + X2 ) * 0.5D0
      DXOLD  = ABS( X2 - X1 )
      DX = DXOLD
      CALL USUFON ( TYPE , PARA , RESU , FR , DFR )
      DO 10 I = 1 , MAXIT
         DXOLD = DX
         IF ( ((RESU-XH)*DFR-FR)*((RESU-XL)*DFR-FR) .GE. ZERO
     +        .OR. ABS(FR*2.D0) .GT. ABS(DXOLD*DFR)    )   THEN
            DX = ( XH - XL ) * 0.5D0
            RESU = DX * XL
C            IF ( XL .EQ. RESU ) GOTO 9999
            IF ( CRIT(1:4) .EQ. 'RELA' ) THEN
               IF ( ABS(XL-RESU) .LE. EPSI * ABS(RESU) ) GOTO 9999
            ELSE
               IF ( ABS(XL - RESU) .LE. EPSI )  GOTO 9999
            ENDIF
         ELSE
            DX = FR / DFR
            TEMP = RESU
            RESU = RESU - DX
C            IF ( TEMP .EQ. RESU ) GOTO 9999
            IF ( CRIT(1:4) .EQ. 'RELA' ) THEN
               IF ( ABS(TEMP-RESU) .LE. EPSI * ABS(RESU) ) GOTO 9999
            ELSE
               IF ( ABS(TEMP - RESU) .LE. EPSI ) GOTO 9999
            ENDIF
         ENDIF
         IF ( ABS(DX) .LT. EPSI )  GOTO 9999
         IF ( TYPE(1:8) .EQ. 'TUBE_BAV' ) THEN
         IF ( ( RESU - DL*DA ) .LT. 0.D0 ) THEN
C            WRITE(8,*)'--->> USUNEW, NOMBRE NEGATIF ',( RESU - DL*DA )
C            WRITE(8,*)'              ON AJUSTE'
            RESU = DL*DA
         ENDIF
         ENDIF
         CALL USUFON ( TYPE , PARA , RESU , FR , DFR )
         IF ( FR .LT. ZERO ) THEN
            XL = RESU
         ELSE
            XH = RESU
         ENDIF
 10   CONTINUE
      IRET = 10
      GOTO 9999
C
9999  CONTINUE
      END
