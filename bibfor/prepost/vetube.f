      SUBROUTINE VETUBE ( R1, R2, ANGDEB, ANGFIN, ANGARE, ANGMAX, 
     &                    ANGVA, PROFON, VOLUME, EPAIS )
      IMPLICIT   NONE
      REAL*8              R1, R2, ANGDEB, ANGFIN, ANGARE, ANGMAX, 
     &                    ANGVA, PROFON, VOLUME, EPAIS
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 28/06/2000   AUTEUR CIBHHLV L.VIVAN 
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
C-----------------------------------------------------------------------
      REAL*8   DELTA, TAU, DENO1, XVAL, XNUME1, XNUME2, DELTAG
      REAL*8   PI, R8PI, RAD, R8DGRD, DEG, R8RDDG
C-----------------------------------------------------------------------
C
      TAU = 0.8D0
      PI  = R8PI( )
      RAD = R8DGRD( )
      DEG = R8RDDG( )
C
      ANGMAX = ANGARE
      PROFON = VOLUME*(1.D0+TAU)/EPAIS*ANGVA/7.D-4*RAD
C
C     ATTENTION PB EQUATION DANS  DELTA  VOIR  NOTE  HP52/99-01
C     ---------------------------------------------------------
C
      XNUME1 = PROFON * ( 2*R2 + PROFON )
      DENO1  = 2*( R2 - R1 + PROFON )
      XVAL   = 1.D0 - ( (1.D0/R1)*(XNUME1/DENO1) )
      XNUME2 = 1.D0 - XVAL
      IF ( XNUME2 .EQ. 0.D0 ) THEN
         DELTA = 0.D0
      ELSE
         IF ( (XVAL.GT.0.D0) .AND. (XNUME2.GT.0.D0) ) THEN
            DELTAG = ATAN(XNUME2/XVAL)
            DELTA  = DELTAG*DEG
         ENDIF
         IF ( (XVAL.LT.0.D0) .AND. (XNUME2.GT.0.D0) ) THEN
            DELTAG = PI - ABS(ATAN(XNUME2/XVAL))
            DELTA  = DELTAG*DEG
         ENDIF
         IF ( (XVAL.LT.0.D0) .AND. (XNUME2.LT.0.D0) ) THEN
            DELTAG = PI + ABS(ATAN(XNUME2/XVAL))
            DELTA  = DELTAG*DEG
         ENDIF
         IF ( (XVAL.GT.0.D0) .AND. (XNUME2.LT.0.D0) ) THEN
            DELTAG = 2*PI - ABS(ATAN(XNUME2/XVAL))
            DELTA  = DELTAG*DEG
         ENDIF
      ENDIF
C
      IF ( ANGMAX .LT. 90.D0 ) THEN
         ANGFIN = ANGMAX + DELTA
         ANGDEB = ANGMAX - DELTA*(1.D0-TAU)/(1.D0+TAU)
      ENDIF
C
      IF ( ANGMAX .GT. 270.D0 ) THEN
         ANGDEB=ANGMAX-DELTA
         ANGFIN = ANGMAX + DELTA*(1.D0-TAU)/(1.D0+TAU)
      ENDIF
C
      IF ( (ANGMAX.GT.90.D0) .AND. (ANGMAX.LT.180.D0) ) THEN
         ANGDEB = ANGMAX - DELTA
         ANGFIN = ANGMAX + DELTA*(1.D0-TAU)/(1.D0+TAU)
        ENDIF
C
      IF ( (ANGMAX.GT.180.D0) .AND. (ANGMAX.LT.270.D0) ) THEN
         ANGFIN = ANGMAX + DELTA
         ANGDEB = ANGMAX - DELTA*(1.D0-TAU)/(1.D0+TAU)
      ENDIF
C
      IF ( 2*DELTA .GT. 360.D0 ) THEN
         CALL UTMESS('F','VETUBE',
     &                   'VOLUME USE TROP GRAND POUR LA MODELISATION')
      ENDIF
C
      END
