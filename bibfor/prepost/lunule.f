      SUBROUTINE LUNULE ( R1, R2, ANGDEB, ANGFIN, ANGMAX, ANSINI, 
     &                    ANSFIN, PROFON, VOLUME, EPAIS )
      IMPLICIT   NONE
      REAL*8              R1, R2, ANGDEB, ANGFIN, ANGMAX, ANSINI, 
     &                    ANSFIN, PROFON, VOLUME, EPAIS
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 24/04/2001   AUTEUR CIBHHLV L.VIVAN 
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
      INTEGER      IRET
      REAL*8       DENO1, DEG, R8RDDG, PARA(7), EPSI, X1, X2, RESU
      CHARACTER*4  CRIT
      CHARACTER*24 TYPE
C-----------------------------------------------------------------------
C
      DEG = R8RDDG( )
      CRIT = 'RELA'
      EPSI = 1.D-06
C
      ANGMAX = ( ANSINI + ANSFIN ) / 2.D0
C
      X1 = 0.D0
      X2 = R2
      PARA(1) = R1
      PARA(2) = R2
      PARA(3) = EPAIS
      PARA(4) = 0.D0
      PARA(5) = 2*VOLUME
      PARA(6) = 2*VOLUME
      TYPE(1:12) = 'TUBE_ALESAGE'
      CALL USUBIS ( TYPE, PARA, CRIT, EPSI, X1, X2, RESU, IRET )
      DENO1 = ( R2 - R1 + RESU )
      X1 = R1*R1 - 
     +    ( ( R2*R2 - R1*R1 - DENO1*DENO1 )**2 / ( 4*DENO1*DENO1) )
      X1 = SQRT ( X1 )
      X2 = ASIN ( X1 / R1 ) * DEG
C
      PROFON = RESU
      ANGDEB = ANGMAX - X2
      ANGFIN = ANGMAX + X2
C
      END
