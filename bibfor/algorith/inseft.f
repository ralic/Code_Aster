      SUBROUTINE INSEFT( EPEQ , SEQ , IPLA )
        IMPLICIT REAL*8 (A-H,O-Z)
C       -----------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 27/03/2002   AUTEUR VABHHTS J.PELLET 
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
C       -----------------------------------------------------------
C       NADAI_B :  BETON FISSURE
C                  CALCUL DE LA DEFORMATION EQUIVALENTE EPEQ
C                  A PARTIR DE LA CONTRAINTE EQUIVALENTE SEQ
C
C       IN
C           SEQ    : CONTRAINTE  EQUIVALENTE
C           IPLA   : INDICATEUR DE PLASTICITE BETON 1-D
C       OUT
C           EPEQ   : DEFORMATION EQUIVALENTE
C           SEQ    : CONTRAINTE  EQUIVALENTE
C       -----------------------------------------------------------
      INTEGER         IPLA
      REAL*8          EPEQ , SEQ
      COMMON /DBETO/ POU1(4),RB,ALPHA,EX,PXY,EMAX,EPSUT,FTC,FLIM,
     1   EPO, EPO1,POU2(6),ICC,IPOU(9)
C-------------------------------------------------------------------
      TU=RB
      IF(SEQ.GE.TU) THEN
         SEQ = TU
         EPEQ = EPO1
         IPLA = 1
         GOTO 9999
      ENDIF
C-------------------------------------------------------------------
C                POST-PIC LINEAIRE
C-------------------------------------------------------------------
      IF( IPLA .NE. 0 ) THEN
       EPEQ = EMAX - SEQ * (EMAX-EPO1) / TU
       GOTO 9999
      ENDIF
C-------------------------------------------------------------------
C                PRE-PIC HYPERBOLE
C-------------------------------------------------------------------
      IF( IPLA .EQ. 0 ) THEN
      EPEQ = 0.D0
      IF( SEQ .EQ. 0.D0 ) GOTO 9999
C
      CC3 = EPO1 * EPO1
      BB3 = CC3 * ( EX/SEQ - EX/TU + 2.D0/EPO1 )
      DELT = BB3 * BB3 - 4.D0 * CC3
C
      IF( DELT .EQ. 0.D0 ) THEN
        EPEQ = EPO1
        GOTO 9999
      ENDIF
      IF( DELT .GT. 0.D0 ) EPEQ = ( BB3 - SQRT(DELT) ) / 2.D0
      ENDIF
C-------------------------------------------------------------------
 9999 CONTINUE
      END
