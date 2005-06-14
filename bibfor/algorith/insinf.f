      SUBROUTINE INSINF (ST,SIE,EPSC,EQSTR,EDC,IPLA,DEFR0)
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
C                  CALCUL :  EPSC , EQSTR , EDC , DEFR
C
C       IN
C           ST     : DEFORMATION UNIAXIALE
C           SIE    : CONTRAINTE  UNIAXIALE
C       OUT
C           EPSC , EQSTR , EDC , DEFR0 , IPLA
C       -----------------------------------------------------------
      INTEGER         IPLA , ICC
      REAL*8          ST , SIE , EPSC , EQSTR , EDC , DEFR0
      COMMON /DBETO/ POU1(4),RB,ALPHA,EX,PXY,EMAX,EPSU,FTC,FLIM,
     1       EPO,EPO1,POU2(6),ICC,IPOU1(9)
C-------------------------------------------------------------------
      DEFRL = RB / EX - EPO1
      DEFR0 = ST - SIE / EX
C
      IF( DEFR0 .GT. DEFRL ) THEN
         CALL INSEQU ( DEFR0 , EPSC , EQSTR )
      ELSE
         A3   = RB / ( EPO1 - EMAX )
         A1   = -1.D0 * EX * DEFR0
         A2   = A3 * EMAX
         EPSC = ( A2 - A1 ) / ( EX - A3 )
         EQSTR= EX * EPSC + A1
         IPLA = 1
      ENDIF
      EDC = EX
C
      DEFR0 = EPSC - EQSTR / EDC
      EQSTR = ABS(EQSTR)
C
      END
