        SUBROUTINE LKLINI (SIGF,NR,YD,DY)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/03/2012   AUTEUR PROIX J-M.PROIX 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
        IMPLICIT   NONE
C RESPONSABLE FOUCAULT A.FOUCAULT
C       ----------------------------------------------------------------
C       ROUTINE INITIALISANT DY POUR LE MODELE LETK IMPLICITE
C       ----------------------------------------------------------------
C       IN  SIGF   :  PREDICTION ELASTIQUE DES CONTRAINTES (LCELAS)
C           NR     : DIMENSION VECTEUR INCONNUES
C           YD     : VALEUR DES INCONNUES A T
C       OUT DY     :  SOLUTION ESSAI  = ( DSIG DLAMBDA DXIP DXIVP )
C       ----------------------------------------------------------------
        INTEGER         NR
        REAL*8          SIGF(6), YD(NR), DY(NR)
C
        INTEGER         NDI,NDT,I
C       --------------------------------------------------------------
        COMMON /TDIM/   NDT  , NDI
C       --------------------------------------------------------------

        DO 10 I = 1, NDT
          DY(I) = SIGF(I)-YD(I)
  10    CONTINUE

        END
