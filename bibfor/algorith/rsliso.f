        SUBROUTINE RSLISO ( IMAT , TEMP , P , RP , DRDP )
        IMPLICIT REAL*8 (A-H,O-Z)
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 01/03/2000   AUTEUR ADBHHVV V.CANO 
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
C       ----------------------------------------------------------------
C       LOI ECROUISSAGE ISOTROPE R(P,T) ENTREE POINT PAR POINT
C       ET  DERIVEE LOI ECROUISSAGE ISOTROPE R(P,T)/ P
C       IN  P      :  DEFORMATION CUMULEE
C           TEMP   :  TEMPERATURE
C           IMAT   :  ADRESSE DU MATERIAU CODE
C       OUT RP     :  R (P,TEMP)
C       OUT DRDP   :  DRDP ( P,TEMP) = INTERPOLATION LINEAIRE SUR P,TEMP
C       ----------------------------------------------------------------
        REAL*8          TEMP , P , RP ,  E , DRDP, AIRERP, DUM
        INTEGER         IMAT, JPROL, JVALE, NBVALE
C       ----------------------------------------------------------------
        CALL RCTRAC(IMAT,'TRACTION','SIGM',TEMP,JPROL,JVALE,
     &              NBVALE,E)
        CALL RCFONC('V','TRACTION',JPROL,JVALE,NBVALE,DUM,DUM,DUM,
     &               P,RP,DRDP,AIRERP,DUM,DUM)
        END
