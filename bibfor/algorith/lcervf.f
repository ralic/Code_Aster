      SUBROUTINE LCERVF(MODE,NDIMSI,EPS,TREPS,EPSDV,GAMEPS,DGAMDE) 

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

      IMPLICIT NONE      
      INTEGER MODE,NDIMSI
      REAL*8  EPS(NDIMSI),TREPS,EPSDV(NDIMSI),GAMEPS,DGAMDE(NDIMSI)
C ----------------------------------------------------------------------
C  CALCUL DE GAMMA(EPS) POUR LA LOI ENDO_SCALAIRE AVEC GRAD_VARI
C ----------------------------------------------------------------------
C  IN  MODE    FONCTION RECHERCHEE (0 = VALEUR, 1 = VALEUR + DERIVEE)
C  IN  EPS     VALEUR DE L'ARGUMENT EPS(1:NDIMSI)
C  OUT TREPS   TRACE DES DEFORMATIONS
C  OUT EPSDV   DEVIATEUR DES DEFORMATIONS
C  OUT GAMEPS  FONCTION GAMMA(EPS)
C  OUT DGAMDE  DERIVEE DE GAMMA - SI MODE = 1
C ----------------------------------------------------------------------
      INTEGER IJ
      REAL*8 KRON(6),DDOT,EPSEQ2,UEPS,HEPS,COEFH,COEFS
C ----------------------------------------------------------------------
      REAL*8 PCT,PCH,PCS
      COMMON /LCER/ PCH,PCT,PCS
C ----------------------------------------------------------------------
      DATA KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/
C ----------------------------------------------------------------------

      TREPS  = EPS(1)+EPS(2)+EPS(3)
      DO 15 IJ = 1,NDIMSI
        EPSDV(IJ) = EPS(IJ) - TREPS/3*KRON(IJ)
 15   CONTINUE        

      EPSEQ2 = 1.5D0 * DDOT(NDIMSI,EPSDV,1,EPSDV,1)
      UEPS   = PCH*TREPS**2+PCS*EPSEQ2
      HEPS   = PCT*TREPS+SQRT(UEPS)
      GAMEPS = HEPS*HEPS

      IF (MODE.EQ.1) THEN
        CALL R8INIR(NDIMSI,0.D0,DGAMDE,1)
        IF (UEPS.NE.0) THEN
          COEFH = PCT + PCH*TREPS/SQRT(UEPS)
          COEFS = 1.5D0*PCS/SQRT(UEPS)
          DO 105 IJ = 1,NDIMSI
            DGAMDE(IJ) = 2*HEPS*(COEFH*KRON(IJ)+COEFS*EPSDV(IJ))
 105      CONTINUE
        END IF
      END IF
      
      END
