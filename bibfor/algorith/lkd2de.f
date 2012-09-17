      SUBROUTINE LKD2DE(DEVSIG,D2DETS)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 17/09/2012   AUTEUR FOUCAULT A.FOUCAULT 
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
C RESPONSABLE FOUCAULT A.FOUCAULT
      IMPLICIT   NONE
C     ------------------------------------------------------------------
C     CALCUL DE DERIVEE 2NDE DE DET(S) PAR RAPPORT A DEVIATEUR SIGMA 
C     IN  DEVSIG : DEVIATEUR DES CONTRAINTES
C
C     OUT D2DETS : DERIVEE 2NDE DET(S) PAR RAPPORT A SIGMA (NDT X NDT)
C     ------------------------------------------------------------------
      REAL*8          DEVSIG(6),D2DETS(6,6)
C
      INTEGER         NDI,NDT
      REAL*8          ZERO,DEUX,R2
      PARAMETER       ( ZERO   = 0.0D0 )
      PARAMETER       ( DEUX   = 2.0D0 )
C     ------------------------------------------------------------------
      COMMON /TDIM/   NDT,NDI
C     ------------------------------------------------------------------
      CALL LCINMA(ZERO,D2DETS)

      R2     = SQRT(DEUX)

      IF(NDT.EQ.6)THEN
        D2DETS(1,2) = DEVSIG(3)
        D2DETS(1,3) = DEVSIG(2)
        D2DETS(1,6) = -DEVSIG(6)
        D2DETS(2,1) = DEVSIG(3)
        D2DETS(2,3) = DEVSIG(1)
        D2DETS(2,5) = -DEVSIG(5)
        D2DETS(3,1) = DEVSIG(2)
        D2DETS(3,2) = DEVSIG(1)
        D2DETS(3,4) = -DEVSIG(4)
        D2DETS(4,3) = -DEVSIG(4)
        D2DETS(4,4) = -DEVSIG(3)
        D2DETS(4,5) = DEVSIG(6)/R2
        D2DETS(4,6) = DEVSIG(5)/R2
        D2DETS(5,2) = -DEVSIG(5)
        D2DETS(5,4) = DEVSIG(6)/R2
        D2DETS(5,5) = -DEVSIG(2)
        D2DETS(5,6) = DEVSIG(4)/R2
        D2DETS(6,1) = -DEVSIG(6)
        D2DETS(6,4) = DEVSIG(5)/R2
        D2DETS(6,5) = DEVSIG(4)/R2
        D2DETS(6,6) = -DEVSIG(1)
      ELSEIF(NDT.EQ.4)THEN
        D2DETS(1,2) = DEVSIG(3)
        D2DETS(2,1) = DEVSIG(3)
        D2DETS(1,3) = DEVSIG(2)
        D2DETS(3,1) = DEVSIG(2)
        D2DETS(2,3) = DEVSIG(1)
        D2DETS(3,2) = DEVSIG(1)
        D2DETS(4,4) = -DEVSIG(3)
        D2DETS(4,3) = -DEVSIG(4)
        D2DETS(3,4) = -DEVSIG(4)
      ENDIF

      END
