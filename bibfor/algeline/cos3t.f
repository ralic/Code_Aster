      FUNCTION COS3T (S, PREF, EPSSIG)
C
      IMPLICIT NONE
      REAL*8   S(6), PREF, EPSSIG, COS3T
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 17/06/2003   AUTEUR CIBHHBC R.FERNANDES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                                                                       
C                                                                       
C ======================================================================
C ======================================================================
C --- BUT : CALCUL DE COS(3T) OU T DESIGNE L'ANGLE DE LODE -------------
C ======================================================================
C IN  : N      : DIMENSION DU TENSEUR ----------------------------------
C --- : S      : DEVIATEUR DU TENSEUR DES CONTRAINTES ------------------
C --- : PREF   : PRESSION DE REFERENCE ---------------------------------
C --- : EPSSIG : EPSILON DE TOLERANCE ----------------------------------
C OUT : COS3T  = RAC(54)*DET(S)/(SII)**3 -------------------------------
C ======================================================================
      INTEGER NDT, NDI
      REAL*8  SII, SIIREL, DETS, UN, MUN
C ======================================================================
C --- INITIALISATION DE PARAMETRES -------------------------------------
C ======================================================================
      PARAMETER       ( MUN    = -1.0D0  )
      PARAMETER       ( UN     =  1.0D0  )
C ======================================================================
      COMMON /TDIM/   NDT , NDI
C ======================================================================
      CALL     PSCAL(NDT, S, S, SII)
      SII    = SQRT(SII)
      SIIREL = SII/PREF
      IF ( SIIREL.GT.EPSSIG ) THEN
         CALL    LCDETE(S,DETS)
         COS3T = SQRT(54.D0)*DETS/(SII*SII*SII)
      ELSE
         COS3T = UN
      ENDIF
C ======================================================================
C --- PROJECTION DU COSINUS POUR COHERENCE -----------------------------
C ======================================================================
      IF ( COS3T.GT. UN) COS3T =  UN
      IF ( COS3T.LT.MUN) COS3T = MUN
C ======================================================================
      END
