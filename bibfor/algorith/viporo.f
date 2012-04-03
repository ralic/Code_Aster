      SUBROUTINE VIPORO(NBVARI,VINTM,VINTP,ADVICO,VICPHI,PHI0,
     +        DEPSV,ALPHA0,DT,DP1,DP2,SIGNE,SAT,CS,BIOT,PHI,PHIM,RETCOM)
      IMPLICIT      NONE
      INTEGER       NBVARI,ADVICO,VICPHI,RETCOM
      REAL*8        VINTM(NBVARI),VINTP(NBVARI),PHI0
      REAL*8        DEPSV,ALPHA0,DT,DP1,DP2,SIGNE,SAT,CS,BIOT,PHI,PHIM
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 02/04/2012   AUTEUR GRANET S.GRANET 
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
C ======================================================================
C --- CALCUL ET STOCKAGE DE LA VARIABLE INTERNE DE POROSITE ------------
C ======================================================================
      INTEGER       IADZI,IAZK24,UMESS,IUNIFI
      REAL*8        VARBIO,EPXMAX
      PARAMETER    (EPXMAX = 5.D0)
C ======================================================================
C ======================================================================
C --- CALCUL DES ARGUMENTS EN EXPONENTIELS -----------------------------
C --- ET VERIFICATION DE SA COHERENCE ----------------------------------
C ======================================================================
      VARBIO = - DEPSV + 3.D0*ALPHA0*DT - ( DP2 - SAT*SIGNE*DP1 )*CS
      IF (VARBIO.GT.EPXMAX) THEN
         RETCOM = 2
         GO TO 30
      ENDIF
      VINTP(ADVICO+VICPHI) = BIOT - PHI0 -
     &                     (BIOT-VINTM(ADVICO+VICPHI)-PHI0)*EXP(VARBIO)

      PHI  = VINTP(ADVICO+VICPHI) + PHI0
      PHIM = VINTM(ADVICO+VICPHI) + PHI0
C ======================================================================
 30   CONTINUE
C =====================================================================
 9001 FORMAT (A8,2X,A30,2X,A8)
C ======================================================================
      END
