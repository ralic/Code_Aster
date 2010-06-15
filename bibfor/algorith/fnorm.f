      SUBROUTINE FNORM( DNORM,VITLOC,KNORM,CNORM,
     &                  COST,SINT,FNORMA,FLOCAL,VNORM)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 02/10/95   AUTEUR GIBHHAY A.Y.PORTABILITE 
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
C
C***********************************************************************
C 01/01/91    G.JACQUART AMV/P61 47 65 49 41
C***********************************************************************
C     FONCTION  : CALCULE LA DISTANCE NORMALE A L'OBSTACLE (<0 SI CHOC)
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C        NOCM       MODE                    ROLE
C  ________________ ____ ______________________________________________
C                         VARIABLES DU SYSTEME DYNAMIQUE MODAL
C  ________________ ____ ______________________________________________
C    DNORM          <--   DISTANCE NORMALE A L'OBSTACLE
C    VITLOC         <--   VITESSE DANS LE REPERE LOCAL
C    COST,SINT      <--   DIRECTION NORMALE A L'OBSTACLE
C    KNORM          <--   RAIDEUR NORMALE DE CHOC
C    CNORM          <--   AMORTISSEUR NORMALE DE CHOC
C    FNORMA          -->  FORCE NORMALE DE CHOC  (MODULE)
C    FLOCAL          -->  FORCE NORMALE DE CHOC REP. LOCAL
C-----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 VITLOC(3),FLOCAL(3),KNORM,CNORM,FNORMA
      VNORM = VITLOC(2)*COST + VITLOC(3)*SINT
      FNORMA = - KNORM*DNORM - CNORM*VNORM
      IF ( FNORMA.LT.0.0D0) THEN
        FNORMA = 0.0D0
      ENDIF
      FLOCAL(1)=0.D0
      FLOCAL(2)=FNORMA*COST
      FLOCAL(3)=FNORMA*SINT
 9999 CONTINUE
      END
