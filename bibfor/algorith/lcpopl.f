      SUBROUTINE LCPOPL(LOI,NMAT,MATERD,MATERF,MOD,SIGF,VIND,VINF)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 18/06/2012   AUTEUR PROIX J-M.PROIX 
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
C     ----------------------------------------------------------------
C     ROUTINE DE POST-TRAITEMENT POUR CERTAINES LOIS
C     ----------------------------------------------------------------
C     ----------------------------------------------------------------
C     IN  NMAT    :  NOMBRE DE PARAMETRES MATERIAU INELASTIQUE
C         MATERD :  COEFFICIENTS MATERIAU A T
C         MATERF :  COEFFICIENTS MATERIAU A T+DT
C         MOD    :  TYPE DE MODELISATION
C     OUT SIGF   :  CONTRAINTE A T+DT
C         VINF   :  VARIABLES INTERNES A T+DT
C     ----------------------------------------------------------------

      INTEGER  NMAT
      REAL*8   MATERD(NMAT,2),MATERF(NMAT,2),SIGF(*),VIND(*),VINF(*)
      CHARACTER*8 MOD
      CHARACTER*16 LOI
C
      IF ( LOI(1:6) .EQ. 'LAIGLE') THEN
         CALL LGLDCM( NMAT, MATERF, SIGF, VINF )
      ENDIF
C
C --  CONTRAINTES PLANES
      IF ( MOD(1:6) .EQ. 'C_PLAN' ) SIGF(3) = 0.D0
C
      IF (LOI.EQ.'HAYHURST') THEN
         MATERD(1,1)=MATERD(1,1)*(1.0D0-VIND(11))
         MATERF(1,1)=MATERF(1,1)*(1.0D0-VINF(11))
      ENDIF
      IF (LOI.EQ.'VENDOCHAB') THEN
         MATERD(1,1)=MATERD(1,1)*(1.0D0-VIND(9))
         MATERF(1,1)=MATERF(1,1)*(1.0D0-VINF(9))
      ENDIF

      END
