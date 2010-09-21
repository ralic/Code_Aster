        SUBROUTINE LCELPL(LOI,NMAT,MATERF,NVI,VIND,VINF)
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/09/2010   AUTEUR FLEJOU J-L.FLEJOU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C ----------------------------------------------------------------
C   MISE A JOUR DES VARIABLES INTERNES EN ELASTICITE
C
C   POST-TRAITEMENTS SPECIFIQUES AUX LOIS
C
C   CAS GENERAL :
C      VINF = VIND
C      VINF(NVI) = 0.0
C ----------------------------------------------------------------
C  IN
C     LOI    :  NOM DE LA LOI
C     NMAT   :  DIMENSION MATER ET DE NBCOMM
C     MATERF :  COEF MATERIAU
C     IDPLAS :  INDICATEUR PLASTIQUE
C     VIND   :  VARIABLES INTERNES A T
C     SIGMF  :  CONTRAINTES A T+DT
C  OUT
C     VINF   :  VARIABLES INTERNES A T+DT
C ----------------------------------------------------------------
      CHARACTER*16 LOI
      INTEGER      NMAT,NVI
      REAL*8       MATERF(NMAT,2),VINF(*),VIND(*)

C ----------------------------------------------------------------
      IF (LOI(1:7).EQ.'IRRAD3M') THEN
         CALL IRRLNF(NMAT,MATERF,VIND,0.0D0,VINF)
      ELSE
C
C --- CAS GENERAL :
C        VINF  = VIND ,  ETAT A T+DT = VINF(NVI) = 0 = ELASTIQUE
         CALL LCEQVN( NVI-1, VIND , VINF )
         VINF(NVI) = 0.0D0
      ENDIF
      END
