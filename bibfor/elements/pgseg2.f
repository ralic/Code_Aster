      SUBROUTINE PGSEG2 ( NNO,NPG,POG,VF,DFDE,COG)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 06/01/95   AUTEUR G8BHHAC A.Y.PORTABILITE 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             NNO,NPG
      REAL*8                      POG(*),VF(*),DFDE(*),COG(*)
C ......................................................................
C  - FONCTION REALISEE:  CALCUL DES POIDS DE GAUSS ET DES COORDONNEES
C                        DES POINTS DE GAUSS POUR UN SEGMENT, AINSI QUE
C                        LES DERIVEES DES FONCTIONS DE FORME
C  - ARGUMENTS:
C      DONNEES:          NNO    -->   NOMBRE DE NOEUDS DU SEGMENT
C                        NPG    -->   NOMBRE DE POINTS DE GAUSS
C    RESULTATS:          POG    <--   POIDS DE GAUSS
C                        VF     <--   VALEURS DES FONCTION DE FORME
C                        DFDE   <--   DERIVEES DES FONCTION DE FORME
C                        COG    <--   POSITION DES POINTS DE GAUSS
C
C ......................................................................
C
      INTEGER            KP
C
      IF ( NPG .EQ. 1 ) THEN
         COG(1) = 0.D0
C
         POG(1) = 2.D0
      ELSE IF ( NPG .EQ. 2 ) THEN
         COG(1) = .577350269189626D0
         COG(2) = -COG(1)
C
         POG(1) = 1.0D0
         POG(2) = POG(1)
      ELSE IF ( NPG .EQ. 4 ) THEN
         COG(1) = .339981043584856D0
         COG(2) = -COG(1)
         COG(3) = .861136311594053D0
         COG(4) = -COG(3)
C
         POG(1) = .652145154862546D0
         POG(2) = POG(1)
         POG(3) = .347854845137454D0
         POG(4) = POG(3)
      ENDIF
C
      IF ( NNO.EQ.2 ) THEN
         DO 1 KP=1,NPG
            K = (KP-1)*NNO+1
            VF(K  ) = (1-COG(KP))/2.D0
            VF(K+1) = (1+COG(KP))/2.D0
            DFDE(K  ) = -0.5D0
            DFDE(K+1) =  0.5D0
 1       CONTINUE
      ELSE IF ( NNO .EQ. 3 ) THEN
         DO 2 KP=1,NPG
            K = (KP-1)*NNO+1
            VF(K  ) = -(1-COG(KP))*COG(KP)/2.D0
            VF(K+1) =  (1+COG(KP))*COG(KP)/2.D0
            VF(K+2) =  (1+COG(KP))*(1-COG(KP))
            DFDE(K  ) =  COG(KP)-0.5D0
            DFDE(K+1) =  COG(KP)+0.5D0
            DFDE(K+2) = -2.D0*COG(KP)
 2       CONTINUE
      ENDIF
C
      END
