      SUBROUTINE PMFFOR(NF,NCF,VF,SE,FF)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 27/06/2006   AUTEUR DURAND C.DURAND 
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
C -----------------------------------------------------------
C ---  INTEGRATIONS SUR LA SECTION 
C IN
C          NF     : NOMBRE DE FIBRES
C          NCF    : NOMBRE DE CARACTERISTIQUES SUR CHAQUE FIBRE
C          VF     : VF(1,*) : Y FIBRES
C                   VF(2,*) : Z FIBRES
C                   VF(3,*) : S FIBRES
C                   VF(4-6,*) : AUTRES CARACTERISTIQUES
C
C          SE(*)  : CONTRAINTES DANS LES FIBRES
C
C OUT
C          EFFORTS GENERALISES INTEGRES DANS LA SECTION
C          FF  : FF(1) = +INT(SE.DS)   = N
C                FF(2) = +INT(SE.Z.DS) = MY
C                FF(3) = -INT(SE.Y.DS) = MZ
C -----------------------------------------------------------
      INTEGER NF,NCF,I
      REAL*8 VF(NCF,NF),SE(NF),FF(3),ZERO,SF
      PARAMETER (ZERO=0.0D+0)
      CHARACTER*2 KNCF

      DO 10 I = 1,3
        FF(I) = ZERO
10    CONTINUE

      IF (NCF.EQ.3) THEN
C --- 3 CARACTERISTIQUES PAR FIBRE : Y, Z ET S
        DO 20 I = 1 , NF
          SF = SE(I)*VF(3,I)
          FF(1) = FF(1) + SF
          FF(2) = FF(2) + VF(2,I)*SF
          FF(3) = FF(3) - VF(1,I)*SF
20      CONTINUE

      ELSE
C --- ERREUR SUR NCARFI
        CALL CODENT(NCF,'G',KNCF)
        CALL UTMESS('F','PMFFOR','ON NE SAIT PAS INTEGRER AVEC '//KNCF//
     &              ' CARACTERISTIQUES PAR FIBRE')
      END IF

      END
