      SUBROUTINE CHRP3D(PPP,SIEPIN,SIEPOO,IOP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/09/2003   AUTEUR JMBHH01 J.M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C--------------------------------------------------------
C ELEMENT SHB8-PS A.COMBESCURE, S.BAGUET INSA LYON 2003 /
C-------------------------------------------------------
C   ------------------------------------------------------------------
C       CHGT DE REPERE POUR LES CONTRAINTES ET LES DEFORMATIONS
C         ECRITES SOUS FORME VECTORIELLE EN 3D
C                                             H. BUNG      02-93
C   ------------------------------------------------------------------
C   PPP      : MATRICE DE PASSAGE
C   SIEPIN   : VECTEUR DE CONTRAINTES OU DE DEFORMATIONS EN ENTREE
C   SIEPOO   : VECTEUR DE CONTRAINTES OU DE DEFORMATIONS EN SORTIE
C   IOP            ENTREE     SORTIE
C        = 0       SIG_G      SIG_L
C        = 1       SIG_L      SIG_G
C        = 2       EPS_G      EPS_L
C        = 3       EPS_L      EPS_G
C--    RAPPEL
C     [SIG] = [ S_11, S_22, S_33,   S_12,   S_23,   S_13 ]
C     [EPS] = [ E_11, E_22, E_33, 2*E_12, 2*E_23, 2*E_13 ]
      IMPLICIT NONE
C---   VARIABLES GLOBALES
      REAL*8 PPP(3,3),SIEPIN(*),SIEPOO(*)
      INTEGER IOP
C---   VARIABLES LOCALES
      REAL*8 AIN(9),AOO(9)
      INTEGER LLM(9),LLV(6),LDEF(6),IK,IKK,KOP
      DATA LLM/1,4,6,4,2,5,6,5,3/
      DATA LLV/1,5,9,2,6,3/
      DATA LDEF/2,3,4,6,7,8/
C----    PASSAGE VERS LA FORME MATRICIELLE
      DO 10 IK = 1,9
        AIN(IK) = SIEPIN(LLM(IK))
   10 CONTINUE

      IF (IOP.GE.2) THEN
C---     TENSEUR DE DEFORMATIONS
        DO 20 IK = 1,6
          IKK = LDEF(IK)
          AIN(IKK) = 0.5D0*AIN(IKK)
   20   CONTINUE
      END IF

      KOP = MOD(IOP,2)
      IF (KOP.EQ.0) THEN
C        ENTREE : GLOBAL --> SORTIE : LOCAL
        CALL DR3GL1(PPP,AIN,AOO)
      END IF
      IF (KOP.EQ.1) THEN
C        ENTREE : LOCAL --> SORTIE : GLOBAL
        CALL DR3GL2(PPP,AOO,AIN)
      END IF
      IF (KOP.NE.0 .AND. KOP.NE.1) THEN
        CALL UTMESS('F','CHRP3D','PB1')
      END IF
C----    PASSAGE VERS LA FORME VECTORIELLE
      DO 30 IK = 1,6
        SIEPOO(IK) = AOO(LLV(IK))
   30 CONTINUE
C----    POUR LES DEFORMATIONS
      IF (IOP.GE.2) THEN
        SIEPOO(4) = 2.D0*SIEPOO(4)
        SIEPOO(5) = 2.D0*SIEPOO(5)
        SIEPOO(6) = 2.D0*SIEPOO(6)
      END IF
      END
