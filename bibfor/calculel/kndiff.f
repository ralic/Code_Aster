      SUBROUTINE KNDIFF ( LONG, LK1,L1, LK2,L2, LK3,L3 )
      IMPLICIT NONE
      INTEGER             LONG, L1 ,L2, L3
      CHARACTER*(*)       LK1(L1), LK2(L2), LK3(L3)
C ---------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 22/02/2000   AUTEUR CIBHHLV L.VIVAN 
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
C RESPONSABLE VABHHTS J.PELLET
C A_UTIL
C
C BUT: DIFFERENCE ENTRE 2 LISTES   LK3 = LK1 - LK2
C ---------------------------------------------------------------------
C     ARGUMENTS:
C LONG   IN   I     : 8,16 OU 24 : LONGUEUR DES CHAINES DE LK1 ET LK2
C LK1    IN   V(K*) : LISTE DE K* 
C L1     IN   I     : LONGUEUR DE LA LISTE LK1
C LK2    IN   V(K*) : LISTE DES K* 
C L2     IN   I     : LONGUEUR DE LA LISTE LK2
C LK3    OUT  V(K*) : LISTE DES K* QUI DOIT CONTENIR LK1 - LK2
C L3     OUT  I     : LONGUEUR DE LA LISTE LK3
C----------------------------------------------------------------------
      INTEGER  KNINDI, K1, K2, K3
C DEB
C
      IF ((LONG.NE.8) .AND. (LONG.NE.16) .AND.
     &    (LONG.NE.24)) CALL UTMESS('F','KNDIFF','LONG=8,16 OU 24')
C
      L3 = 0
      DO 10, K1 = 1 , L1
C          -- ON VERIFIE QUE LK1(K1) SE TROUVE DANS LK2 :
         K2 = KNINDI ( LONG, LK1(K1), LK2, L2 )
C
         IF ( K2 .EQ. 0 ) THEN
            L3 = L3 + 1
            LK3(L3) = LK1(K1)
         ENDIF
C
 10   CONTINUE
C
      DO 20, K2 = 1 , L2
C          -- ON VERIFIE QUE LK2(K2) SE TROUVE DANS LK1 :
         K1 = KNINDI ( LONG, LK2(K2), LK1, L1 )
C
         IF ( K1 .EQ. 0 ) THEN
            K3 = KNINDI ( LONG, LK2(K2), LK3, L3 )
            IF ( K3 .EQ. 0 ) THEN
               L3 = L3 + 1
               LK3(L3) = LK2(K2)
            ENDIF
         ENDIF
C
 20   CONTINUE
C
      END
