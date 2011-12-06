      INTEGER  FUNCTION NUFLOC(NDIM,NSC,ISC)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/03/2010   AUTEUR ANGELINI O.ANGELINI 
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
C ======================================================================
      IMPLICIT NONE
C DONNE LE NUMERO  LOCAL DE LA FACE DONT LES NUMEROS LOCAUX DE SOMMETS
C SONT ISC 
C
C
      INTEGER NDIM,NSC,ISC(1:NSC)
C      
      CHARACTER*8  ELREFE
      INTEGER FA,CLE
C      
      CALL ELREF1(ELREFE)
C      
      IF(NDIM.EQ.2) THEN
      CALL ASSERT(NSC.EQ.2)
      CALL ASSERT(ISC(1).NE.ISC(2))
C
C  ON NE SAIT PAS DANS QUEL ORDRE SONT DONNES LES SOMETS VOISINS
C  SI ABS(ISC1-ISC2)=1 ON N'EST PAS SUR LE DERNIER SOMMET ET IFA = 
C     LE PLUS PETIT
C  SI ABS(ISC1-ISC2)>1 ON EST SUR LE DERNIER SOMMET ET IFA = 
C     LE PLUS GRAND
C
         IF (ABS(ISC(1)-ISC(2)).EQ.1) THEN
          FA= MIN(ISC(1),ISC(2))
         ELSE
          FA= MAX(ISC(1),ISC(2))
         ENDIF  
      ELSE
        IF(ELREFE.EQ.'H27') THEN
         CALL ASSERT(NSC.EQ.4)
         CLE = ISC(1)**2+ISC(2)**2+ISC(3)**2+ISC(4)**2
         IF      (CLE.EQ.30) THEN
          FA = 1
         ELSE IF (CLE.EQ.66) THEN
          FA = 2
         ELSE IF (CLE.EQ.98) THEN
          FA = 3
         ELSE IF (CLE.EQ.90) THEN
          FA = 4
         ELSE IF (CLE.EQ.106) THEN
          FA = 5
         ELSE IF (CLE.EQ.174) THEN
          FA = 6
         ELSE 
          CALL ASSERT(.FALSE.)
         ENDIF
        ELSE IF(ELREFE.EQ.'T9') THEN
         CALL ASSERT(NSC.EQ.3)
         FA=10-ISC(1)-ISC(2)-ISC(3)
         CALL ASSERT(FA.GE.1.AND.FA.LE.4)
        ELSE
         CALL U2MESK('F','VOLUFINI_12', 1 ,ELREFE)
        ENDIF
      ENDIF
      NUFLOC=FA
      END
