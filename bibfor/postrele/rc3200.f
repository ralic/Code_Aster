      SUBROUTINE RC3200
      IMPLICIT   NONE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 01/10/2002   AUTEUR CIBHHLV L.VIVAN 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C     ------------------------------------------------------------------
C     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
C
C     ------------------------------------------------------------------
      INTEGER      N1
      CHARACTER*8  MATER
C DEB ------------------------------------------------------------------
C
C     ------------------------------------------------------------------
C                           LES SITUATIONS
C     ------------------------------------------------------------------
C
      CALL RC32SI
C
C     ------------------------------------------------------------------
C              RECUPERATION DES CARACTERISTIQUES MATERIAU
C     ------------------------------------------------------------------
C
      CALL GETVID ( ' ', 'MATER', 1,1,1, MATER, N1 )
C
      CALL RC32MA ( MATER )
C
C     ------------------------------------------------------------------
C              RECUPERATION DU CALCUL MECANIQUE UNITAIRE 
C     ------------------------------------------------------------------
C
      CALL RC32MU
C
C     ------------------------------------------------------------------
C              RECUPERATION DES CHARGES MECANIQUES
C     ------------------------------------------------------------------
C
      CALL RC32CM
C
C     ------------------------------------------------------------------
C              RECUPERATION DES RESULTATS THERMIQUES
C     ------------------------------------------------------------------
C
      CALL RC32TH
C
C     ------------------------------------------------------------------
C              CALCULS DES AMPLITUDES DE CONTRAINTES
C     ------------------------------------------------------------------
C
C     CALCUL DES AMPLITUDES DE CONTRAINTES QUI CORRESPONDENT AUX
C     COMBINAISONS DE TOUS LES ETATS STABILISES APPARTENANT AUX
C     SITUATIONS D'UN GROUPE DONNE
C
C --- CALCUL DES AMPLITUDES DE CONTRAINTES
C     CALCUL DU FACTEUR D'USAGE 
C     -------------------------
C
      CALL RC32AC ( MATER )
C
C
C --- STOCKAGE DES RESULTATS
C     ----------------------
C
      CALL RC32RS
C
      CALL JEDETC ( 'V' , '&&RC3200' , 1 )
C
      END
