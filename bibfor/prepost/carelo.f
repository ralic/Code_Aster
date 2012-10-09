      SUBROUTINE CARELO(MODELE, CARELE, BASE, CHREL1, CHREL2, CHREL3)
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'
      CHARACTER*1  BASE
      CHARACTER*8  CARELE,MODELE
      CHARACTER*19 CHREL1, CHREL2, CHREL3
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 09/10/2012   AUTEUR DELMAS J.DELMAS 
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
C ----------------------------------------------------------------------
C     BUT:
C       CALCULER LES REPERES LOCAUX DES ELEMENTS
C ----------------------------------------------------------------------
C     IN MODELE  : MODELE
C     IN CARELE  : CARA_ELEM
C     IN BASE    : G/V
C     OUT CHREL1 : 1ER  VECTEUR DU REPERE LOCAL
C     OUT CHREL2 : 2EME VECTEUR DU REPERE LOCAL
C     OUT CHREL3 : 3EME VECTEUR DU REPERE LOCAL
C ---------------------------------------------------------------------
C     VARIABLES LOCALES
C
      CHARACTER*8 LPAIN(4),LPAOU(3)
      CHARACTER*19 LIGRMO,LCHIN(4),LCHOU(3)
      CHARACTER*24 CHGEOM
      LOGICAL LRET
C ----------------------------------------------------------------------
C
      CALL JEMARQ()

      LIGRMO = MODELE//'.MODELE'

      CALL MEGEOM(MODELE,' ',LRET,CHGEOM)
      LCHIN(1) = CHGEOM
      LPAIN(1) = 'PGEOMER'
      LCHIN(2) = CARELE//'.CARORIEN'
      LPAIN(2) = 'PCAORIE'
      LCHIN(3) = CARELE//'.CARCOQUE'
      LPAIN(3) = 'PCACOQU'
      LCHIN(4) = CARELE//'.CARMASSI'
      LPAIN(4) = 'PCAMASS'

      LCHOU(1) = CHREL1
      LPAOU(1) = 'PREPLO1'
      LCHOU(2) = CHREL2
      LPAOU(2) = 'PREPLO2'
      LCHOU(3) = CHREL3
      LPAOU(3) = 'PREPLO3'
      CALL CALCUL('C','REPERE_LOCAL',LIGRMO,4,LCHIN,LPAIN,
     &                                      3,LCHOU,LPAOU,BASE,'NON')

      CALL JEDEMA()
      END
