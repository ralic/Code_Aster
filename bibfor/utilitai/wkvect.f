      SUBROUTINE WKVECT( NOM, CARAC, DIM, LDEC )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)      NOM, CARAC
      INTEGER                        DIM, LDEC
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 21/02/96   AUTEUR VABHHTS J.PELLET 
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

C     ------------------------------------------------------------------
C     CREATION D'UN VECTEUR
C     ------------------------------------------------------------------
C IN  NOM   : CH*24 : NOM (COMPLET)  DU VECTEUR
C IN  CARAC : CH    : DESCRIPTION DES CARACTERISTIQUES POUR JECREO
C IN  DIM   : IS    : TAILLE DU VECTEUR
C OUT LDEC  : IS    : DECALAGE
C     ------------------------------------------------------------------
      CHARACTER*4 CBID
C     ------------------------------------------------------------------
      CALL JECREO( NOM, CARAC)
      CALL JEECRA( NOM, 'LONMAX', DIM , CBID )
      CALL JEECRA( NOM, 'LONUTI', DIM , CBID )
      CALL JEVEUO( NOM, 'E' , LDEC  )
      END
