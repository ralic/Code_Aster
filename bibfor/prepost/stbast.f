      SUBROUTINE STBAST ( NFIE, NFIS, IFM, LGRCOU )
      IMPLICIT  NONE
      INTEGER             NFIE, NFIS, IFM
      LOGICAL             LGRCOU
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 02/12/2002   AUTEUR CIBHHLV L.VIVAN 
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
C
C  =================================================================
C  !                                                               !
C  !   AUTEUR: J.F.LAMAUDIERE                       DATE:03/04/90  !
C  !                                                               !
C  =================================================================
C  !                                                               !
C  !   FONCTION : LANCEMENT DE L'INTERFACE SUPERTAB-TRIFOU         !
C  !                                                               !
C  !   REMARQUE : TOUTE ALTERATION DANS UN COMMON ENTRAINE UNE     !
C  !              ALTERATION DANS L'AUTRE COMMON                   !
C  !                                                               !
C  =================================================================
C
C
      CALL DEFUFI ( NFIE, 'UNIVERSEL'      )
      CALL DEFUFI ( NFIS, 'FICHIER-MODELE' )
C
      CALL PRESUP ( LGRCOU )
C
      WRITE(NFIS,*) 'FIN'
      REWIND NFIS
C
      END
