      SUBROUTINE GOUJP1 ( UN, NGIEXE)
      IMPLICIT   NONE
      INTEGER    UN, NGIEXE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 02/02/2000   AUTEUR F1BHHAJ J.ANGLES 
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
C     MACR_GOUJ2E_MAIL
C
C     ECRIT DANS UN FICHIER  LES DONNEES GIBI DE LA PROCEDURE
C                                                "GOUJON-FILET-BRIDE"
C     PREMIERE PARTIE ( AVANT LES DONNEES UTILISATEUR )
C
C     ------------------------------------------------------------------
C     UN     = UNITE LOGIQUE CORRESPONDANT AU FICHIER DE DONNEES GIBI
C              (.datg)
C     NGIEXE = NIVEAU GIBI DANS LEQUEL LE MAILLAGE SERA ECRIT EN SORTIE
C     ------------------------------------------------------------------
C
      CALL ASPECR(UN,
     +'************************************************************'//
     +'***')
      CALL ASPECR(UN,
     +'*')
      CALL ASPECR(UN,
     +'* CREATION DU MAILLAGE DE GOUJON, FILETS ET BRIDE 2D AXIS')
      CALL ASPECR(UN,
     +'* ----------------------------------------------------------'//
     +'---')
      CALL ASPECR(UN,
     +'*')
      CALL ASPECR(UN,
     +'*************************************************')
      CALL ASPECR(UN,
     +'*                VERSION 1.0                    *')
      CALL ASPECR(UN,
     +'*            VERSION DU 15/07/1999              *')
      CALL ASPECR(UN,
     +'*************************************************')
      CALL ASPECR(UN,
     +'*')
      WRITE(UN,*) 'OPTI NIVE ',NGIEXE,' ;'
      CALL ASPECR(UN,
     +'OPTI ECHO 0;')
      CALL ASPECR(UN,
     +'**********************************************************'//
     +'*****')
      CALL ASPECR(UN,
     +'*****              PROGRAMME  PRINCIPAL                 **'//
     +'*****')
      CALL ASPECR(UN,
     +'**********************************************************'//
     +'*****')
      CALL ASPECR(UN,
     +'*')
      CALL ASPECR(UN,
     +'   OPTION DIME 2 ELEM QUA4 ECHO 0 ;')
C
      END
