      SUBROUTINE RECUPE( NOMA, NDIM, NK1D, LREV, MATREV, DEKLAG, PRODEF,
     +                   LONDEF, ORIDEF)
      IMPLICIT     NONE
      INTEGER      NDIM, NK1D
      REAL*8       LREV, DEKLAG, PRODEF, LONDEF
      CHARACTER*8  NOMA, MATREV, ORIDEF
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 29/03/2002   AUTEUR CIBHHBC R.FERNANDES 
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
C ======================================================================
C --- BUT : RECUPERATION DES DONNEES DE LA COMMANDE POST_K_BETA --------
C ======================================================================
C OUT : NOMA   : NOM DU MAILLAGE ---------------------------------------
C --- : NDIM   : DIMENSION DE L'ESPACE ---------------------------------
C --- : NK1D   : NOMBRE D'OCCURENCE ------------------------------------
C --- : LREV   : LONGUEUR DU REVETEMENT --------------------------------
C --- : MATREV : MATERIAU DU REVETEMENT --------------------------------
C --- : PRODEF : PROFONDEUR DU DEFAUT ----------------------------------
C --- : LONDEF : LONGUEUR DU DEFAUT ------------------------------------
C --- : ORIDEF : ORIENTATION DU DEFAUT ---------------------------------
C ======================================================================
      INTEGER      IBID, IER
      CHARACTER*8  K8B
      CHARACTER*16 MOTFAC
C ======================================================================
      CALL JEMARQ()
C ======================================================================
C --- RECUPERATION DU MAILLAGE -----------------------------------------
C ======================================================================
      CALL GETVID ( ' ', 'MAILLAGE', 1,1,1, NOMA, IBID )
C ======================================================================
C --- DIMENSION DE L'ESPACE --------------------------------------------
C ======================================================================
      CALL DISMOI ( 'F', 'Z_CST', NOMA, 'MAILLAGE', IBID, K8B, IER )
      IF ( K8B(1:3) .EQ. 'OUI' ) THEN
         NDIM = 2
      ELSE
         NDIM = 3
      ENDIF
C ======================================================================
C --- RECUPERATION DES CARACTERISTIQUES DU REVETEMENT ------------------
C ======================================================================
      CALL GETVR8 ( ' ', 'EPAIS_REV'  , 1,1,1, LREV    , IBID )
      CALL GETVID ( ' ', 'MATER_REV'  , 1,1,1, MATREV  , IBID )
C ======================================================================
C --- RECUPERATION DES DONNEES DE LA FISSURE ---------------------------
C ======================================================================
      CALL GETVR8 ( 'FISSURE', 'DECALAGE'   , 1,1,1, DEKLAG , IBID )
      IF ( DEKLAG.GT.0.0D0 ) THEN
         CALL UTMESS('F','RECUPE','LE DECALAGE SE TROUVE'//
     +               ' NECESSAIREMENT COTE REVETEMENT. LE DECALAGE'//
     +               ' DOIT ETRE NEGATIF')
      ENDIF
      CALL GETVR8 ( 'FISSURE', 'PROFONDEUR' , 1,1,1, PRODEF , IBID )
      CALL GETVR8 ( 'FISSURE', 'LONGUEUR'   , 1,1,1, LONDEF , IBID )
      CALL GETVTX ( 'FISSURE', 'ORIENTATION', 1,1,1, ORIDEF , IBID )
C ======================================================================
C --- RECUPERATION DU NOMBRE D'OCCURENCE DE K1D ------------------------
C ======================================================================
      MOTFAC = 'K1D'
      CALL GETFAC ( MOTFAC, NK1D )
C ======================================================================
      CALL JEDEMA()
C ======================================================================
      END
