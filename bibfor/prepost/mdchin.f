      SUBROUTINE MDCHIN ( NOFIMD, NOCHMD, TYPENT, TYPGEO, 
     >                    PREFIX, NBTV,
     >                    CODRET )
C_____________________________________________________________________
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 14/05/2002   AUTEUR DURAND C.DURAND 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
C              SEE THE FILE "LICENSE.TERMS" FOR INFORMATION ON USAGE AND
C              REDISTRIBUTION OF THIS FILE.
C ======================================================================
C ======================================================================
C RESPONSABLE GNICOLAS G.NICOLAS
C ======================================================================
C     FORMAT MED - CHAMP - INFORMATIONS - FICHIER CONNU PAR NOM
C            --    --      -                                -
C     DONNE LE NOMBRE DE TABLEAUX DE VALEURS ET LEURS CARACTERISTIQUES
C     TEMPORELLES POUR UN CHAMP ET UN SUPPORT GEOMETRIQUE
C-----------------------------------------------------------------------
C      ENTREES:
C        NOFIMD : NOM DU FICHIER MED
C        NOCHMD : NOM MED DU CHAMP A LIRE
C        TYPENT : TYPE D'ENTITE AU SENS MED
C        TYPGEO : TYPE DE SUPPORT AU SENS MED
C      ENTREES/SORTIES:
C        PREFIX : BASE DU NOM DES STRUCTURES
C                 POUR LE TABLEAU NUMERO I
C                 PREFIX//'.NUME' : T(2I-1) = NUMERO DE PAS DE TEMPS
C                                   T(2I)   = NUMERO D'ORDRE
C                 PREFIX//'.INST' : T(I) = INSTANT S'IL EXISTE
C                 PREFIX//'.MAIL' : T(I) = NOM DU MAILLAGE (K32)
C                 PREFIX//'.UNII' : T(I) = UNITE DE L'INSTANT (K8)
C      SORTIES:
C        NBTV   : NOMBRE DE TABLEAUX DE VALEURS DU CHAMP
C        CODRET : CODE DE RETOUR (0 : PAS DE PB, NON NUL SI PB)
C_____________________________________________________________________
C
      IMPLICIT NONE
C
C 0.1. ==> ARGUMENTS
C
      INTEGER NBTV
      INTEGER TYPENT, TYPGEO
      INTEGER CODRET
C
      CHARACTER*19 PREFIX
      CHARACTER*32 NOCHMD
      CHARACTER*(*) NOFIMD
C
C 0.2. ==> VARIABLES LOCALES
C
      CHARACTER*6 NOMPRO
      PARAMETER ( NOMPRO = 'MDCHIN' )
C
      INTEGER EDLECT
      PARAMETER (EDLECT=0)
C
      INTEGER IDFIMD
C====
C 1. ON OUVRE LE FICHIER EN LECTURE
C====
C
      CALL EFOUVR ( IDFIMD, NOFIMD, EDLECT, CODRET )
      IF ( CODRET.NE.0 ) THEN
        CALL UTDEBM ( 'A', NOMPRO, 'FICHIER ' )
        CALL UTIMPK ( 'S', 'MED : ', 1, NOFIMD )
        CALL UTIMPI ( 'L', 'ERREUR EFOUVR NUMERO ', 1, CODRET )
        CALL UTFINM ()
        CALL UTMESS ( 'F', NOMPRO, 'PROBLEME A L OUVERTURE DU FICHIER' )
      ENDIF
C
C====
C 2. APPEL DU PROGRAMME GENERIQUE
C====
C
      CALL MDCHII ( IDFIMD, NOCHMD, TYPENT, TYPGEO, 
     >              PREFIX, NBTV,
     >              CODRET )
C
C====
C 3. FERMETURE DU FICHIER MED  
C====
C
      CALL EFFERM ( IDFIMD, CODRET )
      IF ( CODRET.NE.0 ) THEN
        CALL UTDEBM ( 'A', NOMPRO, 'FICHIER ' )
        CALL UTIMPK ( 'S', 'MED : ', 1, NOFIMD )
        CALL UTIMPK ( 'L', 'CHAMP : ', 1, NOCHMD )
        CALL UTIMPI ( 'L', 'ERREUR EFFERM NUMERO ', 1, CODRET )
        CALL UTFINM ()
        CALL UTMESS ( 'F', NOMPRO, 'PROBLEME A LA FERMETURE DU FICHIER')
      ENDIF
C
      END
