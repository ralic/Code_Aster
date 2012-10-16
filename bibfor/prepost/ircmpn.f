      SUBROUTINE IRCMPN ( NOFIMD,
     >                    NCMPRF, NCMPVE, NUMCMP, EXICMP,
     >                    NBVATO, NBNOEC, LINOEC, ADSL,
     >                    CAIMPI, CAIMPK,
     >                    PROFAS, INNOCE)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 16/10/2012   AUTEUR SELLENET N.SELLENET 
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
C RESPONSABLE SELLENET N.SELLENET
C_______________________________________________________________________
C  ECRITURE D'UN CHAMP - FORMAT MED - PROFIL POUR LES NOEUDS
C     -  -       -              -     -               -
C_______________________________________________________________________
C     ENTREES :
C       NOFIMD : NOM DU FICHIER MED
C       NCMPRF : NOMBRE DE COMPOSANTES DU CHAMP DE REFERENCE
C       NCMPVE : NOMBRE DE COMPOSANTES VALIDES EN ECRITURE
C       NUMCMP : NUMEROS DES COMPOSANTES VALIDES
C       EXICMP : EXISTENCE DES COMPOSANTES PAR MAILLES
C       NBVATO : NOMBRE DE VALEURS TOTALES
C       NBNOEC : NOMBRE D'ENTITES A ECRIRE (O, SI TOUTES)
C       LINOEC : LISTE DES ENTITES A ECRIRE SI EXTRAIT
C       ADSK, D, ... : ADRESSES DES TABLEAUX DES CHAMPS SIMPLIFIES
C       INNOCE : TABLEAU INDICATEUR DE NOEUD CENTRE
C                INNOCE(INO)=1 SI LE NOEUD INO EST UN NOEUD CENTRE
C                D'UNE MAILLE TRIA7,QUAD9,PENTA18 OU HEXA27
C       
C     SORTIES :
C         CAIMPI : ENTIERS POUR CHAQUE IMPRESSION
C                  CAIMPI(1,I) = TYPE D'EF / MAILLE ASTER (0, SI NOEUD)
C                  CAIMPI(2,I) = NOMBRE DE POINTS (GAUSS OU NOEUDS)
C                  CAIMPI(3,I) = NOMBRE DE SOUS-POINTS
C                  CAIMPI(4,I) = NOMBRE DE COUCHES
C                  CAIMPI(5,I) = NOMBRE DE SECTEURS
C                  CAIMPI(6,I) = NOMBRE DE FIBTRES
C                  CAIMPI(7,I) = NOMBRE DE MAILLES A ECRIRE
C                  CAIMPI(8,I) = TYPE DE MAILLES ASTER (0, SI NOEUD)
C                  CAIMPI(9,I) = TYPE GEOMETRIQUE AU SENS MED
C                  CAIMPI(10,I) = NOMBRE TOTAL DE MAILLES IDENTIQUES
C         CAIMPK : CARACTERES POUR CHAQUE IMPRESSION
C                  CAIMPK(1,I) = NOM DE LA LOCALISATION ASSOCIEE
C                  CAIMPK(2,I) = NOM DU PROFIL AU SENS MED
C                  CAIMPK(3,I) = NOM DE L'ELEMENT DE STRUCTURE
C       PROFAS : PROFIL ASTER. C'EST LA LISTE DES NUMEROS ASTER DES
C                NOEUDS POUR LESQUELS LE CHAMP EST DEFINI
C
C  COMMENTAIRE : C'EST AVEC L'USAGE DE ZL(ADSL) QU'IL FAUT FILTRER LES
C                COMPOSANTES. SEUL MOYEN FIABLE AVEC UN CHAMELEM
C                SIMPLIFIE.
C_______________________________________________________________________
C
      IMPLICIT NONE
C
C 0.1. ==> ARGUMENTS
C
      INCLUDE 'jeveux.h'
      INTEGER NBVATO, NCMPRF, NCMPVE
      INTEGER NUMCMP(NCMPRF),INNOCE(NBVATO)
      INTEGER NBNOEC
      INTEGER LINOEC(*)
      INTEGER ADSL
      INTEGER CAIMPI(10,1)
      INTEGER PROFAS(NBVATO)
C
      CHARACTER*(*) NOFIMD
      CHARACTER*80 CAIMPK(3,1)
C
      LOGICAL EXICMP(NBVATO)
C
C 0.2. ==> COMMUNS
C
C
C 0.3. ==> VARIABLES LOCALES
C
      CHARACTER*6 NOMPRO
      PARAMETER ( NOMPRO = 'IRCMPN' )
C
      CHARACTER*80 EDNOPF
      PARAMETER ( EDNOPF=' ' )
      CHARACTER*80 EDNOGA
      PARAMETER ( EDNOGA=' ' )
C                         12345678901234567890123456789012
C
      INTEGER TYPNOE
      PARAMETER (TYPNOE=0)
      INTEGER EDNOPG
      PARAMETER (EDNOPG=1)
C
      CHARACTER*64 NOPROF
C
      INTEGER IFM, NIVINF
C
      INTEGER IAUX, JAUX
      INTEGER NRCMP
      INTEGER NVAL
C
C====
C 1. PREALABLES
C====
C
      CALL INFNIV ( IFM, NIVINF )
C
      IF ( NIVINF.GT.1 ) THEN
        WRITE (IFM,1001) 'DEBUT DE '//NOMPRO
      ENDIF
 1001 FORMAT(/,4X,10('='),A,10('='),/)
C
C====
C 2. ON REMPLIT UN PREMIER TABLEAU PAR NOEUD :
C    VRAI DES QU'UNE DES COMPOSANTES DU CHAMP EST PRESENTE SUR LE NOEUD
C    FAUX SINON
C    REMARQUES: 1- ON EXAMINE LES NCMPVE COMPOSANTES QUI SONT DEMANDEES,
C    MAIS IL FAUT BIEN TENIR COMPTE DE NCMPRF, NOMBRE DE COMPOSANTES DE
C    REFERENCE, POUR L'ADRESSAGE DANS LE TABLEAU ADSL
C               2- SI LE NOEUD EST UN NOEUD CENTRE, ON L'OUBLIE
C====
C
      DO 21 , IAUX = 0 , NBVATO-1
C
        IF(INNOCE(IAUX+1).EQ.1)THEN
          EXICMP(IAUX+1) = .FALSE.
          GOTO 21
        ENDIF
C
        JAUX = ADSL-1+IAUX*NCMPRF
        DO 211 , NRCMP = 1 , NCMPVE
          IF ( ZL(JAUX+NUMCMP(NRCMP)) ) THEN
            EXICMP(IAUX+1) = .TRUE.
            GOTO 21
          ENDIF
  211   CONTINUE
C
   21 CONTINUE
C
C====
C 3. PROFAS : LISTE DES NOEUDS POUR LESQUELS ON AURA IMPRESSION
C    UN NOEUD EN FAIT PARTIE SI ET SEULEMENT SI AU MOINS UNE COMPOSANTE
C    Y EST DEFINIE ET S'IL FAIT PARTIE DU FILTRAGE DEMANDE
C====
C
      NVAL = 0
C
C 3.1. ==> SANS FILTRAGE : C'EST LA LISTE DES NOEUDS AVEC UNE COMPOSANTE
C          VALIDE
C
      IF ( NBNOEC.EQ.0 ) THEN
C
        DO 31 , IAUX = 1 , NBVATO
          IF ( EXICMP(IAUX) ) THEN
            NVAL = NVAL + 1
            PROFAS(NVAL) = IAUX
          ENDIF
   31   CONTINUE
C
C 3.2. ==> AVEC FILTRAGE
C
      ELSE
C
        DO 32 , JAUX = 1 , NBNOEC
          IAUX = LINOEC(JAUX)
          IF ( EXICMP(IAUX) ) THEN
            NVAL = NVAL + 1
            PROFAS(NVAL) = IAUX
          ENDIF
   32   CONTINUE
C
      ENDIF
C
C====
C 4. CARACTERISATIONS DES IMPRESSIONS
C====
C
C                  CAIMPI(1,I) = TYPE D'EF / MAILLE ASTER (0, SI NOEUD)
      CAIMPI(1,1) = 0
C                  CAIMPI(2,I) = NOMBRE DE POINTS DE GAUSS
      CAIMPI(2,1) = EDNOPG
C                  CAIMPI(3,I) = NOMBRE DE SOUS-POINTS
      CAIMPI(3,1) = EDNOPG
C                  CAIMPI(4,I) = NOMBRE DE MAILLES A ECRIRE
      CAIMPI(4,1) = EDNOPG
C                  CAIMPI(4,I) = NOMBRE DE MAILLES A ECRIRE
      CAIMPI(5,1) = EDNOPG
C                  CAIMPI(4,I) = NOMBRE DE MAILLES A ECRIRE
      CAIMPI(6,1) = EDNOPG
C                  CAIMPI(4,I) = NOMBRE DE MAILLES A ECRIRE
      CAIMPI(7,1) = NVAL
C                  CAIMPI(5,I) = TYPE DE MAILLES ASTER (0, SI NOEUD)
      CAIMPI(8,1) = 0
C                  CAIMPI(6,I) = TYPE GEOMETRIQUE AU SENS MED
      CAIMPI(9,1) = TYPNOE
C                  CAIMPI(7,I) = NOMBRE DE MAILLES IDENTIQUES
      CAIMPI(10,1) = NBVATO
C                  CAIMPK(1,I) = NOM DE LA LOCALISATION ASSOCIEE
      CAIMPK(1,1) = EDNOGA
C                  CAIMPK(2,I) = NOM DU PROFIL AU SENS MED
      CAIMPK(2,1) = EDNOPF
C                  CAIMPK(3,I) = NOM DE L'ELEMENT DE STRUCTURE
C                                AU SENS MED
      CAIMPK(3,1) = EDNOPF
C
CGN      WRITE(IFM,*) 'A LA FIN DE 4, CAIMPI :'
CGN      WRITE(IFM,4000) (CAIMPI(IAUX,1),IAUX = 1 , 7)
CGN 4000 FORMAT('TYPN =',I4,', NB PG =',I4,', NB SPT =',I4,
CGN     >       ', NBVAL ECR =',I8,', MA ASTER =',I4,
CGN     >       ', TYPE GEO MED =',I4,', NBVAL TOT =',I8)
C
      IF ( NIVINF.GT.1 ) THEN
        WRITE (IFM,3301) NOMPRO, ' : NOMBRE TOTAL DE VALEURS    : ',
     >                   NBVATO
        WRITE (IFM,3301) NOMPRO, ' : NOMBRE DE VALEURS A ECRIRE : ',
     >                   NVAL
      ENDIF
 3301 FORMAT(4X,A6,A,I8)
C
C====
C 5. STOCKAGE DU PROFIL DANS LE FICHIER MED
C    REMARQUE : DANS LE CAS DES NOEUDS, IL Y A IDENTITE ENTRE LES
C               NUMEROTATIONS ASTER ET MED DES NOEUDS (CF IRMMNO)
C====
C
      IF ( NVAL.NE.NBVATO ) THEN
C
        IAUX = 0
        CALL IRCMPF ( NOFIMD, NVAL, PROFAS, NOPROF )
C
        CAIMPK(2,1) = NOPROF
C
      ENDIF
CGN      WRITE(IFM,5000) (CAIMPK(IAUX,1),IAUX = 1 , 2)
CGN 5000 FORMAT('NLOLOPG = ',A,', NOMPROF = ',A)
C
C====
C 6. LA FIN
C====
C
      IF ( NIVINF.GT.1 ) THEN
        WRITE (IFM,1001) 'FIN DE '//NOMPRO
      ENDIF
C
      END
