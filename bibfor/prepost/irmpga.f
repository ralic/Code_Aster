      SUBROUTINE IRMPGA ( NOFIMD,
     &                    CHANOM, TYPECH, NOMTYP,
     &                    NBIMPR, CAIMPI, CAIMPK,
     &                    MODNUM, NUANOM, SDCARM,
     &                    CODRET )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 19/12/2012   AUTEUR PELLET J.PELLET 
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
C     ECRITURE AU FORMAT MED - LOCALISATION POINTS DE GAUSS
C        -  -            -                  -         --
C_______________________________________________________________________
C     ENTREES :
C       NOFIMD : NOM DU FICHIER MED
C       CHANOM : NOM ASTER DU CHAMP
C       TYPECH : TYPE DU CHAMP ('ELEM', 'ELGA')
C       NOMTYP : NOM DES TYPES DE MAILLES ASTER
C       NBIMPR : NOMBRE D'IMPRESSIONS
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
C       MODNUM : INDICATEUR SI LA SPECIFICATION DE NUMEROTATION DES
C                NOEUDS DES MAILLES EST DIFFERENTES ENTRE ASTER ET MED:
C                     MODNUM = 0 : NUMEROTATION IDENTIQUE
C                     MODNUM = 1 : NUMEROTATION DIFFERENTE
C       NUANOM : TABLEAU DE CORRESPONDANCE DES NOEUDS MED/ASTER.
C                NUANOM(ITYP,J): NUMERO DANS ASTER DU J IEME NOEUD DE LA
C                MAILLE DE TYPE ITYP DANS MED.
C     SORTIES :
C         CAIMPK : CARACTERES POUR CHAQUE IMPRESSION
C                  CAIMPK(1,I) = NOM DE LA LOCALISATION ASSOCIEE
C                  CAIMPK(2,I) = NOM DU PROFIL AU SENS MED
C                  CAIMPK(3,I) = NOM DE L'ELEMENT DE STRUCTURE
C       CODRET : CODE DE RETOUR (0 : PAS DE PB, NON NUL SI PB)
C_______________________________________________________________________
C
      IMPLICIT NONE
C
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM
      INTEGER NTYMAX
      PARAMETER (NTYMAX = 69)
C
C 0.1. ==> ARGUMENTS
C
      INTEGER NBIMPR
      INTEGER CAIMPI(10,NBIMPR)
      INTEGER MODNUM(NTYMAX), NUANOM(NTYMAX,*)
C
      CHARACTER*8 NOMTYP(*)
      CHARACTER*8 TYPECH, SDCARM
      CHARACTER*19 CHANOM
      CHARACTER*80 CAIMPK(3,NBIMPR)
      CHARACTER*(*) NOFIMD
C
      INTEGER CODRET
C
C 0.2. ==> COMMUNS
C
C
C 0.3. ==> VARIABLES LOCALES
C
      CHARACTER*6 NOMPRO
      PARAMETER ( NOMPRO = 'IRMPGA' )
C
      INTEGER IFM, NIVINF, NBCOU, NBSEC, NUMAI
      INTEGER IAUX, JAUX, KAUX, LAUX
      INTEGER NBREPG, NBNOSO, NBNOTO, NDIM
      INTEGER NTYPEF, TYGEOM, TYMAST
      INTEGER NBPG, NBSP
      INTEGER NRIMPR
C
      INTEGER LGMAX
      PARAMETER (LGMAX=1000)
C
      REAL*8 REFCOO(3*LGMAX), GSCOO(3*LGMAX), WG(LGMAX)
      REAL*8 RAUX1(3*LGMAX), RAUX2(3*LGMAX), RAUX3(LGMAX)
C
      CHARACTER*4  CHNBCO, CHNBSE
      CHARACTER*10 NONUMA
      CHARACTER*16 NOMTEF, NOMFPG, TYPSEC
      CHARACTER*64 NOLOPG, NOMASU
C
C====
C 1. PREALABLES
C====
C
      CODRET = 0
C
      CALL INFNIV ( IFM, NIVINF )
C
      IF ( NIVINF.GT.1 ) THEN
        WRITE (IFM,1001) 'DEBUT DE '//NOMPRO
        CALL U2MESS('I','MED_74')
        CALL UTFLSH (CODRET)
      ENDIF
 1001 FORMAT(/,4X,10('='),A,10('='),/)
C
C====
C 2. BOUCLAGE SUR LES IMPRESSIONS DEMANDEES
C    ON NE S'INTERESSE QU'A CELLES QUI COMPORTENT PLUS DE 1 POINT DE
C    GAUSS ET/OU PLUS DE 1 SOUS_POINT
C====
C
      DO 20 , NRIMPR = 1 , NBIMPR
C
        NBPG = CAIMPI(2,NRIMPR)
        NBSP = CAIMPI(3,NRIMPR)
C
        NOMASU = ' '
        IF ( NBPG*NBSP.NE.1 ) THEN
C
          TYMAST = CAIMPI(8,NRIMPR)
          TYGEOM = CAIMPI(9,NRIMPR)
C
C 2.1. ==> CARACTERISATIONS DE L'ELEMENT FINI
C          ON DOIT RECUPERER LES COORDONNEES SOUS LA FORME :
C          . ELEMENT 1D : X1 X2 ... ... XN
C          . ELEMENT 2D : X1 Y1 X2 Y2 ... ... XN YN
C          . ELEMENT 3D : X1 Y1 Z1 X2 Y2 Z2 ... ... XN YN ZN
C          C'EST CE QUE MED APPELLE LE MODE ENTRELACE
C          ON DOIT RECUPERER LES POIDS SOUS LA FORME :
C          WG1 WG2 ... ... WGN
C
          IF ( CODRET.EQ.0 ) THEN
C
            IF ( NIVINF.GT.1 ) THEN
              IF ( TYPECH(1:4).EQ.'ELGA' )THEN
                WRITE (IFM,2100) TYPECH, NBPG, NBSP
              ELSE
                WRITE (IFM,2101) TYPECH, NBPG, NBSP
              ENDIF
            ENDIF
 2100     FORMAT('CHAMP DE TYPE ',A,', AVEC,',I5,' POINTS DE GAUSS ET',
     &             I5,' SOUS_POINTS')
 2101     FORMAT('CHAMP DE TYPE ',A,', AVEC,',I5,' POINTS  ET',
     &             I5,' SOUS_POINTS')
C
C 2.1.1. ==> CARACTERISATIONS DE L'ELEMENT FINI QUAND C'EST UN CHAMP
C            AUX POINTS DE GAUSS AVEC PLUS DE 1 POINT DE GAUSS
C
            IF ( TYPECH(1:4).EQ.'ELGA' .AND. NBPG.GT.1 ) THEN
C
C 2.1.1.1. ==> FORMULE GENERALE
C       CODRET : CODE DE RETOUR
C                0 : PAS DE PB
C                1 : LE CHAMP N'EST PAS DEFINI SUR CE TYPE D'ELEMENT
C
              NTYPEF = CAIMPI(1,NRIMPR)
              CALL JENUNO (JEXNUM('&CATA.TE.NOMTE',NTYPEF),NOMTEF)
C
              CALL UTEREF ( CHANOM, TYPECH, NTYPEF, NOMTEF,
     &                      NOMFPG, NBNOSO, NBNOTO, NBREPG, NDIM,
     &                      REFCOO, GSCOO, WG,
     &                      CODRET )
C
              IF ( CODRET.EQ.1 ) THEN
                CODRET = 0
                GOTO 20
              ENDIF
C
C 2.1.1.2. ==> SI CE TYPE DE MAILLE EST RENUMEROTEE ENTRE ASTER ET MED,
C              IL FAUT MODIFIER LA REPARTITION DES NOEUDS
C
              IF ( MODNUM(TYMAST).EQ.1 ) THEN
C
                KAUX = NDIM*NBNOTO
                DO 2111 , IAUX = 1 , KAUX
                  RAUX1(IAUX) = REFCOO(IAUX)
 2111           CONTINUE
                DO 2112 , IAUX = 1 , NBNOTO
                  KAUX = NDIM*(IAUX-1)
                  LAUX = NDIM*(NUANOM(TYMAST,IAUX)-1)
                  DO 2113 , JAUX = 1 , NDIM
                    REFCOO(KAUX+JAUX) = RAUX1(LAUX+JAUX)
 2113             CONTINUE
 2112           CONTINUE
C
              ENDIF
C
              NBCOU = CAIMPI(4,NRIMPR)
              NBSEC = CAIMPI(5,NRIMPR)
              NUMAI = CAIMPI(6,NRIMPR)
              TYPSEC = ' '
              IF ( NBCOU.NE.0.OR.NBSEC.NE.0.OR.NUMAI.NE.0 ) THEN
                IF ( NBCOU.NE.0.AND.NBSEC.EQ.0.AND.NUMAI.EQ.0 ) THEN
                  TYPSEC = 'COQUE'
                  WRITE(CHNBCO,'(I4)')NBCOU
                  NOMASU = 'SECT COQ '//CHNBCO//'C'
                ELSEIF ( NBCOU.NE.0.AND.NBSEC.NE.0.AND.
     &                   NUMAI.EQ.0 )THEN
                  TYPSEC = 'TUYAU'
                  WRITE(CHNBCO,'(I4)')NBCOU
                  WRITE(CHNBSE,'(I4)')NBSEC
                  NOMASU = 'SECT TUY '//CHNBCO//
     &                     'C '//CHNBSE//'S'
                ELSEIF ( NBCOU.EQ.0.AND.NBSEC.EQ.0.AND.
     &                   NUMAI.NE.0 )THEN
                  TYPSEC = 'PMF'
                  WRITE(NONUMA,'(I10)')NUMAI
                  NOMASU = 'SECT PMF '//NONUMA
                ELSE
                  CALL ASSERT(.FALSE.)
                ENDIF
                CALL IRMASE(NOFIMD,TYPSEC,NBCOU,NBSEC,NUMAI,
     &                      SDCARM,NOMASU)
                NBREPG = NBPG
              ELSE
C
C 2.1.1.3. ==> EXTENSION AVEC DES SOUS_POINTS : ON REPRODUIT LA MEME
C              DESCRIPTION DANS CHAQUE 'COUCHE'.
C              C'EST UNE SOLUTION TEMPORAIRE, DANS L'ATTENTE DE
C              L'EVOLUTION MED
                IF ( NBSP.GT.1 ) THEN
C
                  DO 2114 , JAUX = 2 , NBSP
C
                    KAUX = NDIM*NBPG*(JAUX-1)
                    DO 2115 , IAUX = 1 , NDIM*NBPG
                      GSCOO(KAUX+IAUX) = GSCOO(IAUX)
 2115               CONTINUE
                    KAUX = NBPG*(JAUX-1)
                    DO 2116 , IAUX = 1 , NBPG
                      WG(KAUX+IAUX) = WG(IAUX)
 2116               CONTINUE
C
 2114             CONTINUE
C
                ENDIF
                NBREPG = NBPG*NBSP
              ENDIF
C
C 2.1.2. ==> CARACTERISATIONS DE L'ELEMENT FINI QUAND
C            . C'EST UN CHAMP AUX POINTS DE GAUSS AVEC UN SEUL POINT DE
C              GAUSS MAIS PLUSIEURS SOUS_POINTS
C            . UN CHAMP ELEM
C            ON DEFINIT UNE PSEUDO-LOCALISATION AUX POINTS DE GAUSS
C            ON DEDUIT LA DIMENSION DU CODAGE MED DU TYPE DE MAILLE
C            SOUS-JACENTE
C
            ELSEIF((TYPECH(1:4).EQ.'ELGA' .AND. NBPG.EQ.1 ) .OR.
     &             (TYPECH(1:4).EQ.'ELEM'))THEN
C
              IAUX = MOD(CAIMPI(9,NRIMPR),100)
              NDIM = ( CAIMPI(9,NRIMPR) - IAUX ) / 100
C
              NTYPEF = 0
              JAUX = 3*LGMAX
              DO 2121 , IAUX = 1 , JAUX
                REFCOO(IAUX) = 0.D0
                GSCOO(IAUX) = 0.D0
 2121         CONTINUE
              DO 2122 , IAUX = 1 , LGMAX
                WG(IAUX) = 0.D0
 2122         CONTINUE
C
              NOMFPG(1: 8) = NOMTYP(TYMAST)
              DO 2123 , IAUX = 1 , 8
                IF ( NOMFPG(IAUX:IAUX).EQ.' ' ) THEN
                  NOMFPG(IAUX:IAUX) = '_'
                ENDIF
 2123         CONTINUE
              NOMFPG(9:16) = TYPECH
C
              NBNOTO = MOD(TYGEOM,100)
              NBREPG = NBSP
C
            ENDIF
C
          ENDIF
C
C 2.2. ==> ON ECRIT LA LOCALISATION
C
          IF ( CODRET.EQ.0 ) THEN
C
           CALL IRMPG1 ( NOFIMD,
     &                   NOMFPG, NBNOTO, NBREPG, NBSP, NDIM, TYGEOM,
     &                   REFCOO, GSCOO, WG,
     &                   RAUX1, RAUX2, RAUX3,
     &                   NOLOPG, NOMASU, CODRET )
C
           CAIMPK(1,NRIMPR) = NOLOPG
           CAIMPK(3,NRIMPR) = NOMASU
C
          ENDIF
C
        ENDIF
C
   20 CONTINUE
C
C====
C 3. LA FIN
C====
C
      IF ( NIVINF.GT.1 ) THEN
        WRITE (IFM,1001) 'FIN DE '//NOMPRO
      ENDIF
C
      END
