      SUBROUTINE MTEMP2 ( MAILLA, TEMPE, EXITIM, TIME, CHTREF, NOPASE,
     &                    THVRAI, CHTEMP )
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 17/06/2002   AUTEUR GNICOLAS G.NICOLAS 
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
C-----------------------------------------------------------------------
C     BUT:
C     ON CONSTRUIT UN CHAMP DE TEMPERATURE OU SA DERIVEE LAGRANGIENNE
C     AVEC LE CONCEPT : TEMPE ET LE TEMPS TIME, 
C     SOIT PAR INTERPOLATION, SOIT PAR SIMPLE RECOPIE
C     SI L'INSTANT EST VOISIN D'UN PAS DE TEMPS CALCULE.
C
C     SI ON CONSTRUIT UN FAUX CHAMP DE TEMPERATURE, ON LE CREE COMME LA
C     TEMPERATURE DE REFERENCE (POUR EVITER DES DILATATIONS PAR DEFAUT)
C
C     IN:
C        MAILLA : NOM UTILISATEUR DU MAILLAGE
C        TEMPE  : / NOM UTILISATEUR D'1 EVOL_THER
C                 / NOM UTILISATEUR D'1 CHAMP_GD_TEMP_R
C        EXITIM : VRAI SI ON DONNE UNE VALEUR DU TEMPS TIME
C        TIME   : VALEUR REELLE DU TEMPS
C        CHTREF : NOM DU CHAMP DE TEMPERATURE DE REFERENCE
C                 (CE NOM PEUT ETRE BLANC, DANS CE CAS, SI ON DOIT
C                  CREER UNE CARTE DE TEMPERATURE ON LE FAIT A 0.0)
C        NOPASE : NOM DU PARAMETRE SENSIBLE LE CAS ECHEANT
C
C     OUT:
C        THVRAI : VRAI SI ON TROUVE 1 CHAMP DE TEMPERATURE
C                 FAUX SI ON CREE 1 CHAMP DE TEMPERATURE ARBITRAIRE.
C        CHTEMP : NOM DU CHAMP DE TEMPERATURE TROUVE (OU CREE).
C
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       11/12/00 (OB): TOILETTAGE FORTRAN, CREATION.
C----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      CHARACTER*(*) TEMPE
      CHARACTER*8 MAILLA
      CHARACTER*(*) CHTREF
      CHARACTER*(*) CHTEMP
      CHARACTER*(*) NOPASE
      LOGICAL THVRAI,EXITIM
      REAL*8 TIME
C --------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR,TIME2
      COMPLEX*16 ZC,CBID
      LOGICAL ZL
      CHARACTER*8 ZK8,K8BID
      CHARACTER*16 ZK16,TYSD
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80

C -------------------------------------------------------------------

C DECLARATION VARIABLES LOCALES
C
      CHARACTER*6 NOMPRO
      PARAMETER ( NOMPRO = 'MTEMP2' )
C
      INTEGER NBCHAM,IERD,ICORET,IRET,IBID      
      CHARACTER*1 BASE
      CHARACTER*19 CHTRE2,CH19
      CHARACTER*16 NOMCHA
      

      BASE   = 'V'
      CHTRE2 = CHTREF

C     -- SI LE CHAMP CHTEMP EXISTE DEJA , ON LE DETRUIT:
      CALL DETRSD('CHAMP_GD',CHTEMP(1:19))

      IF (TEMPE(1:8).NE.'        ') THEN
        THVRAI = .TRUE.

        CALL GETTCO(TEMPE(1:8),TYSD)
C
C            DANS LE CAS D'UN CALCUL DERIVE
C            REMARQUE : IL FAUT APPLIQUER GETTCO SUR LA STRUCTURE
C            PREMIERE CAR LE TYPE DES STRUCTURES DERIVEES EST INCONNU
C            PAR GETTCO, FONCTION SUPERVISEUR
C
        IF ( NOPASE.NE.' ' ) THEN
          K8BID = TEMPE
          CALL PSRENC ( K8BID, NOPASE, TEMPE, IRET )
          IF ( IRET.NE.0 ) THEN
            CALL UTMESS('F', NOMPRO,' ON NE TROUVE PAS LE RESULTAT '//
     >                 'DERIVE ASSOCIE A '//K8BID)
          ENDIF
        ENDIF


         IF (TYSD(1:9).EQ.'EVOL_THER') THEN
C           ----------------------------
            CALL DISMOI('F','NB_CHAMP_MAX',TEMPE(1:8),'RESULTAT',NBCHAM,
     &                  K8BID,IERD)
            IF (NBCHAM.GT.0) THEN
               IF ( .NOT.EXITIM ) THEN
                 CALL UTMESS('I',NOMPRO,
     &                        'L''INSTANT DU CALCUL EST PRIS '//
     &                        ' ARBITRAIREMENT A 0.0 ')
                  TIME2 = 0.0D0
                  IF (NBCHAM.GT.1) THEN
                     CALL UTMESS('F',NOMPRO,
     &                           ' ON N''ACCEPTE UN INSTANT ARBITRAIRE'
     &                           //' QUE SI LE CONCEPT TEMPERATURE N''A'
     &                           //' QU''1 CHAMP.')
                  END IF
               ELSE
                  TIME2 = TIME
               END IF
               
C DETERMINATION DU TYPE DE CHAMP A RECUPERER
               NOMCHA = 'TEMP'

C              RECUPERATION DU CHAMP DE TEMPERATURE DANS TEMPE:
C              ------------------------------------------------
               CALL RSINCH(TEMPE(1:8),NOMCHA,'INST',TIME2,CHTEMP(1:19),
     &                     'CONSTANT','CONSTANT',1,BASE,ICORET) 
               IF (ICORET.GE.10) THEN
                 CALL UTDEBM('F',NOMPRO,'INTERPOLATION TEMPERATURE:')
                  CALL UTIMPK('L','EVOL_THER:',1,TEMPE(1:8))
                  CALL UTIMPK('L','NOM SYMBOLIQUE:',1,NOMCHA)
                  CALL UTIMPR('S','INSTANT:',1,TIME2)
                  CALL UTIMPI('L','ICORET:',1,ICORET)
                  CALL UTFINM()
               END IF

            ELSE
              CALL UTMESS('F',NOMPRO,' LE CONCEPT EVOL_THER : '//
     &                     TEMPE(1:8)//
     &                     ' NE CONTIENT AUCUN CHAMP DE TEMPERATURE.')
            END IF

         ELSE IF ((TYSD(1:14).EQ.'CHAM_NO_TEMP_R') .OR.
     &            (TYSD(1:12).EQ.'CARTE_TEMP_R') .OR.
     &            (TYSD(1:16).EQ.'CHAM_ELEM_TEMP_R')) THEN
C           ----------------------------------------------
           CALL UTMESS('I',NOMPRO,'LE CHAMP DE TEMPERATURE UTILISE'//
     &                  ' EST INDEPENDANT DU TEMPS.')
            CH19 = TEMPE(1:8)
            CALL COPISD('CHAMP_GD','V',CH19,CHTEMP(1:19))
            CALL JEDETR(CHTEMP(1:19)//'.TITR')

         ELSE IF (TYSD(1:12).EQ.'CARTE_TEMP_F') THEN
C           ----------------------------------------------
            CH19 = TEMPE(1:8)
            CALL COPISD('CHAMP_GD','V',CH19,CHTEMP(1:19))
            CALL JEDETR(CHTEMP(1:19)//'.TITR')
         ELSE
           CALL UTMESS('F',NOMPRO,'2')
         END IF
      ELSE
         THVRAI = .FALSE.

C        CREATION D'UN CHAMP BIDON ( COPIE DE CHTREF OU CARTE NULLE)
C        ------------------------------------------------------------

         IF (CHTRE2(1:1).NE.' ') THEN
            CALL EXISD('CHAMP_GD',CHTRE2,IRET)
            IF (IRET.EQ.0) THEN
              CALL UTMESS ('F',NOMPRO,
     &            'TEMPERATURE DE REFERENCE A PROBLEME.')
            END IF
            CALL COPISD('CHAMP_GD','V',CHTRE2(1:19),CHTEMP(1:19))
         ELSE
            CALL MECACT('V',CHTEMP(1:19),'MAILLA',MAILLA,'TEMP_R',1,
     &   'TEMP',IBID,0.0D0,CBID,'  ')
         END IF
      END IF
C
      END
