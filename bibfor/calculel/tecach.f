      SUBROUTINE TECACH(STPCAT,STPEXI,NMPARZ,NVAL,ITAB)
      IMPLICIT NONE

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 13/01/2003   AUTEUR VABHHTS J.PELLET 
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
C RESPONSABLE                            VABHHTS J.PELLET
C     ARGUMENTS:
C     ----------
      LOGICAL STPCAT,STPEXI
      CHARACTER*8 NOMPAR
      CHARACTER*(*) NMPARZ

      INTEGER NVAL,ITAB(NVAL)
C     -----------------------------------------------------------------

C     BUT:
C     ---
C     OBTENIR DES INFORMATIONS SUR LE CHAMP LOCAL ASSOCIE A UN
C     PARAMETRE DANS UNE ROUTINE TE00IJ (OU EN DESSOUS)

C     LA 1ERE INFO OBTENUE (ITAB(1)) EST TRES IMPORTANTE :
C       SI (ITAB(1)>0 ) : C'EST L'ADRESSE DU CHAMP LOCAL
C       SI (ITAB(1)=0)  : LE CHAMP LOCAL N'EXISTE PAS


C     ENTREES:
C     --------
C NMPARZ  : NOM DU PARAMETRE DE L'OPTION
C NVAL    : NOMBRE DE VALEURS DESIREES DANS ITAB(*)
C STPCAT  : PERMET DE DIRE A TECACH DE S'ARRETER SI
C           LE PARAMETRE N'APPARAIT PAS DANS LE CATALOGUE
C           DU TYPE_ELEMENT (OU CELUI DE L'OPTION)

C            .TRUE.  -> ON S'ARRETE EN ERREUR <F>
C            .FALSE. -> ON NE S'ARRETE PAS EN ERREUR <F>

C STPEXI  : PERMET DE DIRE A TECACH DE S'ARRETER SI
C       LE CHAMP_LOCAL ASSOCIE AU PARAMETRE N'EXISTE PAS
C      (CE QUI PEUT ARRIVER POUR 2 RAISONS :
C        1) LE PARAMETRE N'APPARTIENT PAS A LPAIN (OU LPAOUT)
C        2) LE CHAMP GLOBAL (CHIN) ASSOCIE AU PARAMETRE N'EXISTE PAS)

C       .TRUE.  -> ON S'ARRETE EN ERREUR <F>
C       .FALSE. -> ON NE S'ARRETE PAS EN ERREUR <F>



C SORTIES:
C --------
C ITAB(1)   : / ADRESSE DU CHAMP_LOCAL (DANS ZR, ZC, ....)
C            / CODE RETOUR DE L'INTERROGATION

C ITAB(1) >0 : LE PARAMETRE EXISTE DANS LES CATALOGUES
C              LE CHAMP GLOBAL EXISTE
C              => LE CHAMP LOCAL EXISTE

C ITAB(1) =0 : LE PARAMETRE N'EXISTE PAS DANS LES CATALOGUES
C              OU LE CHAMP GLOBAL N'EXISTE PAS
C              => LE CHAMP LOCAL N'EXISTE PAS


C ITAB(2)   : LONGUEUR DU CHAMP_LOCAL DANS LE CATALOGUE
C             (NE TIENT PAS COMPTE DE NCDYN ET NBSPT
C              VOIR CI-DESSOUS ITAB(6) ET ITAB(7) )
C ITAB(3)   : NOMBRE DE POINTS DE LOCALISATION DU CHAMP
C ITAB(4)   : 9999 (INUTILISE)
C ITAB(5)   : TYPE_SCALAIRE DU CHAMP :
C             1 --> REEL
C             2 --> COMPLEXE
C             3 --> ENTIER
C             4 --> K8
C             5 --> K16
C             6 --> K24
C ITAB(6)   : NCDYN : NOMBRE DE CMP POUR LA GRANDEUR VARI_R
C ITAB(7)   : NBSPT : NOMBRE DE SOUS-POINTS

C     -----------------------------------------------------------------
      LOGICAL EXICHL,ETENDU
      INTEGER IAOPTT,LGCO,IAOPMO,ILOPMO,IAOPNO,ILOPNO,IAOPDS,IAOPPA
      INTEGER IAMLOC,ILMLOC,IACOVE,ILCOVE,IADSGD,IAWMOL
      INTEGER NPARIO,IAWLOC,IAWTYP,NBELGR,IGR,IEL,IPARG,INDIK8,IAD
      INTEGER NPARIN,JCELD,IACHII,IACHOI,ADIEL,DEBUGR,NBSPT,NCDYN
      INTEGER LGCATA,IACHIK,IACHIX,IACHOK,DECAEL,LONCHL,IACHLO,ILCHLO
      INTEGER K,KK

      COMMON /CAII02/IAOPTT,LGCO,IAOPMO,ILOPMO,IAOPNO,ILOPNO,IAOPDS,
     +       IAOPPA,NPARIO,NPARIN,IAMLOC,ILMLOC,IADSGD
      CHARACTER*16 OPTION,NOMTE,NOMTM
      COMMON /CAKK01/OPTION,NOMTE,NOMTM
      COMMON /CAII06/IAWLOC,IAWTYP,NBELGR,IGR
      COMMON /CAII04/IACHII,IACHIK,IACHIX
      COMMON /CAII07/IACHOI,IACHOK
      COMMON /CAII08/IEL
      INTEGER CAINDZ(512),CAPOIZ
      COMMON /CAII12/CAINDZ,CAPOIZ

C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR,JEXR8
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
C ---------------- FIN COMMUNS NORMALISES  JEVEUX  --------------------

C     DEB--------------------------------------------------------------

      NOMPAR = NMPARZ
      ITAB(1)=0

      IF (NVAL.LT.1) GO TO 10
      IF (NVAL.GT.7) GO TO 10


C     -- SI LE PARAMETRE N'APPARTIENT PAS A L'OPTION :
C     -------------------------------------------------
      EXICHL = .FALSE.

C     -- RECHERCHE DE LA CHAINE NOMPAR AVEC MEMOIRE SUR TOUT 'CALCUL'
      CAPOIZ=CAPOIZ+1
      IF (CAPOIZ.GT.512) THEN
        IPARG = INDIK8(ZK8(IAOPPA),NOMPAR,1,NPARIO)
      ELSE
        IF (ZK8(IAOPPA-1+CAINDZ(CAPOIZ)).EQ.NOMPAR) THEN
          IPARG=CAINDZ(CAPOIZ)
        ELSE
          IPARG = INDIK8(ZK8(IAOPPA),NOMPAR,1,NPARIO)
          CAINDZ(CAPOIZ)=IPARG
        ENDIF
      ENDIF

      IF (IPARG.EQ.0) THEN
        IF (STPCAT) THEN
          CALL UTMESS('E','TECACH','LE PARAMETRE:'//NOMPAR//
     +                          ' N''EST PAS UN '//
     +                          'PARAMETRE DE L''OPTION:'//OPTION)
          CALL CONTEX(OPTION,0,' ',' ',0,'STOP')
        END IF
        GO TO 10

      ELSE
        IACHLO = ZI(IAWLOC-1+7* (IPARG-1)+1)
        ILCHLO = ZI(IAWLOC-1+7* (IPARG-1)+2)
        LGCATA = ZI(IAWLOC-1+7* (IPARG-1)+4)

C     -- SI IACHLO=-1    : LE CHAMP N'EXISTE PAS (GLOBALEMENT)
C     -- SI LGCATA=-1 : LE PARAMETRE N'EXISTE PAS POUR LE TYPE_ELEMENT
C     -------------------------------------------------
        IF (IACHLO.EQ.-1) THEN
          IF (STPEXI) THEN
            CALL UTMESS('E','TECACH',
     +                            'ERREUR DE PROGRAMMATION :'//
     +               'ON NE TROUVE PAS DANS LES ARGUMENTS DE LA ROUTINE'
     +                            //
     +                       ' CALCUL DE CHAMP A ASSOCIER AU PARAMETRE:'
     +                            //NOMPAR//' (OPTION:'//OPTION//
     +                            ' TYPE_ELEMENT:'//NOMTE//')')
            CALL CONTEX(OPTION,0,NOMPAR,' ',0,'STOP')
          END IF
          IF (LGCATA.EQ.-1) THEN
            IF (STPCAT) THEN
              CALL UTMESS('E','TECACH',
     +                              'LE PARAMETRE:'//NOMPAR//
     +                              ' N''EST PAS UN '//
     +                              'PARAMETRE DE L''OPTION:'//OPTION//
     +                              ' POUR '//'LE TYPE_ELEMENT: '//
     +                              NOMTE)
              CALL CONTEX(OPTION,0,NOMPAR,' ',0,'STOP')
            END IF
          END IF
        ELSE
          IF (LGCATA.EQ.-1) THEN
            IF (STPCAT) THEN
              CALL UTMESS('E','TECACH',
     +                              'LE PARAMETRE:'//NOMPAR//
     +                              ' N''EST PAS UN '//
     +                              'PARAMETRE DE L''OPTION:'//OPTION//
     +                              ' POUR '//'LE TYPE_ELEMENT: '//
     +                              NOMTE)
              CALL CONTEX(OPTION,0,NOMPAR,' ',0,'STOP')
            END IF
          ELSE
            EXICHL = .TRUE.
          END IF
        END IF
      END IF


C     -------------------------------------------------

      IF (.NOT.EXICHL) THEN
        ITAB(1) = 0
        GO TO 10
      ELSE


C     ITAB(1) : ADRESSE DU CHAMP LOCAL POUR L'ELEMENT IEL :
C     -----------------------------------------------------

C     -- CALCUL DE ITAB(1),LONCHL,DECAEL,NBSPT,NCDYN :
C     -------------------------------------------------
      CALL CHLOET(IPARG,ETENDU,JCELD)
      IF (ETENDU) THEN
        ADIEL = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+4+4* (IEL-1)+4)
        DEBUGR = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+8)
        NBSPT = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+4+4* (IEL-1)+1)
        NCDYN = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+4+4* (IEL-1)+2)
        IF (LGCATA.NE.ZI(JCELD-1+ZI(JCELD-1+4+IGR)+3)) CALL UTMESS('F',
     +      'TECACH','STOP')
        DECAEL= (ADIEL-DEBUGR)
        LONCHL = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+4+4* (IEL-1)+3)
      ELSE
        DECAEL= (IEL-1)*LGCATA
        LONCHL=LGCATA
        NCDYN = 0
        NBSPT = 1
      END IF
      ITAB(1) = IACHLO + DECAEL


C     -- ON VERIFIE QUE L'EXTRACTION EST COMPLETE SUR L'ELEMENT:
C     ----------------------------------------------------------
      IF (ILCHLO.NE.-1) THEN
        DO 777,K=1,LONCHL
          IF (.NOT.ZL(ILCHLO+DECAEL-1+K)) THEN
            WRITE(6,*) 'ERREUR TECACH ZL :',NOMPAR,
     +      (ZL(ILCHLO+DECAEL-1+KK),KK=1,LONCHL)
            CALL UTMESS('E','TECACH','ERREUR DE PROGRAMMATION :'//
     +       'ON N''A PAS PU EXTRAIRE TOUTES LES CMPS VOULUES '//
     +       'DU CHAMP GLOBAL ASSOCIE AU PARAMETRE:'//
     +              NOMPAR//' (OPTION:'//OPTION//' TYPE_ELEMENT:'//
     +              NOMTE//')')
            CALL CONTEX(OPTION,0,NOMPAR,' ',0,'STOP')
          END IF
777     CONTINUE
      END IF

        IF (NVAL.LT.2) GO TO 10


C     ITAB(2) : LONGUEUR DU CHAMP LOCAL (CATALOGUE) :
C     -----------------------------------------------------
        ITAB(2) = LGCATA
        IF (NVAL.LT.3) GO TO 10


C     ITAB(3) : NOMBRE DE POINTS (CATALOGUE) :
C     -----------------------------------------------------
        ITAB(3) = ZI(IAWLOC-1+7* (IPARG-1)+6)
        IF (NVAL.LT.4) GO TO 10
        ITAB(4) = 9999
        IF (NVAL.LT.5) GO TO 10


C     ITAB(5) : TYPE DU CHAMP LOCAL  :
C           R/C/I/K8/K16/K24
C           1/2/3/4 /5  /6
C     -----------------------------------------------------
        IF (ZK8(IAWTYP-1+IPARG) (1:1).EQ.'R') THEN
          ITAB(5) = 1
        ELSE IF (ZK8(IAWTYP-1+IPARG) (1:1).EQ.'C') THEN
          ITAB(5) = 2
        ELSE IF (ZK8(IAWTYP-1+IPARG) (1:1).EQ.'I') THEN
          ITAB(5) = 3
        ELSE IF (ZK8(IAWTYP-1+IPARG) (1:3).EQ.'K8 ') THEN
          ITAB(5) = 4
        ELSE IF (ZK8(IAWTYP-1+IPARG) (1:3).EQ.'K16') THEN
          ITAB(5) = 5
        ELSE IF (ZK8(IAWTYP-1+IPARG) (1:3).EQ.'K24') THEN
          ITAB(5) = 6
        ELSE
          CALL UTMESS('F','TECACH','MESSAGE VIDE ')
        END IF
        IF (NVAL.LT.6) GO TO 10


C     ITAB(6) : NCDYN : NOMBRE DE CMP POUR LA GRANDEUR VARI_R (IEL)
C     -------------------------------------------------------
        ITAB(6) = NCDYN
        IF (NVAL.LT.7) GO TO 10


C     ITAB(7) : NBSPT : NOMBRE DE SOUS-POINTS POUR IEL
C     -----------------------------------------------------
        ITAB(7) = NBSPT
        IF (NVAL.LT.8) GO TO 10

      END IF

   10 CONTINUE

      END
