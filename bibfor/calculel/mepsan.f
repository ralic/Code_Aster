      SUBROUTINE MEPSAN(EPSANE,EXITIM,TIME,THVRAI,CHEPSA)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 06/04/2004   AUTEUR DURAND C.DURAND 
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
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     ARGUMENTS:
C     ----------
      CHARACTER*(*) EPSANE
      CHARACTER*(*) CHEPSA
      LOGICAL THVRAI,EXITIM
      REAL*8 TIME
C ----------------------------------------------------------------------
C     BUT:
C     ON CONSTRUIT UN CHAMP DE DEFORMATIONS ANELASTIQUES
C     AVEC LE CONCEPT : EPSANE
C     ET LE TEMPS TIME, SOIT PAR INTERPOLATION, SOIT PAR SIMPLE RECOPIE
C     SI L'INSTANT EST VOISIN D'UN PAS DE TEMPS CALCULE.
C
C     IN:
C        EPSANE : NOM UTILISATEUR D'1 EVOL_NOLI
C        EXITIM : VRAI SI ON DONNE UNE VALEUR DU TEMPS TIME
C        TIME   : VALEUR REELLE DU TEMPS
C        CHEPSA : NOM DU CHAMP DE DEFORMATIONS ANELASTIQUES
C                TROUVE (OU CREE).
C
C     OUT:
C        THVRAI : VRAI SI ON TROUVE 1 CHAMP DE DEFORMATIONS ANELASTIQUES
C                 FAUX SI ON CREE 1 CHAMP DE DEFORMATIONS ANELASTIQUES
C                 ARBITRAIRE.
C        CHEPSA : EST REMPLI SI THVRAI EST EGAL A VRAI
C
C ----------------------------------------------------------------------
C
C     FONCTIONS EXTERNES:
C     -------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR
C
C     VARIABLES LOCALES:
C     ------------------
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
      CHARACTER*16 ZK16,TYSD,TYCH
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
C
C DEB-------------------------------------------------------------------
C
C
C     -- SI LE CHAMP CHEPSA EXISTE DEJA , ON LE DETRUIT:
      CALL DETRSD('CHAMP_GD',CHEPSA(1:19))
C
      IF (EPSANE(1:8).NE.'        ') THEN
         THVRAI = .TRUE.
C
         CALL GETTCO(EPSANE(1:8),TYSD)
C
         IF (TYSD(1:9).EQ.'EVOL_NOLI') THEN
C           ----------------------------
            CALL DISMOI('F','NB_CHAMP_UTI',EPSANE(1:8),'RESULTAT',
     +                  NBCHAM,K8BID,IERD)
            IF (NBCHAM.GT.0) THEN
               IF (.NOT. (EXITIM)) THEN
                  CALL UTMESS('I',' MEPSAN ',
     +                        'L''INSTANT DU CALCUL EST PRIS '//
     +                        ' ARBITRAIREMENT A 0.0 ')
                  TIME2 = 0.0D0
                  IF (NBCHAM.GT.1) THEN
                     CALL UTMESS('F',' MEPSAN ',
     +                           ' ON N''ACCEPTE UN INSTANT ARBITRAIRE'
     +                           //' QUE SI LE CONCEPT DEFORMATIONS '
     +                           //'ANELASTIQUES N''A QU''1 CHAMP.')
                  END IF
               ELSE
                  TIME2 = TIME
               END IF
C
C              RECUPERATION DU CHAMP DE DEFORMATIONS ANELASTIQUES
C              DANS EPSANE:
C              -----------
               CALL RSINCH(EPSANE(1:8),'EPSA_ELNO','INST',TIME2,
     +                     CHEPSA(1:19),'CONSTANT','CONSTANT',1,'V',
     +                     ICORET)
               IF (ICORET.GE.10) THEN
                  CALL UTDEBM('F','MEPSAN','INTERPOLATION DEFORMATIONS'
     +                        //' ANELASTIQUES : ')
                  CALL UTIMPK('L','EVOL_NOLI:',1,EPSANE(1:8))
                  CALL UTIMPR('S','INSTANT:',1,TIME2)
                  CALL UTIMPI('L','ICORET:',1,ICORET)
                  CALL UTFINM()
               END IF
            ELSE
               CALL UTMESS('F',' MEPSAN ',' LE CONCEPT EVOL_NOLI : '//
     +                     EPSANE(1:8)//
     +                     ' NE CONTIENT AUCUN CHAMP DE DEFORMATIONS'
     +                   //' ANELASTIQUES.')
            END IF
C
         ELSE
            CALL UTMESS('F','MEPSAN','2')
         END IF
      END IF
 9999 CONTINUE
      END
