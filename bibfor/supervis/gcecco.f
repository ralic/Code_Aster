      SUBROUTINE GCECCO( APPELA, IMPR, CODE, NOMOBJ )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)      APPELA,       CODE, NOMOBJ
      INTEGER                    IMPR
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 11/04/97   AUTEUR VABHHTS J.PELLET 
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
C     ECRITURE DES CONCEPTS UTILISATEURS EXISTANT DANS L'ETUDE
C     ------------------------------------------------------------------
C IN  IMPR   : IS  : NUMERO LOGIQUE D'IMPRESSION SI IMPR>0 SINON VOIR
C IN  CODE   : CH1 : CODE POUR UTMESS SI IMPR=0, INUTILISE SI IMPR >0
C IN  NOMOBJ : CH8 : NOM DU CONCEPT A EDITER, (SI '  ', ALORS TOUS)
C     ------------------------------------------------------------------
      CHARACTER*24    KINFO , KRESU, KSTAT
      COMMON /GCUCC1/ KINFO , KRESU, KSTAT
C     ------------------------------------------------------------------
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C     ------------------------------------------------------------------
      CHARACTER*8 CBID
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL JEVEUO ( KRESU , 'E' , LGRESU )
C
C     DESCRIPTION DE KRESU :
C          ZK80(JCMD)( 1: 8) = NOM UTILISATEUR DU RESULTAT
C          ZK80(JCMD)( 9:24) = NOM DU CONCEPT DU RESULTAT
C          ZK80(JCMD)(25:40) = NOM DE L'OPERATEUR
C          ZK80(JCMD)(41:48) = STATUT DE L'OBJET
C
      CALL JELIRA ( KRESU , 'LONMAX', LONMAX, CBID )
      CALL JELIRA ( KRESU , 'LONUTI', LONUTI, CBID )
      IERR = 0
C
C     --- DETERMINATION DES EXISTANTS ---
      IF ( NOMOBJ .NE. '   ') THEN
         IDEB =  0
         IFIN = -1
         DO 10 JCMD = 0, LONUTI
            IF ( ZK80(LGRESU+JCMD)(1:8) .EQ. NOMOBJ ) THEN
               IDEB = JCMD
               IFIN = JCMD
               GOTO 11
            ENDIF
 10      CONTINUE
         IERR = IERR + 1
 11      CONTINUE
      ELSE
         IDEB = 0
         IFIN = LONUTI-1
      ENDIF
C
      IF (IMPR .GT. 0 ) WRITE(IMPR,'(1X,72(''-''))')
C     --- ECRITURE DES EXISTANTS ---
      IF ( IFIN .GT. IDEB ) THEN
         IIFIN = 0
         IF (IMPR .GT. 0 ) THEN
            WRITE(IMPR,*) '<',APPELA,'> ',
     +           '<INFORMATION SUR LES CONCEPTS EXISTANTS.>'
         ELSE
            CALL UTDEBM(CODE,APPELA,
     +           'INFORMATION SUR LES CONCEPTS EXISTANTS.')
            IIFIN = 1
         ENDIF
         IPASS = 1
         IF (IMPR .GT.0 ) THEN
            WRITE(IMPR,*)
            WRITE(IMPR,*)  '<NO  CMDE> <CONCEPT.> <TYPE DU CONCEPT.> '
     +           //'< A ETE CREE PAR >'
            WRITE(IMPR,*)
         ELSE
            CALL UTSAUT()
            CALL UTIMPK('L',' ',1,'NO  CMDE')
            CALL UTIMPK('S',' ',1,'CONCEPT.')
            CALL UTIMPK('S',' ',1,'TYPE DU CONCEPT.')
            CALL UTIMPK('S',' ',1,' A ETE CREE PAR ')
            CALL UTSAUT()
         ENDIF
         DO 100 JCMD = IDEB, IFIN
            IF ( ZK80(LGRESU+JCMD)( 1: 8) .NE. '&ABSENT' ) THEN
               CALL CODENT( JCMD+1, 'D', CBID)
               IF (IMPR .GT.0 ) THEN
                  WRITE(IMPR,*)  '<',CBID,'> ',
     +                 '<',ZK80(LGRESU+JCMD)( 1: 8),'> ',
     +                 '<',ZK80(LGRESU+JCMD)( 9:24),'> ',
     +                 '<',ZK80(LGRESU+JCMD)(25:40),'> ',
     +                 '<',ZK80(LGRESU+JCMD)(41:48),'>'
               ELSE
                  CALL UTIMPK('L',' ',1,CBID)
                  CALL UTIMPK('S',' ',1,ZK80(LGRESU+JCMD)( 1: 8) )
                  CALL UTIMPK('S',' ',1,ZK80(LGRESU+JCMD)( 9:24) )
                  CALL UTIMPK('S',' ',1,ZK80(LGRESU+JCMD)(25:40) )
                  CALL UTIMPK('S',' ',1,ZK80(LGRESU+JCMD)(41:48) )
               ENDIF
CCCCC       ELSEIF(ZK80(LGRESU+JCMD)(25:40) .EQ. 'FIN' ) THEN
C              IF ( JCMD .NE. IFIN ) THEN
C                 IPASS = IPASS+1
C                 IF (IMPR .GT.0 ) THEN


C                 ELSE


C                 ENDIF
CCCCCCCCCCCC   ENDIF
            ELSEIF(ZK80(LGRESU+JCMD)(25:40) .EQ. 'POURSUITE' ) THEN
               IF ( JCMD .NE. IFIN ) THEN
                  IPASS = IPASS+1
                  IF (IMPR .GT.0 ) THEN
                     WRITE(IMPR,*)
                     WRITE(IMPR,*) 'PASSAGE NUMERO <',IPASS,'>'
                  ELSE
                     CALL UTSAUT()
                     CALL UTIMPI('L','PASSAGE NUMERO',1,IPASS)
                  ENDIF
               ENDIF
            ENDIF
 100     CONTINUE
         IF (IMPR.EQ.0 .AND. IIFIN.EQ.1) CALL UTFINM()
      ENDIF
C
C     --- DETERMINATION DES POTENTIELS ---
      IF ( NOMOBJ .NE. '   ' ) THEN
         IDEB =  0
         IFIN = -1
         DO 20 JCMD = LONUTI,LONMAX-1
            IF ( ZK80(LGRESU+JCMD)(1:8) .EQ. NOMOBJ ) THEN
               IDEB = JCMD
               IFIN = JCMD
               GOTO 21
            ENDIF
 20      CONTINUE
         IERR = IERR + 1
 21      CONTINUE
      ELSE
         IDEB = LONUTI
         IFIN = LONMAX-1
      ENDIF
C
C     --- ECRITURE DES POTENTIELS ---
      IIFIN = 0
      IF ( IFIN .GT. IDEB ) THEN
         IENTE = 0
         DO 200 JCMD = IDEB, IFIN
            IF ( ZK80(LGRESU+JCMD)( 1: 8) .NE. '&ABSENT' ) THEN
C
C             ENTETE LA PREMIERE FOIS
               IF ( IENTE .EQ. 0 ) THEN
                  IENTE = 1
                  IF (IMPR .GT.0 ) THEN
                     WRITE(IMPR,*)
                     WRITE(IMPR,*) '<',APPELA,'> ',
     +                    'INFORMATION SUR LES CONCEPTS DEVANT ETRE '//
     +                    'CREES.'
                     WRITE(IMPR,*)
                     WRITE(IMPR,*)
     +                    '<NO  CMDE> <CONCEPT.> <TYPE DU CONCEPT.> '
     +                    //'<SERA  CREE  PAR >'
                     WRITE(IMPR,*)
                  ELSE
                     IIFIN = 1
                     CALL UTDEBM(CODE,APPELA,
     +                    'INFORMATION SUR LES CONCEPTS DEVANT ETRE '//
     +                    'CREES.')
                     CALL UTSAUT()
                     CALL UTIMPK('L',' ',1,' NO CMD ')
                     CALL UTIMPK('S',' ',1,'CONCEPT.')
                     CALL UTIMPK('S',' ',1,'TYPE DU CONCEPT.')
                     CALL UTIMPK('S',' ',1,'SERA  CREE  PAR ')
                     CALL UTSAUT()
                  ENDIF
               ENDIF
C
               CALL CODENT( JCMD+1, 'D', CBID)
               IF (IMPR .GT.0 ) THEN
                  WRITE(IMPR,*)  '<',CBID,'> ',
     +                 '<',ZK80(LGRESU+JCMD)( 1: 8),'> ',
     +                 '<',ZK80(LGRESU+JCMD)( 9:24),'> ',
     +                 '<',ZK80(LGRESU+JCMD)(25:40),'> ',
     +                 '<',ZK80(LGRESU+JCMD)(41:48),'>'
               ELSE
                  CALL UTIMPK('L',' ',1,CBID)
                  CALL UTIMPK('S',' ',1,ZK80(LGRESU+JCMD)( 1: 8) )
                  CALL UTIMPK('S',' ',1,ZK80(LGRESU+JCMD)( 9:24) )
                  CALL UTIMPK('S',' ',1,ZK80(LGRESU+JCMD)(25:40) )
                  CALL UTIMPK('S',' ',1,ZK80(LGRESU+JCMD)(41:48) )
               ENDIF
            ELSEIF(ZK80(LGRESU+JCMD)(25:40) .EQ. 'FIN' ) THEN
               GOTO 201
            ELSEIF(ZK80(LGRESU+JCMD)(25:25) .EQ. '&' ) THEN
               GOTO 201
            ENDIF
 200     CONTINUE
 201     CONTINUE
         IF (IMPR.EQ.0 .AND. IIFIN.EQ.1) CALL UTFINM()
      ENDIF
C
      IF ( NOMOBJ .NE. '   ' ) THEN
         IF ( IERR .EQ. 2 ) THEN
            IF (IMPR .GT. 0 ) THEN
               WRITE(IMPR,*) '<',APPELA,'> ','LE CONCEPT "',NOMOBJ,
     +              '" EST INCONNU. IL N''EST NI PARMI LES '
     +              //'CREES, NI PARMI CEUX A CREER.'
            ELSE
               CBID = NOMOBJ
               CALL UTMESS(CODE,APPELA,'LE CONCEPT "'//CBID
     +              //'" EST INCONNU. IL N''EST NI PARMI LES '
     +              //'CREES, NI PARMI CEUX A CREER.')
            ENDIF
         ENDIF
      ENDIF
C
      CALL JEDEMA()
      END
