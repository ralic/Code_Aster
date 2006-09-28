      SUBROUTINE FOINTR(NOMFON,CHPROL,NBVAR,VAR,FON,
     &                                         NBRES,VARRES,FONRES,IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     NOMFON,CHPROL(*)
      INTEGER                         NBVAR,   NBRES,              IER
      REAL*8                         VAR(*),FON(*),VARRES(*),FONRES(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C     INTERPOLATION-EXTRAPOLATION DE TOUTE UNE FONCTION
C     ------------------------------------------------------------------
C IN  NOMFON : K19 : NOM DE LA FONCTION A INTERPOLER
C            N'EST OBLIGATOIRE QUE POUR LES FONCTIONS INTERPRETEES
C IN  CHPROL : DESCRIPTEUR DES FONCTION
C           CHPROL(1) = 'FONCTION' / 'INTERPR'
C           CHPROL(2) = 'LINEAIRE' OU 'LOGARITH'
C           CHPROL(3) = NOM_PARA
C           CHPROL(4) = NOM_RESU
C           CHPROL(5) = 'GD      ' G CODE D'EXTRAPOLATION A GAUCHE
C                                  D CODE D'EXTRAPOLATION A DROITE
C IN  NBVAR  : IS : NOMBRE DE POINTS SUR LEQUEL EST DEFINIT LA FONCTION
C IN  VAR    : R8 : ABCSISSES DES POINTS DE DEFINITION DE LA FONCTION
C IN  FON    : R8 : ORDONNEES DES POINTS DE DEFINITION DE LA FONCTION
C IN  NBRES  : IS : NOMBRE DE POINTS DE DEFINITION DE L'INTERPOLEE
C IN  VARRES : R8 : ABCSISSES DES POINTS DE DEFINITION DE L'INTERPOLEE
C OUT FONRES : R8 : ORDONNEES DES POINTS DE DEFINITION DE L'INTERPOLEE
C OUT IER    : IS : CODE RETOUR
C               = 0 : O.K.
C               = 1 : ON A FAIT UNE EXTRAPOLATION A DROITE AVEC "EXCLU"
C               = 2 : ON A FAIT UNE EXTRAPOLATION A GAUCHE AVEC "EXCLU"
C               = 3 : ON A FAIT UNE EXTRAPOLATION A DROITE ET A GAUCHE
C                   : AVEC "EXCLU"
C     ------------------------------------------------------------------
C     SI EXTRAPOLATION "EXCLU" ALORS ON ARRETE EN FATAL
C     ------------------------------------------------------------------
C     SI CHPROL(1) /= 'CONSTANT'/'FONCTION'  ALORS ERREUR (AVEC ARRET)
C     ------------------------------------------------------------------
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
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*8 K8BID
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C     ------------------------------------------------------------------
      CHARACTER*19 NOMF
      REAL*8       LINLIN, LINLOG, LOGLIN, LOGLOG
C     ------------------------------------------------------------------
C     FONCTION EN LIGNE
C
      LINLIN(X,X1,Y1,X2,Y2)= Y1+(X-X1)*(Y2-Y1)/(X2-X1)
      LINLOG(X,X1,Y1,X2,Y2)=EXP(LOG(Y1)+(X-X1)*(LOG(Y2)-LOG(Y1))
     &                                        /(X2-X1))
      LOGLOG(X,X1,Y1,X2,Y2)=EXP(LOG(Y1)+(LOG(X)-LOG(X1))*(LOG(Y2)
     &                                     -LOG(Y1))/(LOG(X2)-LOG(X1)))
      LOGLIN(X,X1,Y1,X2,Y2)=Y1+(LOG(X)-LOG(X1))*(Y2-Y1)
     &                                         /(LOG(X2)-LOG(X1))
C     ------------------------------------------------------------------
      CALL JEMARQ()
      IER  = 0
      NOMF = NOMFON
      IF (CHPROL(1) .NE.'FONCTION' ) GOTO 1000
C
C     INITIALISATION
C
      IVAR = 1
      IRES = 1
C
C     --- TRAITEMENT PARTICULIER POUR 1 POINT ---
C
      IF ( NBVAR .EQ. 1 ) THEN
         IF ( NBRES .NE. 1  .AND.  CHPROL(5)(1:2) .NE. 'CC' ) THEN
            CALL U2MESS('F','UTILITAI2_22')
         ENDIF
         IF ( CHPROL(5)(1:2) .EQ. 'CC' ) THEN
            DO 10 I = 1 , NBRES
               FONRES(I) = FON(IVAR)
 10         CONTINUE
         ELSE
            IF ( VARRES(IRES) .EQ. VAR(IVAR) ) THEN
               FONRES(IRES) = FON(IVAR)
            ELSE
               CALL U2MESS('F','UTILITAI2_23')
            ENDIF
         ENDIF
         GOTO 9999
      ENDIF
C
C     RECHERCHE DU DEBUT DE L'INTERVALLE D'INTERPOLATION
C
 100  CONTINUE
      IF ( (VARRES(IRES).LT. VAR(IVAR)).AND.(IRES.LT.NBRES) ) THEN
         IRES = IRES + 1
         GOTO 100
      ENDIF
C
      IF ( IRES .GT. 1 ) THEN
C
C        --- EXTRAPOLATION A GAUCHE ---
C
         IF ( CHPROL(5)(1:1) .EQ. 'C' ) THEN
C           --- EXTRAPOLATION CONSTANTE ---
            DO 120 JRES=1,IRES-1
               FONRES(JRES) = FON(IVAR)
  120       CONTINUE
C
         ELSEIF ( CHPROL(5)(1:1) .EQ. 'L' ) THEN
C           --- EXTRAPOLATION LINEAIRE ---
            DO 130 JRES=1,IRES-1
               FONRES(JRES)=LINLIN(VARRES(JRES),VAR(IVAR),FON(IVAR),
     &                             VAR(IVAR+1),FON(IVAR+1))
  130          CONTINUE
C
         ELSEIF ( CHPROL(5)(1:1) .EQ. 'I' ) THEN
            CALL JEVEUO(NOMF//'.NOVA','L',LNOVA)
            DO 140  JRES= 1,NBRES
               CALL FOINTE('F ',NOMF,1,ZK8(LNOVA),
     &                                   VARRES(JRES),FONRES(JRES),IER)
  140       CONTINUE
C
         ELSEIF ( CHPROL(5)(1:1) .EQ. 'E' ) THEN
C           --- EXTRAPOLATION EXCLUE ---
            IER = IER + 1
            CALL UTDEBM('F','FOINTR','ERREUR EXTRAPOLATION')
            CALL UTIMPK('L','L''EXTRAPOLATION A GAUCHE DE ',0,' ')
            CALL UTIMPK('S','LA FONCTION ',1,NOMF)
            CALL UTIMPK('S',' EST EXCLUE',0,' ')
            CALL UTFINM()
         ELSE
            CALL UTDEBM('F','FOINTR','ERREUR EXTRAPOLATION')
            CALL UTIMPK('L','EXTRAPOLATION A GAUCHE INCONNUE',1,
     &                                                   CHPROL(5)(1:1))
            CALL UTFINM()
         ENDIF
      ENDIF
C
C     --- INTERPOLATION ---
C
 200  CONTINUE
      IF ( IRES .LE. NBRES ) THEN
 210     CONTINUE
         IF ( VARRES(IRES) .LE. VAR(IVAR+1) ) THEN
            IF (CHPROL(2)(1:8).EQ.'LIN LIN ') THEN
C              --- INTERPOLATION LINEAIRE ---
              FONRES(IRES) = LINLIN( VARRES(IRES), VAR(IVAR), FON(IVAR),
     &                               VAR(IVAR+1), FON(IVAR+1)  )
            ELSEIF (CHPROL(2)(1:8).EQ.'LOG LOG ') THEN
C              --- INTERPOLATION LOGARITHMIQUE ---
              FONRES(IRES) = LOGLOG( VARRES(IRES), VAR(IVAR), FON(IVAR),
     &                               VAR(IVAR+1), FON(IVAR+1)  )
            ELSEIF (CHPROL(2)(1:8).EQ.'LIN LOG ') THEN
C              --- INTERPOLATION LIN-LOG ---
              FONRES(IRES) = LINLOG( VARRES(IRES), VAR(IVAR), FON(IVAR),
     &                               VAR(IVAR+1), FON(IVAR+1)  )
            ELSEIF (CHPROL(2)(1:8).EQ.'LOG LIN ') THEN
C              --- INTERPOLATION LOG-LIN ---
              FONRES(IRES) = LOGLIN( VARRES(IRES), VAR(IVAR), FON(IVAR),
     &                               VAR(IVAR+1), FON(IVAR+1)  )
            ELSEIF (CHPROL(2)(1:3).EQ.'INT') THEN
               CALL JEVEUO(NOMF//'.NOVA','L',LNOVA)
               CALL FOINTE('F ',NOMF,1,ZK8(LNOVA),
     &                                   VARRES(IRES),FONRES(IRES),IER)
            ELSEIF (CHPROL(2)(1:3).EQ.'NON') THEN
               IF ( VARRES(IRES) .EQ. VAR(IVAR) ) THEN
                  FONRES(IRES) = FON(IVAR)
               ELSE
                  IF ( VARRES(IRES) .EQ. VAR(IVAR+1) ) THEN
                     FONRES(IRES) = FON(IVAR+1)
                  ELSE
                     IER = IER + 1
                     CALL UTDEBM('F','FOINTR (ERREUR.03)',
     &                                         'ERREUR INTERPOLATION.')
                  CALL UTIMPK('L','INTERPOLATION NON PERMISE',0,' ')
                CALL UTIMPR('L','  VALEUR A INTERPOLER:',1,VARRES(IRES))
                CALL UTIMPR('L','  BORNE INFERIEURE:',1,VAR(IVAR))
                CALL UTIMPR('L','  BORNE SUPERIEURE:',1,VAR(IVAR+1))
                  CALL UTFINM()
                  ENDIF
               ENDIF
            ELSE
               IER = IER + 1
               CALL UTDEBM('F','FOINTR (ERREUR.03)',
     &                                         'ERREUR INTERPOLATION.')
               CALL UTIMPK('L','INTERPOLATION INCONNUE',1, CHPROL(2) )
               CALL UTFINM()
            ENDIF
            IRES = IRES + 1
            GOTO 200
         ELSE
            IVAR = IVAR + 1
            IF ( IVAR .LT. NBVAR ) GOTO 210
         ENDIF
      ENDIF
C
C
      IF ( IRES .LT. NBRES ) THEN
C
C        --- EXTRAPOLATION A DROITE ---
C
         IF ( CHPROL(5)(2:2) .EQ. 'C' ) THEN
C           --- EXTRAPOLATION CONSTANTE ---
            DO 310 JRES=IRES,NBRES
               FONRES(JRES) = FON(NBVAR)
 310        CONTINUE
         ELSEIF ( CHPROL(5)(2:2) .EQ. 'L' ) THEN
            DO 320 JRES=IRES,NBRES
             FONRES(JRES)=LINLIN(VARRES(JRES),VAR(NBVAR-1),FON(NBVAR-1),
     &                  VAR(NBVAR),FON(NBVAR))
 320        CONTINUE
         ELSEIF ( CHPROL(5)(2:2) .EQ. 'I' ) THEN
C           --- EXTRAPOLATION INTERPRETEE ----
            CALL JEVEUO(NOMF//'.NOVA','L',LNOVA)
            DO 330 JRES=IRES,NBRES
               CALL FOINTE('F ',NOMF,1,ZK8(LNOVA),
     &                                   VARRES(JRES),FONRES(JRES),IER)
 330        CONTINUE
C
         ELSEIF ( CHPROL(5)(2:2) .EQ. 'E' ) THEN
C           --- EXTRAPOLATION EXCLUE ---
            IER = IER + 2
            CALL UTDEBM('F','FOINTR','ERREUR EXTRAPOLATION')
            CALL UTIMPK('L','L''EXTRAPOLATION A DROITE DE ',0,' ')
            CALL UTIMPK('S','LA FONCTION ',1,NOMF)
            CALL UTIMPK('S',' EST EXCLUE',0,' ')
            CALL UTFINM()
         ELSE
            CALL UTDEBM('F','FOINTR','ERREUR EXTRAPOLATION')
            CALL UTIMPK('L','EXTRAPOLATION A DROITE INCONNUE',1,
     &                                                   CHPROL(5)(2:2))
            CALL UTFINM()
         ENDIF
      ENDIF
      GOTO 9999
C     ------------------------------------------------------------------
 1000 CONTINUE
      IF (CHPROL(1) .EQ.'CONSTANT' ) THEN
         DO 1100  JRES= 1,NBRES
            FONRES(JRES) = FON(1)
 1100    CONTINUE
      ELSEIF (CHPROL(1) .EQ.'INTERPRE' ) THEN
         CALL JELIRA(NOMF//'.NOVA','LONUTI',LONUTI,K8BID)
         IF ( LONUTI .NE. 1 ) THEN
            CALL U2MESS('F','UTILITAI2_24')
         ENDIF
         CALL JEVEUO(NOMF//'.NOVA','L',LNOVA)
         DO 1200  JRES= 1,NBRES
            CALL FOINTE('F ',NOMF,1,ZK8(LNOVA),
     &                                   VARRES(JRES),FONRES(JRES),IER)
 1200    CONTINUE
      ELSE
 2000    CONTINUE
         CALL U2MESS('F','UTILITAI2_25')
      ENDIF
 9999 CONTINUE
      CALL JEDEMA()
      END
