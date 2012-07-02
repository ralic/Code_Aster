      SUBROUTINE IBCATA ( IER )
      IMPLICIT NONE
      INTEGER             IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     ALLOCATION ET LECTURE DES DIFFERENTS CATALOGUES
C     ------------------------------------------------------------------
C     ROUTINE(S) UTILISEE(S) :
C         GETRES GETFAC GETVXX
C         IBOPER
C     ------------------------------------------------------------------
      REAL*8        TEMPS(6)
      REAL*8 VALR
      CHARACTER*8   NOMRES
      CHARACTER*16  CONCEP, NOMCMD , MOTFAC
C     ------------------------------------------------------------------
C     --- DEFAUT POUR LES CATALOGUES D'ELEMENTS
C     --- 3  = CATAELEM
C     --- 4  = BASELEM
C-----------------------------------------------------------------------
      INTEGER ICATA ,IELTDF ,IER1 ,IOCC ,IPLACE ,IUN ,MXCATA
      INTEGER MXDFCA ,NBCATA ,NBNOM ,NBOCC ,NBUNIT
C-----------------------------------------------------------------------
      PARAMETER          ( IELTDF = 4 )
C     ------------------------------------------------------------------
      PARAMETER          ( MXDFCA = 4 ,       MXCATA = 10 )
      CHARACTER*32  DFNOM(MXDFCA)     , NOM (MXCATA)
      CHARACTER*24  VALK
      INTEGER       DFUNIT(MXDFCA)    , UNITE(MXCATA)
      INTEGER      IARG,I
C     ------------------------------------------------------------------
C     OPTIONS PAR DEFAUT :
C
      DATA ( DFNOM(I), DFUNIT(I), I=1,MXDFCA) /
     &    'COMMANDE_PRIVEE  ',03,
     &    'COMMANDE         ',02,
     &    'CATAELEM         ',04,
     &    'ELEMBASE         ',04/
C     ------------------------------------------------------------------
C
      CALL ULDEFI(6,' ','MESSAGE','A','N','N')
C     --- LA ROUTINE NE S'INTERRESSE QU'AU MOT CLE FACTEUR "CATALOGUE" -
C     --- DANS LA COMMANDE DEBUT
      IER    = 0
      MOTFAC = 'CATALOGUE'
C
C     --- RECUPERATION DU NOM DE LA COMMANDE UTILISATEUR ---
      CALL GETRES( NOMRES , CONCEP , NOMCMD )
C
C     --- NOMBRE DE CATALOGUES SPECIFIES PAR L'UTILISATEUR ---
      CALL GETFAC(MOTFAC,NBOCC)
C
      IF ( NBOCC .GT. MXCATA ) THEN
         IER   = IER + 1
         CALL U2MESS('F','SUPERVIS_18')
         NBOCC = MXCATA
      ENDIF
C
      IUN  = 1
      DO 10 IOCC = 1, NBOCC
         CALL GETVTX(MOTFAC,'FICHIER',IOCC,IARG,IUN,NOM(IOCC),NBNOM)
         CALL LXCADR( NOM(IOCC) )
         CALL GETVIS(MOTFAC,'UNITE',IOCC,IARG,IUN,UNITE(IOCC),NBUNIT)
         IF ( NBUNIT .EQ. 0 ) THEN
            CALL UTREMT( NOM(IOCC) , DFNOM , MXDFCA , IPLACE )
            IF ( IPLACE .GT. 0 )  UNITE(IOCC) = DFUNIT(IPLACE)
         ENDIF
  10  CONTINUE
C
C     --- CATALOGUE DES ELEMENTS ---
      NBCATA = 0
      CALL U2MESS('I','SUPERVIS_19')
      CALL UTTCPU('CPU.IBCATA','INIT',' ')
      CALL UTTCPU('CPU.IBCATA','DEBUT',' ')
      DO 300 ICATA = 1 , NBOCC
         IF (NOM(ICATA).EQ.DFNOM(3) .OR. NOM(ICATA).EQ.DFNOM(4) ) THEN
            IF (UNITE(ICATA).GT.0) THEN
               CALL IBCATC(NOM(ICATA),UNITE(ICATA),IER1)
               IER = IER + IER1
            ENDIF
            NOM(ICATA) = '  '
            NBCATA = NBCATA + 1
         ENDIF
 300  CONTINUE
      IF ( NBCATA .EQ. 0 .AND. NOMCMD .EQ. 'DEBUT' ) THEN
         CALL IBCATC(DFNOM(IELTDF),DFUNIT(IELTDF),IER1)
         IER = IER + IER1
      ENDIF
      CALL UTTCPU('CPU.IBCATA','FIN',' ')
      CALL UTTCPR('CPU.IBCATA',6,TEMPS)
      VALR = TEMPS(5)
      VALK = ' '
      CALL U2MESG('I', 'SUPERVIS_52',1,VALK,0,0,1,VALR)
C
C     --- VERIFICATION DE LA COMPLETUDE DE L'EXECUTION ---
      DO 900 ICATA = 1 , NBOCC
         IF ( NOM(ICATA) .NE. ' ' ) THEN
            CALL U2MESK('F','SUPERVIS_20',1,NOM(ICATA))
            IER = IER + 1
         ENDIF
 900  CONTINUE
C
      IF (IER.GT.0) CALL U2MESS('F','SUPERVIS_21')
C
      END
