      SUBROUTINE IBCATA ( IER )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 08/03/2002   AUTEUR D6BHHJP J.P.LEFEBVRE 
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
C     ALLOCATION ET LECTURE DES DIFFERENTS CATALOGUES
C     ------------------------------------------------------------------
C     ROUTINE(S) UTILISEE(S) :
C         GETRES GETFAC GETVXX
C         IBOPER
C     ROUTINE(S) FORTRAN     :
C         -
C     ------------------------------------------------------------------
C FIN IBCATA
C     ------------------------------------------------------------------
      REAL*8        TEMPS(6)
      CHARACTER*8   NOMRES
      CHARACTER*16  CONCEP, NOMCMD , MOTFAC
C     ------------------------------------------------------------------
C     --- DEFAUT POUR LES CATALOGUES D'ELEMENTS
C     --- 3  = CATAELEM
C     --- 4  = BASELEM
      PARAMETER          ( IELTDF = 4 )
C     ------------------------------------------------------------------
      PARAMETER          ( MXDFCA = 4 ,       MXCATA = 10 )
      CHARACTER*32  DFNOM(MXDFCA)     , NOM (MXCATA)
      CHARACTER*72  DFTITR(MXDFCA)    , TITRE(MXCATA)
      INTEGER       DFUNIT(MXDFCA)    , UNITE(MXCATA)
C     ------------------------------------------------------------------
C     OPTIONS PAR DEFAUT :
C
      DATA ( DFNOM(I), DFTITR(I), DFUNIT(I), I=1,MXDFCA) /
     +    'COMMANDE_PRIVEE  ', 'COMMANDES PRIVEES                  ',03,
     +    'COMMANDE         ', 'COMMANDES OFFICIELLES              ',02,
     +    'CATAELEM         ', 'CATALOGUES DE CALCUL               ',04,
     +    'ELEMBASE         ', 'BASE CATALOGUE ELEMENT             ',04/
C     ------------------------------------------------------------------
C
      CALL DEFUFI(6,'MESSAGE')
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
         CALL UTMESS('F','IBCATA','TROP DE CATALOGUES MAXIMUM (10) ' )
         NBOCC = MXCATA
      ENDIF
C
      IUN  = 1
      DO 10 IOCC = 1, NBOCC
         CALL GETVTX(MOTFAC,'FICHIER' ,IOCC,IUN,IUN,NOM(IOCC),NBNOM)
         CALL LXCADR( NOM(IOCC) )
         CALL GETVIS(MOTFAC,'UNITE',IOCC,IUN,IUN,UNITE(IOCC),NBUNIT)
         IF ( NBUNIT .EQ. 0 ) THEN
            CALL UTREMT( NOM(IOCC) , DFNOM , MXDFCA , IPLACE )
            IF ( IPLACE .GT. 0 )  UNITE(IOCC) = DFUNIT(IPLACE)
         ENDIF
         TITRE(IOCC) = ' '
         CALL GETVTX(MOTFAC,'TITRE',IOCC,IUN,IUN,TITRE(IOCC),NBTITR)
  10  CONTINUE
C
C     --- CATALOGUE DES ELEMENTS ---
      NBCATA = 0
      CALL UTMESS('I','CATALOGUE(S) DES ELEMENTS','DEBUT DE LECTURE')
      CALL UTTCPU( -2,'INIT',6,TEMPS)
      CALL UTTCPU( -2,'DEBUT',6,TEMPS)
      DO 300 ICATA = 1 , NBOCC
         IF (NOM(ICATA).EQ.DFNOM(3) .OR. NOM(ICATA).EQ.DFNOM(4) ) THEN
            IF (UNITE(ICATA).GT.0) THEN
               CALL IBCATC(NOM(ICATA),UNITE(ICATA),TITRE(ICATA),IER1)
               IER = IER + IER1
            ENDIF
            NOM(ICATA) = '  '
            NBCATA = NBCATA + 1
         ENDIF
 300  CONTINUE
      IF ( NBCATA .EQ. 0 .AND. NOMCMD .EQ. 'DEBUT' ) THEN
         CALL IBCATC(DFNOM(IELTDF),DFUNIT(IELTDF),DFTITR(IELTDF),IER1)
         IER = IER + IER1
      ENDIF
      CALL UTTCPU( -2,'FIN',6,TEMPS)
      CALL UTDEBM('I','CATALOGUE(S) DES ELEMENTS','FIN DE LECTURE')
      CALL UTIMPR('S',' (DUREE ',1,TEMPS(5))
      CALL UTIMPK('S',' S.)',0,' ')
      CALL UTFINM()
C
C     --- VERIFICATION DE LA COMPLETUDE DE L'EXECUTION ---
      DO 900 ICATA = 1 , NBOCC
         IF ( NOM(ICATA) .NE. ' ' ) THEN
            CALL UTMESS('F','IBCATA',
     +                      '"'//NOM(ICATA)//'" ARGUMENT INVALIDE DU'//
     +             ' MOT CLE "FICHIER" DU MOT CLE FACTEUR "CATALOGUE"')
            IER = IER + 1
         ENDIF
 900  CONTINUE
C
      IF (IER.GT.0) CALL UTMESS('F','IBCATA',
     +                              'ERREUR(S) FATALE(S) LORS DE LA '//
     +                              'LECTURE DES CATALOGUES')
C
      END
