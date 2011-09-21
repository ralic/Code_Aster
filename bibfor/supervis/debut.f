      SUBROUTINE DEBUT  ( IPASS,IER )
      IMPLICIT NONE
      INTEGER                 IER,IPASS
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 21/09/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_7
C    DECODAGE DE LA COMMANDE DEBUT OU POURSUITE
C     ------------------------------------------------------------------
C     ROUTINE(S) UTILISEE(S) :
C        IBBASE  IBCATA
C     ------------------------------------------------------------------
C
      INTEGER    MXCMD
      PARAMETER (MXCMD = 500 )
      CHARACTER*8 K8B, REPONS
      CHARACTER*16 NOMCMD, K16B, CMPDEF, CMPUT, CMPOUT
      CHARACTER*80 FICHDF
      INTEGER IERIMP, ICMD, LOUT, N, NCODE
      INTEGER      IARG
C
      FICHDF=' '
C
C --- ERREUR / ERREUR_F :
C     MOT-CLE CODE PRESENT ?
      CALL GETFAC('CODE', NCODE)
      IF ( NCODE .NE. 0 ) THEN
         CMPDEF = 'ABORT'
      ELSE
         CMPDEF = 'EXCEPTION'
C        FERMETURE DU .CODE (OUVERT PAR IB0MAI)
         CALL ULOPEN(-15, ' ', ' ', ' ', ' ')
      ENDIF
      CALL GETVTX('ERREUR', 'ERREUR_F', 1,IARG, 1, CMPUT, N)
      IF ( N .EQ. 1 ) THEN
         CMPDEF = CMPUT
      ENDIF
      CALL ONERRF(CMPDEF, CMPOUT, LOUT)
C
      IER    = 0
      IF(IPASS .NE. 1) GOTO 9999
         CALL IBIMPR( IERIMP )
         CALL PRINIT()
         CALL PRENTE()
C --- LECTURE DU MOT CLE FACTEUR DEBUG OU DE GESTION MEMOIRE DEMANDE
      CALL IBDBGS()
C
C --- ALARME GENERIQUE
      CALL GETVTX(' ','PAR_LOT',1,IARG,1,REPONS,N)
      IF ( REPONS .EQ. 'NON' ) THEN
         CALL U2MESS('A', 'SUPERVIS_1')
      ENDIF
C
C --- LECTURE DU MOT CLEF TEMPS_CPU
C
      IF ( IER .EQ. 0 ) CALL IBTCPU (IER)
C
C --- LECTURE DU MOT CLE HDF ---
C
      IF ( IER .EQ. 0 ) CALL IBFHDF( IER , FICHDF )
C
C --- LECTURE DU MOT CLE FACTEUR BASE ET ---
C --- ALLOCATION DES BASES DE DONNEES ---
      IF ( IER .EQ. 0 ) CALL IBBASE( IER , FICHDF )
      IF ( IER. EQ. 0 ) THEN
         CALL GETRES(K8B,K16B,NOMCMD)
C        -- INITIALISATION DE LA FONCTION NULLE : '&FOZERO'
C           ET DU COMMON FOSAV
         CALL FOZERO('&FOZERO')
         CALL FOINT0
      ENDIF
C
C --- POUR EVITER QUE LA CREATION DE '&&_NUM_CONCEPT_UNIQUE'
C        NE SOIT REPROCHE A UNE COMMANDE CREANT UNE SD
C        (DEBUT/DEBUG/SDVERI='OUI')
      CALL GCNCON('.',K8B)
C
C --- INITIALISATION DE LA TABLE DES CONCEPTS ---
      CALL GCUINI( MXCMD , 'G' , IER )
C
C --- LECTURE DU MOT CLE FACTEUR  CATALOGUE ---
      IF ( IER .EQ. 0 .AND. FICHDF .EQ. '  ') CALL IBCATA( IER )
C
C --- STATS SUR LA COMMANDE DE DEMARRAGE  ---
C
      IF ( IER .EQ. 0 ) CALL GCUOPR( 1, ICMD )
 9999 CONTINUE
      END
