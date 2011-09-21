      SUBROUTINE MDICHO ( NOMRES,NBSTOC,TEMPS,FORCHO,DEPLOC,VITCHO,
     +                    NBCHTO,NBCHOC,PARCHO,NOECHO )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             NBSTOC,NBCHOC,NBCHTO
      REAL*8                     PARCHO(NBCHTO,*)
      REAL*8                     TEMPS(*),FORCHO(*),DEPLOC(*),VITCHO(*)
      CHARACTER*8         NOMRES,NOECHO(NBCHTO,*),NIVEAU
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/09/2011   AUTEUR COURTOIS M.COURTOIS 
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
C
C     IMPRESSION DES RESULTATS DE CHOC
C     ------------------------------------------------------------------
C IN  : NBSTOC : NOMBRE DE PAS STOCKES
C IN  : TEMPS  : TABLEAU DES TEMPS STOCKES
C IN  : FORCHO : TABLEAU DES FORCES DE CHOC STOCKEES
C IN  : DEPLOC : TABLEAU DES DEPLACEMENTS LOCAUX AUX NOEUDS DE CHOC
C IN  : VITCHO : TABLEAU DES VITESSES AUX NOEUDS DE CHOC
C IN  : NBCHTO : DIMENSION DES TABLEAUX
C IN  : NBCHOC : NOMBRE DE NOEUDS DE CHOC
C IN  : PARCHO : TABLEAU DES PARAMETRES DES NOEUDS DE CHOC
C IN  : NOECHO : TABLEAU DES NOMS DES NOEUDS DE CHOC
C ----------------------------------------------------------------------
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
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      INTEGER       I, IFR, ICHO, IPAS, IX, IY, IZ, NBTITR
      CHARACTER*8   K8B, TYPEOB, BLAN8
      INTEGER      IARG
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      IFR = IUNIFI('RESULTAT')
      BLAN8='        '
      NIVEAU = BLAN8
C
C     --- LECTURE MOT-CLE FACTEUR IMPRESSION ---
C
      CALL GETVTX('IMPRESSION','NIVEAU',1,IARG,1,NIVEAU,NIMPR)
      IF (NIMPR.EQ.0) THEN
         CALL GETVTX('IMPRESSION','TOUT',1,IARG,1,NIVEAU,NIMPR)
         IF (NIVEAU(1:3).NE.'OUI') GOTO 9999
         NIVEAU='TOUT_LOC'
      ENDIF
      CALL GETVR8('IMPRESSION','INST_INIT',1,IARG,1,DEBUT,NDEB)
      CALL GETVR8('IMPRESSION','INST_FIN',1,IARG,1,FIN,NFIN)
      IF (NDEB.EQ.0) DEBUT=TEMPS(1)
      IF (NFIN.EQ.0) FIN=TEMPS(NBSTOC)
C
C     --- IMPRESSION DU TITRE ---
      CALL TITRE
      CALL JEEXIN(NOMRES//'           .TITR',IRET)
      IF (IRET .NE. 0 ) THEN
         CALL JEVEUO(NOMRES//'           .TITR','L',LTITR)
         CALL JELIRA(NOMRES//'           .TITR','LONMAX',NBTITR,K8B)
         DO 2 I= 1, NBTITR
            IF (NIMPR.NE.0) WRITE(IFR,1010) ZK80(LTITR+I-1)
 2       CONTINUE
      ENDIF
      IF (NIMPR.NE.0) WRITE(IFR,1010) ' ***** RESULTATS DE CHOC ***** '
      IF (NIMPR.NE.0) WRITE(IFR,1000) NBCHOC
C
      DO 10 ICHO = 1,NBCHOC
         TYPEOB = NOECHO(ICHO,9)
         IF (NIMPR.NE.0)
     +      WRITE(IFR,1050) 'OBSTACLE NO: ',ICHO,' DE TYPE ',TYPEOB
C
C        --- TAUX DE RECONSTITUTION ---
         IF (NIVEAU.EQ.'TOUT_LOC' .OR. NIVEAU.EQ.'TAUX_CHOC') THEN
            WRITE(IFR,1070)
     +     'TAUX DE RECONSTITUTION DE LA SOLUTION STATIQUE',
     +     'AU NOEUD DE CHOC:',NOECHO(ICHO,1),':',PARCHO(ICHO,48)
         IF (TYPEOB(1:2).EQ.'BI')
     +      WRITE(IFR,1070)
     +     'TAUX DE RECONSTITUTION DE LA SOLUTION STATIQUE',
     +     'AU NOEUD DE CHOC:',NOECHO(ICHO,5),':',PARCHO(ICHO,49)
         ENDIF
C
C        --- DEPLACEMENTS LOCAUX ---
         IF (NIVEAU.EQ.'TOUT_LOC' .OR. NIVEAU.EQ.'DEPL_LOC') THEN
            WRITE(IFR,1060)
     +           'DEPLACEMENTS LOCAUX AU NOEUD DE CHOC:',NOECHO(ICHO,1)
            WRITE(IFR,1020) NBSTOC
            WRITE(IFR,1060) '    PAS INSTANT       ',
     +      ' EN X              EN Y              EN Z'
            DO 40 IPAS = 1,NBSTOC
               IF (TEMPS(IPAS).GE.DEBUT .AND. TEMPS(IPAS).LE.FIN) THEN
                  IX = 1 + 3 * ( ICHO - 1 ) + 3 * NBCHTO * ( IPAS - 1 )
                  IY = 2 + 3 * ( ICHO - 1 ) + 3 * NBCHTO * ( IPAS - 1 )
                  IZ = 3 + 3 * ( ICHO - 1 ) + 3 * NBCHTO * ( IPAS - 1 )
                 WRITE(IFR,1040) IPAS, TEMPS(IPAS), DEPLOC(IX),
     +                           DEPLOC(IY),DEPLOC(IZ)
               ENDIF
 40         CONTINUE
         ENDIF
C
C        --- FORCES DE CONTACT ---
         IF (NIVEAU.EQ.'TOUT_LOC' .OR. NIVEAU.EQ.'FORC_LOC') THEN
            WRITE(IFR,1060)
     +         'FORCES DE CONTACT AU NOEUD DE CHOC:',NOECHO(ICHO,1)
            WRITE(IFR,1020) NBSTOC
            WRITE(IFR,1060) '    PAS INSTANT        ',
     +      'NORMALE           TANGENTIELLE      TANGENTIELLE_SENS X'
            DO 50 IPAS = 1,NBSTOC
               IF (TEMPS(IPAS).GE.DEBUT .AND. TEMPS(IPAS).LE.FIN) THEN
                  IX = 1 + 3 * ( ICHO - 1 ) + 3 * NBCHTO * ( IPAS - 1 )
                  IY = 2 + 3 * ( ICHO - 1 ) + 3 * NBCHTO * ( IPAS - 1 )
                  IZ = 3 + 3 * ( ICHO - 1 ) + 3 * NBCHTO * ( IPAS - 1 )
                  WRITE(IFR,1040) IPAS, TEMPS(IPAS), FORCHO(IX),
     +                            FORCHO(IY),FORCHO(IZ)
               ENDIF
 50         CONTINUE
         ENDIF
C
C        --- VITESSES LOCALES ---
         IF (NIVEAU.EQ.'TOUT_LOC' .OR. NIVEAU.EQ.'VITE_LOC') THEN
            WRITE(IFR,1060)
     +         'VITESSES LOCALES AU NOEUD DE CHOC:',NOECHO(ICHO,1)
            WRITE(IFR,1020) NBSTOC
            WRITE(IFR,1060) '    PAS INSTANT        ',
     +      'NORMALE           TANGENTIELLE      TANGENTIELLE_SENS X'
            DO 60 IPAS = 1,NBSTOC
               IF (TEMPS(IPAS).GE.DEBUT .AND. TEMPS(IPAS).LE.FIN) THEN
                  IX = 1 + 3 * ( ICHO - 1 ) + 3 * NBCHTO * ( IPAS - 1 )
                  IY = 2 + 3 * ( ICHO - 1 ) + 3 * NBCHTO * ( IPAS - 1 )
                  IZ = 3 + 3 * ( ICHO - 1 ) + 3 * NBCHTO * ( IPAS - 1 )
                  WRITE(IFR,1040) IPAS, TEMPS(IPAS), VITCHO(IX),
     +                            VITCHO(IY),VITCHO(IZ)
               ENDIF
 60         CONTINUE
         ENDIF
 10   CONTINUE
C
 1000 FORMAT(' NOMBRE DE LIEUX D''OBSTACLES ',I6)
 1010 FORMAT(A)
 1020 FORMAT(' NOMBRE DE PAS ',I6)
 1040 FORMAT(1P,I8,' ',D13.7,3(' ',D17.10))
 1050 FORMAT(A,I8,2A)
 1060 FORMAT(2A)
 1070 FORMAT(A,/,A,A,A,1PD12.5)
C
 9999 CONTINUE
      CALL JEDEMA()
      END
