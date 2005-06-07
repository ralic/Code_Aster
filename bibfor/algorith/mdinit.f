      SUBROUTINE MDINIT (BASEMO,NBMODE,NBCHOC,DEPGEN,VITGEN,VINT, IER )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER                   NBMODE,               IER
      REAL*8                    DEPGEN(*),VITGEN(*),VINT(*)
      CHARACTER*8        BASEMO
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/10/1999   AUTEUR SABJLMA P.LATRUBESSE 
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
C     DONNEES INITIALES
C     ------------------------------------------------------------------
C IN  : BASEMO : NOM DU CONCEPT BASE MODALE
C IN  : NBMODE : NOMBRE DE MODES
C IN  : NBCHOC : NOMBRE DE CHOCS
C OUT : DEPGEN : DEPLACEMENTS GENERALISES
C OUT : VITGEN : VITESSES GENERALISEES
C OUT : VINT   : VARIABLES INTERNES (POUR LE FLAMBAGE DE CHOC)
C                (ON RETOURNE UNE VALEUR UNIQUEMENT SI NBCHOC>0 ET QU'ON
C                 EST DANS UN CAS  DE REPRISE)
C OUT : IER    : CODE RETOUR
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
      INTEGER       IM, JDEPI, JVITI
      CHARACTER*16  NOMCMD
      CHARACTER*19  NOMDEP, NOMVIT
      CHARACTER*8   TRAN,CRIT,INTER
      CHARACTER*1   K1BID
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      IER = 0
      NOMCMD = 'DYNA_TRAN_MODAL'
C
C     --- DEPLACEMENT ---
      CALL GETVID('ETAT_INIT','DEPL_INIT_GENE',1,1,1,NOMDEP,N1)
      IF (N1.NE.0) THEN
         CALL JEVEUO(NOMDEP//'.VALE','L',JDEPI)
C
C        --- VERIF COMPATIBILITE DES BASES DE PROJECTION
         CALL JEVEUO(NOMDEP//'.REFE','L',JREFE)
         IF (ZK24(JREFE)(1:8).NE.BASEMO) THEN
            IER = IER + 1
            CALL UTMESS('E',NOMCMD,'LES BASES UTILISEES POUR LA '//
     +                             'PROJECTION SONT DIFFERENTES.')
         ENDIF
         CALL JEVEUO(NOMDEP//'.DESC','L',JDESC)
         IF (ZI(JDESC+1).NE.NBMODE) THEN
            IER = IER + 1
            CALL UTMESS('E',NOMCMD,'LES BASES UTILISEES N''ONT PAS '//
     +                             'LE MEME NOMBRE DE VECTEURS.')
         ENDIF
         DO 10 IM = 1,NBMODE
            DEPGEN(IM) = ZR(JDEPI+IM-1)
 10      CONTINUE
      ENDIF
C
C     --- VITESSE ---
      CALL GETVID('ETAT_INIT','VITE_INIT_GENE',1,1,1,NOMVIT,N1)
      IF (N1.NE.0) THEN
         CALL JEVEUO(NOMVIT//'.VALE','L',JVITI)
C
C        --- VERIF COMPATIBILITE DES BASES DE PROJECTION
         CALL JEVEUO(NOMVIT//'.REFE','L',JREFE)
         IF (ZK24(JREFE)(1:8).NE.BASEMO) THEN
            IER = IER + 1
            CALL UTMESS('E',NOMCMD,'LES BASES UTILISEES POUR LA '//
     +                             'PROJECTION SONT DIFFERENTES.')
         ENDIF
         CALL JEVEUO(NOMVIT//'.DESC','L',JDESC)
         IF (ZI(JDESC+1).NE.NBMODE) THEN
            IER = IER + 1
            CALL UTMESS('E',NOMCMD,'LES BASES UTILISEES N''ONT PAS '//
     +                             'LE MEME NOMBRE DE VECTEURS.')
         ENDIF
         DO 20 IM = 1,NBMODE
            VITGEN(IM) = ZR(JVITI+IM-1)
 20      CONTINUE
      ENDIF
C
C     --- CAS D UNE REPRISE ---
      CALL GETVID('ETAT_INIT','RESU_GENE',1,1,1,TRAN,NT)
      IF (NT.NE.0) THEN 
C     --- RECUPERATION DES CHAMPS DEPL VITE ET ACCE ---
         CALL GETVTX('ETAT_INIT','CRITERE',1,1,1,CRIT,NC)
         CALL GETVR8('ETAT_INIT','PRECISION',1,1,1,PREC,NP)
         CALL GETVR8('ETAT_INIT','INST_INIT',1,1,1,TINIT,NI)
         CALL JEVEUO(TRAN//'           .DEPL' ,'E',JDEPLT)
         CALL JEVEUO(TRAN//'           .INST' ,'E',JINST)
         CALL JELIRA(TRAN//'           .INST' ,'LONUTI',NBINST,K1BID)
         IF (NI.EQ.0) TINIT = ZR(JINST+NBINST-1)
         INTER = 'NON'
         CALL EXTRAC(INTER,PREC,CRIT,NBINST,ZR(JINST),TINIT,
     +               ZR(JDEPLT),NBMODE,DEPGEN,IER)
         IF (IER.NE.0) THEN
            CALL UTMESS('F',NOMCMD,'ON N''A PAS PU TROUVE LES '//
     +                  'DEPLACEMENTS INITIAUX ')
         ENDIF
         CALL JEVEUO(TRAN//'           .VITE' ,'E',JVITET)
         INTER = 'NON'
         CALL EXTRAC(INTER,PREC,CRIT,NBINST,ZR(JINST),TINIT,
     +               ZR(JVITET),NBMODE,VITGEN,IER)
         IF (IER.NE.0) THEN
            CALL UTMESS('F',NOMCMD,'ON N''A PAS PU TROUVE LES '//
     +                  'VITESSES INITIALES ')
         ENDIF
         IF (NBCHOC .GT. 0) THEN
            CALL JEVEUO(TRAN//'           .VINT' ,'E',JVINT)
            INTER = 'NON'
            CALL EXTRAC(INTER,PREC,CRIT,NBINST,ZR(JINST),TINIT,
     +                  ZR(JVINT),NBCHOC,VINT,IER)
            IF (IER.NE.0) THEN
               CALL UTMESS('F',NOMCMD,'ON N''A PAS PU TROUVE LES '//
     +                     'VARIABLES INTERNES INITIALES : REPRISE '//
     +                     ' CHOC AVEC FLAMBAGE ')
            ENDIF
         ENDIF
      ENDIF
C
      CALL JEDEMA()
      END
