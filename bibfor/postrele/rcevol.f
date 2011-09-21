      SUBROUTINE RCEVOL ( TYPTAB, NOMMAT, SYMAX, NBOPT, OPTION )
      IMPLICIT   NONE
      INTEGER             NBOPT
      REAL*8              SYMAX
      CHARACTER*8         NOMMAT
      CHARACTER*16        TYPTAB, OPTION(*)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 21/09/2011   AUTEUR COURTOIS M.COURTOIS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     ------------------------------------------------------------------
C
C     OPERATEUR POST_RCCM:    TYPE_ANALYSE = 'COMPOSANT'
C                           TYPE_RESU_MECA = 'EVOLUTION'
C
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
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER      I, J, N1, NBINTI, JINTI, NBTRAN
      REAL*8        PARA(3), SM, R8VIDE
      LOGICAL      LPMPB, LSN, LFATIG, FLEXIO, LROCHT, LAMORC,KEMIXT
      CHARACTER*8  K8B,TYPEKE
      CHARACTER*16 KINTI
      CHARACTER*24 CINST, CSILI, CSIEX, CSNO, CSNE, CSNEO, CSNEE,
     +             CSPO, CSPE, CFAO, CFAE, CNOC, CRESU, CRESP, INTITU,
     +             CSPTO, CSPTE, CSPMO, CSPME, CSTEX, CSMEX
      INTEGER      IARG
C DEB ------------------------------------------------------------------
C
C --- VECTEUR DES INSTANTS DEMANDES
C
      CINST = '&&RCEVOL.INSTANTS'
C
C --- VECTEUR DES TRANSITOIRES
C
      CRESU = '&&RCEVOL.RESU_MECA'
      CRESP = '&&RCEVOL.RESU_PRES'
C
C --- VECTEUR DES CONTRAINTES LINEARISEES AUX EXTREMITES (PMPB, SN)
C
      CSILI = '&&RCEVOL.SIGM_LINE'
C
C --- VECTEUR DES CONTRAINTES TOTALES AUX EXTREMITES (SP)
C
      CSIEX = '&&RCEVOL.SIGM_EXTR'
C
C --- VECTEUR DES CONTRAINTES THERMIQUES AUX EXTREMITES (SPTH)
C
      CSTEX = '&&RCEVOL.STHE_EXTR'
C
C --- VECTEUR DES CONTRAINTES MECANIQUES AUX EXTREMITES (SPMECA)
C
      CSMEX = '&&RCEVOL.SMEC_EXTR'
C
C --- VECTEUR DES NB_OCCUR
C
      CNOC  = '&&RCEVOL.NB_OCCUR'
C
C --- CALCUL DE GRANDEURS A L'ORIGINE ET A L'EXTREMITE
C
      CSNO  = '&&RCEVOL.CALCUL_SN .ORIG'
      CSNE  = '&&RCEVOL.CALCUL_SN .EXTR'
      CSNEO = '&&RCEVOL.CALCUL_SNE.ORIG'
      CSNEE = '&&RCEVOL.CALCUL_SNE.EXTR'
      CSPO  = '&&RCEVOL.CALCUL_SP .ORIG'
      CSPE  = '&&RCEVOL.CALCUL_SP .EXTR'
      CSPTO  = '&&RCEVOL.CALCUL_SPT.ORIG'
      CSPME  = '&&RCEVOL.CALCUL_SPT.EXTR'
      CSPMO  = '&&RCEVOL.CALCUL_SPM.ORIG'
      CSPTE  = '&&RCEVOL.CALCUL_SPM.EXTR'
      CFAO  = '&&RCEVOL.FATIGUE   .ORIG'
      CFAE  = '&&RCEVOL.FATIGUE   .EXTR'
C
C     ------------------------------------------------------------------
C                             LES OPTIONS
C     ------------------------------------------------------------------
      LFATIG = .FALSE.
      LSN    = .FALSE.
      LPMPB  = .FALSE.
      FLEXIO = .FALSE.
      LROCHT = .FALSE.
      LAMORC = .FALSE.
C
      DO 10 I = 1 , NBOPT
         IF ( OPTION(I) .EQ. 'PM_PB' ) THEN
            LPMPB = .TRUE.
         ELSEIF ( OPTION(I) .EQ. 'SN' ) THEN
            LSN = .TRUE.
         ELSEIF ( OPTION(I) .EQ. 'FATIGUE_ZH210' ) THEN
            LFATIG = .TRUE.
            LSN    = .TRUE.
         ELSEIF ( OPTION(I) .EQ. 'AMORCAGE' ) THEN
            LAMORC = .TRUE.
         ENDIF
 10   CONTINUE
C
      IF ( LAMORC .AND. (LPMPB .OR. LSN .OR. LFATIG) ) THEN
         CALL U2MESG('F', 'POSTRCCM_3',0,K8B,0,0,0,0.D0)
      ENDIF
C
      KEMIXT = .FALSE.
      CALL GETVTX (' ', 'TYPE_KE', 0,IARG,1, TYPEKE, N1 )
      IF (TYPEKE.EQ.'KE_MIXTE')   KEMIXT = .TRUE.
C
C     ------------------------------------------------------------------
C                      TRAITEMENT DE L'AMORCAGE
C     ------------------------------------------------------------------
C
      IF ( LAMORC ) THEN
         CALL RCEVOA ( TYPTAB, NOMMAT )
         GOTO 9999
      ENDIF
C
C     ------------------------------------------------------------------
C                            LE MATERIAU
C     ------------------------------------------------------------------
C
      CALL RCEVO1 ( NOMMAT, LFATIG, SM, PARA, SYMAX )
C
C     ------------------------------------------------------------------
C                     NOMBRE DE LIGNE A "POST_RCCM"
C     ------------------------------------------------------------------
C
      INTITU = '&&RCEVOL.INTITULE'
      CALL RCEVO0 ( INTITU, NBINTI, LSN, LFATIG, NBTRAN )
      CALL JEVEUO ( INTITU, 'L', JINTI )
C
C     ------------------------------------------------------------------
C
      DO 100 I = 1, NBINTI
C
        DO 110 J = 1, NBTRAN
C
           KINTI = ZK16(JINTI-1+NBTRAN*(I-1)+J)
C
C         --------------------------------------------------------------
C                    TRAITEMENT DU MOT CLE FACTEUR TRANSITOIRE
C         --------------------------------------------------------------
C
           IF ( LSN .AND. .NOT.LFATIG .AND. NBTRAN.GT.1 ) THEN
             CALL RCEV22 ( NBINTI, KINTI, J, CSILI, CINST, CSIEX,
     +                     LFATIG, FLEXIO, LROCHT, CNOC, CRESU, CRESP )
           ELSE
             CALL RCEVO2 ( NBINTI, KINTI, CSILI, CINST, CSIEX,
     +                     KEMIXT, CSTEX, CSMEX, LFATIG, FLEXIO,
     +                     LROCHT, CNOC, CRESU, CRESP )
           ENDIF
C
           IF ( LROCHT .AND. SYMAX.EQ.R8VIDE() ) THEN
              CALL U2MESS('A','POSTRCCM_4')
              LROCHT = .FALSE.
           ENDIF
C
C         --------------------------------------------------------------
C                          TRAITEMENT DES OPTIONS
C         --------------------------------------------------------------
C
          IF ( LSN ) CALL RCEVSN ( CSILI, CINST, CSNO, CSNE )
C
          IF ( FLEXIO )  CALL RCEVSE ( CSILI, CINST, CSNEO, CSNEE )
C
          IF ( LFATIG ) THEN
            CALL RCEVSP ( CSIEX, KEMIXT, CSTEX, CSMEX, CINST, CSPO,
     +                  CSPE, CSPTO, CSPTE, CSPMO, CSPME)
            CALL RCEVFA ( NOMMAT, PARA, SM, CNOC, CSNO, CSNE, CSPO,
     +                    CSPE, KEMIXT, CSPTO, CSPTE, CSPMO, CSPME,
     +                    CFAO, CFAE )
          ENDIF
C
C         --------------------------------------------------------------
C                                 ARCHIVAGE
C         --------------------------------------------------------------
C
          IF ( TYPTAB .EQ. 'VALE_MAX' ) THEN
C
           CALL RCEVOM ( CSILI, CINST, CNOC, SM, LFATIG, LPMPB, LSN,
     +                   CSNO, CSNE, FLEXIO, CSNEO, CSNEE, CFAO, CFAE,
     +                   CSPO, CSPE, CRESU, KINTI, I, J, LROCHT, SYMAX,
     +                   CRESP, KEMIXT, CSPTO, CSPTE, CSPMO, CSPME )
C
          ELSE
C
           CALL RCEVOD ( CSILI, CINST, CNOC, SM, LFATIG, LPMPB, LSN,
     +                   CSNO, CSNE, FLEXIO, CSNEO, CSNEE, CFAO, CFAE,
     +                   CSPO, CSPE, CRESU, KINTI, I, J, LROCHT, SYMAX,
     +                   CRESP, KEMIXT, CSPTO, CSPTE, CSPMO, CSPME )
C
          ENDIF
C
          CALL JEDETR ( CINST )
          CALL JEDETR ( CRESU )
          CALL JEDETR ( CRESP )
          CALL JEDETR ( CSILI )
          CALL JEDETR ( CSIEX )
          CALL JEDETR ( CNOC  )
          CALL JEDETR ( CSNO  )
          CALL JEDETR ( CSNE  )
          CALL JEDETR ( CSNEO )
          CALL JEDETR ( CSNEE )
          CALL JEDETR ( CSPO  )
          CALL JEDETR ( CSPE  )
          CALL JEDETR ( CFAO  )
          CALL JEDETR ( CFAE  )
          IF (KEMIXT) THEN
            CALL JEDETR ( CSTEX )
            CALL JEDETR ( CSMEX )
            CALL JEDETR ( CSPMO  )
            CALL JEDETR ( CSPME  )
            CALL JEDETR ( CSPTO  )
            CALL JEDETR ( CSPTE  )
          ENDIF
C
 110    CONTINUE
C
 100  CONTINUE
C
      CALL JEDETR ( INTITU )
C
 9999 CONTINUE
C
      END
