      SUBROUTINE RCEVOL ( TYPTAB, NOMMAT, NBOPT, OPTION )
      IMPLICIT   NONE
      INTEGER             NBOPT
      CHARACTER*8         NOMMAT
      CHARACTER*16        TYPTAB, OPTION(*)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 08/02/2005   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
      REAL*8       RBID, PARA(3), SM
      LOGICAL      LPMPB, LSN, LFATIG, FLEXIO
      CHARACTER*8  K8B
      CHARACTER*16 KINTI
      CHARACTER*24 CINST, CSILI, CSIEX, CSNO, CSNE, CSNEO, CSNEE, 
     +             CSPO, CSPE, CFAO, CFAE, CNOC, CRESU, INTITU
C DEB ------------------------------------------------------------------
C
C --- VECTEUR DES INSTANTS DEMANDES
C
      CINST = '&&RCEVOL.INSTANTS'
C
C --- VECTEUR DES TRANSITOIRES
C
      CRESU = '&&RCEVOL.RESU_MECA'
C
C --- VECTEUR DES CONTRAINTES LINEARISEES AUX EXTREMITES (PMPB, SN)
C
      CSILI = '&&RCEVOL.SIGM_LINE'
C
C --- VECTEUR DES CONTRAINTES AUX EXTREMITES (SP)
C
      CSIEX = '&&RCEVOL.SIGM_EXTR'
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
C
      DO 10 I = 1 , NBOPT
         IF ( OPTION(I) .EQ. 'PM_PB' ) THEN
            LPMPB = .TRUE.
         ELSEIF ( OPTION(I) .EQ. 'SN' ) THEN
            LSN = .TRUE.
         ELSEIF ( OPTION(I) .EQ. 'FATIGUE_ZH210' ) THEN
            LFATIG = .TRUE.
            LSN    = .TRUE.
         ENDIF
 10   CONTINUE
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
C                            LE MATERIAU
C     ------------------------------------------------------------------
C
      CALL RCEVO1 ( NOMMAT, LFATIG, SM, PARA )
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
     +                     LFATIG, FLEXIO, CNOC, CRESU )
           ELSE
             CALL RCEVO2 ( NBINTI, KINTI, CSILI, CINST, CSIEX, 
     +                     LFATIG, FLEXIO, CNOC, CRESU )
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
            CALL RCEVSP ( CSIEX, CINST, CSPO, CSPE )
            CALL RCEVFA ( NOMMAT, PARA, SM, CNOC, CSNO, CSNE, CSPO,
     +                    CSPE, CFAO, CFAE )
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
     +                   CSPO, CSPE, CRESU, KINTI, I, J )
C
          ELSE
C
           CALL RCEVOD ( CSILI, CINST, CNOC, SM, LFATIG, LPMPB, LSN,
     +                   CSNO, CSNE, FLEXIO, CSNEO, CSNEE, CFAO, CFAE,
     +                   CSPO, CSPE, CRESU, KINTI, I, J )
C
          ENDIF
C
          CALL JEDETR ( CINST )
          CALL JEDETR ( CRESU )
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
C
 110    CONTINUE
C
 100  CONTINUE
C
      CALL JEDETR ( INTITU )
C
      END
