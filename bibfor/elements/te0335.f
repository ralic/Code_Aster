      SUBROUTINE TE0335(OPTION,NOMTE)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 04/04/2002   AUTEUR VABHHTS J.PELLET 
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
C ----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     BUT:       POUR LES ELEMENTS ISOPARAMETRIQUES 3D , CALCUL DES
C                GRANDEURS EQUIVALENTES SUIVANTES
C                AUX POINTS DE GAUSS :
C                    POUR LES CONTRAINTES  A PARTIR DE SIEF_ELGA
C                                                   OU SIEF_ELGA_DEPL
C                    POUR LES DEFORMATIONS A PARTIR DE EPSI_ELGA_DEPL
C                                                   OU EPME_ELGA_DEPL
C                    (POUR LES DEFORMATIONS HORS THERMIQUES)
C                AUX NOEUDS :
C                    POUR LES CONTRAINTES  A PARTIR DE SIEF_ELNO_ELGA
C                                                   OU SIGM_ELNO_DEPL
C                    POUR LES DEFORMATIONS A PARTIR DE EPSI_ELNO_DEPL
C                                                   OU EPME_ELNO_DEPL
C                    (POUR LES DEFORMATIONS HORS THERMIQUES)
C
C                DANS CET ORDRE :
C
C                . CONTRAINTES EQUIVALENTES  :
C                        . VON MISES                    (= 1 VALEUR)
C                        . TRESCA                       (= 1 VALEUR)
C                        . CONTRAINTES PRINCIPALES      (= 3 VALEURS)
C                        . VON-MISES * SIGNE (PRESSION) (= 1 VALEUR)
C               . DEFORMATIONS EQUIVALENTES  :
C                        . SECOND INVARIANT             (= 1 VALEUR)
C                        . DEFORMATIONS PRINCIPALES     (= 3 VALEURS)
C                        . 2EME INV. * SIGNE (1ER.INV.) (= 1 VALEUR)
C
C     OPTIONS :  'EQUI_ELNO_SIGM'
C                'EQUI_ELGA_SIGM'
C                'EQUI_ELNO_EPSI'
C                'EQUI_ELGA_EPSI'
C                'EQUI_ELNO_EPME'
C                'EQUI_ELGA_EPME'
C
C     ENTREES :  OPTION : OPTION DE CALCUL
C                NOMTE  : NOM DU TYPE ELEMENT
C ----------------------------------------------------------------------
C     REMARQUE:  LA DERNIERE GRANDEUR EST UTILISE
C                PARTICULIEREMENT POUR DES CALCULS DE CUMUL DE
C                DOMMAGE EN FATIGUE
C                EQPG (CONT/DEF EQUIVALENT PG)
C                DIMENSIONNE  A  NEQMAX CMP MAX * 21 PG MAX
C                EQNO (CONT/DEF EQUIVALENT NOEUDS)
C                DIMENSIONNE  A  NEQMAX CMP MAX * 27 NO MAX
C ----------------------------------------------------------------------
      PARAMETER         ( NPGMAX = 27 , NNOMAX = 27 , NEQMAX = 6 )
C ----------------------------------------------------------------------
      INTEGER            IDEFO,  ICONT, IEQUIF
      INTEGER            IDCP  , KP,    J,   I, INO, JIN
      INTEGER            NNO   ,  NBPG(10) , NCEQ, NPG
      REAL*8             EQPG(NEQMAX*NPGMAX), EQNO(NEQMAX*NNOMAX)
      CHARACTER*8        ELREFE
      CHARACTER*16       NOMTE,OPTION
      CHARACTER*24       CHCTE
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      CALL ELREF1(ELREFE)
      IF ( OPTION(11:14) .EQ. 'EPSI' )  THEN
          NCEQ = 5
      ELSE IF ( OPTION(11:14) .EQ. 'EPME' )  THEN
          NCEQ = 5
      ELSE IF (OPTION(11:14) .EQ. 'SIGM' )  THEN
          NCEQ = 6
      ENDIF
C
      CHCTE = '&INEL.'//ELREFE//'.CARACTE'
      CALL JEVETE(CHCTE,'L',JIN)
      NNO   = ZI(JIN+2-1)
      NBFPG = ZI(JIN+3-1)
      DO 110 I = 1,NBFPG
          NBPG(I) = ZI(JIN+3-1+I)
110   CONTINUE
      NPG   = NBPG(1)
C
      IF      ( OPTION(11:14) .EQ. 'EPSI' ) THEN
          CALL JEVECH('PDEFORR','L',IDEFO)
          CALL JEVECH('PDEFOEQ','E',IEQUIF)
      ELSEIF  ( OPTION(11:14) .EQ. 'EPME' ) THEN
          CALL JEVECH('PDEFORR','L',IDEFO)
          CALL JEVECH('PDEFOEQ','E',IEQUIF)
      ELSE IF ( OPTION(11:14) .EQ. 'SIGM' ) THEN
          CALL JEVECH('PCONTRR','L',ICONT)
          CALL JEVECH('PCONTEQ','E',IEQUIF)
      ENDIF
C
      DO 111 I  = 1,NCEQ*NPG
          EQPG(I)  = 0.0D0
111   CONTINUE
      DO 114 I  = 1,NCEQ*NNO
          EQNO(I) = 0.0D0
114   CONTINUE
C
C -   DEFORMATIONS ET CONTRAINTES EQUIVALENTES AUX POINTS DE GAUSS
C
      IF ( OPTION(6:9) .EQ. 'ELGA' ) THEN
C
C -       DEFORMATIONS
C
          IF ( OPTION(11:14) .EQ. 'EPSI' )  THEN
              DO 101 KP = 1,NPG
                  IDCP = (KP-1) * NCEQ
                  CALL FGEQUI(ZR(IDEFO+(KP-1)*6),'EPSI',3,EQPG(IDCP+1))
101           CONTINUE
C
C -       DEFORMATIONS HORS THERMIQUES
C
          ELSEIF ( OPTION(11:14) .EQ. 'EPME' )  THEN
              DO 102 KP = 1,NPG
                  IDCP = (KP-1) * NCEQ
                  CALL FGEQUI(ZR(IDEFO+(KP-1)*6),'EPSI',3,EQPG(IDCP+1))
102           CONTINUE
C
C -       CONTRAINTES
C
          ELSE IF ( OPTION(11:14) .EQ. 'SIGM' )  THEN
              DO 103 KP = 1,NPG
                  IDCP = (KP-1) * NCEQ
                  CALL FGEQUI(ZR(ICONT+(KP-1)*6),'SIGM',3,EQPG(IDCP+1))
103           CONTINUE
          ENDIF
C
C -       STOCKAGE
C
          DO 200 KP = 1,NPG
              DO 300 J  = 1,NCEQ
                  ZR(IEQUIF-1+(KP-1)*NCEQ+J)  = EQPG((KP-1)*NCEQ+J)
300           CONTINUE
200       CONTINUE
C
C -   DEFORMATIONS ET CONTRAINTES EQUIVALENTES AUX NOEUDS
C
      ELSE IF ( OPTION(6:9) .EQ. 'ELNO' ) THEN
C
C -       DEFORMATIONS
C
          IF ( OPTION(11:14) .EQ. 'EPSI' )  THEN
              DO 201 INO = 1,NNO
                  IDCP = (INO-1) * NCEQ
                  CALL FGEQUI(ZR(IDEFO+(INO-1)*6),'EPSI',3,EQNO(IDCP+1))
201           CONTINUE
C
C -       DEFORMATIONS HORS THERMIQUES
C
          ELSEIF ( OPTION(11:14) .EQ. 'EPME' )  THEN
              DO 202 INO = 1,NNO
                  IDCP = (INO-1) * NCEQ
                  CALL FGEQUI(ZR(IDEFO+(INO-1)*6),'EPSI',3,EQNO(IDCP+1))
202           CONTINUE
C
C -       CONTRAINTES
C
          ELSE IF ( OPTION(11:14) .EQ. 'SIGM' )  THEN
              DO 203 INO = 1,NNO
                  IDCP = (INO-1) * NCEQ
                  CALL FGEQUI(ZR(ICONT+(INO-1)*6),'SIGM',3,EQNO(IDCP+1))
203           CONTINUE
          ENDIF
C
C -       STOCKAGE
C
          DO 400 INO = 1,NNO
              DO 500 J   = 1,NCEQ
                  ZR(IEQUIF-1+(INO-1)*NCEQ+J) = EQNO((INO-1)*NCEQ+J)
500           CONTINUE
400       CONTINUE
      ENDIF
C
      END
