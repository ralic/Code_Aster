      SUBROUTINE TE0336 ( OPTION , NOMTE )
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 11/08/2003   AUTEUR SMICHEL S.MICHEL-PONNELLE 
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
C     BUT:       POUR LES ELEMENTS ISOPARAMETRIQUES 2D , CALCUL DES
C                GRANDEURS EQUIVALENTES SUIVANTES
C                AUX POINTS DE GAUSS :
C                    POUR LES CONTRAINTES  A PARTIR DE SIEF_ELGA
C                                                   OU SIEF_ELGA_DEPL
C                    POUR LES DEFORMATIONS A PARTIR DE EPSI_ELGA_DEPL
C                                                   OU EPME_ELGA_DEPL
C                    (POUR LES DEFORMATIONS HORS THERMIQUES)
C                AUX NOEUDS :
C                    POUR LES CONTRAINTES  A PARTIR DE SIEF_ELGA
C                                                   OU SIEF_ELGA_DEPL
C                    POUR LES DEFORMATIONS A PARTIR DE EPSI_ELNO_DEPL
C                                                   OU EPME_ELNO_DEPL
C                    (POUR LES DEFORMATIONS HORS THERMIQUES)
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
C                DIMENSIONNE  A  NEQMAX CMP MAX * 9 PG MAX
C                EQNO (CONT/DEF EQUIVALENT NOEUDS)
C                DIMENSIONNE  A  NEQMAX CMP MAX * 9 NO MAX
C ----------------------------------------------------------------------
      PARAMETER         ( NPGMAX = 9 , NNOMAX = 9 , NEQMAX = 6 )
C ----------------------------------------------------------------------
      CHARACTER*24       CARAC
      CHARACTER*16       OPTION , NOMTE
      CHARACTER*8        ELREFE
      INTEGER            NNO ,    KP ,   I ,     K
      INTEGER            NCEQ,    IDCP,  ICONT,  IDEFO, IEQUIF, ICARAC
      INTEGER            NPG , NNOS
      REAL*8             EQPG(NEQMAX*NPGMAX),    EQNO(NEQMAX*NNOMAX)
      REAL*8             SIGMA(6) ,DEFORM(6)
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      CALL ELREF1(ELREFE)
      IF ( OPTION(11:14) .EQ. 'EPSI' )  THEN
          NCEQ = 5
      ELSEIF ( OPTION(11:14) .EQ. 'EPME' )  THEN
          NCEQ = 5
      ELSE IF (OPTION(11:14) .EQ. 'SIGM' )  THEN
          NCEQ = 6
      ENDIF
C
C
      CARAC='&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,'L',ICARAC)
      NNO  = ZI(ICARAC)
      NPG  = ZI(ICARAC+2)
C
      IF (NOMTE(5:8).EQ.'TR3 ') THEN
        NNOS = NNO
      ELSE IF (NOMTE(5:8).EQ.'QU4 ') THEN
        NNOS = NNO
      ELSE IF (NOMTE(5:8).EQ.'TR6 ') THEN
        NNOS = 3
      ELSE IF (NOMTE(5:8).EQ.'QS8 ') THEN
        NNOS = 4
      ELSE IF (NOMTE(5:8).EQ.'QU8 ' .OR. NOMTE(5:8).EQ.'QU9 ') THEN
        NNOS = 4
      END IF
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
C -   DEFORMATIONS ET CONTRAINTES EQUIVALENTES AUX POINTS DE GAUSS
C
      IF ( OPTION(6:9) .EQ. 'ELGA' ) THEN
C
          DO 111 I  = 1,NCEQ*NPG
              EQPG(I)  = 0.0D0
111       CONTINUE
C
C -       DEFORMATIONS
C
          IF ( OPTION(11:14) .EQ. 'EPSI' )  THEN
              DO 101 KP = 1,NPG
                  IDCP = (KP-1) * NCEQ
                  DO 107 I = 1,4
                      DEFORM(I) = ZR(IDEFO+(KP-1)*4+I-1)
 107              CONTINUE
                  DEFORM(5) = 0.D0
                  DEFORM(6) = 0.D0
                  CALL FGEQUI(DEFORM,'EPSI',2,EQPG(IDCP+1))
101           CONTINUE
C
C -       DEFORMATIONS HORS THERMIQUES
C
          ELSEIF ( OPTION(11:14) .EQ. 'EPME' )  THEN
              DO 102 KP = 1,NPG
                  IDCP = (KP-1) * NCEQ
                  DO 108 I = 1,4
                      DEFORM(I) = ZR(IDEFO+(KP-1)*4+I-1)
 108              CONTINUE
                  DEFORM(5) = 0.D0
                  DEFORM(6) = 0.D0
                  CALL FGEQUI(DEFORM,'EPSI',2,EQPG(IDCP+1))
102           CONTINUE
C
C -       CONTRAINTES
C
          ELSE IF ( OPTION(11:14) .EQ. 'SIGM' )  THEN
              DO 103 KP = 1,NPG
                  IDCP = (KP-1) * NCEQ
                  DO 109 I = 1,4
                      SIGMA(I) = ZR(ICONT+(KP-1)*4+I-1)
 109              CONTINUE
                  SIGMA(5) = 0.D0
                  SIGMA(6) = 0.D0
                  CALL FGEQUI(SIGMA,'SIGM',2,EQPG(IDCP+1))
103           CONTINUE
          ENDIF
C
C -       STOCKAGE
C
          DO 200 KP = 1,NPG
              DO 300 J  = 1,NCEQ
                  ZR(IEQUIF-1+(KP-1)*NCEQ+J) = EQPG((KP-1)*NCEQ+J)
300           CONTINUE
200       CONTINUE
C
C -   DEFORMATIONS ET CONTRAINTES EQUIVALENTES AUX NOEUDS
C
      ELSE IF ( OPTION(6:9) .EQ. 'ELNO' ) THEN
C
          DO 114 I  = 1,NCEQ*NNO
              EQNO(I) = 0.0D0
114       CONTINUE
C
C -       DEFORMATIONS
C
          IF ( OPTION(11:14) .EQ. 'EPSI' )  THEN
              DO 201 INO = 1,NNO
                  IDCP = (INO-1) * NCEQ
                  DO 207 I = 1,4
                      DEFORM(I) = ZR(IDEFO+(INO-1)*4+I-1)
 207              CONTINUE
                  DEFORM(5) = 0.D0
                  DEFORM(6) = 0.D0
                  CALL FGEQUI(DEFORM,'EPSI',2,EQNO(IDCP+1))
201           CONTINUE
C
C -       DEFORMATIONS
C
          ELSEIF ( OPTION(11:14) .EQ. 'EPME' )  THEN
              DO 202 INO = 1,NNO
                  IDCP = (INO-1) * NCEQ
                  DO 208 I = 1,4
                      DEFORM(I) = ZR(IDEFO+(INO-1)*4+I-1)
 208              CONTINUE
                  DEFORM(5) = 0.D0
                  DEFORM(6) = 0.D0
                  CALL FGEQUI(DEFORM,'EPSI',2,EQNO(IDCP+1))
202           CONTINUE
C
C -       CONTRAINTES
C
          ELSE IF ( OPTION(11:14) .EQ. 'SIGM' )  THEN
              DO 303 KP = 1,NPG
                  IDCP = (KP-1) * NCEQ
                  DO 209 I = 1,4
                      SIGMA(I) = ZR(ICONT+(KP-1)*4+I-1)
 209              CONTINUE
                  SIGMA(5) = 0.D0
                  SIGMA(6) = 0.D0
                  CALL FGEQUI(SIGMA,'SIGM',2,EQPG(IDCP+1))
303           CONTINUE
C
C -       EXTRAPOLATION AUX NOEUDS
C
              CALL PPGANO(NNOS,NPG,NCEQ,EQPG,ZR(IEQUIF))
C
          ENDIF
C
C -       STOCKAGE
C
          IF ( OPTION(11:14) .NE. 'SIGM' )  THEN
             DO 400 INO = 1,NNO
                DO 500 J   = 1,NCEQ
                    ZR(IEQUIF+NCEQ*(INO-1)-1+J) = EQNO(NCEQ*(INO-1)+J)
500             CONTINUE
400          CONTINUE
          ENDIF
C
      ENDIF
C
      END
