      SUBROUTINE TE0588(OPTION,NOMTE)
C ---------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 19/06/2002   AUTEUR JMBHH01 J.M.PROIX 
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
C----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     BUT:       POUR LES ELEMENTS PLAQUES ET COQUES , CALCUL DES
C                CONTRAINTES EQUIVALENTES AUX NOEUDS SUIVANTES
C                  POUR L'ELASTICITE  : A PARTIR DE SIGM_ELNO_DEPL
C                  POUR LA PLASTICITE : A PARTIR DE SIGM_ELNO_COQU
C
C                DANS CET ORDRE :
C
C                . CONTRAINTES EQUIVALENTES  :
C                        . VON MISES                    (= 1 VALEUR)
C                        . TRESCA                       (= 1 VALEUR)
C                        . CONTRAINTES PRINCIPALES      (= 3 VALEURS)
C                        . VON-MISES * SIGNE (PRESSION) (= 1 VALEUR)
C
C     OPTIONS :  'EQUI_ELNO_SIGM'
C
C     ENTREES :  OPTION : OPTION DE CALCUL
C                NOMTE  : NOM DU TYPE ELEMENT
C ----------------------------------------------------------------------
      PARAMETER         (NNOMAX = 27 , NEQMAX = 6 )
C ----------------------------------------------------------------------
      INTEGER            ICONT, IEQUIF
      INTEGER            IDCP  , J,   I, INO, JIN
      INTEGER            NNO   , NCEQ
      REAL*8             EQNO(NEQMAX*NNOMAX)
C      CHARACTER*8        ELREFE
      CHARACTER*8        ALIAS
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
C      CALL ELREF1(ELREFE)
      ALIAS=NOMTE(1:8)
      NCEQ = 6
      ZERO = 0.0D0
C
C
C      CHCTE = '&INEL.'//ELREFE//'.DESI'
      CHCTE = '&INEL.'//ALIAS//'.DESI'
      CALL JEVETE(CHCTE,'L',JIN)
      NNO   = ZI(JIN)
C
C --- RECUPERATION DU CHAMP DE CONTRAINTES AUX NOEUDS DE L'ELEMENT
C --- POUR LEQUEL ON VA CALCULER LA CONTRAINTE EQUIVALENTE .
C --- CE CHAMP DE CONTRAINTES EST ISSU D'UN CALCUL PAR CALC_ELEM
C --- AVEC L'OPTION 'SIGM_ELNO_DEPL' POUR L'ELASTICITE
C --- AVEC L'OPTION 'SIGM_ELNO_COQU' POUR LA PLASTICITE :
C     -------------------------------------------------
      CALL JEVECH('PCONCOR','L',ICONT)
      CALL JEVECH('PCONTEQ','E',IEQUIF)
C
      DO 10 I  = 1,NCEQ*NNO
          EQNO(I) = ZERO
10    CONTINUE
C
C --- CONTRAINTES EQUIVALENTES AUX NOEUDS :
C     -----------------------------------
      DO 20 INO = 1,NNO
          IDCP = (INO-1) * NCEQ
          CALL FGEQUI(ZR(ICONT+(INO-1)*6),'SIGM',3,EQNO(IDCP+1))
20    CONTINUE
C
C ---- STOCKAGE :
C      --------
      DO 30 INO = 1,NNO
          DO 40 J   = 1,NCEQ
              ZR(IEQUIF-1+(INO-1)*NCEQ+J) = EQNO((INO-1)*NCEQ+J)
40        CONTINUE
30    CONTINUE
C
      END
