      SUBROUTINE NMPILO(PILOTE, DINST , RHO   , DEPDEL, DEPOLD,
     &                  MODELE, MATE  , COMPOR, VALMOI, NBATTE,
     &                  NBEFFE, DDEPLA, ETA   , LICCVG)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/02/2002   AUTEUR GODARD V.GODARD 
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
C RESPONSABLE ADBHHVV V.CANO

      IMPLICIT NONE
      INTEGER       NBATTE, LICCVG(NBATTE), NBEFFE
      REAL*8        DINST, RHO, ETA(NBATTE)
      CHARACTER*14  PILOTE
      CHARACTER*24  DEPDEL, DDEPLA(NBATTE), DEPOLD
      CHARACTER*24  MODELE, MATE, COMPOR, VALMOI

C ----------------------------------------------------------------------
C     CALCUL DE ETA_PILOTAGE : DU = DUN + RHO.DU0 + ETA.RHO.DU1
C  PILOTAGE: DDL_IMPO, LONG_ARC, DEFORMATION, PRED_ELAS, ANA_LIM
C ----------------------------------------------------------------------
C
C       IN PILOTE : SD PILOTAGE
C       IN DINST  : INCREMENT DE TEMPS
C       IN RHO    : VALEUR DU PAS DE RECHERCHE LINEAIRE
C       IN DEPDEL : INCREMENT DE DEPLACEMENT
C       IN DEPOLD : INCREMENT DE DEPLACEMENT PAS PRECEDENT ('LONG_ARC')
C       IN MODELE : MODELE               ('DEFORMATION', 'PRED_ELAS_*')
C       IN MATE   : MATERIAU             ('DEFORMATION', 'PRED_ELAS_*')
C       IN COMPOR : COMPORTEMENT         ('DEFORMATION', 'PRED_ELAS_*')
C       IN VALMOI : ETAT EN T-           ('DEFORMATION', 'PRED_ELAS_*')
C IN/JXOUT DDEPLA : CORRECTION DE DEPLACEMENT
C       IN NBATTE : NOMBRE DE SOLUTIONS ATTENDUES
C      OUT NBEFFE : NOMBRE DE SOLUTIONS EFFECTIVES
C      OUT ETA    : ETA_PILOTAGE
C      OUT LICCVG : CODE DE CONVERGENCE POUR LE PILOTAGE
C                     - 1 : BORNE ATTEINTE -> FIN DU CALCUL
C                       0 : RAS
C                       1 : PILOTAGE NON CONVERGE
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ---------------------------
C
      CHARACTER*32       JEXNUM , JEXNOM , JEXATR
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
C
C ---------- FIN  DECLARATIONS  NORMALISEES  JEVEUX -------------------

      LOGICAL      BORMIN, BORMAX
      INTEGER      JPLTK, JPLIR, IBID, IER
      INTEGER      NEQ, N, JU0, JU1, I, JDEPM
      INTEGER      JDEPDE, JLINE, JDEP0, JDEP1, JDDEPL, JDUREF, JCOEF
      REAL*8       DTAU, ETAMAX, ETAMIN, UM, DU, RN, RD, BORNE(2)
      REAL*8       R8DOT, R8VIDE
      CHARACTER*8  K8BID
      CHARACTER*16 TYPILO
      CHARACTER*19 CNDEP0, CNDEP1, PROFCH, LIGRPI, CARTYP, CARETA
      CHARACTER*24 DEPMOI, K24BID
C ----------------------------------------------------------------------


      CALL JEMARQ()


C    TYPE DE PILOTAGE
      CALL JEVEUO (PILOTE // '.PLTK','L',JPLTK)
      TYPILO = ZK24(JPLTK)

C    INCREMENT DELTA TAU
      CALL JEVEUO(PILOTE // '.PLIR','L',JPLIR)
      DTAU = DINST / ZR(JPLIR)

C    BORNES POUR ETA
      ETAMAX = ZR(JPLIR+1)
      ETAMIN = ZR(JPLIR+2)
      BORNE(1)=ETAMAX
      BORNE(2)=ETAMIN
      BORMAX = ETAMAX .NE. R8VIDE()
      BORMIN = ETAMIN .NE. R8VIDE()

C   INCREMENTS DE DEPLACEMENT RHO.DU0 ET RHO.DU1
      CNDEP0 = '&&CNPART.CHP1'
      CNDEP1 = '&&CNPART.CHP2'
      CALL JEVEUO(PILOTE // '.PL0R.VALE','L',JU0)
      CALL JEVEUO(CNDEP0 //      '.VALE','E',JDEP0)
      CALL JEVEUO(PILOTE // '.PL1R.VALE','L',JU1)
      CALL JEVEUO(CNDEP1 //      '.VALE','E',JDEP1)
      CALL JELIRA(PILOTE // '.PL0R.VALE', 'LONMAX', NEQ, K8BID)
      DO 10 N = 0, NEQ-1
        ZR(JDEP0+N) = RHO * ZR(JU0+N)
        ZR(JDEP1+N) =       ZR(JU1+N)
 10   CONTINUE



C ======================================================================
C                       PILOTAGE PAR UN DDL IMPOSE
C ======================================================================

      IF (TYPILO.EQ.'DDL_IMPO') THEN

        CALL JEVEUO (DEPDEL(1:19) // '.VALE','L',JDEPDE)
        CALL JEVEUO (PILOTE //'.PLCR.VALE','L',JLINE)
        RN = R8DOT(NEQ,ZR(JDEP0) ,1,ZR(JLINE),1)
        RD = R8DOT(NEQ,ZR(JDEP1) ,1,ZR(JLINE),1)
        DU = R8DOT(NEQ,ZR(JDEPDE),1,ZR(JLINE),1)
        IF (RD.EQ.0.D0) CALL UTMESS ('F','NMPILO','DENOMINATEUR NUL '
     &      // 'DANS LE CALCUL DE ETA_PILOTAGE')
        ETA(1) = (DTAU - DU - RN) / RD
        NBEFFE = 1
        LICCVG(1)=0

C ======================================================================
C            PILOTAGE POUR ANALYSE LIMITE : TRAVAIL UNITAIRE
C ======================================================================

      ELSE IF (TYPILO.EQ.'ANA_LIM') THEN

        CALL DESAGG(VALMOI, DEPMOI, K24BID, K24BID, K24BID,
     &                      K24BID, K24BID, K24BID, K24BID)

        CALL JEVEUO (DEPMOI(1:19) // '.VALE','L',JDEPM)
        CALL JEVEUO (DEPDEL(1:19) // '.VALE','L',JDEPDE)
        K24BID = '&&OP0070.CNFEPI'
        CALL JEVEUO (K24BID(1:19) // '.VALE','L',JLINE)
        UM = R8DOT(NEQ,ZR(JDEPM) ,1,ZR(JLINE),1)
        DU = R8DOT(NEQ,ZR(JDEPDE),1,ZR(JLINE),1)
        RN = R8DOT(NEQ,ZR(JDEP0) ,1,ZR(JLINE),1)
        RD = R8DOT(NEQ,ZR(JDEP1) ,1,ZR(JLINE),1)
        IF (RD.EQ.0.D0) CALL UTMESS ('F','NMPILO','DENOMINATEUR NUL '
     &      // 'DANS LE CALCUL DE ETA_PILOTAGE')
        ETA(1) = (1 - UM - DU - RN) / RD
        NBEFFE = 1
        LICCVG(1)=0

C ======================================================================
C                       PILOTAGE PAR LONGUEUR D'ARC
C ======================================================================

      ELSE IF (TYPILO.EQ.'LONG_ARC') THEN

        CALL JEVEUO(DEPOLD(1:19) // '.VALE','L',JDUREF)
        CALL JEVEUO(DEPDEL(1:19) // '.VALE','L',JDEPDE)
        CALL JEVEUO(PILOTE // '.PLCR.VALE','L',JCOEF)
        CALL NMPILA(NEQ, ZR(JDEPDE), ZR(JDEP0), ZR(JDEP1), ZR(JCOEF),
     &              DTAU, ZR(JDUREF), NBATTE, NBEFFE, ETA, LICCVG)



C ======================================================================
C                         PILOTAGE PAR CRITERE
C ======================================================================

      ELSE IF (TYPILO.EQ.'PRED_ELAS'
     &    .OR. TYPILO.EQ.'DEFORMATION') THEN

        CALL DISMOI('F','PROF_CHNO',DDEPLA,'CHAM_NO',IBID, PROFCH,IER)
        LIGRPI = ZK24(JPLTK+1)
        CARTYP = ZK24(JPLTK+2)
        CARETA = ZK24(JPLTK+3)

        CALL NMPIPE(MODELE, LIGRPI, CARTYP, CARETA, MATE  , COMPOR,
     &              VALMOI, DEPDEL, CNDEP0, CNDEP1, PROFCH, DTAU  ,
     &              NBATTE, NBEFFE, ETA   , LICCVG, BORNE, TYPILO)

      END IF



C ======================================================================
C                   CALCUL DE L'INCREMENT DE DEPLACEMENT
C ======================================================================


      DO 20 I = 1, NBEFFE


C -- TEST PAR RAPPORT A LA VALEUR MAXI

        IF (BORMAX) THEN
          IF (ETA(I) .GT. ETAMAX) THEN
            ETA(I)    = ETAMAX
            LICCVG(I) = -1
          END IF
        END IF


C -- TEST PAR RAPPORT A LA VALEUR MINI

        IF (BORMIN) THEN
          IF (ETA(I) .LT. ETAMIN) THEN
            ETA(I)    = ETAMIN
            LICCVG(I) = -1
          END IF
        END IF

 20   CONTINUE


C -- CALCUL DU DEPLACEMENT
      DO 30 I = 1, NBEFFE

        CALL JEVEUO(DDEPLA(I)(1:19) // '.VALE','E',JDDEPL)
        CALL R8COPY(NEQ, ZR(JDEP0),1, ZR(JDDEPL),1)
        CALL R8AXPY(NEQ, ETA(I), ZR(JDEP1),1, ZR(JDDEPL),1)

 30   CONTINUE
      CALL JEDEMA()
      END
