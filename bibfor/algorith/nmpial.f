      SUBROUTINE NMPIAL(NUMEDD,DEPDEL,DEPMOI,CNFEPI,DDEPL0,
     &                  DDEPL1,ETA   ,PILCVG,NBEFFE)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 02/04/2012   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      INTEGER       NBEFFE,PILCVG
      REAL*8        ETA
      CHARACTER*19  DDEPL0,DDEPL1,DEPDEL,CNFEPI
      CHARACTER*19  DEPMOI
      CHARACTER*24  NUMEDD
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME - PILOTAGE - CALCUL DE ETA)
C
C RESOLUTION DE L'EQUATION DE PILOTAGE POUR ANALYSE LIMITE
C
C ----------------------------------------------------------------------
C
C
C IN  NUMEDD : NUME_DDL
C IN  DEPMOI : DEPLACEMENT AU DEBUT DU PAS DE TEMPS
C IN  DEPDEL : INCREMENT DE DEPLACEMENT DEPUIS DEBUT PAS DE TEMPS
C IN  DDEPL0 : INCREMENT DE DEPLACEMENT K-1.F_DONNE
C IN  DDEPL1 : INCREMENT DE DEPLACEMENT K-1.F_PILO
C IN  CNFEPI : VECT_ASSE FORCES PILOTEES
C OUT NBEFFE : NOMBRE DE SOLUTIONS EFFECTIVES
C OUT ETA    : ETA_PILOTAGE
C OUT PILCVG : CODE DE CONVERGENCE POUR LE PILOTAGE
C                -1 : PAS DE CALCUL DU PILOTAGE
C                 0 : CAS DU FONCTIONNEMENT NORMAL
C                 1 : PAS DE SOLUTION
C                 2 : BORNE ATTEINTE -> FIN DU CALCUL
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ---------------------------
C
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
C
      REAL*8       DU, RN, RD,UM
      REAL*8       DDOT
      INTEGER      JLINE,JDEPDE,JDEPM,JDEP0,JDEP1
      INTEGER      NEQ,IRET
      CHARACTER*8  K8BID
      INTEGER      IFM,NIV
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFDBG('PILOTAGE',IFM,NIV)
C
C --- AFFICHAGE
C
      IF (NIV.GE.2) THEN
        WRITE (IFM,*) '<PILOTAGE> ...... PILOTAGE PAR TRAVAIL UNITAIRE'
      ENDIF
C
C --- INITIALISATIONS
C
      PILCVG = -1
      CALL DISMOI('F','NB_EQUA',NUMEDD,'NUME_DDL',NEQ,K8BID,IRET)
C
C --- ACCES OBJETS JEVEUX
C
      CALL JEVEUO(DDEPL0(1:19)//'.VALE','L',JDEP0)
      CALL JEVEUO(DDEPL1(1:19)//'.VALE','L',JDEP1)
      CALL JEVEUO(DEPMOI(1:19)//'.VALE','L',JDEPM)
      CALL JEVEUO(DEPDEL(1:19)//'.VALE','L',JDEPDE)
      CALL JEVEUO(CNFEPI(1:19)//'.VALE','L',JLINE)
C
C --- RESOLUTION DE L'EQUATION
C
      UM = DDOT(NEQ,ZR(JDEPM) ,1,ZR(JLINE),1)
      DU = DDOT(NEQ,ZR(JDEPDE),1,ZR(JLINE),1)
      RN = DDOT(NEQ,ZR(JDEP0) ,1,ZR(JLINE),1)
      RD = DDOT(NEQ,ZR(JDEP1) ,1,ZR(JLINE),1)
      IF (RD.EQ.0.D0) THEN
        PILCVG = 1
      ELSE
        ETA    = (1.D0 - UM - DU - RN) / RD
        NBEFFE = 1
        PILCVG = 0
      ENDIF
C
C --- AFFICHAGE
C
      IF (NIV.GE.2) THEN
        WRITE (IFM,*) '<PILOTAGE> ...... (RN,RD,DU,UM) : ',RN,RD,DU,UM
      ENDIF
C
      CALL JEDEMA()
C
      END
