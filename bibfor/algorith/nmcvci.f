      SUBROUTINE NMCVCI(CHARGE,INFOCH,FOMULT,NUMEDD,DEPMOI,
     &           INSTAP,CNCINE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 28/01/2005   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE VABHHTS J.PELLET
      IMPLICIT NONE


C BUT : CALCULER LE CHAM_NO CNCINE QUI CONTIENT  L'INCREMENT DE
C       DEPLACEMENT IMPOSE PAR LES CHARGES CINEMATIQUES.
C       POUR CELA, ON FAIT LA DIFFERENCE ENTRE LES INSTANTS "+" ET "-"
C       MAIS POUR L'INSTANT "-", IL FAUT PARTIR DU "VRAI" CHAMP
C       DE DEPLACEMENT.
C----------------------------------------------------------------------
      CHARACTER*24 CHARGE, INFOCH, FOMULT,NUMEDD,DEPMOI,CNCINE
      CHARACTER*24 L2CNCI(2),CNCINM,CNCINP
      REAL*8 INSTAP, COEFR(2)
      INTEGER JDLCI,NEQ,IEQ,NEQ2,JCNCIM,IRET,J1,IDINFO,ICHAR
      CHARACTER*1 KBID ,TYPCH(2)
      LOGICAL LVCINE
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
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
C----------------------------------------------------------------------

      CALL JEMARQ()

C     -- CREATION DE CNCINE = 0. PARTOUT :
C     --------------------------------------
      CALL EXISD('CHAMP_GD',CNCINE,IRET)
      IF (IRET.EQ.0) CALL VTCREB (CNCINE,NUMEDD,'V','R',NEQ)
      CALL JELIRA(CNCINE(1:19)//'.VALE','LONMAX',NEQ,KBID)
      CALL JELIRA(DEPMOI(1:19)//'.VALE','LONMAX',NEQ2,KBID)
      CALL ASSERT(NEQ.EQ.NEQ2)
      CALL JEVEUO(CNCINE(1:19)//'.VALE','E',J1)
      DO 2, IEQ=1,NEQ
         ZR(J1-1+IEQ)=0.D0
2     CONTINUE


C     -- Y-A-T-IL DES CHARGES CINEMATIQUES ?
C     -----------------------------------------------------------------
      CALL JEVEUO(INFOCH,'L',IDINFO)
      LVCINE=.FALSE.
      DO 10 ICHAR = 1,ZI(IDINFO)
        IF (ZI(IDINFO+ICHAR).LT.0) LVCINE=.TRUE.
   10 CONTINUE

C     -- S'IL N'Y A PAS DE CHARGES CINEMATIQUES, IL N'Y A RIEN A FAIRE:
C     -----------------------------------------------------------------
      WRITE(6,*) 'AJACOT DEBUT NMVCI LVCINE=',LVCINE
      IF (.NOT.LVCINE) GO TO 9999


C     -- S'IL Y A DES CHARGES CINEMATIQUES :
C     -----------------------------------------------------------------
      CNCINM='&&NMCHAR.CNCIMM'
      CNCINP='&&NMCHAR.CNCIMP'


C     CALCUL DE UIMP+ :
C     ---------------------
      CALL ASCAVC(CHARGE,INFOCH,FOMULT,NUMEDD,INSTAP,CNCINP)
      CALL JEVEUO(CNCINP(1:19)//'.DLCI','L',JDLCI)


C     CALCUL DE UIMP- : C'EST U- LA OU ON IMPOSE LE DEPLACEMENT
C                       ET 0. AILLEURS
C     ---------------------------------------------------------
      CALL COPISD('CHAMP_GD','V',DEPMOI,CNCINM)
      CALL JEVEUO(CNCINM(1:19)//'.VALE','E',JCNCIM)
      DO 1, IEQ=1,NEQ
           IF (ZI(JDLCI-1+IEQ).EQ.0) THEN
             ZR(JCNCIM-1+IEQ)=0.D0
           END IF
1     CONTINUE

C     DIFFERENCE UIMP+ - UIMP- :
C     ---------------------------
      COEFR(1)=-1.D0
      COEFR(2)=+1.D0
      L2CNCI(1)=CNCINM
      L2CNCI(2)=CNCINP
      TYPCH(1)='R'
      TYPCH(2)='R'
      CALL VTCMBL(2,TYPCH,COEFR,TYPCH,L2CNCI,TYPCH,CNCINE)

C     MENAGE :
C     ---------
      CALL IMPRSD('CHAMP',CNCINE,8,'AJACOT CNCINE DANS NMCVCI')
      CALL DETRSD('CHAM_NO',CNCINM)
      CALL DETRSD('CHAM_NO',CNCINP)

9999  CONTINUE
      CALL JEDEMA()

      END
