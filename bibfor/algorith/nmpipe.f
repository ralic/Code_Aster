       SUBROUTINE NMPIPE(MODELE,LIGRPI,CARTYP,CARETA,
     &                  MATE  ,COMPOR,RESOCO,VALINC,DEPDEL,
     &                  DDEPL0,DDEPL1,TAU   ,NBEFFE,ETA   ,
     &                  PILCVG,TYPPIL,CARELE)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C RESPONSABLE MABBAS M.ABBAS
C
      IMPLICIT NONE
      INTEGER      PILCVG,NBEFFE
      REAL*8       TAU, ETA(2)
      CHARACTER*24 TYPPIL
      CHARACTER*19 DDEPL0,DDEPL1
      CHARACTER*19 LIGRPI,CARTYP,CARETA
      CHARACTER*24 MODELE,MATE  ,COMPOR,CARELE
      CHARACTER*19 DEPDEL,VALINC(*)
      CHARACTER*24  RESOCO
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME - PILOTAGE)
C
C RESOLUTION DE L'EQUATION DE PILOTAGE PAR PREDICTION ELASTIQUE OU
C DEFORMATION
C
C ----------------------------------------------------------------------
C
C
C IN  MODELE : MODELE
C IN  LIGRPI : LIGREL DES MAILLES CONTROLEES PAR LE PILOTAGE
C IN  CARTYP : CARTE CONTENANT LE TYPE DE PILOTAGE
C IN  MATE   : MATERIAU
C IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
C IN  COMPOR : COMPORTEMENT
C IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C IN  DEPDEL : INCREMENT DE DEPLACEMENT
C IN  DDEPL0 : VARIATION DE DEPLACEMENT K-1.F0
C IN  DDEPL1 : VARIATION DE DEPLACEMENT K-1.F1
C IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
C IN  TAU    : SECOND MEMBRE DE L'EQUATION DE PILOTAGE
C IN  TYPPIL : TYPE PILOTAGE : PRED_ELAS OU DEFORMATION
C OUT NBEFFE : NOMBRE DE SOLUTIONS EFFECTIVES
C OUT ETA    : ETA_PILOTAGE
C OUT PILCVG : VAUT 1 S'IL N'Y A PAS DE SOLUTION, 0 SINON
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
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
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER      NBOUT,NBIN
      PARAMETER    (NBOUT=1, NBIN=23)
      CHARACTER*8  LPAOUT(NBOUT),LPAIN(NBIN)
      CHARACTER*19 LCHOUT(NBOUT),LCHIN(NBIN)
C
      LOGICAL      LBID
      INTEGER      NBMA, NBPT, ICMP, MA, PT, NPG,NBGMAX
      INTEGER      JCESD, JCESL, JCESV, JA0A1, JA0, JA1, JA2, JA3,JTRAV
      INTEGER      IRET, IBID, JA4
      REAL*8       R8VIDE,RESULT
      COMPLEX*16   CBID
      CHARACTER*8  K8BID,CPAR
      CHARACTER*19 COPILO, COPILS,CTAU
      CHARACTER*24 A0A1, TRAV
      CHARACTER*19 CHGEOM
      CHARACTER*19 DEPMOI,SIGMOI,VARMOI,COMMOI
      CHARACTER*16 OPTION
      INTEGER      IFMDBG,NIVDBG
      LOGICAL      DEBUG
      CHARACTER*19 XDONCO,XINDCO,LNNO,LTNO,PINTER,AINTER,CFACE
      CHARACTER*19 FACLON,BASECO,XCOHES,DEPPLU
      LOGICAL      LCONTX
      INTEGER      IER
C
      DATA COPILO, COPILS  /'&&NMPIPE.COPILO','&&NMPIPE.COPILS'/
      DATA CTAU            /'&&NMPIPE.CTAU'/
      DATA A0A1, TRAV      /'&&NMPIPE.A0A1', '&&NMPIPE.TRAV'/
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFDBG('PRE_CALCUL',IFMDBG,NIVDBG)

C    --------------------------------------------------------
C    MODELE X-FEM
C    --------------------------------------------------------
      CALL JEEXIN(MODELE(1:8)//'.XFEM_CONT',IER)
      IF (IER.EQ.0) THEN
        LCONTX = .FALSE.
      ELSE
        LCONTX = .TRUE.
      ENDIF
C
C --- INITIALISATIONS
C
      IF (TYPPIL.EQ.'PRED_ELAS') THEN
        IF (LCONTX)  THEN
          OPTION = 'PILO_PRED_XLAS'
        ELSE
          OPTION = 'PILO_PRED_ELAS'
        ENDIF
      ELSEIF (TYPPIL.EQ.'DEFORMATION') THEN
        OPTION = 'PILO_PRED_DEFO'
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
      PILCVG = 0
      IF (NIVDBG.GE.2) THEN
        DEBUG  = .TRUE.
      ELSE
        DEBUG  = .FALSE.
      ENDIF
C
C --- RECUPERATION DES DONNEES XFEM
C
      XINDCO = RESOCO(1:14)//'.XFIP'
      XDONCO = RESOCO(1:14)//'.XFDO'
      XCOHES = RESOCO(1:14)//'.XCOH'
      LNNO   = MODELE(1:8)//'.LNNO'
      LTNO   = MODELE(1:8)//'.LTNO'
      PINTER = MODELE(1:8)//'.TOPOFAC.OE'
      AINTER = MODELE(1:8)//'.TOPOFAC.AI'
      CFACE  = MODELE(1:8)//'.TOPOFAC.CF'
      FACLON = MODELE(1:8)//'.TOPOFAC.LO'
      BASECO = MODELE(1:8)//'.TOPOFAC.BA'
C
C
C
C --- INITIALISATION DES CHAMPS POUR CALCUL
C
      CALL INICAL(NBIN  ,LPAIN ,LCHIN ,
     &            NBOUT ,LPAOUT,LCHOUT)
C
C --- DECOMPACTION VARIABLES CHAPEAUX
C
      CALL NMCHEX(VALINC,'VALINC','DEPMOI',DEPMOI)
      CALL NMCHEX(VALINC,'VALINC','SIGMOI',SIGMOI)
      CALL NMCHEX(VALINC,'VALINC','VARMOI',VARMOI)
      CALL NMCHEX(VALINC,'VALINC','COMMOI',COMMOI)
      CALL NMCHEX(VALINC,'VALINC','DEPPLU',DEPPLU)
C
      CALL SDMPIC('CHAM_ELEM',SIGMOI)
      CALL SDMPIC('CHAM_ELEM',VARMOI)
C
C --- CHAMP DE GEOMETRIE
C
      CALL MEGEOM(MODELE,' ',LBID  ,CHGEOM)
C
C --- ALLOCATION DE LA CARTE RESULTAT
C
      CALL DETRSD('CARTE',CTAU)
      CPAR  = 'A0'
      CALL MECACT('V',CTAU,'LIGREL',LIGRPI,'PILO_R', 1, CPAR,
     &            IBID, TAU, CBID, K8BID)
C
C --- REMPLISSAGE DES CHAMPS D'ENTREE
C
      LPAIN(1) = 'PGEOMER'
      LCHIN(1) =  CHGEOM
      LPAIN(2) = 'PMATERC'
      LCHIN(2) =  MATE(1:19)
      LPAIN(3) = 'PCOMPOR'
      LCHIN(3) =  COMPOR(1:19)
      LPAIN(4) = 'PDEPLMR'
      LCHIN(4) =  DEPMOI
      LPAIN(5) = 'PCONTMR'
      LCHIN(5) =  SIGMOI
      LPAIN(6) = 'PVARIMR'
      LCHIN(6) =  VARMOI
      LPAIN(7) = 'PDDEPLR'
      LCHIN(7) =  DEPDEL
      LPAIN(8) = 'PDEPL0R'
      LCHIN(8) =  DDEPL0
      LPAIN(9) = 'PDEPL1R'
      LCHIN(9) =  DDEPL1
      LPAIN(10)= 'PTYPEPI'
      LCHIN(10)=  CARTYP
      LPAIN(11)= 'PBORNPI'
      LCHIN(11)=  CARETA
      LPAIN(12)= 'PCDTAU'
      LCHIN(12)=  CTAU
      LPAIN(13)= 'PCAMASS'
      LCHIN(13)=  CARELE(1:8)//'.CARMASSI'
      LPAIN(14)= 'PINDCOI'
      LCHIN(14)=  XINDCO
      LPAIN(15)= 'PDONCO'
      LCHIN(15)=  XDONCO
      LPAIN(16) = 'PLSN'
      LCHIN(16) = LNNO
      LPAIN(17) = 'PLST'
      LCHIN(17) = LTNO
      LPAIN(18) = 'PPINTER'
      LCHIN(18) = PINTER
      LPAIN(19) = 'PAINTER'
      LCHIN(19) = AINTER
      LPAIN(20) = 'PCFACE'
      LCHIN(20) = CFACE
      LPAIN(21) = 'PLONCHA'
      LCHIN(21) = FACLON
      LPAIN(22) = 'PBASECO'
      LCHIN(22) = BASECO
      LPAIN(23) = 'PCOHES'
      LCHIN(23) = XCOHES(1:19)
C
C --- REMPLISSAGE DU CHAMP DE SORTIE
C
      LPAOUT(1) = 'PCOPILO'
      LCHOUT(1) =  COPILO
C
C --- CALCUL DE L'OPTION
C
      CALL CALCUL('S',OPTION,LIGRPI,NBIN ,LCHIN ,LPAIN ,
     &                     NBOUT,LCHOUT,LPAOUT,'V','OUI')
C
      IF (DEBUG) THEN
        CALL DBGCAL(OPTION,IFMDBG,
     &              NBIN  ,LPAIN ,LCHIN ,
     &              NBOUT ,LPAOUT,LCHOUT)
      ENDIF
C
C --- EN ATTENDANT DE FAIRE MIEUX, POUR PERMETTRE MUMPS/DISTRIBUE :
C
      CALL SDMPIC('CHAM_ELEM',COPILO)
C
C --- TRANSFORMATION EN CHAM_ELEM_S
C
      CALL CELCES(COPILO,'V',COPILS)
      CALL JEVEUO(COPILS//'.CESD','L',JCESD)
      CALL JEVEUO(COPILS//'.CESL','L',JCESL)
      CALL JEVEUO(COPILS//'.CESV','L',JCESV)
      NBMA   = ZI(JCESD-1 + 1)
      NBPT   = ZI(JCESD-1 + 3)
      NBGMAX = NBMA*NBPT
C
C --- ESPACE MEMOIRE POUR LE TABLEAU A0,A1
C
      CALL JEEXIN(A0A1,IRET)
      IF (IRET .EQ. 0) THEN
        CALL WKVECT(A0A1,'V V R',4*NBGMAX    ,JA0A1)
        CALL WKVECT(TRAV,'V V I',4*(NBGMAX+1),JTRAV)
      ELSE
        CALL JEVEUO(A0A1,'E',JA0A1)
        CALL JEVEUO(TRAV,'E',JTRAV)
      END IF
C
C --- LECTURE DES COMPOSANTES DU CHAM_ELEM_S
C
      ICMP = 0
      DO 100 MA = 1,NBMA
        DO 200 PT = 1,NBPT
          CALL CESEXI('C',JCESD,JCESL,MA,PT,1,1,JA0)
          CALL CESEXI('C',JCESD,JCESL,MA,PT,1,2,JA1)
          CALL CESEXI('C',JCESD,JCESL,MA,PT,1,3,JA2)
          CALL CESEXI('C',JCESD,JCESL,MA,PT,1,4,JA3)
          CALL CESEXI('C',JCESD,JCESL,MA,PT,1,5,JA4)



          IF (LCONTX)  THEN
C - XFEM : SI PAS DE SOL AU PT DE GAUSS, ON N AJOUTE PAS DE DROITE
             RESULT = ABS(ZR(JCESV-1+JA0))+ABS(ZR(JCESV-1+JA1))+
     &             ABS(ZR(JCESV-1+JA2))+ABS(ZR(JCESV-1+JA3))
             IF (RESULT.EQ.0) THEN
               GOTO 200
             ENDIF
          ENDIF
C
C ---     LECTURE DU CODE RETOUR
C
          IF (JA4.NE.0) THEN
            IF (ZR(JCESV-1 + JA4).NE.R8VIDE()) THEN
C ---         A T ON REMPLI CODE-RETOUR ? OUI -> PAS DE SOLUTION
              PILCVG = 1
              GOTO 9999
            ENDIF
          ENDIF
C
C ---     COEFFICIENTS DE LA OU DES DROITES
C
          IF (JA0.NE.0) THEN
            ZR(JA0A1 + ICMP    ) = ZR(JCESV-1 + JA0)
            ZR(JA0A1 + ICMP + 1) = ZR(JCESV-1 + JA1)
            ICMP = ICMP+2
            IF (ZR(JCESV-1 + JA2).NE.R8VIDE()) THEN
              ZR(JA0A1 + ICMP )    = ZR(JCESV-1 + JA2)
              ZR(JA0A1 + ICMP + 1) = ZR(JCESV-1 + JA3)
              ICMP = ICMP+2
            ENDIF
          END IF
 200    CONTINUE
 100  CONTINUE
C
      NPG = ICMP / 2
C
C --- RESOLUTION DE L'EQUATION DE PILOTAGE P(U(ETA)) = TAU
C
      CALL PIPERE(NPG,ZR(JA0A1),TAU,NBEFFE,ETA)

      IF (NBEFFE .EQ. 0) THEN
        PILCVG = 1
      END IF
C
 9999 CONTINUE
C
      CALL JEDEMA()
      END
