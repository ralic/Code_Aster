      SUBROUTINE NMEVDG(SDDISC,VALE  ,IECHEC,IEVDAC)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/09/2011   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT     NONE
      INTEGER      IECHEC,IEVDAC
      CHARACTER*19 SDDISC,VALE(*)
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME - EVENEMENTS)
C
C GESTION DE L'EVENEMENT DELTA_GRANDEUR
C
C ----------------------------------------------------------------------
C
C
C IN  SDDISC : SD DISCRETISATION TEMPORELLE
C IN  VALE   : INCREMENTS DES VARIABLES
C               OP0070: VARIABLE CHAPEAU
C               OP0033: TABLE
C IN  IECHEC : OCCURRENCE DE L'ECHEC
C OUT IEVDAC : VAUT IECHEC SI EVENEMENT DECLENCHE
C                   0 SINON
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
      INTEGER      IFM,NIV
      REAL*8       R8BID,VALREF,DVAL
      INTEGER      IBID
      CHARACTER*8  K8BID,CRIT,TYPEXT
      COMPLEX*16   C16BID
      CHARACTER*16 NOCHAM,NOCMP
      PARAMETER   (TYPEXT = 'MAX_ABS')
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFDBG('MECA_NON_LINE',IFM,NIV)
C
C --- AFFICHAGE
C
      IF (NIV.GE.2) THEN
        WRITE (IFM,*) '<MECANONLINE> ... DELTA_GRANDEUR'
      ENDIF
C
C --- INITIALISATIONS
C
      IEVDAC = 0
C
C --- PARAMETRES
C
      CALL UTDIDT('L'   ,SDDISC,'ECHE',IECHEC,'NOM_CHAM',
     &            R8BID ,IBID  ,NOCHAM)
      CALL UTDIDT('L'   ,SDDISC,'ECHE',IECHEC,'NOM_CMP',
     &            R8BID ,IBID  ,NOCMP )
      CALL UTDIDT('L'   ,SDDISC,'ECHE',IECHEC,'VALE_REF',
     &            VALREF,IBID  ,K8BID )
      CALL UTDIDT('L'   ,SDDISC,'ECHE',IECHEC,'CRIT_COMP',
     &            R8BID ,IBID  ,CRIT  )
C
C --- DVAL :MAX EN VALEUR ABSOLUE DU DELTA(CHAMP+CMP)
C
      IF (VALE(1)(1:8).EQ.'&&OP0033') THEN
        CALL TBACCE(VALE(1)(1:16),1     ,NOCMP ,'L',IBID,
     &              DVAL  ,C16BID,K8BID )
      ELSE
        CALL EXTDCH(TYPEXT,VALE,NOCHAM,NOCMP,DVAL)
      ENDIF
C
      CALL ASSERT(CRIT.EQ.'GT')
      
      IF (DVAL.GT.VALREF) THEN
        IEVDAC = IECHEC
      ENDIF
C
      CALL JEDEMA()
      END
