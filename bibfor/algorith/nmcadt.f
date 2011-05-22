      SUBROUTINE NMCADT(SDDISC,IADAPT,NUMINS,VALINC,DTP)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 24/05/2011   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE GENIAUT S.GENIAUT
C
      IMPLICIT NONE
      INTEGER      IADAPT,NUMINS
      CHARACTER*19 VALINC(*)
      CHARACTER*19 SDDISC   
      REAL*8       DTP  
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME)
C
C CALCUL DU NOUVEAU PAS DE TEMPS EN CAS D'ADAPTATIONS
C      
C ----------------------------------------------------------------------
C
C
C IN  SDDISC : SD DISCRETISATION TEMPORELLE
C IN  IADAPT : NUMERO DE LA METHODE D ADAPTATION TRAITEE
C IN  NUMINS : NUMERO D'INSTANT
C IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
C OUT DTP    : NOUVEAU PAS DE TEMPS (DT+)
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C
      INTEGER ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C 
      INTEGER     IB,NIT,JITER,NBITER
      REAL*8      R8B,DTM,PCENT,VALREF,DVAL,R8MAEM
      CHARACTER*8 K8B,TYPEXT
      CHARACTER*16 MODETP,NOCHAM,NOCMP
      REAL*8 ETA,ETAD
C 
C ----------------------------------------------------------------------
C


      CALL JEMARQ()

C     METHODE DE CALCUL DE DT+
      CALL UTDIDT('L',SDDISC,'ADAP',IADAPT,'METHODE',R8B,IB,MODETP)

C     PAS DE TEMPS PAR DEFAUT (LE DERNIER, SAUF SI JALON)
      CALL UTDIDT('L',SDDISC,'LIST',IB,'DT-',DTM,IB,K8B)

C     ------------------------------------------------------------------
      IF (MODETP.EQ.'FIXE') THEN
C     ------------------------------------------------------------------

        CALL UTDIDT('L',SDDISC,'ADAP',IADAPT,'PCENT_AUGM',PCENT,IB,K8B)
        DTP = DTM * (1.D0 + PCENT / 100.D0)

C     ------------------------------------------------------------------
      ELSEIF (MODETP.EQ.'DELTA_GRANDEUR') THEN
C     ------------------------------------------------------------------

        CALL UTDIDT('L',SDDISC,'ADAP',IADAPT,'NOM_CHAM',R8B,IB,NOCHAM)
        CALL UTDIDT('L',SDDISC,'ADAP',IADAPT,'NOM_CMP',R8B,IB,NOCMP)
        CALL UTDIDT('L',SDDISC,'ADAP',IADAPT,'VALE_REF',VALREF,IB,K8B)
        TYPEXT = 'MAX_ABS'
C       CALCUL DE C = MIN (VREF / |DELTA(CHAMP+CMP)| )
C                   = VREF / MAX ( |DELTA(CHAMP+CMP)| )

C       DVAL :MAX EN VALEUR ABSOLUE DU DELTA(CHAMP+CMP)
        CALL EXTDCH(TYPEXT,VALINC,NOCHAM,NOCMP,DVAL)

C       LE CHAMP DE VARIATION EST IDENTIQUEMENT NUL : ON SORT
        IF (DVAL.EQ.0.D0) GOTO 9999

        DTP = DTM * VALREF/DVAL

C     ------------------------------------------------------------------
      ELSEIF (MODETP.EQ.'ITER_NEWTON') THEN
C     ------------------------------------------------------------------

        CALL UTDIDT('L',SDDISC,'ADAP',IADAPT,'NB_ITER_NEWTON_REF',
     &                                                   R8B,NIT,K8B)
        CALL JEVEUO(SDDISC//'.ITER','L',JITER)
        NBITER = ZI(JITER-1+NUMINS)
        DTP = DTM * SQRT( DBLE(NIT) / DBLE(NBITER+1) )

C     ------------------------------------------------------------------
      ELSEIF (MODETP.EQ.'FORMULE') THEN
C     ------------------------------------------------------------------

        CALL ASSERT(.FALSE.)

C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C     ------------------------------------------------------------------
      ELSEIF (MODETP.EQ.'IMPLEX') THEN
C     ------------------------------------------------------------------

C       FACTEUR D'ACCELERATION ETA
        ETA = 1.2D0
C       FACTEUR DE DECELERATION ETAD
        ETAD = 0.5D0
                        
C        CALL UTDIDT('L',SDDISC,'ADAP',IADAPT,'NU_CMP',R8B,NUCMP,K8B)
        TYPEXT='MIN_VAR'

C       CALCUL DE C = MIN (VREF / |DELTA(CHAMP+CMP)| )
C                   = VREF / MAX ( |DELTA(CHAMP+CMP)| )

C       DVAL :MAX EN VALEUR ABSOLUE DU DELTA(CHAMP+CMP)
        NOCHAM='VARI_ELGA'
        NOCMP='V1'
        CALL EXTDCH(TYPEXT,VALINC,NOCHAM,NOCMP,DVAL)
        
C       LE CHAMP DE VARIATION EST IDENTIQUEMENT NUL : ON SORT
C        IF (DVAL.GE.R8MAEM()) GOTO 9999
        IF (DVAL.GE.R8MAEM()) DVAL=ETA
        DTP = DTM * DVAL
        
C       ON IMPOSE QUE LE DT SOIT COMPRIS ENTRE ETAD*DTM ET ETA*DTM
        IF(DTP/DTM.GE.ETA) THEN
          DTP = ETA * DTM
        ELSEIF (DTP/DTM.LE.ETAD) THEN
           DTP = ETAD * DTM
        ENDIF
                
        

C     ------------------------------------------------------------------
C      ELSEIF (MODETP.EQ.'IMPLEX2') THEN
C     ------------------------------------------------------------------
        
C        CALL UTDIDT('L',SDDISC,'ADAP',IADAPT,'NU_CMP',R8B,NUCMP,K8B)
        
C     ------------------------------------------------------------------
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF

 9999 CONTINUE
      CALL JEDEMA()
      END
