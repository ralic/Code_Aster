      SUBROUTINE NMCADT(SDDISC,IADAPT,NUMINS,VALINC,DTP)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C RESPONSABLE GENIAUT S.GENIAUT
C
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER      IADAPT,NUMINS
      CHARACTER*19 VALINC(*)
      CHARACTER*19 SDDISC   
      REAL*8       DTP  
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME)
C
C CALCUL DU NOUVEAU PAS DE TEMPS EN CAS D'ADAPTATION
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
C
C
C 
      INTEGER      IBID,NIT, NBITER
      REAL*8       R8BID,DTM,PCENT,VALREF,DVAL
      REAL*8       R8MAEM,R8VIDE
      CHARACTER*8  K8BID,TYPEXT
      CHARACTER*16 MODETP,NOCHAM,NOCMP
      REAL*8       ETA,ETAD
      CHARACTER*24 TPSITE
      INTEGER      JITER
C 
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C 
C --- ACCES SD
C
      TPSITE = SDDISC(1:19)//'.ITER'
      CALL JEVEUO(TPSITE,'L',JITER)
C
C --- METHODE DE CALCUL DE DT+
C
      CALL UTDIDT('L'   ,SDDISC,'ADAP',IADAPT,'METHODE',
     &            R8BID ,IBID  ,MODETP)
C
C --- PAS DE TEMPS PAR DEFAUT (LE DERNIER, SAUF SI JALON)
C
      CALL UTDIDT('L'   ,SDDISC,'LIST',IBID  ,'DT-',
     &            DTM   ,IBID  ,K8BID )

C     ------------------------------------------------------------------
      IF (MODETP.EQ.'FIXE') THEN
C     ------------------------------------------------------------------

        CALL UTDIDT('L'   ,SDDISC,'ADAP',IADAPT,'PCENT_AUGM',
     &              PCENT ,IBID  ,K8BID )
        DTP = DTM * (1.D0 + PCENT / 100.D0)

C     ------------------------------------------------------------------
      ELSEIF (MODETP.EQ.'DELTA_GRANDEUR') THEN
C     ------------------------------------------------------------------

        CALL UTDIDT('L'   ,SDDISC,'ADAP',IADAPT,'NOM_CHAM',
     &              R8BID ,IBID  ,NOCHAM)
        CALL UTDIDT('L'   ,SDDISC,'ADAP',IADAPT,'NOM_CMP',
     &              R8BID ,IBID  ,NOCMP )
        CALL UTDIDT('L'   ,SDDISC,'ADAP',IADAPT,'VALE_REF',
     &              VALREF,IBID  ,K8BID )
        TYPEXT = 'MAX_ABS'
C
C ----- CALCUL DE C = MIN (VREF / |DELTA(CHAMP+CMP)| )
C -----             = VREF / MAX ( |DELTA(CHAMP+CMP)| )
C ----- DVAL :MAX EN VALEUR ABSOLUE DU DELTA(CHAMP+CMP)
C
        CALL EXTDCH(TYPEXT,VALINC,NOCHAM,NOCMP ,DVAL  )
C
C ----- LE CHAMP DE VARIATION EST IDENTIQUEMENT NUL : ON SORT
C
        IF (DVAL.EQ.0.D0) THEN
          DTP = R8VIDE()
        ELSE
          DTP = DTM * VALREF/DVAL
        ENDIF

C     ------------------------------------------------------------------
      ELSEIF (MODETP.EQ.'ITER_NEWTON') THEN
C     ------------------------------------------------------------------

        CALL UTDIDT('L'   ,SDDISC,'ADAP',IADAPT,'NB_ITER_NEWTON_REF',
     &              R8BID ,NIT   ,K8BID )
        NBITER = ZI(JITER-1+NUMINS)
        DTP    = DTM * SQRT( DBLE(NIT) / DBLE(NBITER+1) )

C     ------------------------------------------------------------------
      ELSEIF (MODETP.EQ.'IMPLEX') THEN
C     ------------------------------------------------------------------
C
C ----- FACTEUR D'ACCELERATION ETA
C
        ETA    = 1.2D0
C
C ----- FACTEUR DE DECELERATION ETAD
C
        ETAD   = 0.5D0
        TYPEXT = 'MIN_VAR'
C
C ----- CALCUL DE C = MIN (VREF / |DELTA(CHAMP+CMP)| )
C                   = VREF / MAX ( |DELTA(CHAMP+CMP)| )
C ----- DVAL :MAX EN VALEUR ABSOLUE DU DELTA(CHAMP+CMP)
C
        NOCHAM = 'VARI_ELGA'
        NOCMP  = 'V1'
        CALL EXTDCH(TYPEXT,VALINC,NOCHAM,NOCMP ,DVAL  )
C
C ----- LE CHAMP DE VARIATION EST IDENTIQUEMENT NUL : ON SORT
C
        IF (DVAL.GE.R8MAEM()) DVAL=ETA
        DTP = DTM * DVAL
C
C ----- ON IMPOSE QUE LE DT SOIT COMPRIS ENTRE ETAD*DTM ET ETA*DTM
C
        IF(DTP/DTM.GE.ETA) THEN
          DTP = ETA * DTM
        ELSEIF (DTP/DTM.LE.ETAD) THEN
          DTP = ETAD * DTM
        ENDIF
                
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
      CALL JEDEMA()
      END
