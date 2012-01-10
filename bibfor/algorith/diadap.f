      LOGICAL FUNCTION DIADAP(SDDISC,IADAPT)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/01/2012   AUTEUR GENIAUT S.GENIAUT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBIDUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY  
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR     
C (AT YOUR OPTION) ANY LATER VERSION.                                   
C                                                                       
C THIS PROGRAM IS DISTRIBIDUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT 
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
      INTEGER      IADAPT
      CHARACTER*19 SDDISC
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (UTILITAIRE)
C
C ACCES AU DECLENCHEUR DE L'ADAPTATION DU PAS DE TEMPS
C
C ----------------------------------------------------------------------
C
C
C IN  SDDISC : SD DISCRETISATION
C IN  IADAPT : NUMERO DE LA METHODE D ADAPTATION TRAITEE
C OUT DIADAP : .TRUE. SI ON DOIT ADAPTER LE PAS DE TEMPS
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
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
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      REAL*8       R8BID
      INTEGER      IBID,NBINSE,NBOK
      CHARACTER*8  K8BID
      CHARACTER*19 EVEN
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
C --- NOM DE L'EVENEMENT
C   
      CALL UTDIDT('L'   ,SDDISC,'ADAP',IADAPT,'NOM_EVEN',
     &            R8BID ,IBID  ,EVEN  )
C
      IF (EVEN.EQ.'AUCUN') THEN

        DIADAP = .FALSE.

      ELSEIF (EVEN.EQ.'TOUT_INST') THEN
      
        DIADAP = .TRUE.

      ELSEIF (EVEN.EQ.'SEUIL_SANS_FORMULE') THEN
C
C ----- RECUP DU SEUIL SUR LE NB DE SUCCES CONSECUTIFS
C
        CALL UTDIDT('L'   ,SDDISC,'ADAP',IADAPT,'NB_INCR_SEUIL',
     &              R8BID ,NBINSE,K8BID )
C
C ----- RECUP DU NB DE SUCCES CONSECUTIFS
C
        CALL UTDIDT('L'   ,SDDISC,'ADAP',IADAPT,'NB_EVEN_OK',
     &              R8BID ,NBOK  ,K8BID )
C
        IF (NBOK.LT.NBINSE) THEN
          DIADAP = .FALSE.
        ELSE
C         ICI NBOK EST NORMALEMENT EGAL A NBINSE
C         MAIS NBOK PEUT ETRE AUSSI SUPERIEUR A NBINSE SI ON UTILISE
C         LA METHODE CONTINUE
          DIADAP = .TRUE.
C         REMISE A ZERO DE NBOK
          NBOK   = 0
          CALL UTDIDT('E'   ,SDDISC,'ADAP',IADAPT,'NB_EVEN_OK',
     &                R8BID ,NBOK  ,K8BID )
        ENDIF
C
      ELSEIF (EVEN.EQ.'SEUIL_AVEC_FORMULE') THEN

        CALL ASSERT(.FALSE.)

      ENDIF
C     
      CALL JEDEMA()
      END
