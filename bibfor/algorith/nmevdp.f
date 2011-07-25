      SUBROUTINE NMEVDP(SDDISC,IEVDAC,RETSWA)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 12/07/2011   AUTEUR ABBAS M.ABBAS 
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
      INTEGER      IEVDAC,RETSWA
      CHARACTER*19 SDDISC
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME - EVENEMENTS)
C
C GESTION DE L'EVENEMENT DIVE_ITER_PILO
C
C ----------------------------------------------------------------------
C
C
C IN  SDDISC : SD DISCRETISATION TEMPORELLE
C IN  IEVDAC : INDICE DE L'EVENEMENT ACTIF
C OUT RETSWA : CODE RETOUR CHANGE PILOTAGE
C               0 ECHEC DU SWAP
C               1 SWAP OK - ON REFAIT LE PAS DE TEMPS
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
      REAL*8       R8BID
      INTEGER      IBID
      INTEGER      PILESS
      CHARACTER*8  K8BID,PILCHO
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      RETSWA = 0
C
C --- PARAMETRES
C          
      CALL UTDIDT('L'   ,SDDISC,'ECHE',IEVDAC,'ESSAI_ITER_PILO',
     &            R8BID ,PILESS,K8BID )
C
C --- L'UTILISATEUR UTILISE LE PILOTAGE
C --- ET SOUHAITE BASCULER SI NON-CONVERGENCE
C
      CALL ASSERT(PILESS.EQ.1.OR.PILESS.EQ.2)
C
C --- ON RETENTE EN CHOISISSANT L'AUTRE SOLUTION
C
      IF (PILESS.EQ.1) THEN
        PILESS = 2
        CALL UTDIDT('E'   ,SDDISC,'ECHE',IEVDAC,'ESSAI_ITER_PILO',
     &              R8BID ,PILESS,K8BID )
        PILCHO = 'AUTRE'
        CALL UTDIDT('E'   ,SDDISC,'ECHE',IEVDAC,'CHOIX_SOLU_PILO',
     &              R8BID ,IBID  ,PILCHO) 
        CALL NMIMPR('IMPR','AUTR_SOLU_PILO',K8BID ,R8BID,IBID)
        RETSWA = 1
      ENDIF
C
      CALL JEDEMA()
      END
