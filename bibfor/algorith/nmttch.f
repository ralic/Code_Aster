      SUBROUTINE NMTTCH(SDDISC,INST  ,NUME  )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/06/2011   AUTEUR ABBAS M.ABBAS 
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
      REAL*8       INST
      INTEGER      NUME
      CHARACTER*19 SDDISC
C
C ----------------------------------------------------------------------
C
C ROUTINE *_NON_LINE (UTILITAIRE - SELEC. INST.)
C
C RECHERCHE DE L'iNDICE DANS LA SD RESULTAT JUSTE AVANT L'INSTANT
C DONNE
C
C ----------------------------------------------------------------------
C
C
C IN  SDDISC : SD DISCRETISATION
C IN  INST   : INSTANT A RECHERCHER
C OUT NUME   : INDICE A ECRASER
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      INTEGER      ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8       ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16   ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL      ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8  ZK8
      CHARACTER*16    ZK16
      CHARACTER*24        ZK24
      CHARACTER*32            ZK32
      CHARACTER*80                ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      CHARACTER*24 TPSDIT
      INTEGER      JTEMPS
      INTEGER      NBINST,IBID,I,NBINTV
      CHARACTER*8  K8BID
      REAL*8       R8BID,TOLE,R8PREM
      REAL*8       DTMIN,INS,DT
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- ACCES SD_DISC
C    
      TPSDIT = SDDISC(1:19)//'.DITR'
      CALL JEVEUO(TPSDIT,'L',JTEMPS) 
      
      CALL UTDIDT('L'   ,SDDISC,'LIST',IBID  ,'NBINST',R8BID ,
     &            NBINST,K8BID )

      TOLE = R8PREM()
C
C --- RECHERCHE INSTANT
C
      CALL UTACLI(INST  ,ZR(JTEMPS),NBINST,TOLE  ,NUME)
      NBINTV = NBINST - 1
C
C --- SI INST NON PRESENT DANS LA LISTE D INSTANT
C --- ON CHERCHE L INSTANT LE PLUS PROCHE AVANT L'INSTANT CHERCHE
C
      IF (NUME.LT.0) THEN
        DTMIN  = INST-ZR(JTEMPS)
        INS    = ZR(JTEMPS)
        DO 40 I=1,NBINTV
          DT     = INST-ZR(JTEMPS+I)
          IF (DT.LE.0.D0) THEN
            GOTO 45
          ENDIF  
          IF (DT.LT.DTMIN) THEN
            DTMIN  = DT
            INS    = ZR(JTEMPS+I)
          ENDIF
 40     CONTINUE
 45     CONTINUE
        INST   = INS
        CALL UTACLI(INST  ,ZR(JTEMPS),NBINST,TOLE  ,NUME  )
        NUME = NUME + 1    
      ENDIF
C
      IF (NUME .LT. 0) THEN
        CALL U2MESS('F','DISCRETISATION_89')
      ENDIF
C
      CALL JEDEMA()

      END
