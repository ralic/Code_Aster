      SUBROUTINE  ASGEEL (NOMRES,OPTION,NUGENE)

      IMPLICIT NONE
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
C***********************************************************************
C    M. CORUS     DATE 26/02/10
C-----------------------------------------------------------------------
C  BUT:      < ASSEMBLAGE GENERALISEE DE MATRICE PLEINE ET ELIMINATION >
C
C  ASSEMBLER UNE MATRICE A PARTIR D'UNE NUMEROTATION GENERALISEE
C  ET D'UNE OPTION (RIGI_GENE,MASS_GENE,AMOR_GENE)
C
C REMARQUE : L'ASSEMBLAGE DONNE UNE MATRICE ASSEMBLEE PLEINE
C
C-----------------------------------------------------------------------
C
C NOMRES   /I/: NOM K8 DE LA MATRICE GENERALISEE RESULTAT
C OPTION   /I/: OPTION DE CALCUL (RIGI_GENE,MASS_GENE)
C NUGENE   /I/: NOM K14 DE LA NUMEROTATION GENERALISEE
C STOPLE   /I/: NOM K19 DU STOCKAGE DE LA MATRICE (PLEIN)
C
C
C
      INCLUDE 'jeveux.h'
C
C
      CHARACTER*8  NOMRES,MODGEN
      CHARACTER*14 NUGENE
      CHARACTER*19 PRGENE
      CHARACTER*11 OPTION
      CHARACTER*24 K24BID,SELIAI,SIZLIA,SST,NOMSST
      REAL*8       DDOT,TEMP
C
      INTEGER      IBID,I1,J1,K1,L1,N1,LRES,NEQ,LNEQ,LPROJ,
     &             NBDDL,INDSST,LSST,LLREF,NBMACR,EXIST,
     &             LNOMCR,LIMACR,LMACR,LLMACR,INDMCR,DECAL,
     &             DDLCP,LSELIA,NLT,IND,LDCONL,IRET,NMADDL,LTEMP,
     &             JREFA,LDLIM,NBSST,LSILIA
      CHARACTER*8  KBID,K8BID
      CHARACTER*1  K1BID
C-----------C
C--       --C      
C-- DEBUT --C      
C--       --C
C-----------C

      CALL JEMARQ()

C----------------------------------C
C--                              --C
C-- INITIALISATION DE LA MATRICE --C      
C--                              --C
C----------------------------------C

      PRGENE=NUGENE//'.NUME'

      
      CALL WKVECT(NOMRES//'           .REFA','G V K24',11,JREFA)
      ZK24(JREFA-1+11)='MPI_COMPLET'
      ZK24(JREFA-1+1)=' '
      ZK24(JREFA-1+2)=NUGENE
      ZK24(JREFA-1+8) = 'ASSE'
      ZK24(JREFA-1+9) = 'MS'
      ZK24(JREFA-1+10) = 'GENE'
C
C--------------------RECUPERATION DU MODE_GENE AMONT--------------------
C
      K24BID=PRGENE//'.REFN'
      CALL JEVEUO(K24BID,'L',LLREF)
      MODGEN=ZK24(LLREF)(1:8)
      NOMSST=MODGEN//'      .MODG.SSNO'
C
C--------------------------CREATION DU .LIME----------------------------
C   POUR L'INSTANT ON DONNE LE NOM DU MODELE GENERALISE
C
      CALL WKVECT(NOMRES//'           .LIME','G V K24',1,LDLIM)
      ZK24(LDLIM)=MODGEN
C      CALL WKVECT(NOMRES//'           .DESC','G V I',1,LDDESC)
C      ZI(LDDESC)=2

C
C------------------RECUPERATION DU NOMBRE DE SOUS-STRUCTURE-------------
C
      CALL JELIRA(NOMSST,'NOMMAX',NBSST,K1BID)
C      
C-- 
C
C      SELIAI= '&&'//NUGENE(1:8)//'PROJ_EQ_LIAI'
C      SIZLIA='&&'//NUGENE(1:8)//'VECT_SIZE_SS'
C      SST=    '&&'//NUGENE(1:8)//'VECT_NOM_SS'
      SELIAI=NUGENE(1:14)//'.ELIM.BASE'
      SIZLIA=NUGENE(1:14)//'.ELIM.TAIL'
      SST=   NUGENE(1:14)//'.ELIM.NOMS'

      K24BID=PRGENE//'.NEQU'
      CALL JEVEUO(K24BID,'L',LNEQ)
      NEQ=ZI(LNEQ)

      CALL WKVECT(NOMRES//'           .CONL','G V R',NEQ,LDCONL)
      DO 10 I1=1,NEQ
        ZR(LDCONL+I1-1)=1.D0
  10  CONTINUE      
C
C-- RECUPERATION DES DIFFERENTS MACRO ELEMENTS      
C
      CALL WKVECT('&&ASGEEL.NOMS_MACRO','V V K8',NBSST,LNOMCR)
      CALL WKVECT('&&ASGEEL.INDICES_MACRO','V V I',NBSST*3,LIMACR)

      CALL JEVEUO(SIZLIA,'L',LSILIA)
      CALL JEVEUO(SST,'L',LSST)
      
      DDLCP=0  
      NLT=0
      NMADDL=0       
      DO 20 I1=1,NBSST
        NBDDL=ZI(LSILIA+I1-1)
        IF (NBDDL .GT. NMADDL) THEN
          NMADDL=NBDDL
        ENDIF
        NLT=NLT+NBDDL
        DDLCP=DDLCP+((NBDDL*(NBDDL+1))/2)
        KBID='        '
        INDSST=I1
        CALL JENONU(JEXNOM(NOMSST,ZK8(LSST+I1-1)),INDSST)
        CALL MGUTDM(MODGEN,KBID,INDSST,'NOM_MACR_ELEM',IBID,K8BID)
                
        IF (I1 .EQ. 1) THEN
          NBMACR=1
          ZI(LIMACR)=1
          ZI(LIMACR+NBSST)=NBDDL          
          ZI(LIMACR+2*NBSST)=NLT-NBDDL
          ZK8(LNOMCR)=K8BID
        ELSE
          EXIST=0
          DO 30 J1=1,NBMACR
            IF (ZK8(LNOMCR+J1-1) .EQ. K8BID) THEN
              EXIST=1
              ZI(LIMACR+I1-1)=J1
              ZI(LIMACR+NBSST+I1-1)=NBDDL
              ZI(LIMACR+2*NBSST+I1-1)=NLT-NBDDL
            ENDIF
  30      CONTINUE
          IF (EXIST .EQ. 0) THEN
            NBMACR=NBMACR+1
            ZI(LIMACR+I1-1)=NBMACR
            ZI(LIMACR+NBSST+I1-1)=NBDDL
            ZI(LIMACR+2*NBSST+I1-1)=NLT-NBDDL
            ZK8(LNOMCR+NBMACR-1)=K8BID
          ENDIF
        ENDIF
  20  CONTINUE

C  
C-- RECUPERATION DES MATRICES DES DIFERENTS MACROS ELEMENTS 
C
      CALL WKVECT('&&ASGEEL.POINTEURS_MACRO','V V I',NBMACR,LLMACR)
      DO 40 I1=1,NBMACR
        IF (OPTION .EQ. 'RIGI_GENE') THEN
          CALL JEVEUO(ZK8(LNOMCR+I1-1)//'.MAEL_RAID_VALE','L',LMACR)
          K1BID='K'
        ELSEIF (OPTION .EQ. 'MASS_GENE') THEN
          CALL JEVEUO(ZK8(LNOMCR+I1-1)//'.MAEL_MASS_VALE','L',LMACR)
          K1BID='M'     
        ELSEIF (OPTION .EQ. 'AMOR_GENE') THEN
          CALL JEVEUO(ZK8(LNOMCR+I1-1)//'.MAEL_AMOR_VALE','L',LMACR)  
        ENDIF
        ZI(LLMACR+I1-1)=LMACR
  40  CONTINUE
 
C-- .VALM NE DOIT PAS EXISTER :
      CALL JEEXIN(NOMRES//'           .VALM',IRET) 
      CALL ASSERT(IRET.EQ.0)
      
C-- ALLOCATION DE LA MATRICE PROJETEE    
      CALL JECREC(NOMRES//'           .VALM','G V R','NU','DISPERSE',
     &            'CONSTANT',1)
      CALL JECROC(JEXNUM(NOMRES//'           .VALM',1))
      CALL JEECRA(JEXNUM(NOMRES//'           .VALM',1),'LONMAX',
     &            INT((NEQ*(NEQ+1))/2),' ')
      CALL JEVEUO(JEXNUM(NOMRES//'           .VALM',1),'E',LRES)

C----------------------------------------C
C--                                    --C
C-- REMPLISSAGE DE LA MATRICE PROJETEE --C
C--                                    --C
C----------------------------------------C

      CALL JEVEUO(SELIAI,'L',LSELIA)
           
      CALL WKVECT('&&ASGEEL.MATR_TEMP','V V R',NMADDL**2,LTEMP)
      CALL WKVECT('&&ASGEEL.PROJ_TEMP','V V R',NMADDL*NEQ,LPROJ)
      
      DO 50 N1=1,NBSST
        INDMCR=ZI(LIMACR+N1-1)
        NBDDL=ZI(LIMACR+NBSST+N1-1)
        DECAL=ZI(LIMACR+2*NBSST+N1-1)
        LMACR=ZI(LLMACR+INDMCR-1)        
        
C-- ON RECOPIE LA MATRICE DU MACRO ELEMENT (STOCKAGE PLEIN)      
        DO 60 J1=1,INT(NBDDL*(NBDDL+1)/2)
          TEMP=(SQRT(1.D0+8.D0*J1)-1.D0)/2.D0
          L1=INT(TEMP)
          IF (TEMP .GT. L1) THEN
            L1=L1+1
          ENDIF
          K1=J1-INT(L1*(L1-1)/2)
          ZR(LTEMP+NBDDL*(K1-1)+L1-1)=ZR(LMACR+J1-1)
          ZR(LTEMP+NBDDL*(L1-1)+K1-1)=ZR(LMACR+J1-1)
  60    CONTINUE
    
C-- ON FAIT K*T
        DO 70 I1=1,NBDDL
          DO 80 J1=1,NEQ
            ZR(LPROJ+(J1-1)*NBDDL+I1-1)=DDOT(NBDDL,
     &                 ZR(LTEMP+(I1-1)*NBDDL),1,
     &                 ZR(LSELIA+(J1-1)*NLT+DECAL),1)
  80      CONTINUE
  70    CONTINUE        
  
C-- ON FAIT T^T*(K*T)
        DO 90 J1=1,NEQ
          DO 100 I1=1,J1
            IND=INT(((J1-1)*J1)/2)+I1-1
            ZR(LRES+IND)=ZR(LRES+IND)+DDOT(NBDDL,
     &                 ZR(LPROJ+(J1-1)*NBDDL),1,
     &                 ZR(LSELIA+(I1-1)*NLT+DECAL),1)
  100     CONTINUE
  90    CONTINUE

  50  CONTINUE

      CALL JEDETR('&&ASGEEL.POINTEURS_MACRO')
      CALL JEDETR('&&ASGEEL.NOMS_MACRO')
      CALL JEDETR('&&ASGEEL.INDICES_MACRO')
      CALL JEDETR('&&ASGEEL.MATR_TEMP')
      CALL JEDETR('&&ASGEEL.PROJ_TEMP')
      
C---------C
C--     --C
C-- FIN --C
C--     --C
C---------C
C      CALL MATIMP(NOMRES//'           ',8,'MATLAB')
      CALL JEDEMA()
      
      END
