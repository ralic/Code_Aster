      SUBROUTINE TRASST(MODGEN,NUMSST,ISST1,LISINT,NBEQ1,NBMOD,NBINT)
      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/09/2011   AUTEUR COURTOIS M.COURTOIS 
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

C-------------------------------------------------------------C
C--       ROUTINE XXXXX2           M. CORUS - AOUT 2011     --C
C--       CALCUL DES TRAVAUX DANS LES SOUS STRUCTURES       --C
C--                                                         --C
C-------------------------------------------------------------C
C--   VARIABLES E/S  :
C--   MODGEN   /IN/  : NOM DU MODELE GENERALISE
C--   NUMSST   /IN/  : NUMERO DE LA SOUS STRUCTURE TRAITEE
C--   ISST1    /IN/  : NUMERO DE LA SOUS STRUCTURE
C--   LISINT   /IN/  : LISTE DES NOMS D'INTERFACES
C--   NBEQ1    /IN/  : NB DE DDL DE LA SST
C--   NBMOD    /IN/  : NOMBRE DE MODE DU MODELE REDUIT
C--   NBINT    /IN/  : NB D'INTERFACES ASSOCIEES A LA SST

      IMPLICIT NONE
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)

      CHARACTER*32 JEXNUM
C
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ---------------------------
C
      CHARACTER*1  LISTYP(2)
      CHARACTER*4  K4BID
      CHARACTER*8  MODGEN,KB,REST1,MRAID,MMASS
      CHARACTER*19 IMPED,LISMAT(2),NUME91,SOLVEU
      CHARACTER*24 INDIN1
      INTEGER      I1,IBID,IRET,J1,K1,L1,NBEQ1,NBMOD,ISST1,LLINT1,
     &             NBDDL1,TACH1,LOMEG,LMOD1,LMASS,LBID,LTRSST,LRAID,
     &             LEFF1,LEFF2,LINTF,NBINT,IDEEQ,LCOPY1,LSECME,
     &             LIMPED,UNIT,NUMSST
      REAL*8       TRAVM,TRAVK,DDOT,TRAINT,COMLIN(2),SHIFT
      CHARACTER*24 LISINT
      INTEGER      IARG

      CALL GETVIS(' ','UNITE',1,IARG,1,UNIT,IBID)
      I1=NUMSST
      
C-- RECHERCHE DU MACRO ELEMENT ASSOCIE A LA SST
      CALL JEVEUO(JEXNUM(MODGEN//'      .MODG.SSME',ISST1),'L',IBID)
      CALL JEVEUO(ZK8(IBID)//'      .NUME.DEEQ','L',IDEEQ)
        
C------------------------------------------------------------C
C--                                                        --C
C-- CONSTRUCTION DES MATRICES D'IMPEDANCE DYNAMIQUE K+MU*M --C
C--                   POUR L'ENRICHISSEMENT                --C
C--                                                        --C
C------------------------------------------------------------C

        CALL CODENT(NUMSST,'D0',K4BID)
        IMPED='&&OP0091.IMPED'//K4BID

        CALL JEVEUO(JEXNUM(MODGEN//'      .MODG.SSME',ISST1),'L',
     &                  IBID)

        CALL JEVEUO(ZK8(IBID)//'.MAEL_MASS_REFE','L',LBID)
        MMASS=ZK24(LBID+1)(1:8)
        CALL JEVEUO(ZK8(IBID)//'.MAEL_RAID_REFE','L',LBID)
        MRAID=ZK24(LBID+1)(1:8)
        CALL MTDEFS(IMPED,MMASS,'V',' ')
        LISMAT(1)=MRAID
        LISMAT(2)=MMASS

        CALL DISMOI('F','NOM_NUME_DDL',MRAID,'MATR_ASSE',IBID,
     &              NUME91,IBID)

        CALL GETVR8(' ','SHIFT',1,IARG,1,SHIFT,IBID)
        COMLIN(1)=1.D0        
        COMLIN(2)=-((SHIFT*2.D0*3.1415927D0)**2)
        LISTYP(1)='R'
        LISTYP(2)='R'
        CALL MTCMBL(2,LISTYP,COMLIN,LISMAT,
     &                    IMPED,' ',NUME91,'ELIM1')
        CALL MTDSCR(IMPED)
        CALL JEVEUO(IMPED(1:19)//'.&INT','E',LIMPED)

        CALL DISMOI('F','SOLVEUR',MRAID,'MATR_ASSE',IBID,SOLVEU,IBID)
        
        CALL PRERES(SOLVEU,'V',IRET,'&&OP0091.MATPRE',IMPED,IBID,
     &              -9999)
        IF (IRET.EQ.2) THEN
           CALL U2MESK('F', 'ALGELINE4_37',1,IMPED)
        ENDIF

        REST1='&&91'//K4BID
        CALL JEVEUO(JEXNUM(REST1//'           .TACH',1),'L',TACH1)
        CALL JEVEUO('&&OP0091.MODE_SST1','E',LMOD1)
        CALL JEVEUO('&&OP0091.MODE_SST1_EFF1','E',LEFF1)
        CALL JEVEUO('&&OP0091.MODE_SST1_EFF2','E',LEFF2)
        CALL JEVEUO('&&OP0091.MODE_SST1_COPY','E',LCOPY1)  
        CALL JEVEUO(LISINT,'L',LINTF)
        
        CALL JEVEUO('&&OP0091.MATRICE_MASS','L',LMASS)
        CALL JEVEUO('&&OP0091.MATRICE_RAID','L',LRAID)
        CALL JEVEUO('&&OP0091.TRAV_SST','E',LTRSST)
        CALL JEVEUO('&&OP0091.PULSA_PROPRES','L',LOMEG)
        CALL JEVEUO('&&OP0091.MODE_INTF_DEPL','E',LSECME)
        
C-- BOUCLE SUR LES MODES        
        DO 80 J1=1,NBMOD
          CALL JEVEUO(ZK24(TACH1+J1-1)(1:19)//'.VALE','L',IBID)

C-- RECOPIE DANS UN VECTEUR DE TRAVAIL          
          CALL LCEQVN(NBEQ1,ZR(IBID),ZR(LCOPY1))
          
C-- ANNULATION DES DDL DE LAGRANGE          
          CALL ZERLAG(ZR(LCOPY1),NBEQ1,ZI(IDEEQ))

C-- NOUVELLE COPIE          
          CALL LCEQVN(NBEQ1,ZR(LCOPY1),ZR(LMOD1))
          
C-- ANNULATION DES COMPOSANTES ASSOCIEES AUX INTERFACES          
          DO 90 K1=1,NBINT
            INDIN1='&&VEC_DDL_INTF_'//ZK8(LINTF+K1-1)
            CALL JEVEUO(INDIN1,'L',LLINT1)
            CALL JELIRA(INDIN1,'LONMAX',NBDDL1,KB)
            DO 140 L1=1,NBDDL1
              IF (ZI(LLINT1+L1-1) .GT. 0) THEN
                ZR(LMOD1+ZI(LLINT1+L1-1)-1)=0
              ENDIF
  140       CONTINUE
  90      CONTINUE

C-- CALCUL DES TRAVAUX          
          CALL MRMULT('ZERO',ZI(LMASS+ISST1-1),ZR(LCOPY1),'R',
     &                ZR(LEFF1),1)
          
          TRAVM=DDOT(NBEQ1,ZR(LMOD1),1,ZR(LEFF1),1)
          CALL MRMULT('ZERO',ZI(LRAID+ISST1-1),ZR(LCOPY1),'R',
     &                ZR(LEFF2),1)
     
          TRAVK=DDOT(NBEQ1,ZR(LMOD1),1,ZR(LEFF2),1)
          TRAINT=TRAVK-(ZR(LOMEG+J1-1)**2)*TRAVM 
          IF (ZR(LOMEG+J1-1) .GT. 1) TRAINT=TRAINT/ZR(LOMEG+J1-1)
          WRITE(UNIT,*)'MODE ',J1,' -  TRAVAIL SST =',TRAINT
          ZR(LTRSST+NBMOD*(I1-1)+J1-1)=TRAINT
          
C--
C-- CALCUL DU SECOND MEMBRE ET DES ENRICHISSEMENTS
C--
          CALL DAXPY(NBEQ1,-(ZR(LOMEG+J1-1)**2),ZR(LEFF1),1,
     &               ZR(LEFF2),1)
          CALL ZERLAG(ZR(LEFF1),NBEQ1,ZI(IDEEQ))
          LBID=LSECME
          CALL LCEQVN(NBEQ1,ZR(LEFF1),ZR(LSECME+NBEQ1*(J1-1)))

C-- DIFFERENTIATION DES SECONDS MEMBRES : INTERFACE / INTERIEUR
          
          DO 160 K1=1,NBINT
            INDIN1='&&VEC_DDL_INTF_'//ZK8(LINTF+K1-1)
            CALL JEVEUO(INDIN1,'L',LLINT1)
            CALL JELIRA(INDIN1,'LONMAX',NBDDL1,KB)
            DO 170 L1=1,NBDDL1
              IBID=ZI(LLINT1+L1-1)
              IF (IBID .GT. 0) THEN
                ZR(LSECME+NBEQ1*(J1-1)+IBID-1)=0
                ZR(LSECME+NBEQ1*(NBMOD+J1-1)+IBID-1)=ZR(LEFF1+IBID-1)
              ENDIF  
  170       CONTINUE
  160     CONTINUE
  
  
  80    CONTINUE 
  
      END     
