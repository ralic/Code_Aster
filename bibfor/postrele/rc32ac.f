      SUBROUTINE RC32AC ( LPMPB, LSN, LSNET, LFATIG, LROCHT, MATER )
      IMPLICIT   NONE        
      LOGICAL             LPMPB, LSN, LSNET, LFATIG, LROCHT
      CHARACTER*8         MATER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 21/10/2008   AUTEUR VIVAN L.VIVAN 
C TOLE CRP_20
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
C     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
C     CALCUL DES AMPLITUDES DE CONTRAINTES
C     CALCUL DU FACTEUR D'USAGE 
C
C     Pour chaque extremite :
C
C     pour une situation P, on a 2 états stabilisés
C     pour une situation Q, on a 2 états stabilisés
C
C     Soit 2 états stabilisés I et J appartenant respectivement aux 
C     situations P et Q :
C
C     on calcule le SALT(I,J) = 0,5*(EC/E)*Ke*Sn(P,Q)*Sp(I,J)
C
C     avec Sn(P,Q) = Max( Sn(I,J) )
C          Sn(I,J) = Max( Max(Sn(I,J,ThP)), Max(Sn(I,J,ThQ)) )
C
C     avec Sp(I,J) = Max( Max(Sp(I,J,ThP)), Max(Sp(I,J,ThQ)) )
C
C
C Etape 1 : on calcule le SALT qui correspond aux combinaisons de tous
C           les états stabilisés appartenant aux situations d'un groupe
C           donné.
C
C Etape 2 : on calcule le SALT pour les situations non combinables
C
C Etape 3 : traitement des situations de passage
C           on calcule le SALT(I,J)  
C              - avec I appartenant au premier groupe
C              - avec J appartenant au deuxieme groupe
C              - on lui associe le nombre d'occurrences de la
C                situation de passage
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32     JEXNOM, JEXNUM, JEXATR
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER      IG, NBGR, NBSIGR, JNSG, IS, IS1, IOC1, NOCC, NUMGR, 
     +             JCOMBI, JPRESA, JPRESB, JNBOCC, IM, JNUMGR,
     +             NPASS, NUM1, NUM2, IFM, NIV, IOCS, JSEIGR, JRESU,
     +             JNSITU, NSITUP, NSITUQ, IRET, I1, JFACT, I, J,
     +             JREAS, JRESS, JRECA, JRECS, NDIM, NBP12, NBP23, NBP13
      REAL*8       PPI, PPJ, SNMAX, SPMAX, SAMAX, UTOT, SALTIJ, VALRES,
     +             UG, NADM, MPI(12), MPJ(12), SM, SN, SNET, SP(2), SMM,
     +             MATPI(8), MATPJ(8), MSE(12),TYPEKE,SPMECA,SPTHER,
     +             SPTHEM,SPMECM,KEMECA,KETHER, PM, PB, PMPB,
     +             SIPMAX, SIMPIJ, SNEMAX, KEMAX, R8VIDE, PMMAX, PBMAX,
     +             PMBMAX, VALE(2)
      LOGICAL      SEISME, ENDUR, CFAIT
      CHARACTER*2  CODRET
      CHARACTER*4  LIEU(2)
      CHARACTER*8  K8B
      CHARACTER*24 K24AS, K24SS, K24CA, K24CS, K24FU
C
      DATA LIEU / 'ORIG' , 'EXTR' /
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
C
      CALL INFNIV ( IFM, NIV )
C
      CALL JEVEUO ( '&&RC3200.SITU_NUMERO'    , 'L', JNSITU )
      CALL JELIRA ( '&&RC3200.SITU_NUME_GROUP', 'LONMAX', NBGR, K8B )
      CALL JEVEUO ( '&&RC3200.SITU_NUME_GROUP', 'L', JNUMGR )
      CALL JEVEUO ( '&&RC3200.SITU_SEISME'    , 'L', JSEIGR )
C
      CALL JEVEUO ( '&&RC3200.SITU_COMBINABLE', 'L', JCOMBI )
      CALL JEVEUO ( '&&RC3200.SITU_PRES_A'    , 'L', JPRESA )
      CALL JEVEUO ( '&&RC3200.SITU_PRES_B'    , 'L', JPRESB )
      CALL JEVEUO ( '&&RC3200.SITU_NB_OCCUR'  , 'L', JNBOCC )
C
      CALL JELIRA ('&&RC32SI.PASSAGE_1_2','LONUTI', NBP12, K8B )
      CALL JELIRA ('&&RC32SI.PASSAGE_2_3','LONUTI', NBP23, K8B )
      CALL JELIRA ('&&RC32SI.PASSAGE_1_3','LONUTI', NBP13, K8B )
C
C --- IL FAUT CALCULER LE FACTEUR D'USAGE A CHAQUE EXTREMITE
C
      DO 10 IM = 1 , 2
C
C ------ POUR CHAQUE SITUATION, ON ARCHIVE :
C         * 10 QUANTITES AVEC LA PRISE EN COMPTE DU SEISME
C         * 10 QUANTITES SANS LA PRISE EN COMPTE DU SEISME
C         1  : PM
C         2  : PB
C         3  : PMPB
C         4  : SN
C         5  : SN*
C         6  : SP
C         7  : KE_MECA
C         8  : KE_THER
C         9  : SALT
C         10 : UG
C
C ------ POUR CHAQUE COMBINAISON, ON ARCHIVE :
C         1  : SN(P,Q)
C         2  : SP1_MIN
C         3  : SP2_MAX
C         4  : SALT(P,Q)
C
         K24AS = '&&RC3200.AVEC_SEISME'//LIEU(IM)
         CALL JECREC(K24AS, 'V V R', 'NU', 'DISPERSE', 'VARIABLE', NBGR)
C
         K24SS = '&&RC3200.SANS_SEISME'//LIEU(IM)
         CALL JECREC(K24SS, 'V V R', 'NU', 'DISPERSE', 'VARIABLE', NBGR)
C
         K24CA = '&&RC3200.COMBI_A_SEI'//LIEU(IM)
         CALL JECREC(K24CA, 'V V R', 'NU', 'DISPERSE', 'VARIABLE', NBGR)
C
         K24CS = '&&RC3200.COMBI_S_SEI'//LIEU(IM)
         CALL JECREC(K24CS, 'V V R', 'NU', 'DISPERSE', 'VARIABLE', NBGR)
C
         K24FU = '&&RC3200.FACT_USAGE '//LIEU(IM)
         CALL JECREC(K24FU, 'V V R', 'NU', 'DISPERSE', 'VARIABLE', NBGR)
C
         IF ( NIV .GE. 2 ) THEN
            IF (IM.EQ.1) THEN
               WRITE(IFM,*)'  '
               WRITE(IFM,*)'******* ORIGINE DU SEGMENT *******'
            ELSE
               WRITE(IFM,*)'  '
               WRITE(IFM,*)'******* EXTREMITE DU SEGMENT *******'
            ENDIF
            WRITE(IFM,*) ' '
            WRITE(IFM,*)
     +       '=> ON TRAITE LES SITUATIONS COMBINABLES DANS LEUR GROUPE'
         ENDIF
         PMMAX  = 0.D0
         PBMAX  = 0.D0
         PMBMAX = 0.D0
         SM     = 0.D0
         SNMAX  = 0.D0
         SNEMAX = 0.D0
         SPMAX  = 0.D0
         KEMAX  = 0.D0
         SPMECM = 0.D0
         SPTHEM = 0.D0
         SAMAX  = 0.D0
         UTOT   = 0.D0
         SIPMAX = 0.D0
C
C ----------------------------------------------------------------------
C                           E T A P E   1   
C ----------------------------------------------------------------------
C
C ------ ON TRAITE LES SITUATIONS COMBINABLES DANS LEUR GROUPE
C        -----------------------------------------------------
C
         DO 100 IG = 1 , NBGR
C
            NUMGR = ZI(JNUMGR+IG-1)
            IF ( NUMGR .LT. 0 ) GOTO 100
C
            IOCS  = ZI(JSEIGR+IG-1)
C
            CALL JELIRA(JEXNUM('&&RC3200.LES_GROUPES',NUMGR),'LONUTI',
     +                                                    NBSIGR, K8B )
            CALL JEVEUO(JEXNUM('&&RC3200.LES_GROUPES',NUMGR),'L',JNSG)
            IF (NIV.GE.2) THEN
               WRITE (IFM,3000) NUMGR,NBSIGR
               WRITE (IFM,3002) (ZI(JNSITU+ZI(JNSG+I1-1)-1),I1=1,NBSIGR)
            END IF
            CALL JECROC (JEXNUM(K24AS,IG))
            CALL JEECRA (JEXNUM(K24AS,IG), 'LONMAX', 10*NBSIGR, ' ' )
            CALL JEVEUO (JEXNUM(K24AS,IG), 'E', JREAS )
C
            CALL JECROC (JEXNUM(K24SS,IG))
            CALL JEECRA (JEXNUM(K24SS,IG), 'LONMAX', 10*NBSIGR, ' ' )
            CALL JEVEUO (JEXNUM(K24SS,IG), 'E', JRESS )
C
            NDIM = MAX(4,4*NBSIGR*(NBSIGR-1)/2)
            CALL JECROC (JEXNUM(K24CA,IG))
            CALL JEECRA (JEXNUM(K24CA,IG), 'LONMAX', NDIM, ' ' )
            CALL JEVEUO (JEXNUM(K24CA,IG), 'E', JRECA )
C
            CALL JECROC (JEXNUM(K24CS,IG))
            CALL JEECRA (JEXNUM(K24CS,IG), 'LONMAX', NDIM, ' ' )
            CALL JEVEUO (JEXNUM(K24CS,IG), 'E', JRECS )
C
            CALL JECROC (JEXNUM(K24FU,IG))
            CALL JEECRA (JEXNUM(K24FU,IG), 'LONMAX', 4*50, ' ' )
            CALL JEVEUO (JEXNUM(K24FU,IG), 'E', JFACT )
C
            IF ( IG .EQ. 1 ) THEN
               IF ( NBP12.NE.0 .OR. NBP13.NE.0 ) GOTO 100
            ELSEIF ( IG .EQ. 2 ) THEN
               IF ( NBP12.NE.0 .OR. NBP23.NE.0 ) GOTO 100
            ELSEIF ( IG .EQ. 3 ) THEN
               IF ( NBP13.NE.0 .OR. NBP23.NE.0 ) GOTO 100
            ENDIF
C
            NPASS = 0
            IF ( IOCS .EQ. 0 ) THEN
               SEISME = .FALSE.
            ELSE
               SEISME = .TRUE.
            ENDIF
C  
            CALL RC3201 ( LPMPB, LSN, LSNET, LFATIG, LROCHT, LIEU(IM),
     +                    NUMGR, IOCS, SEISME, NPASS, MATER, SNMAX,
     +                    SNEMAX, SPMAX, KEMAX, SPMECM, SPTHEM, SAMAX,  
     +                    UTOT, SM, SIPMAX, ZR(JREAS), ZR(JRESS),
     +                    ZR(JRECA), ZR(JRECS), ZR(JFACT), PMMAX,
     +                    PBMAX, PMBMAX )
C
 100     CONTINUE
C
C ----------------------------------------------------------------------
C                           E T A P E   2 
C ----------------------------------------------------------------------
C
         SEISME = .FALSE.
         DO 220 I = 1, 12
            MSE(I) = 0.D0
 220     CONTINUE
C
C ------ ON TRAITE LES SITUATIONS NON COMBINABLES
C        ----------------------------------------
C
         CFAIT = .FALSE.
         DO 200 IG = 1 , NBGR
C
            NUMGR = ZI(JNUMGR+IG-1)
            IF ( NUMGR .LT. 0 ) GOTO 200
C
            CALL JELIRA (JEXNUM('&&RC3200.LES_GROUPES',NUMGR),
     +                                             'LONUTI',NBSIGR,K8B)
            CALL JEVEUO (JEXNUM('&&RC3200.LES_GROUPES',NUMGR),'L',JNSG)
C
            NPASS = 0
C
            DO 210 IS1 = 1 , NBSIGR
              IOC1 = ZI(JNSG+IS1-1)
              IF ( ZL(JCOMBI+IOC1-1) )  GOTO 210
C
              CALL JEEXIN (JEXNUM(K24AS,IG), IRET )
              IF ( IRET .EQ. 0 ) THEN
                 CALL JECROC (JEXNUM(K24AS,IG))
                 CALL JEECRA (JEXNUM(K24AS,IG),'LONMAX',10*NBSIGR,' ')
              ENDIF
              CALL JEVEUO ( JEXNUM(K24AS,IG), 'E', JREAS )
              DO 212 J = 1 , 10
                 ZR(JREAS-1+10*(IS1-1)+J) = R8VIDE()
 212          CONTINUE
C
              CALL JEEXIN (JEXNUM(K24SS,IG), IRET )
              IF ( IRET .EQ. 0 ) THEN
                 CALL JECROC (JEXNUM(K24SS,IG))
                 CALL JEECRA (JEXNUM(K24SS,IG),'LONMAX',10*NBSIGR,' ')
              ENDIF
              CALL JEVEUO ( JEXNUM(K24SS,IG), 'E', JRESS )
C   
              IF ( .NOT.CFAIT .AND. NIV.GE.2 ) THEN
                CFAIT = .TRUE.
                WRITE(IFM,*) ' '
                WRITE(IFM,*)
     +   '=> ON TRAITE LES SITUATIONS NON COMBINABLES DANS LEUR GROUPE'
              ENDIF
              IF ( NIV.GE.2 )  WRITE (IFM,2000) IG, IOC1
C
              NSITUP = ZI(JNSITU+IOC1-1)
C
              NOCC = ZI(JNBOCC+2*IOC1-2)
C
              PPI = ZR(JPRESA+IOC1-1)
              CALL RCMO02 ( 'A', NSITUP, MPI )
              CALL RCMA02 ( 'A', IOC1, MATPI )
C
              PPJ = ZR(JPRESB+IOC1-1)
              CALL RCMO02 ( 'B', NSITUP, MPJ )
              CALL RCMA02 ( 'B', IOC1, MATPJ )
C
              NSITUQ = 0
C
C ----------- CALCUL DU PM_PB
C
              IF ( LPMPB ) THEN
                 PM = 0.D0
                 PB = 0.D0
                 PMPB = 0.D0
                 CALL RC32PM ( LIEU(IM), SEISME, PPI, MPI, 
     +                                            MSE, PM, PB, PMPB )
                 CALL RC32PM ( LIEU(IM), SEISME, PPJ, MPJ,
     +                                            MSE, PM, PB, PMPB )
                 ZR(JRESS-1+10*(IS1-1)+1) = PM
                 ZR(JRESS-1+10*(IS1-1)+2) = PB
                 ZR(JRESS-1+10*(IS1-1)+3) = PMPB
                 IF (NIV.GE.2) THEN
                    WRITE (IFM,2020) NSITUP, PM, PB, PMPB
                 END IF
                 PMMAX  = MAX (  PMMAX , PM   )
                 PBMAX  = MAX (  PBMAX , PB   )
                 PMBMAX = MAX ( PMBMAX , PMPB )
              ENDIF
C
C ----------- CALCUL DU SN
C
              IF ( LSN ) THEN
                 SN = 0.D0
                 CALL RC32SN ( 'SN_SITU', LIEU(IM), NSITUP, PPI, MPI, 
     +                         NSITUQ, PPJ, MPJ, SEISME, MSE, SN )
                 SNMAX = MAX ( SNMAX , SN )
                 ZR(JRESS-1+10*(IS1-1)+4) = SN
                 IF (NIV.GE.2) THEN
                    WRITE (IFM,2030) NSITUP, SN
                 END IF
              ENDIF
C
C ----------- CALCUL DU SN*
C
              IF ( LSN .AND. LSNET ) THEN
                 SNET = 0.D0
                 CALL RC32SN ( 'SN*_SITU', LIEU(IM), NSITUP, PPI, MPI, 
     +                         NSITUQ, PPJ, MPJ, SEISME, MSE, SNET )
                 SNEMAX = MAX ( SNEMAX , SNET )
                 ZR(JRESS-1+10*(IS1-1)+5) = SNET
                 IF (NIV.GE.2) THEN
                    WRITE (IFM,2032) NSITUP, SNET
                 END IF
              ENDIF
C
C ----------- CALCUL DU ROCHET THERMIQUE
C
              IF ( LROCHT ) THEN
                 CALL RC32RT ( LIEU(IM), PPI, PPJ, SIMPIJ )
                 SIPMAX = MAX ( SIPMAX, SIMPIJ )
                 WRITE (IFM,2034) NSITUP, SIMPIJ
              ENDIF
C
              IF ( .NOT.LFATIG ) GOTO 210
C
C ----------- CALCUL DU SP
C
              SP(1) = 0.D0
              SP(2) = 0.D0
              SPTHER = 0.D0
              TYPEKE=MATPI(8)
              CALL RC32SP ( 'SP_SITU', LIEU(IM), NSITUP, PPI, MPI, 
     +                      NSITUQ, PPJ, MPJ, SEISME, MSE, 
     +                      SP, TYPEKE, SPMECA, SPTHER )
              SPTHEM = MAX ( SPTHEM , SPTHER )
              SPMAX = MAX ( SPMAX , SP(1) )
              IF (NIV.GE.2) WRITE (IFM,2040) NSITUP, SP(1)
              ZR(JRESS-1+10*(IS1-1)+6) = SP(1)
C
C ----------- CALCUL DU SALT
C
              CALL RC32SA ( 'SITU',MATER, MATPI, MATPJ, SN, SP, TYPEKE,
     &                    SPMECA, SPTHER, KEMECA, KETHER, SALTIJ, SMM )
              KEMAX = MAX ( KEMAX , KEMECA )
              IF (NIV.GE.2) THEN
                 WRITE (IFM,2050) NSITUP, SALTIJ
              END IF
              ZR(JRESS-1+10*(IS1-1)+7) = KEMECA
              ZR(JRESS-1+10*(IS1-1)+8) = KETHER
              ZR(JRESS-1+10*(IS1-1)+9) = SALTIJ
C
              IF ( SALTIJ .GT. SAMAX ) THEN
                 SAMAX = SALTIJ
                 SM = SMM
              ENDIF
C
C ----------- CALCUL DU FACTEUR D'USAGE
C
              CALL LIMEND( MATER,SALTIJ,'WOHLER',ENDUR)
              IF ( ENDUR ) THEN
                 UG=0.D0
              ELSE
                 CALL RCVALE(MATER, 'FATIGUE', 1, 'SIGM    ', SALTIJ, 1,
     +                                   'WOHLER  ', NADM, CODRET, 'F ')
                 IF ( NADM .LT. 0 ) THEN
                    VALE(1) = SALTIJ
                    VALE(2) = NADM
                    CALL U2MESG('A', 'POSTRCCM_32',0,' ',0,0,2,VALE)
                 ENDIF
                 UG = DBLE( NOCC ) / NADM
              ENDIF
              ZR(JRESS-1+10*(IS1-1)+10) = UG
              IF (NIV.GE.2) THEN
                 WRITE (IFM,2060) NSITUP, UG
              END IF
              UTOT = UTOT + UG
C
 210        CONTINUE
C
 200     CONTINUE
C
C ----------------------------------------------------------------------
C                           E T A P E   3 
C ----------------------------------------------------------------------
C
C ------ ON TRAITE LES SITUATIONS DE PASSAGE
C        -----------------------------------
C
         DO 310 IG = 1 , NBGR
C
            NUMGR = ZI(JNUMGR+IG-1)
            IF ( NUMGR .GE. 0 ) GOTO 310
            NUMGR = -NUMGR
            IOCS  = ZI(JSEIGR+IG-1)
            IF ( IOCS .EQ. 0 ) THEN
               SEISME = .FALSE.
            ELSE
               SEISME = .TRUE.
            ENDIF
C
            CALL JELIRA(JEXNUM('&&RC3200.LES_GROUPES',NUMGR),'LONUTI',
     +                                                    NBSIGR, K8B )
            CALL JEVEUO(JEXNUM('&&RC3200.LES_GROUPES',NUMGR),'L',JNSG)
            IF (NIV.GE.2) THEN
               WRITE (IFM,3004)
               WRITE (IFM,3002) (ZI(JNSITU+ZI(JNSG+I1-1)-1),I1=1,NBSIGR)
            END IF
C
            CALL JECROC (JEXNUM(K24AS,IG))
            CALL JEECRA (JEXNUM(K24AS,IG), 'LONMAX', 10*NBSIGR, ' ' )
            CALL JEVEUO (JEXNUM(K24AS,IG), 'E', JREAS )
C
            CALL JECROC (JEXNUM(K24SS,IG))
            CALL JEECRA (JEXNUM(K24SS,IG), 'LONMAX', 10*NBSIGR, ' ' )
            CALL JEVEUO (JEXNUM(K24SS,IG), 'E', JRESS )
C
            NDIM = MAX(4,4*NBSIGR*(NBSIGR-1)/2)
            CALL JECROC (JEXNUM(K24CA,IG))
            CALL JEECRA (JEXNUM(K24CA,IG), 'LONMAX', NDIM, ' ' )
            CALL JEVEUO (JEXNUM(K24CA,IG), 'E', JRECA )
C
            CALL JECROC (JEXNUM(K24CS,IG))
            CALL JEECRA (JEXNUM(K24CS,IG), 'LONMAX', NDIM, ' ' )
            CALL JEVEUO (JEXNUM(K24CS,IG), 'E', JRECS )
C
            CALL JECROC (JEXNUM(K24FU,IG))
            CALL JEECRA (JEXNUM(K24FU,IG), 'LONMAX', 4*50, ' ' )
            CALL JEVEUO (JEXNUM(K24FU,IG), 'E', JFACT )
C
            NPASS = 7
C  
            CALL RC3201 ( LPMPB, LSN, LSNET, LFATIG, LROCHT, LIEU(IM),
     +                    NUMGR, IOCS, SEISME, NPASS, MATER, SNMAX,
     +                    SNEMAX, SPMAX, KEMAX, SPMECM, SPTHEM, SAMAX,  
     +                    UTOT, SM, SIPMAX, ZR(JREAS), ZR(JRESS),
     +                    ZR(JRECA), ZR(JRECS), ZR(JFACT), PMMAX,
     +                    PBMAX, PMBMAX )
C
 320        CONTINUE
C
 310     CONTINUE
C
C ----------------------------------------------------------------------
C
C ------ ON STOCKE LES RESULTATS DE CALCUL
C        ---------------------------------
C
         CALL WKVECT ( '&&RC3200.RESULTAT  .'//LIEU(IM), 'V V R',
     +                                                  13, JRESU )
C        - LE PM
         ZR(JRESU  ) = PMMAX
C        - LE PB
         ZR(JRESU+1) = PBMAX
C        - LE PMPB
         ZR(JRESU+2) = PMBMAX
C        - LE SM
         ZR(JRESU+3) = SM
C        - LE SN/3SM
         IF ( SM .EQ. 0.D0 ) THEN
            ZR(JRESU+4) = 0.D0
         ELSE
            ZR(JRESU+4) = SNMAX / ( 3 * SM )
         ENDIF
C        - LE SN
         ZR(JRESU+5) = SNMAX
C        - LE SN*
         ZR(JRESU+6) = SNEMAX
C        - LE SP
         ZR(JRESU+7) = SPMAX
C        - LE KE
         ZR(JRESU+8) = KEMAX
C        - LE SALT
         ZR(JRESU+9) = SAMAX
C        - LE FACTEUR D'USAGE TOTAL
         ZR(JRESU+10) = UTOT
C        - LE ROCHET THERMIQUE
         ZR(JRESU+11) = SIPMAX
         ZR(JRESU+12) = SPTHEM
C
         IF ( LFATIG )  WRITE (IFM,2070) UTOT
C
 10   CONTINUE
C
 3000 FORMAT (/,'=> GROUPE: ',I4,' , NOMBRE DE SITUATIONS: ',I4)
 3002 FORMAT ('=> LISTE DES NUMEROS DE SITUATION: ',100 (I4,1X))
 3004 FORMAT (/,'=> SITUATION DE PASSAGE')
 2000 FORMAT ('=> GROUPE: ',I4,' , SITUATION: ',I4)
 2010 FORMAT ('=> PASSAGE DU GROUPE: ',I4,' AU GROUPE: ',I4)
 2020 FORMAT (1P,' SITUATION ',I4,' PM =',E12.5,
     +                            ' PB =',E12.5,' PMPB =',E12.5)
 2030 FORMAT (1P,' SITUATION ',I4,' SN =',E12.5 )
 2032 FORMAT (1P,' SITUATION ',I4,' SN* =',E12.5 )
 2034 FORMAT (1P,' SITUATION ',I4,' ROCHET THERMIQUE =',E12.5 )
 2040 FORMAT (1P,' SITUATION ',I4,' SP =',E12.5)
 2050 FORMAT (1P,' SITUATION ',I4,' SALT =',E12.5)
 2060 FORMAT (1P,' SITUATION ',I4,' FACT_USAGE =',E12.5)
 2070 FORMAT (1P,' SOMME(FACT_USAGE) =',E12.5)
C
      CALL JEDEMA( )
      END
