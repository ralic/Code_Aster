      SUBROUTINE RC32AC ( OPMPB, OSN, OSNET, OFATIG, MATER )
      IMPLICIT   NONE
      LOGICAL             OPMPB, OSN, OSNET, OFATIG
      CHARACTER*8         MATER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 13/03/2006   AUTEUR CIBHHLV L.VIVAN 
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
     +             JCOMBI, JPRESA, JPRESB, JNBOCC, IM, JNUMGR, JPASSA,
     +             NPASS, NUM1, NUM2, IFM, NIV, IOCS, JSEIGR, JRESU,
     +             JNSITU, NSITUP, NSITUQ, JPMPB, IRET, I1, JFACT, I
      REAL*8       PPI, PPJ, SNMAX, SPMAX, SAMAX, UTOT, SALTIJ,
     +             UG, NADM, MPI(12), MPJ(12), SM, SN, SNET, SP, SMM,
     +             MATPI(8), MATPJ(8), MSE(12),TYPEKE,SPMECA,SPTHER,
     +             SPTHEM,SPMECM,KEMECA,KETHER, PM, PB, PMPB
      LOGICAL      SEISME, ENDUR, CFAIT
      CHARACTER*2  CODRET
      CHARACTER*4  LIEU(2)
      CHARACTER*8  K8B
      CHARACTER*24 K24B, K24T
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
      CALL JEVEUO ( '&&RC3200.SITU_PASSAGE'   , 'L', JPASSA )
C
C --- IL FAUT CALCULER LE FACTEUR D'USAGE A CHAQUE EXTREMITE
C
      DO 10 IM = 1 , 2
C
         K24B = '&&RC3200.PMPB       '//LIEU(IM)
         CALL JECREC(K24B, 'V V R', 'NU', 'DISPERSE', 'VARIABLE', NBGR)
C
         K24T = '&&RC3200.FACT_USAGE '//LIEU(IM)
         CALL JECREC(K24T, 'V V R', 'NU', 'DISPERSE', 'VARIABLE', NBGR)
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
         SM    = 0.D0
         SNMAX = 0.D0
         SPMAX = 0.D0
         SPMECM = 0.D0
         SPTHEM = 0.D0
         SAMAX = 0.D0
         UTOT  = 0.D0
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
            IOCS  = ZI(JSEIGR+IG-1)
C
            CALL JELIRA(JEXNUM('&&RC3200.LES_GROUPES',NUMGR),'LONMAX',
     +                                                    NBSIGR, K8B )
            CALL JECROC (JEXNUM(K24B,IG))
            CALL JEECRA (JEXNUM(K24B,IG), 'LONMAX', 6*NBSIGR, ' ' )
            CALL JEVEUO (JEXNUM(K24B,IG), 'E', JPMPB )
C
            CALL JECROC (JEXNUM(K24T,IG))
            CALL JEECRA (JEXNUM(K24T,IG), 'LONMAX', 4*50, ' ' )
            CALL JEVEUO (JEXNUM(K24T,IG), 'E', JFACT )
C
            NPASS = 0
            IF ( IOCS .EQ. 0 ) THEN
               SEISME = .FALSE.
            ELSE
               SEISME = .TRUE.
            ENDIF
C
            CALL RC3201 ( OPMPB, OSN, OSNET, OFATIG, LIEU(IM), NUMGR,
     +              IOCS, SEISME, NPASS, MATER, SNMAX, SPMAX, SPMECM,
     +                    SPTHEM, SAMAX, UTOT, SM, ZR(JPMPB),ZR(JFACT))
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
C
            CALL JELIRA (JEXNUM('&&RC3200.LES_GROUPES',NUMGR),
     +                                             'LONMAX',NBSIGR,K8B)
            CALL JEVEUO (JEXNUM('&&RC3200.LES_GROUPES',NUMGR),'L',JNSG)
C
            NPASS = 0
C
            DO 210 IS1 = 1 , NBSIGR
              IOC1 = ZI(JNSG+IS1-1)
              IF ( ZL(JCOMBI+IOC1-1) )  GOTO 210
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
              IF ( OPMPB ) THEN
                 PM = 0.D0
                 PB = 0.D0
                 PMPB = 0.D0
                 CALL JEEXIN (JEXNUM(K24B,IG), IRET )
                 IF ( IRET .EQ. 0 ) THEN
                    CALL JECROC (JEXNUM(K24B,IG))
                    CALL JEECRA (JEXNUM(K24B,IG),'LONMAX',6*NBSIGR,' ')
                 ENDIF
                 CALL JEVEUO ( JEXNUM(K24B,IG), 'E', JPMPB )
                 CALL RC32PM ( LIEU(IM), SEISME, PPI, MPI, 
     +                                            MSE, PM, PB, PMPB )
                 CALL RC32PM ( LIEU(IM), SEISME, PPJ, MPJ,
     +                                            MSE, PM, PB, PMPB )
                 ZR(JPMPB-1+6*(IS1-1)+1) = PM
                 ZR(JPMPB-1+6*(IS1-1)+2) = PB
                 ZR(JPMPB-1+6*(IS1-1)+3) = PMPB
                 IF (NIV.GE.2) THEN
                    WRITE (IFM,2020) NSITUP, PM, PB, PMPB
                 END IF
              ENDIF
C
C ----------- CALCUL DU SN
C
              IF ( OSN ) THEN
                 SN = 0.D0
                 CALL RC32SN ( 'SN_SITU', LIEU(IM), NSITUP, PPI, MPI, 
     +                         NSITUQ, PPJ, MPJ, SEISME, MSE, SN )
                 SNMAX = MAX ( SNMAX , SN )
                 CALL JEEXIN (JEXNUM(K24B,IG), IRET )
                 IF ( IRET .EQ. 0 ) THEN
                    CALL JECROC (JEXNUM(K24B,IG))
                    CALL JEECRA (JEXNUM(K24B,IG),'LONMAX',6*NBSIGR,' ')
                 ENDIF
                 CALL JEVEUO ( JEXNUM(K24B,IG), 'E', JPMPB )
                 ZR(JPMPB-1+6*(IS1-1)+4) = SN
                 IF (NIV.GE.2) THEN
                    WRITE (IFM,2030) NSITUP, SN
                 END IF
              ENDIF
C
C ----------- CALCUL DU SN*
C
              IF ( OSN .AND. OSNET ) THEN
                 SNET = 0.D0
                 CALL RC32SN ( 'SN*_SITU', LIEU(IM), NSITUP, PPI, MPI, 
     +                         NSITUQ, PPJ, MPJ, SEISME, MSE, SNET )
                 CALL JEEXIN (JEXNUM(K24B,IG), IRET )
                 IF ( IRET .EQ. 0 ) THEN
                    CALL JECROC (JEXNUM(K24B,IG))
                    CALL JEECRA (JEXNUM(K24B,IG),'LONMAX',6*NBSIGR,' ')
                 ENDIF
                 CALL JEVEUO ( JEXNUM(K24B,IG), 'E', JPMPB )
                 ZR(JPMPB-1+6*(IS1-1)+6) = SNET
                 IF (NIV.GE.2) THEN
                    WRITE (IFM,2032) NSITUP, SNET
                 END IF
              ENDIF
C
              IF ( .NOT.OFATIG ) GOTO 210
C
C ----------- CALCUL DU SP
C
              SP = 0.D0
              TYPEKE=MATPI(8)
              CALL RC32SP ( 'SP_SITU', LIEU(IM), NSITUP, PPI, MPI, 
     +                      NSITUQ, PPJ, MPJ, SEISME, MSE, 
     +                      SP, TYPEKE, SPMECA, SPTHER )
              SPMAX = MAX ( SPMAX , SP )
              IF (NIV.GE.2) WRITE (IFM,2040) NSITUP, SP
C
C ----------- CALCUL DU SALT
C
              CALL RC32SA ( MATER, MATPI, MATPJ, SN, SP, TYPEKE,  
     &                    SPMECA, SPTHER, KEMECA, KETHER, SALTIJ, SMM )
              IF (NIV.GE.2) THEN
                 WRITE (IFM,2050) NSITUP, SALTIJ
              END IF
C
              IF ( SALTIJ .GT. SAMAX ) THEN
                 SAMAX = SALTIJ
                 SM = SMM
              ENDIF
C
C ----------- CALCUL DU FACTEUR D'USAGE
C
              CALL LIMEND( MATER,SALTIJ,'WOHLER',ENDUR)
              IF (ENDUR) THEN
                  UG=0.D0
              ELSE
                 CALL RCVALE ( MATER, 'FATIGUE', 1, 'SIGM', SALTIJ, 1,
     +                                   'WOHLER', NADM, CODRET, 'F ' )
         IF ( NADM .LT. 0 ) THEN
            CALL UTDEBM ('A','WOHLER','NOMBRE DE CYCLES ADMISSIBLES'//
     +                       ' NEGATIF, VERIFIER LA COURBE DE WOHLER')
            CALL UTIMPR ('L','   CONTRAINTE CALCULEE = ',1,SALTIJ)
            CALL UTIMPR ('L','   NADM = ',1,NADM)
            CALL UTFINM ()
         ENDIF
C
                 UG = DBLE( NOCC ) / NADM
              ENDIF
              ZR(JPMPB-1+6*(IS1-1)+5) = UG
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
         CFAIT = .FALSE.
         DO 310 IG = 1 , NBGR
C
            NUMGR = ZI(JNUMGR+IG-1)
C
            CALL JELIRA (JEXNUM('&&RC3200.LES_GROUPES',NUMGR),
     +                                             'LONMAX',NBSIGR,K8B)
            CALL JEVEUO (JEXNUM('&&RC3200.LES_GROUPES',NUMGR),'L',JNSG)
C
            DO 320 IS = 1 , NBSIGR
              IOC1 = ZI(JNSG+IS-1)
              IF ( ZI(JPASSA+2*IOC1-2).EQ.0 .AND.
     +             ZI(JPASSA+2*IOC1-1).EQ.0 )  GOTO 320
C   
              IF ( .NOT.CFAIT .AND. NIV.GE.2 ) THEN
                CFAIT = .TRUE.
                WRITE(IFM,*) ' '
                WRITE(IFM,*) '=> ON TRAITE LES SITUATIONS DE PASSAGE'
              ENDIF
C
              NUM1 = ZI(JPASSA+2*IOC1-2)
              NUM2 = ZI(JPASSA+2*IOC1-2)
              IF ( NIV.GE.2 )  WRITE (IFM,2010) NUM1, NUM2
C
              NPASS = ZI(JNBOCC+2*IOC1-2)
C
              CALL RC3203 ( LIEU(IM), NUM1, NUM2, NPASS,
     +              MATER, SNMAX, SPMAX,SPMECM,SPTHEM, SAMAX, UTOT, SM )
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
     +                                                   6, JRESU )
C        - LE SM
         ZR(JRESU  ) = SM
C        - LE SN/3SM
         IF ( SM .EQ. 0.D0 ) THEN
            ZR(JRESU+1) = 0.D0
         ELSE
            ZR(JRESU+1) = SNMAX / ( 3 * SM )
         ENDIF
C        - LE SN
         ZR(JRESU+2) = SNMAX
C        - LE SP
         ZR(JRESU+3) = SPMAX
C        - LE SALT
         ZR(JRESU+4) = SAMAX
C        - LE U_TOTAL
         ZR(JRESU+5) = UTOT
C
         IF ( OFATIG )  WRITE (IFM,2070) UTOT
C
 10   CONTINUE
C
 2000 FORMAT ('=> GROUPE: ',I4,' , SITUATION: ',I4)
 2010 FORMAT ('=> PASSAGE DU GROUPE: ',I4,' AU GROUPE: ',I4)
 2020 FORMAT (1P,' SITUATION ',I4,' PM =',E12.5,
     +                            ' PB =',E12.5,' PMPB =',E12.5)
 2030 FORMAT (1P,' SITUATION ',I4,' SN =',E12.5 )
 2032 FORMAT (1P,' SITUATION ',I4,' SN* =',E12.5 )
 2040 FORMAT (1P,' SITUATION ',I4,' SP =',E12.5)
 2050 FORMAT (1P,' SITUATION ',I4,' SALT =',E12.5)
 2060 FORMAT (1P,' SITUATION ',I4,' FACT_USAGE =',E12.5)
 2070 FORMAT (1P,' SOMME(FACT_USAGE) =',E12.5)
 1000 FORMAT(A,A8,A,A8)
 1010 FORMAT(1P,' COMBINAISON P ',I4,' SN =',E12.5,' SP =',E12.5)
 1020 FORMAT(1P,' COMBINAISON P Q ',I4,I4,' SN =',E12.5)
 1031 FORMAT(1P,'                 I I ',' SP =',E12.5)
 1032 FORMAT(1P,'                 I J ',' SP =',E12.5)
 1033 FORMAT(1P,'                 J J ',' SP =',E12.5)
 1034 FORMAT(1P,'                 J I ',' SP =',E12.5)
C
      CALL JEDEMA( )
      END
