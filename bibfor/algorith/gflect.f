      SUBROUTINE GFLECT ( NOMCHA, CHGRFL )
      IMPLICIT NONE
      CHARACTER*8         NOMCHA
      CHARACTER*24        CHGRFL
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/10/2003   AUTEUR BOYERE E.BOYERE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_20
C ----------------------------------------------------------------------
C.========================= DEBUT DES DECLARATIONS ====================
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
      INTEGER      I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12,
     +             I13, I14, I15, I16, I17, I18, I19, I20, I21, I22,
     +             II, JFFL, JIFL, INO
      INTEGER      IDRDH, IDRGR, IDRMC, IDRMA, IDRTG, IDRAS, IDRPC,
     +             IDVDIR, IDGEOM, IDABSC, NBNO, IDLINO
      REAL*8       Q, ROC, ROD, ROP, ROM, ROML, ROG, NUC, NUM, NUML,
     +             NUG, P2, P3, P4, G
      REAL*8       M, DTIGE, DTM, ROTIGE, LTIGE, LLT, LCT, VARAI,
     +             ROARAI, DCRAY , ROCRAY, LCRAY, LCHUT, CFCM, CFCI,
     +             CFCG, HRUGC, HRUGTC, Z0
      REAL*8       LI, LML, LG, LIG, DIML, DEML, DCSP, DG, HRUGML,
     +             HRUGSP, HRUGG
      REAL*8       LM, LA, LIM, DIMT, DEMT, DCMT, VMT, ROMT, DA, HRUGM,
     +             HRUGA
      REAL*8       L0, L1, L2, L3, L4, DTG, DR, 
     +             DOR, D0, D00, HRUGTG
      REAL*8       SASS, DCC, DTI, NGM, NGMDP, KM, KS, KI, KES, KEI,
     +             KF
      REAL*8       CD0, CD1, CD2, CDELG, CDRET, CDM, CDA, CDML, CDI,
     +             CDG
      REAL*8       ROA, ROI, NUA, NUI, LDASH, SPASS, P0, 
     +             A, A1, AC, AR, A0, AA0, AM, AA, AI, AML, AG, AT, P1,
     +             AMT, ACMT, DH, DHR, DH0, DHM, DHA, DHI, DHML, DHG
      REAL*8       CGG, NRET
      REAL*8       DEMI, QUATRE, R8PI, PI, PIS4
      CHARACTER*8  K8BID
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
      CALL JEMARQ()
C
      DEMI   = 0.5D0
      QUATRE = 4.0D0
      PI     = R8PI()
      PIS4   = PI / QUATRE
C
C --- RECUPERATION DES DONNEES HYDRAULIQUES :
C     -------------------------------------
      CALL JEVEUO ( NOMCHA//'.CHME.GF_DH.VALE', 'L', IDRDH )
      Q      = ZR(IDRDH+ 1-1)
      ROC    = ZR(IDRDH+ 2-1)
      ROD    = ZR(IDRDH+ 3-1)
      ROP    = ZR(IDRDH+ 4-1)
      ROM    = ZR(IDRDH+ 5-1)
      ROML   = ZR(IDRDH+ 6-1)
      ROG    = ZR(IDRDH+ 7-1)
      NUC    = ZR(IDRDH+ 8-1)
      NUM    = ZR(IDRDH+ 9-1)
      NUML   = ZR(IDRDH+10-1)
      NUG    = ZR(IDRDH+11-1)
      P2     = ZR(IDRDH+12-1)
      P3     = ZR(IDRDH+13-1)
      P4     = ZR(IDRDH+14-1)
      CGG    = ZR(IDRDH+15-1)
      G      = ZR(IDRDH+16-1)
C
C --- RECUPERATION DES DONNEES GEOMETRIQUES GRAPPES :
C     ---------------------------------------------
      CALL JEVEUO ( NOMCHA//'.CHME.GF_GR.VALE', 'L', IDRGR )
      M      = ZR(IDRGR+ 1-1)
      DTIGE  = ZR(IDRGR+ 2-1)
      DTM    = ZR(IDRGR+ 3-1)
      ROTIGE = ZR(IDRGR+ 4-1)
      LTIGE  = ZR(IDRGR+ 5-1)
      LLT    = ZR(IDRGR+ 6-1)
      LCT    = ZR(IDRGR+ 7-1)
      VARAI  = ZR(IDRGR+ 8-1)
      ROARAI = ZR(IDRGR+ 9-1)
      DCRAY  = ZR(IDRGR+10-1)
      ROCRAY = ZR(IDRGR+11-1)
      LCRAY  = ZR(IDRGR+12-1)
      LCHUT  = ZR(IDRGR+13-1)
      CFCM   = ZR(IDRGR+14-1)
      CFCI   = ZR(IDRGR+15-1)
      CFCG   = ZR(IDRGR+16-1)
      HRUGC  = ZR(IDRGR+17-1)
      HRUGTC = ZR(IDRGR+18-1)
      Z0     = ZR(IDRGR+20-1)
C
C --- RECUPERATION DES DONNEES GEOMETRIQUES MECANISME DE COMMANDE :
C     -----------------------------------------------------------
      CALL JEVEUO ( NOMCHA//'.CHME.GF_MC.VALE', 'L', IDRMC )
      LI     = ZR(IDRMC+ 1-1)
      LML    = ZR(IDRMC+ 2-1)
      LG     = ZR(IDRMC+ 3-1)
      LIG    = ZR(IDRMC+ 4-1)
      DIML   = ZR(IDRMC+ 5-1)
      DEML   = ZR(IDRMC+ 6-1)
      DCSP   = ZR(IDRMC+ 7-1)
      DG     = ZR(IDRMC+ 8-1)
      HRUGML = ZR(IDRMC+ 9-1)
      HRUGSP = ZR(IDRMC+10-1)
      HRUGG  = ZR(IDRMC+11-1)
C
C --- RECUPERATION DES DONNEES GEOMETRIQUES MANCHETTE ET ADAPTATEUR :
C     -------------------------------------------------------------
      CALL JEVEUO ( NOMCHA//'.CHME.GF_MA.VALE', 'L', IDRMA )
      LM     = ZR(IDRMA+ 1-1)
      LA     = ZR(IDRMA+ 2-1)
      LIM    = ZR(IDRMA+ 3-1)
      DIMT   = ZR(IDRMA+ 4-1)
      DEMT   = ZR(IDRMA+ 5-1)
      DCMT   = ZR(IDRMA+ 6-1)
      VMT    = ZR(IDRMA+ 7-1)
      ROMT   = ZR(IDRMA+ 8-1)
      DA     = ZR(IDRMA+ 9-1)
      HRUGM  = ZR(IDRMA+10-1)
      HRUGA  = ZR(IDRMA+11-1)
C
C --- RECUPERATION DES DONNEES GEOMETRIQUES TUBES GUIDES :
C     --------------------------------------------------
      CALL JEVEUO ( NOMCHA//'.CHME.GF_TG.VALE', 'L', IDRTG )
      NRET   = ZR(IDRTG+ 1-1)
      L0     = ZR(IDRTG+ 2-1)
      L1     = ZR(IDRTG+ 3-1)
      L2     = ZR(IDRTG+ 4-1)
      L3     = ZR(IDRTG+ 5-1)
      L4     = ZR(IDRTG+ 6-1)
      DTG    = ZR(IDRTG+ 7-1)
      DR     = ZR(IDRTG+ 8-1)
      DOR    = ZR(IDRTG+ 9-1)
      D0     = ZR(IDRTG+10-1)
      D00    = ZR(IDRTG+11-1)
      HRUGTG = ZR(IDRTG+12-1)
C
C --- RECUPERATION DES DONNEES GEOMETRIQUES ASSEMBLAGES : 
C     -------------------------------------------------
      CALL JEVEUO ( NOMCHA//'.CHME.GF_AS.VALE', 'L', IDRAS )
      SASS   = ZR(IDRAS+ 1-1)
      DCC    = ZR(IDRAS+ 2-1)
      DTI    = ZR(IDRAS+ 3-1)
      NGM    = ZR(IDRAS+ 4-1)
      NGMDP  = ZR(IDRAS+ 5-1)
      KM     = ZR(IDRAS+ 6-1)
      KS     = ZR(IDRAS+ 7-1)
      KI     = ZR(IDRAS+ 8-1)
      KES    = ZR(IDRAS+ 9-1)
      KEI    = ZR(IDRAS+10-1)
      KF     = ZR(IDRAS+11-1)
C
C --- RECUPERATION DES COEFFICIENTS DE PERTE DE CHARGE SINGULIERE :
C     -----------------------------------------------------------
      CALL JEVEUO ( NOMCHA//'.CHME.GF_PC.VALE', 'L', IDRPC )
      CD0    = ZR(IDRPC+ 1-1)
      CD1    = ZR(IDRPC+ 2-1)
      CD2    = ZR(IDRPC+ 3-1)
      CDELG  = ZR(IDRPC+ 4-1)
      CDRET  = ZR(IDRPC+ 5-1)
      CDM    = ZR(IDRPC+ 6-1)
      CDA    = ZR(IDRPC+ 7-1)
      CDML   = ZR(IDRPC+ 8-1)
      CDI    = ZR(IDRPC+ 9-1)
      CDG    = ZR(IDRPC+10-1)
C
C --- DONNEES HYDRAULIQUES
C
      ROA = ROM
      ROI = ROML
      NUA = NUM
      NUI = NUM
      LDASH = L2 + L3
C
C --- CALCUL DES PRESSIONS P0 ET P1 EN FONCTION DU DEBIT DU COEUR ET 
C     DE LA PRESSION P2 :
C
      SPASS = SASS - PIS4*(264*DCC**2 + DTI**2)
      P0 = P2+DEMI*ROC*(Q/SPASS)**2*(KS+KI+KES+KEI+NGM*KM+KF)
      P1 = P2+DEMI*ROC*(Q/SPASS)**2*(KS+KES+(NGM-NGMDP)*KM+
     +                                      (L1-L4)/(L1+LDASH)*KF)
C
C --- CALCUL DES SECTIONS DE PASSAGE DU FLUIDE :
C
      A    = PIS4*(DTG**2 - DCRAY**2)
      A1   = PI*DOR**2
      AC   = PIS4*DCRAY**2
      AR   = PIS4*(DR**2 - DCRAY**2)
      A0   = PIS4*D0**2
      AA0  = PIS4*D00**2
      AM   = PIS4*(DIMT**2 - DTIGE**2)
      AA   = PIS4*(DA**2 - DEMT**2)
      AI   = PIS4*(DIML**2 - DTIGE**2)
      AML  = PIS4*(DCSP**2 - DEML**2)
      AG   = PIS4*(DG**2 - DTIGE**2)
      AT   = PIS4*DTIGE**2
      AMT  = PIS4*(DCMT**2 - DIMT**2)
      ACMT = PIS4*(DCMT**2 - DEMT**2)
      DH   = DTG - DCRAY
      DHR  = DR - DCRAY
      DH0  = D0
      DHM  = DIMT - DTIGE
      DHA  = DA - DEMT
      DHI  = DIML - DTIGE
      DHML = DCSP - DEML
      DHG  = DG - DTIGE
C
C --- RECUPERATION DU NOMBRE DE NOEUDS MODELISANT LA GRAPPE :
C     -----------------------------------------------------
      CALL JELIRA (NOMCHA//'.CHME.GRFLU.LINO','LONMAX',NBNO,K8BID)
      CALL JEVEUO (NOMCHA//'.CHME.GRFLU.LINO','L',IDLINO)
C
C
C --- INITIALISATION DES "COMMON" PEGASE :
C     ----------------------------------
C
      CALL WKVECT ( CHGRFL, 'V V R', 2*NBNO+1000, JFFL ) 
      CALL WKVECT ( '&&GFLECT.INDICE', 'V V I', NBNO+30, JIFL ) 
C
C
CCC      COMMON/CARTER/Li,Lml,Lig,deml,diml,dcsp,roml,numl,Aml,Ai,Dhml,
CCC                    Dhi,roi,nui
      ZR(JFFL-1+1) = LI
      ZR(JFFL-1+2) = LML
      ZR(JFFL-1+3) = LIG
      ZR(JFFL-1+4) = DEML
      ZR(JFFL-1+5) = DIML
      ZR(JFFL-1+6) = DCSP
      ZR(JFFL-1+7) = ROML
      ZR(JFFL-1+8) = NUML
      ZR(JFFL-1+9) = AML
      ZR(JFFL-1+10) = AI
      ZR(JFFL-1+11) = DHML
      ZR(JFFL-1+12) = DHI
      ZR(JFFL-1+13) = ROI
      ZR(JFFL-1+14) = NUI
      I1 = 14
C
CCC      COMMON/COEF_PDC1/Cd0,Cd1,Cd2,Cdelarg,Cdret,nret
      ZR(JFFL-1+I1+1) = CD0
      ZR(JFFL-1+I1+2) = CD1
      ZR(JFFL-1+I1+3) = CD2
      ZR(JFFL-1+I1+4) = CDELG
      ZR(JFFL-1+I1+5) = CDRET
      ZR(JFFL-1+I1+6) = NRET
      I2 = I1 + 6
C
CCC      COMMON/COEF_PDC2/Cdm,Cda,Cdml,Cdi,Cdg
      ZR(JFFL-1+I2+1) = CDM
      ZR(JFFL-1+I2+2) = CDA
      ZR(JFFL-1+I2+3) = CDML
      ZR(JFFL-1+I2+4) = CDI
      ZR(JFFL-1+I2+5) = CDG
      I3 = I2 + 5
C
CCC      COMMON/FLUID/roc,rod,rop,nuc,p0,p1,p2,Q
      ZR(JFFL-1+I3+1) = ROC
      ZR(JFFL-1+I3+2) = ROD
      ZR(JFFL-1+I3+3) = ROP
      ZR(JFFL-1+I3+4) = NUC
      ZR(JFFL-1+I3+5) = P0
      ZR(JFFL-1+I3+6) = P1
      ZR(JFFL-1+I3+7) = P2
      ZR(JFFL-1+I3+8) = Q
      I4 = I3 + 8
C
CCC      COMMON/FLUID2/rog0,roml0,roi0,roa0,rom0
      ZR(JFFL-1+I4+1) = 0.0D0
      ZR(JFFL-1+I4+2) = 0.0D0
      ZR(JFFL-1+I4+3) = 0.0D0
      ZR(JFFL-1+I4+4) = 0.0D0
      ZR(JFFL-1+I4+5) = 0.0D0
      I5 = I4 + 5
C
CCC      COMMON/FORCE/CGG
      ZR(JFFL-1+I5+1) = CGG
      I6 = I5 + 1
C
CCC      COMMON/GAINE/Lg,dg,rog,nug,Ag,Dhg,At
      ZR(JFFL-1+I6+1) = LG
      ZR(JFFL-1+I6+2) = DG
      ZR(JFFL-1+I6+3) = ROG
      ZR(JFFL-1+I6+4) = NUG
      ZR(JFFL-1+I6+5) = AG
      ZR(JFFL-1+I6+6) = DHG
      ZR(JFFL-1+I6+7) = AT
      I7 = I6 + 7
C
CCC      COMMON/GEOM1/dcray,dtg,dr,A,A0,A1,AA0,Ac,Ar,Dh,Dh0,Dhr,
CCC                   L0,L1,L2,L3
      ZR(JFFL-1+I7+1) = DCRAY
      ZR(JFFL-1+I7+2) = DTG
      ZR(JFFL-1+I7+3) = DR
      ZR(JFFL-1+I7+4) = A
      ZR(JFFL-1+I7+5) = A0
      ZR(JFFL-1+I7+6) = A1
      ZR(JFFL-1+I7+7) = AA0
      ZR(JFFL-1+I7+8) = AC
      ZR(JFFL-1+I7+9) = AR
      ZR(JFFL-1+I7+10) = DH
      ZR(JFFL-1+I7+11) = DH0
      ZR(JFFL-1+I7+12) = DHR
      ZR(JFFL-1+I7+13) = L0
      ZR(JFFL-1+I7+14) = L1
      ZR(JFFL-1+I7+15) = L2
      ZR(JFFL-1+I7+16) = L3
      I8 = I7 + 16
C
CCC      COMMON/GRAPPE/M,Ma,g,Lchut
      ZR(JFFL-1+I8+1) = M
      ZR(JFFL-1+I8+2) = 0.0D0
      ZR(JFFL-1+I8+3) = G
      ZR(JFFL-1+I8+4) = LCHUT
      I9 = I8 + 4
C
CCC      COMMON/GRAPPE2/dtige,rotige,Ltige,Varai,roarai,rocray,Lcray,
CCC                     dtm
      ZR(JFFL-1+I9+1) = DTIGE
      ZR(JFFL-1+I9+2) = ROTIGE
      ZR(JFFL-1+I9+3) = LTIGE
      ZR(JFFL-1+I9+4) = VARAI
      ZR(JFFL-1+I9+5) = ROARAI
      ZR(JFFL-1+I9+6) = ROCRAY
      ZR(JFFL-1+I9+7) = LCRAY
      ZR(JFFL-1+I9+8) = DTM
      I10 = I9 + 8
C
CCC      COMMON/MANCHETTE/Lm,La,Lim,dimt,demt,dcmt,romt,Vmt,num,Am,Aa,
CCC                       Dhm,Dha,rom,p3,p4,Amt,Acmt,da,roa,nua
      ZR(JFFL-1+I10+1) = LM
      ZR(JFFL-1+I10+2) = LA
      ZR(JFFL-1+I10+3) = LIM
      ZR(JFFL-1+I10+4) = DIMT
      ZR(JFFL-1+I10+5) = DEMT
      ZR(JFFL-1+I10+6) = DCMT
      ZR(JFFL-1+I10+7) = ROMT
      ZR(JFFL-1+I10+8) = VMT
      ZR(JFFL-1+I10+9) = NUM
      ZR(JFFL-1+I10+10) = AM
      ZR(JFFL-1+I10+11) = AA
      ZR(JFFL-1+I10+12) = DHM
      ZR(JFFL-1+I10+13) = DHA
      ZR(JFFL-1+I10+14) = ROM
      ZR(JFFL-1+I10+15) = P3
      ZR(JFFL-1+I10+16) = P4
      ZR(JFFL-1+I10+17) = AMT
      ZR(JFFL-1+I10+18) = ACMT
      ZR(JFFL-1+I10+19) = DA
      ZR(JFFL-1+I10+20) = ROA
      ZR(JFFL-1+I10+21) = NUA
      I11 = I10 + 21
C
CCC      COMMON/PARAM/pi,it,itdash,z0,imanch
      ZR(JFFL-1+I11+1) = PI
      ZR(JFFL-1+I11+2) = 0
      ZR(JFFL-1+I11+3) = 0
      ZR(JFFL-1+I11+4) = Z0
      ZR(JFFL-1+I11+5) = 0
      I12 = I11 + 5
C
CCC      COMMON/RUGOSITE/hrugc,hrugtc,hrugtg,hrugm,hruga,hrugml,
CCC                      hrugcsp,hrugg
      ZR(JFFL-1+I12+1) = HRUGC
      ZR(JFFL-1+I12+2) = HRUGTC
      ZR(JFFL-1+I12+3) = HRUGTG
      ZR(JFFL-1+I12+4) = HRUGM
      ZR(JFFL-1+I12+5) = HRUGA
      ZR(JFFL-1+I12+6) = HRUGML
      ZR(JFFL-1+I12+7) = HRUGSP
      ZR(JFFL-1+I12+8) = HRUGG
      I13 = I12 + 8
C
CCC      COMMON/TIGE/Llt,Lct,Cfcm,Cfci,Cfcg
      ZR(JFFL-1+I13+1) = LLT
      ZR(JFFL-1+I13+2) = LCT
      ZR(JFFL-1+I13+3) = CFCM
      ZR(JFFL-1+I13+4) = CFCI
      ZR(JFFL-1+I13+5) = CFCG
      I14 = I13 + 5
C
CCC      COMMON/VARIABLE1/z,dz,d2z,d2zm1,dt
      ZR(JFFL-1+I14+1) = 0.0D0
      ZR(JFFL-1+I14+2) = 0.0D0
      ZR(JFFL-1+I14+3) = 0.0D0
      ZR(JFFL-1+I14+4) = 0.0D0
      ZR(JFFL-1+I14+5) = 0.0D0
      I15 = I14 + 5
C
CCC      COMMON/MECANISME1/um,ua,uml,ui,ug,umm1,uam1,umlm1,uim1,pm,pa,
C                          pml,ps,Cfm,Cfg,Cfi,Cfa
      ZR(JFFL-1+I15+1) = 0.0D0
      ZR(JFFL-1+I15+2) = 0.0D0
      ZR(JFFL-1+I15+3) = 0.0D0
      ZR(JFFL-1+I15+4) = 0.0D0
      ZR(JFFL-1+I15+5) = 0.0D0
      ZR(JFFL-1+I15+6) = 0.0D0
      ZR(JFFL-1+I15+7) = 0.0D0
      ZR(JFFL-1+I15+8) = 0.0D0
      ZR(JFFL-1+I15+9) = 0.0D0
      ZR(JFFL-1+I15+10) = 0.0D0
      ZR(JFFL-1+I15+11) = 0.0D0
      ZR(JFFL-1+I15+12) = 0.0D0
      ZR(JFFL-1+I15+13) = 0.0D0
      ZR(JFFL-1+I15+14) = 0.0D0
      ZR(JFFL-1+I15+15) = 0.0D0
      ZR(JFFL-1+I15+16) = 0.0D0
      ZR(JFFL-1+I15+17) = 0.0D0
      I16 = I15 + 17
C
C      X : VITESSE ET PRESSION DU FLUIDE (GFGUID)
C
      ZR(JFFL-1+I16+1) = 0.0D0
      ZR(JFFL-1+I16+2) = 0.0D0
      ZR(JFFL-1+I16+3) = 0.0D0
      ZR(JFFL-1+I16+4) = 0.0D0
      ZR(JFFL-1+I16+5) = 0.0D0
      I17 = I16 + 5
C
C --- RECUPERATION DU VECTEUR CONTENANT LES DONNEES GEOMETRIQUES
C --- DECRIVANT LA GRAPPE :
C     -------------------
      CALL JEVEUO(NOMCHA//'.CHME.GRFLU.GEOM','L',IDGEOM)
      ZR(JFFL-1+I17+1) = ZR(IDGEOM+1-1)
      ZR(JFFL-1+I17+2) = ZR(IDGEOM+2-1)
      ZR(JFFL-1+I17+3) = ZR(IDGEOM+3-1)
      ZR(JFFL-1+I17+4) = ZR(IDGEOM+4-1)
      ZR(JFFL-1+I17+5) = ZR(IDGEOM+5-1)
      ZR(JFFL-1+I17+6) = ZR(IDGEOM+6-1)
      ZR(JFFL-1+I17+7) = ZR(IDGEOM+7-1)
      ZR(JFFL-1+I17+8) = ZR(IDGEOM+8-1)
      I18 = I17 + 8
C
C --- RECUPERATION DU VECTEUR UNITAIRE ORIENTANT LE CRAYON :
C     ----------------------------------------------------
      CALL JEVEUO(NOMCHA//'.CHME.GRFLU.VDIR','L',IDVDIR)
      ZR(JFFL-1+I18+1) = ZR(IDVDIR-1+1)
      ZR(JFFL-1+I18+2) = ZR(IDVDIR-1+2)
      ZR(JFFL-1+I18+3) = ZR(IDVDIR-1+3)
      I19 = I18 + 3
C
C     YY : VITESSE DU FLUIDE (DASHPOT)
C
      ZR(JFFL-1+I19+1) = 0.0D0
      ZR(JFFL-1+I19+2) = 0.0D0
      I20 = I19 + 2
C
C --- RECUPERATION DES ABSCISSES CURVILIGNES DE CES NOEUDS :
C     ----------------------------------------------------
      CALL JEVEUO(NOMCHA//'.CHME.GRFLU.ABSC','L',IDABSC)
      DO 10 INO = 1 , NBNO
         ZR(JFFL-1+I20+INO) = ZR(IDABSC-1+INO)
 10   CONTINUE
      I21 = I20 + NBNO
C
C --- VECTEUR DES ABSCISSES CURVILIGNES REACTUALISEES :
C     -----------------------------------------------
      DO 12 INO = 1 , NBNO
         ZR(JFFL-1+I21+INO) = 0.0D0
 12   CONTINUE
      I22 = I21 + NBNO
C
CCC      COMMON/POINTEUR/eps,eps0
      ZI(JIFL-1+1) = 0
      ZI(JIFL-1+2) = 0

C --- IT : NUMERO D'ITERATION
      ZI(JIFL-1+3) = 0

C --- ITDASH : NOMBRE D'APPEL A DASHPOT
      ZI(JIFL-1+4) = 0

      ZI(JIFL-1+5) = NBNO

      DO 20 INO = 1 , NBNO
         ZI(JIFL-1+5+INO) = ZI(IDLINO-1+INO)
 20   CONTINUE
C
      II = 5 + NBNO
      ZI(JIFL-1+II+1) = I1
      ZI(JIFL-1+II+2) = I2
      ZI(JIFL-1+II+3) = I3
      ZI(JIFL-1+II+4) = I4
      ZI(JIFL-1+II+5) = I5
      ZI(JIFL-1+II+6) = I6
      ZI(JIFL-1+II+7) = I7
      ZI(JIFL-1+II+8) = I8
      ZI(JIFL-1+II+9) = I9
      ZI(JIFL-1+II+10) = I10
      ZI(JIFL-1+II+11) = I11
      ZI(JIFL-1+II+12) = I12
      ZI(JIFL-1+II+13) = I13
      ZI(JIFL-1+II+14) = I14
      ZI(JIFL-1+II+15) = I15
      ZI(JIFL-1+II+16) = I16
      ZI(JIFL-1+II+17) = I17
      ZI(JIFL-1+II+18) = I18
      ZI(JIFL-1+II+19) = I19
      ZI(JIFL-1+II+20) = I20
      ZI(JIFL-1+II+21) = I21
      ZI(JIFL-1+II+22) = I22
C
      CALL JEDEMA()
C.============================ FIN DE LA ROUTINE ======================
      END
