      SUBROUTINE NDNPAS(FONACT,NUMEDD,NUMINS,SDDISC,SDSENS,
     &                  SDDYNA,DEFICO,RESOCO,VALMOI,VALPLU,
     &                  POUGD ,SOLALG)
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/06/2009   AUTEUR GREFFET N.GREFFET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_21 CRP_20
C
      IMPLICIT NONE
      INTEGER      NUMINS
      CHARACTER*24 POUGD(8),VALMOI(8),VALPLU(8)
      CHARACTER*24 DEFICO,RESOCO
      CHARACTER*24 SDSENS,NUMEDD
      CHARACTER*19 SDDYNA,SDDISC
      CHARACTER*19 SOLALG
      LOGICAL      FONACT(*)
C 
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (DYNAMIQUE)
C
C INITIALISATION DES CHAMPS D'INCONNUES POUR UN NOUVEAU PAS DE TEMPS
C      
C ----------------------------------------------------------------------
C
C    
C IN  FONACT : FONCTIONNALITES ACTIVEES
C IN  NUMEDD : NUME_DDL
C IN  NUMINS : NUMERO INSTANT COURANT
C IN  SDDISC : SD DISCRETISATION TEMPORELLE
C IN  SDSENS : SD SENSIBILITE
C IN  DEFICO : SD DEFINITION DU CONTACT
C IN  RESOCO : SD RESOLUTION DU CONTACT
C IN  SDDYNA : SD DYNAMIQUE 
C IN  VALMOI : VARIABLE CHAPEAU POUR ETAT EN T-
C IN  VALPLU : VARIABLE CHAPEAU POUR ETAT EN T+
C IN  POUGD  : VARIABLE CHAPEAU POUR POUTRES EN GRANDES ROTATIONS
C IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
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
      REAL*8       ZERO,UN,DEUX
      PARAMETER    (UN   = 1.D0,DEUX = 2.D0)
      PARAMETER    (ZERO = 0.D0)
C      
      INTEGER      JCFSC,JTFOR
      CHARACTER*24 TFOR,CFSC
      INTEGER      NDYNIN
      REAL*8       NDYNRE
      REAL*8       ALPHA,BETA,GAMMA,THETA,PHI,KAPPA   
      REAL*8       DIINST,INSTAM,INSTAP,DELTAT    
      LOGICAL      LEXGE,LCTCC,LMUAP,REAROT,LEXPL,LMPAS,LHHTC,LIMPL
      LOGICAL      NDYNLO,ISFONC
      CHARACTER*24 MDECOL
      INTEGER      JMDECO
      CHARACTER*8  K8BID
      LOGICAL      SCOTCH
      LOGICAL      LDEPL,LVITE,LACCE
      LOGICAL      LNEWMA,LTHETA,LKRENK
      LOGICAL      LCVITE
      REAL*8       COERIG,COEAMO,COEMAS
      REAL*8       COEEXT,COEINT,COEEQU,COEEX2  
      INTEGER      IRET,IMODE
      INTEGER      NEQ,NBMODP
      REAL*8       COEFD(3),COEFV(3),COEFA(3)         
      REAL*8       COEDEP,COEVIT,COEACC
      REAL*8       COERMA,COERAM,COERRI
      REAL*8       COINER,R8PREM,UNTHET
      CHARACTER*24 K24BID
      CHARACTER*24 DEPPLU,VITPLU,ACCPLU
      CHARACTER*24 DEPMOI,VITMOI,ACCMOI 
      CHARACTER*24 NDYNKK,SDPRMO
      CHARACTER*24 DEPGEM,VITGEM,ACCGEM,DEPGEP,VITGEP,ACCGEP
      INTEGER      JDEPGM,JVITGM,JACCGM,JDEPGP,JVITGP,JACCGP
      INTEGER      IFM,NIV         
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFDBG('MECA_NON_LINE',IFM,NIV)
C
C --- AFFICHAGE
C
      IF (NIV.GE.2) THEN
        WRITE (IFM,*) '<MECANONLINE> INITIALISATIONS EN DYNAMIQUE' 
      ENDIF         
C
C --- INITIALISATIONS
C
      CALL DISMOI('F','NB_EQUA',NUMEDD,'NUME_DDL',NEQ,K8BID,IRET)
      INSTAM = DIINST(SDDISC,NUMINS-1)
      INSTAP = DIINST(SDDISC,NUMINS)
      DELTAT = INSTAP - INSTAM     
C
C --- FONCTIONNALITES ACTIVEES
C 
      LEXGE  = NDYNLO(SDDYNA,'EXPL_GENE')
      LCTCC  = ISFONC(FONACT,'CONT_CONTINU')
      LMUAP  = NDYNLO(SDDYNA,'MULTI_APPUI')
      REAROT = ISFONC(FONACT,'REAROT')
      LEXPL  = NDYNLO(SDDYNA,'EXPLICITE')
      LMPAS  = NDYNLO(SDDYNA,'MULTI_PAS')
      LIMPL  = NDYNLO(SDDYNA,'IMPLICITE')
C
C --- ACCES SD DYNA
C      
      TFOR = SDDYNA(1:15)//'.TYPE_FOR'
      CFSC = SDDYNA(1:15)//'.COEF_SCH'      
      CALL JEVEUO(TFOR,'E',JTFOR)
      CALL JEVEUO(CFSC,'E',JCFSC)   
C
C --- DECOMPACTION DES VARIABLES CHAPEAUX
C       
      CALL DESAGG(VALMOI,DEPMOI,K24BID,K24BID,K24BID,
     &            VITMOI,ACCMOI,K24BID,K24BID)
      CALL DESAGG(VALPLU,DEPPLU,K24BID,K24BID,K24BID,
     &            VITPLU,ACCPLU,K24BID,K24BID)     
C
C --- TYPE DE FORMULATION SCHEMA DYNAMIQUE GENERAL
C
      LDEPL = NDYNIN(SDDYNA,'FORMUL_DYNAMIQUE').EQ.1
      LVITE = NDYNIN(SDDYNA,'FORMUL_DYNAMIQUE').EQ.2
      LACCE = NDYNIN(SDDYNA,'FORMUL_DYNAMIQUE').EQ.3
      IF (REAROT.AND..NOT.LDEPL) THEN
        CALL ASSERT(.FALSE.)
      ENDIF     
C
C --- TYPE DE FORMULATION SCHEMA DYNAMIQUE CONTACT 
C
      IF (LCTCC) THEN
        LCVITE = NDYNIN(SDDYNA,'FORMUL_CONTACT').EQ.2
        IF (LCVITE) THEN
          IF (LVITE) THEN
            CALL ASSERT(.FALSE.)
          ENDIF
        ENDIF
      ENDIF 
C
C --- TYPE DE SCHEMA: NEWMARK (ET SES DERIVEES) OU THETA    
C
      LNEWMA = NDYNLO(SDDYNA,'FAMILLE_NEWMARK')
      LTHETA = NDYNLO(SDDYNA,'THETA_METHODE')
      LKRENK = NDYNLO(SDDYNA,'KRENK')
      IF (.NOT.(LNEWMA.OR.LTHETA.OR.LKRENK)) THEN
        CALL ASSERT(.FALSE.)
      ENDIF   
C
C --- HHT COMPLET (MULTI-PAS)
C
      LHHTC  = NDYNLO(SDDYNA,'HHT_COMPLET')  
C
C --- COEFFICIENTS DU SCHEMA EN TEMPS      
C     
      BETA   = NDYNRE(SDDYNA,'BETA')
      GAMMA  = NDYNRE(SDDYNA,'GAMMA')
      THETA  = NDYNRE(SDDYNA,'THETA')
      UNTHET = UN-THETA
      IF (ABS(UNTHET).LE.R8PREM()) UNTHET = UN
      PHI    = NDYNRE(SDDYNA,'PHI') 
      ALPHA  = NDYNRE(SDDYNA,'ALPHA')
      KAPPA  = NDYNRE(SDDYNA,'KAPPA')
C
C --- SI NOEUD COLLE, THETA-SCHEMA PUREMENT IMPLICITE
C
      IF (LCTCC) THEN
        MDECOL = RESOCO(1:14)//'.MDECOL'
        CALL JEVEUO(MDECOL,'L',JMDECO)  
        SCOTCH = ZL(JMDECO+1-1)        
        IF (SCOTCH) THEN
          THETA   = 1.D0
        ENDIF      
      ENDIF     
C  
C --- COEFFICIENTS POUR MATRICES
C   
      IF (LNEWMA) THEN
        IF (LDEPL) THEN
          COERIG = UN
          COEAMO = GAMMA/(BETA*DELTAT)
          COEMAS = UN/(BETA*DELTAT*DELTAT)            
        ELSEIF (LACCE) THEN
          COERIG = BETA*DELTAT*DELTAT
          COEAMO = GAMMA*DELTAT
          COEMAS = UN 
        ELSE
          CALL ASSERT(.FALSE.)  
        ENDIF
        IF (LHHTC) THEN        
          COEAMO = COEAMO/(UN+ALPHA)
          COEMAS = COEMAS/(UN+ALPHA)    
        ENDIF
      ELSEIF (LTHETA) THEN
        IF (LDEPL) THEN
          COERIG = THETA
          COEAMO = UN/(THETA*DELTAT)
          COEMAS = UN/(THETA*DELTAT*DELTAT)        
        ELSEIF (LVITE) THEN
          COERIG = THETA*THETA*DELTAT
          COEAMO = THETA
          COEMAS = UN/DELTAT      
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF   
      ELSEIF (LKRENK) THEN
        IF (LDEPL) THEN
          COERIG = (KAPPA)/DEUX
          COEAMO = UN/DELTAT
          COEMAS = DEUX/((KAPPA)*DELTAT*DELTAT)        
        ELSEIF (LVITE) THEN
          COERIG = ((KAPPA)/DEUX)*((KAPPA)/DEUX)*DELTAT
          COEAMO = (KAPPA)/DEUX
          COEMAS = UN/DELTAT      
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF        
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF  
C
      ZR(JCFSC-1+1) = COERIG 
      ZR(JCFSC-1+2) = COEAMO 
      ZR(JCFSC-1+3) = COEMAS
C      
      IF (NIV.GE.2) THEN
        WRITE (IFM,*) '<MECANONLINE> ... COEF. RIGI.: ',COERIG 
        WRITE (IFM,*) '<MECANONLINE> ... COEF. AMOR.: ',COEAMO 
        WRITE (IFM,*) '<MECANONLINE> ... COEF. MASS.: ',COEMAS         
      ENDIF    
C
C --- COEFFICIENTS POUR MISE A JOUR DEPL/VITE/ACCE
C
      COEDEP = 1.D0
      IF (LNEWMA) THEN
        IF (LDEPL) THEN
          COEDEP = UN
          COEVIT = GAMMA/(BETA*DELTAT)
          COEACC = UN/(BETA*DELTAT*DELTAT)         
        ELSEIF (LACCE) THEN
          COEDEP = BETA*DELTAT*DELTAT
          COEVIT = GAMMA*DELTAT
          COEACC = UN             
        ELSE
          CALL ASSERT(.FALSE.)  
        ENDIF
      ELSEIF (LTHETA) THEN
        IF (LDEPL) THEN
          COEDEP = UN
          COEVIT = UN/(THETA*DELTAT)
          COEACC = DEUX/(THETA*DELTAT*DELTAT)          
        ELSEIF (LVITE) THEN
          COEDEP = DELTAT*THETA
          COEVIT = UN
          COEACC = DEUX/(DELTAT)               
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF   
      ELSEIF (LKRENK) THEN
        IF (LDEPL) THEN
          COEDEP = UN
          COEVIT = DEUX/((KAPPA)*DELTAT)
          COEACC =  DEUX*DEUX/(KAPPA*DELTAT*DELTAT)          
        ELSEIF (LVITE) THEN
          COEDEP = DELTAT*((KAPPA)/DEUX)
          COEVIT = UN
          COEACC = DEUX/DELTAT               
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF   
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF        
      ZR(JCFSC-1+13) = COEDEP
      ZR(JCFSC-1+14) = COEVIT
      ZR(JCFSC-1+15) = COEACC  
C      
      IF (NIV.GE.2) THEN
        WRITE (IFM,*) '<MECANONLINE> ... COEF. DEPL.: ',COEDEP 
        WRITE (IFM,*) '<MECANONLINE> ... COEF. VITE.: ',COEVIT 
        WRITE (IFM,*) '<MECANONLINE> ... COEF. ACCE.: ',COEACC         
      ENDIF               
C
C --- COEFFICIENTS POUR PREDICTEURS
C    
      IF (LNEWMA) THEN
        IF (LDEPL) THEN
          COEFD(1) = ZERO
          COEFD(2) = ZERO
          COEFD(3) = ZERO         
          COEFV(1) = ZERO
          COEFV(2) = (BETA-GAMMA)/BETA
          COEFV(3) = ((DEUX*BETA-GAMMA)*DELTAT)/(DEUX*BETA)
          COEFA(1) = ZERO
          COEFA(2) = -UN/(BETA*DELTAT)
          COEFA(3) = (DEUX*BETA-UN)/(DEUX*BETA)      
        ELSEIF (LACCE) THEN
          IF (LEXPL) THEN
            IF (NDYNLO(SDDYNA,'TCHAMWA')) THEN
              COEFD(1) = UN
              COEFD(2) = DELTAT
              COEFD(3) = DELTAT*DELTAT*PHI     
              COEFV(1) = ZERO
              COEFV(2) = UN 
              COEFV(3) = DELTAT
              COEFA(1) = ZERO
              COEFA(2) = ZERO
              COEFA(3) = ZERO
            ELSE
              COEFD(1) = UN
              COEFD(2) = DELTAT
              COEFD(3) = DELTAT*DELTAT/DEUX     
              COEFV(1) = ZERO
              COEFV(2) = UN 
              COEFV(3) = DELTAT*(UN-GAMMA)
              COEFA(1) = ZERO
              COEFA(2) = ZERO
              COEFA(3) = ZERO                            
            ENDIF 
          ELSE
            COEFD(1) = UN
            COEFD(2) = DELTAT
            COEFD(3) = DELTAT*DELTAT/DEUX
            COEFV(1) = ZERO
            COEFV(2) = UN 
            COEFV(3) = DELTAT
            COEFA(1) = ZERO
            COEFA(2) = ZERO
            COEFA(3) = ZERO            
          ENDIF
        ELSE
          CALL ASSERT(.FALSE.)  
        ENDIF
      ELSEIF (LTHETA) THEN
        IF (LDEPL) THEN
          COEFD(1) = ZERO
          COEFD(2) = ZERO
          COEFD(3) = ZERO         
          COEFV(1) = ZERO
          COEFV(2) = (THETA-UN)/(THETA)
          COEFV(3) = ZERO
          COEFA(1) = ZERO
          COEFA(2) = -DEUX/(THETA*DELTAT)
          COEFA(3) = -UN       
        ELSEIF (LVITE) THEN
          COEFD(1) = UN
          COEFD(2) = DELTAT
          COEFD(3) = ZERO
          COEFV(1) = ZERO
          COEFV(2) = UN
          COEFV(3) = ZERO            
          COEFA(1) = ZERO
          COEFA(2) = ZERO
          COEFA(3) = -UN               
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF   
      ELSEIF (LKRENK) THEN
        IF (LDEPL) THEN
          COEFD(1) = UN
          COEFD(2) = ZERO
          COEFD(3) = ZERO    
          COEFV(1) = ZERO
          COEFV(2) = ((KAPPA)-DEUX)/(KAPPA)
          COEFV(3) = ZERO
          COEFA(1) = ZERO
          COEFA(2) = -DEUX*DEUX/(KAPPA*DELTAT)
          COEFA(3) = -UN        
        ELSEIF (LVITE) THEN
          COEFD(1) = UN
          COEFD(2) = DELTAT
          COEFD(3) = ZERO
          COEFV(1) = ZERO
          COEFV(2) = UN
          COEFV(3) = ZERO            
          COEFA(1) = ZERO
          COEFA(2) = ZERO
          COEFA(3) = -UN               
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF           
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
            
      ZR(JCFSC-1+4)  = COEFD(1)
      ZR(JCFSC-1+5)  = COEFD(2)
      ZR(JCFSC-1+6)  = COEFD(3)
      ZR(JCFSC-1+7)  = COEFV(1)
      ZR(JCFSC-1+8)  = COEFV(2)
      ZR(JCFSC-1+9)  = COEFV(3)
      ZR(JCFSC-1+10) = COEFA(1)
      ZR(JCFSC-1+11) = COEFA(2)
      ZR(JCFSC-1+12) = COEFA(3)
C      
C --- CALCUL DES PREDICTEURS
C     
      CALL NDPRED(SDDISC,SDSENS,SDDYNA,VALMOI,VALPLU,
     &            POUGD ,SOLALG)       
C
C --- COEFFICIENTS POUR SCHEMAS A PLUSIEURS PAS
C --- COEEXT: COEF. DE PONDERATION DES FORCES EXTERNES
C --- COEINT: COEF. DE PONDERATION DES FORCES INTERNES
C --- COEEQU: COEF. PERMETTANT DE RESPECTER L'EQUILIBRE SU RLES AUTRES
C             TERMES NON PONDERES
C

      IF (LMPAS) THEN
        IF (LHHTC) THEN
          COEEXT = -ALPHA/(UN+ALPHA)
          COEINT = -ALPHA/(UN+ALPHA)          
          COEEQU = UN/(UN+ALPHA)
          COEEX2 = UN
        ELSEIF (LTHETA) THEN  
          COEEXT = (UN-THETA)
          IF (ABS(UN-THETA).LE.R8PREM()) THEN
            COEEXT = ZERO
          ENDIF
          COEINT = ZERO         
          COEEQU = UN
          COEEX2 = THETA         
        ELSEIF (LKRENK) THEN
          IF (LDEPL) THEN
            COEEXT = UN/DEUX
            COEINT = ZERO         
            COEEQU = UN
            COEEX2 = UN/DEUX          
          ELSEIF (LVITE) THEN
            COEEXT = UN/DEUX
            COEINT = ZERO         
            COEEQU = UN
            COEEX2 = UN/DEUX
          ENDIF                     
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
      ELSE  
        COEEXT = ZERO
        COEINT = ZERO        
        COEEQU = UN
        COEEX2 = UN
      ENDIF
      ZR(JCFSC-1+16) = COEEXT
      ZR(JCFSC-1+17) = COEEQU
      ZR(JCFSC-1+18) = COEINT
      ZR(JCFSC-1+19) = COEEX2      
C     
      IF (LMPAS) THEN 
        IF (NIV.GE.2) THEN
          WRITE (IFM,*) '<MECANONLINE> ... MULTI-PAS F. EXT. N-1: ',
     &                    COEEXT
          WRITE (IFM,*) '<MECANONLINE> ... MULTI-PAS F. EXT. N  : ',
     &                    COEEX2     
          WRITE (IFM,*) '<MECANONLINE> ... MULTI-PAS F. INT. N-1: ',
     &                    COEINT 
          WRITE (IFM,*) '<MECANONLINE> ... MULTI-PAS F. EQU.    : ',
     &                    COEEQU
        ENDIF 
      ENDIF 
C
C --- COEFFICENT POUR CALCUL FORCE D'INERTIE DE REFERENCE (NDINER)
C
      IF (LNEWMA) THEN
        IF (LIMPL) THEN
          COINER = UN/(BETA*DELTAT)
        ELSE
          COINER = UN/DELTAT  
        ENDIF 
      ELSEIF (LTHETA) THEN
        IF (LDEPL) THEN
          COINER = UN/DELTAT
       ELSE
          COINER = UN/DELTAT  
        ENDIF   
      ELSEIF (LKRENK) THEN
        IF (LDEPL) THEN
          COINER = UN/DELTAT
        ELSE
          COINER = UN/DELTAT  
        ENDIF   
      ELSE
        COINER = UN/DELTAT
      ENDIF
      ZR(JCFSC-1+23) = COINER
      IF (NIV.GE.2) THEN
          WRITE (IFM,*) '<MECANONLINE> ... COEF. FORC. INERTIE REF: ',
     &                    COINER
      ENDIF  
C
C --- COEFFICIENTS DEVANT MATRICE POUR TERME DE RAPPEL DYNAMIQUE
C
      IF (LTHETA) THEN
        IF (LVITE) THEN
          COERMA = ZERO
          COERAM = UN
          COERRI = THETA*DELTAT         
          IF (ABS(UN-THETA).LE.R8PREM()) THEN
            COERRI = DELTAT 
          ENDIF
        ELSEIF (LDEPL) THEN
          IF (ABS(UN-THETA).LE.R8PREM()) THEN
            COERMA = -UN/(THETA*DELTAT)
          ELSE
            COERMA = -UN/((THETA)*DELTAT)
          ENDIF
          COERAM = UN 
          COERRI = UN
        ENDIF
      ELSEIF (LKRENK) THEN
         IF (LDEPL) THEN
           COERMA = DEUX/((DEUX-KAPPA)*DELTAT)
           COERAM = UN
           COERRI = UN         
         ELSEIF (LVITE) THEN
           COERMA = ZERO
           COERAM = ZERO
           COERRI = ((KAPPA)/DEUX)*DELTAT 
         ENDIF 
      ELSE
        COERMA = UN
        COERAM = UN
        COERRI = UN
      ENDIF
      ZR(JCFSC-1+20) = COERMA
      ZR(JCFSC-1+21) = COERAM
      ZR(JCFSC-1+22) = COERRI
      
      IF (NIV.GE.2) THEN
        WRITE (IFM,*) '<MECANONLINE> ... COEF. FDYNA RIGI: ',COERRI
        WRITE (IFM,*) '<MECANONLINE> ... COEF. FDYNA AMOR: ',COERAM
        WRITE (IFM,*) '<MECANONLINE> ... COEF. FDYNA MASS: ',COERMA
      ENDIF      
              
C
C --- INITIALISATION DES CHAMPS D'ENTRAINEMENT EN MULTI-APPUI
C
      IF (LMUAP) THEN
        CALL NDMUAP(NUMINS,NUMEDD,SDDYNA,SDDISC)
      ENDIF
C
C --- INITIALISATION DES DEPL. GENERALISES SI PROJECTION MODALE
C
      IF (LEXGE) THEN
        SDPRMO = NDYNKK(SDDYNA,'PROJ_MODAL') 
        DEPGEM = SDPRMO(1:14)//'.DGEM'
        VITGEM = SDPRMO(1:14)//'.VGEM'
        ACCGEM = SDPRMO(1:14)//'.AGEM'         
        DEPGEP = SDPRMO(1:14)//'.DGEP'
        VITGEP = SDPRMO(1:14)//'.VGEP'
        ACCGEP = SDPRMO(1:14)//'.AGEP'   
        NBMODP = NDYNIN(SDDYNA,'NBRE_MODE_PROJ')              
        CALL JEVEUO(ACCGEM,'E',JACCGM)
        CALL JEVEUO(ACCGEP,'E',JACCGP)
        CALL JEVEUO(VITGEM,'E',JVITGM)
        CALL JEVEUO(VITGEP,'E',JVITGP)
        CALL JEVEUO(DEPGEM,'E',JDEPGM)
        CALL JEVEUO(DEPGEP,'E',JDEPGP)          
        CALL DCOPY(NBMODP,ZR(JDEPGM),1,ZR(JDEPGP),1)
        CALL DCOPY(NBMODP,ZR(JVITGM),1,ZR(JVITGP),1)
        CALL DCOPY(NBMODP,ZR(JACCGM),1,ZR(JACCGP),1)                
C
C --- PREDICTION DEPLACEMENT GENERALISE
C              
        DO 54 IMODE = 1,NBMODP
          ZR(JDEPGP+IMODE-1) = ZR(JDEPGM+IMODE-1) 
     &                    + COEFD(2)*ZR(JVITGM+IMODE-1)
     &                    + COEFD(3)*ZR(JACCGM+IMODE-1)
   54   CONTINUE
        IF (NIV.GE.2) THEN
          WRITE (IFM,*) '<MECANONLINE> ...... PRED. DEPL. GENE'
          CALL NMDEBG('VECT',DEPGEP,IFM)                                
        ENDIF  
C
      ENDIF    
C    
      CALL JEDEMA()

      END
