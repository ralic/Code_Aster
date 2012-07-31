      SUBROUTINE AMUMPU(OPTION,TYPE,KXMPS,USERSM,NPREC,LRESOL)
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION                  
!& MODIF AMUMPU MUMPS  DATE 30/07/2012   AUTEUR LEFEBVRE J-P.LEFEBVRE 
! ================================================================== 
! COPYRIGHT (C) 1991 - 2012  EDF R&D              WWW.CODE-ASTER.ORG 
!                                                                    
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR      
! MODIFY IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS     
! PUBLISHED BY THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE 
! LICENSE, OR (AT YOUR OPTION) ANY LATER VERSION.                    
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL,    
! BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF     
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU   
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.                           
!                                                                    
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE  
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO : EDF R&D CODE_ASTER,    
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.     
! ================================================================== 
C--------------------------------------------------------------
C BUT : UTILITAIRE POUR LES TRAITEMENTS CONNEXES LORS DU LANCEMENT
C       DES DIFFERENTES ETAPES DE MUMPS.
C
C OPTION=1 GESTION DE LA STRATEGIE MEMOIRE MUMPS (APRES ANALYSE)
C       CETTE ROUTINE DOIT ETRE APPELLEE ENTRE LA PHASE D'ANALYSE ET
C       CELLE DE FACTORISATION NUMERIQUE
C OPTION=2 DETECTION DES SINGULARITES (APRES FACTO)
C
C IN  KXMPS  :   IN   : INDICE DE L'INSTANCE MUMPS DANS DMPS
C IN  TYPE   :   K1   : TYPE DU POINTEUR R OU C
C SI OPTION=1
C IN  USERSM :   K8   : STRATEGIE MEMOIRE DE L'UTILISATEUR 
C                 (INFORMATION SOUVENT ISSUE DE SD_SOLVEUR.SLVK(8))
C SI OPTION=2
C IN  NPREC  :   IN   : NBRE DE DIGITS POUR DETECTION DE SINGULARITE
C IN LRESOL  :  LOG   : .TRUE. SI ON FAIT LE SOLVE, .FALSE. SINON
C---------------------------------------------------------------
C RESPONSABLE BOITEAU O.BOITEAU
 
      INTEGER      OPTION,KXMPS,NPREC
      CHARACTER*1  TYPE
      CHARACTER*12 USERSM
      LOGICAL      LRESOL

#ifdef _HAVE_MUMPS

C============================================================
      INCLUDE 'mpif.h'
      INCLUDE 'smumps_struc.h'
      INCLUDE 'cmumps_struc.h'
      INCLUDE 'dmumps_struc.h'
      INCLUDE 'zmumps_struc.h'
      INCLUDE 'jeveux.h'
C============================================================
      INTEGER    NMXINS
      PARAMETER (NMXINS=5)
      TYPE (SMUMPS_STRUC) , TARGET  :: SMPS(NMXINS)
      TYPE (CMUMPS_STRUC) , TARGET  :: CMPS(NMXINS)
      TYPE (SMUMPS_STRUC) , POINTER :: SMPSK
      TYPE (CMUMPS_STRUC) , POINTER :: CMPSK
      TYPE (DMUMPS_STRUC) , TARGET  :: DMPS(NMXINS)
      TYPE (ZMUMPS_STRUC) , TARGET  :: ZMPS(NMXINS)
      TYPE (DMUMPS_STRUC) , POINTER :: DMPSK
      TYPE (ZMUMPS_STRUC) , POINTER :: ZMPSK
      REAL*8       RVAL(3)
      INTEGER      INFO16,INFO26,VALI(10),ICOEFM,ICN22,ICN23,RANG,N,
     &             IAUX1,IAUX2,INFO3,NBPROC,IFM,NIV,IBID,IPIV,INFO28,
     &             INFO12,I,IERR,TMAX,TMAXB,LTOT,IRET
      LOGICAL      LPARA
      CHARACTER*1  ROUCS(NMXINS),PRECS(NMXINS)
      CHARACTER*4  ETAMS(NMXINS)
      CHARACTER*8  K8TAB(3)
      CHARACTER*14 NONUS(NMXINS)
      CHARACTER*19 NOMATS(NMXINS),NOSOLS(NMXINS)
      CHARACTER*24 KPIV,VALK(2)
      CHARACTER*80 NVERS
      CHARACTER*256 JNOM(2)
      COMMON /MUMPSS/ SMPS
      COMMON /MUMPSC/ CMPS
      COMMON /MUMPSD/ DMPS
      COMMON /MUMPSZ/ ZMPS
      COMMON /MUMPSH/ NONUS,NOMATS,NOSOLS,ETAMS,ROUCS,PRECS
      
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)
      
C       ------------------------------------------------
C ---   INITS
C       ------------------------------------------------
C ---   REMPLISSAGE DE DIFFERENTS OBJETS SUIVANT LE TYPE DU POINTEUR
C ---   DE MUMPS: DMUMPS_STRUC OU ZMUMPS_STRUC
      SELECT CASE(TYPE)
      CASE ('S')
        SMPSK=>SMPS(KXMPS)
        LPARA=(SMPSK%NPROCS.GT.1)
        NBPROC=SMPSK%NPROCS
        RANG=SMPSK%MYID
        NVERS(1:15)='SMUMPS '//SMPSK%VERSION_NUMBER
        N=SMPSK%N
      CASE ('C')
        CMPSK=>CMPS(KXMPS)
        LPARA=(CMPSK%NPROCS.GT.1)
        NBPROC=CMPSK%NPROCS
        RANG=CMPSK%MYID
        NVERS(1:15)='CMUMPS '//CMPSK%VERSION_NUMBER
        N=CMPSK%N
      CASE ('D')
        DMPSK=>DMPS(KXMPS)
        LPARA=(DMPSK%NPROCS.GT.1)
        NBPROC=DMPSK%NPROCS
        RANG=DMPSK%MYID
        NVERS(1:15)='DMUMPS '//DMPSK%VERSION_NUMBER
        N=DMPSK%N
      CASE ('Z')
        ZMPSK=>ZMPS(KXMPS)
        LPARA=(ZMPSK%NPROCS.GT.1)
        NBPROC=ZMPSK%NPROCS
        RANG=ZMPSK%MYID
        NVERS(1:15)='ZMUMPS '//ZMPSK%VERSION_NUMBER
        N=ZMPSK%N
      CASE DEFAULT
        CALL ASSERT(.FALSE.)
      END SELECT

C       ------------------------------------------------
C ---   GESTION STRATEGIE MEMOIRE MUMPS (APRES ANALYSE)
C       ------------------------------------------------
      IF (OPTION.EQ.1) THEN

C ---   INITS. PROPRE A L'OPTION
        SELECT CASE(TYPE)
        CASE ('S')
          INFO16=SMPSK%INFOG(16)
          INFO26=SMPSK%INFOG(26)
          INFO3=SMPSK%INFOG(3)*4
        CASE ('C')
          INFO16=CMPSK%INFOG(16)
          INFO26=CMPSK%INFOG(26)
          INFO3=CMPSK%INFOG(3)*8
        CASE ('D')
          INFO16=DMPSK%INFOG(16)
          INFO26=DMPSK%INFOG(26)
          INFO3=DMPSK%INFOG(3)*8
        CASE ('Z')
          INFO16=ZMPSK%INFOG(16)
          INFO26=ZMPSK%INFOG(26)
          INFO3=ZMPSK%INFOG(3)*16
        END SELECT      
        IF (INFO3.LT.0) THEN
          INFO3=-INFO3/NBPROC
        ELSE
          INFO3=INFO3/(1024*1024*NBPROC)
        ENDIF
    
C ---   MARGE POUR LES ESTIMATIONS (EN %)
        IF (LPARA) THEN
          ICOEFM=30
        ELSE
          ICOEFM=10
        ENDIF
        IF (N.LT.100) ICOEFM=50
        INFO16=INT(INFO16*((ICOEFM+100)*1.D0/100.D0))
        INFO26=INT(INFO26*((ICOEFM+100)*1.D0/100.D0))
C ---
C ---   TMAX: MAX DE LA RAM DISPO (ICI POUR MUMPS)
C ---   LA RECHERCHE VIA LA ROUTINE MEMRES NE FONCTIONNE PAS DE MANIE
C ---   RE SATISFAISANTE (IL FAUT BORNER LA MEMOIRE VIRTUELLE).
C ---   POUR L'INSTANT ON ESSAIE DE RETROUVER LA MEMOIRE TOTALE DISPO
C ---   DE MANIERE FRUSTRE. ON SUPPOSE QUE:
C ---      *LA MEMOIRE MAX DEVOLUE A JEVEUX EST CELLE PAR DEFAUT LORS
C ---       QUE L'UTILISATEUR NE RENSEIGNE PAS CE CHIFFRE DANS ASTK.
C ---      *LA MEMOIRE PRISE PAR PYTHON OU D'AUTRES PRODUITS EXTERNES 
C ---       EST NEGLIGEABLE.
C ---   PROCHAINEMENT UNE FONCTIONNALITE GENERIQUE DEVRAIT PERMETTRE
C ---   D'ACCEDER A UNE VALEUR PLUS REALISTE DE CE CHIFFRE
C ---
        K8TAB(1)='MEM_TOTA'
        K8TAB(2)='COUR_JV '
        K8TAB(3)='RLQ_MEM '
        CALL UTGTME(3,K8TAB,RVAL,IRET) 
        IF ( IRET .EQ. 0 ) THEN 
          TMAX=INT(RVAL(1)-(RVAL(2)+RVAL(3)))
        ELSE
          CALL ASSERT(.FALSE.)   
        ENDIF
        TMAXB=-999
        ICN22=-999
        ICN23=-999

        SELECT CASE(USERSM)
        CASE('IN_CORE')
C --------------
C ---   IN-CORE
C --------------
          ICN22=0
          ICN23=TMAX
          IF (ICN23.LT.INFO16) THEN
            VALI(1)=INFO16
            VALI(2)=ICOEFM
            VALI(3)=ICN23
            CALL U2MESI('A','FACTOR_74',3,VALI)
C ---    CHOIX ISOFONCTIONNEL
C ---    ON LAISSE MUMPS DECIDER SEUL DE SA MEMOIRE MAX. EN CAS DE
C ---    PB L'UTILISATEUR AURA ETE AVERTI PAR L'ALARME PRECEDENTE
            ICN23=0
          ENDIF
        CASE ('OUT_OF_CORE')
C ------------------
C ---   OUT-OF-CORE
C-------------------
          ICN22=1
          ICN23=TMAX
          IF (ICN23.LT.INFO26) THEN
            VALI(1)=INFO26
            VALI(2)=ICOEFM
            VALI(3)=ICN23
            CALL U2MESI('A','FACTOR_75',3,VALI)
C ---    CHOIX ISOFONCTIONNEL
C ---    ON LAISSE MUMPS DECIDER SEUL DE SA MEMOIRE MAX. EN CAS DE
C ---    PB L'UTILISATEUR AURA ETE AVERTI PAR L'ALARME PRECEDENTE
            ICN23=0
          ENDIF
        CASE ('AUTO')
C -----------------------------------------------------------------
C ----- STRATEGIE DECIDEE EN FONCTION DES CAPACITES MACHINES ET DES
C ----- CONSOMMATIONS REQUISES PAR MUMPS
C -----------------------------------------------------------------
          IF (TMAX.GE.INFO16) THEN
            ICN22=0
            ICN23=TMAX
          ELSE
            CALL JJLDYN(0,-1,LTOT)
            K8TAB(1)='MEM_TOTA'
            K8TAB(2)='COUR_JV'
            K8TAB(3)='RLQ_MEM'
            CALL UTGTME(3,K8TAB,RVAL,IRET) 
            IF ( IRET .EQ. 0 ) THEN 
              TMAXB=INT(RVAL(1)-(RVAL(2)+RVAL(3)))
            ELSE
              CALL ASSERT(.FALSE.)   
            ENDIF
            ICN23=TMAXB
            IF (NIV.GE.2) THEN
              VALI(1)=TMAXB-TMAX
              CALL U2MESI('I','FACTOR_51',1,VALI)
            ENDIF
            IF (ICN23.GE.INFO16) THEN
              ICN22=0
            ELSE IF ((ICN23.GE.INFO26).AND.(ICN23.LT.INFO16)) THEN
              ICN22=1
            ELSE
              VALI(1)=TMAX
              VALI(2)=TMAXB
              VALI(3)=INFO16
              VALI(4)=INFO26
              VALI(5)=ICOEFM
              CALL U2MESI('F','FACTOR_76',5,VALI)
            ENDIF
          ENDIF
        CASE ('EVAL')
C --------------------------------------------------
C ---   OPTION DE PRE-EVALUATION DES CONSOS MEMOIRE
C --------------------------------------------------
          ICN22=-1
          ICN23=-1
          K8TAB(1)='CUSE_JV'
          CALL UTGTME(1,K8TAB,RVAL,IRET)
          IAUX1=INT(RVAL(1))
          VALI(1)=N
          VALI(2)=MAX(IAUX1,1)
          VALI(3)=MAX(INFO16,1)
          VALI(4)=MAX(INFO26,1)
          VALI(5)=MAX(INFO3,1)
          VALI(6)=MAX(VALI(2),VALI(3))
          VALI(7)=MAX(VALI(2),VALI(4))
          CALL U2MESG('I','FACTOR_81',0,VALK,7,VALI,0,0.D0)
        CASE DEFAULT
          CALL ASSERT(.FALSE.)
        END SELECT

C ---  MODIFICATION DU PARAMETRAGE MUMPS POUR LA SUITE DU PROCESSUS
C ---- (FACTORISATION NUMERIQUE + SOLVE)
        SELECT CASE(TYPE)
        CASE ('S')
          SMPSK%ICNTL(22)=ICN22
          SMPSK%ICNTL(23)=ICN23
          SMPSK%OOC_TMPDIR='.'
        CASE ('C')
          CMPSK%ICNTL(22)=ICN22
          CMPSK%ICNTL(23)=ICN23
          CMPSK%OOC_TMPDIR='.'
        CASE ('D')
          DMPSK%ICNTL(22)=ICN22
          DMPSK%ICNTL(23)=ICN23
          DMPSK%OOC_TMPDIR='.'
        CASE ('Z')
          ZMPSK%ICNTL(22)=ICN22
          ZMPSK%ICNTL(23)=ICN23
          ZMPSK%OOC_TMPDIR='.'
        END SELECT      

C ---  AFFICHAGE DE CONTROLE
        IF (NIV.GE.2) THEN
          WRITE(IFM,*)
          WRITE(IFM,*)'*********************************************'
          WRITE(IFM,*)'<AMUMPU> GESTION MEMOIRE USERSM/ICN22/ICN23: ',
     &      USERSM,ICN22,ICN23
          WRITE(IFM,*)'<AMUMPU> TMAX/TMAXB/IC/OOC: ',
     &      TMAX,TMAXB,INFO16,INFO26
          WRITE(IFM,*)'*********************************************'
        ENDIF

C       ------------------------------------------------
C ---   DETECTION DES SINGULARITES (APRES FACTO)
C       ------------------------------------------------
      ELSE IF (OPTION.EQ.2) THEN

C ---   INITS. PROPRE A L'OPTION
        SELECT CASE(TYPE)
        CASE ('S')
          INFO28=SMPSK%INFOG(28)
          INFO12=SMPSK%INFOG(12)
        CASE ('C')
          INFO28=CMPSK%INFOG(28)
          INFO12=CMPSK%INFOG(12)
        CASE ('D')
          INFO28=DMPSK%INFOG(28)
          INFO12=DMPSK%INFOG(12)
        CASE ('Z')
          INFO28=ZMPSK%INFOG(28)
          INFO12=ZMPSK%INFOG(12)
        END SELECT
        
        IF (NPREC.GE.0) THEN
          KPIV='&&AMUMP.PIVNUL'
          CALL JEEXIN(KPIV,IBID)
          IF (IBID.NE.0) THEN
            CALL ASSERT(.FALSE.)
          ELSE
            CALL WKVECT(KPIV,'V V I',N+2,IPIV)
            IF (LRESOL) THEN
C ---   KPIV(1)= NOMBRE DE PIVOTS QUASI NULS (TOUS LE PROCS)
              IF (INFO28.GT.N) THEN
                CALL ASSERT(.FALSE.)
              ELSE
                ZI(IPIV)=INFO28
              ENDIF
C ---   KPIV(2)= NOMBRE DE PIVOTS NEGATIFS (TOUS LE PROCS)
              IF (INFO12.GT.N) THEN
                CALL ASSERT(.FALSE.)
              ELSE
                ZI(IPIV+1)=INFO12
              ENDIF
              IF (RANG.EQ.0) THEN
C ---   KPIV(3..) LES PIVOTS QUASI NULS (ONLY PROC 0)
                SELECT CASE(TYPE)
                CASE ('S')
                  DO I=1,INFO28
                    ZI(IPIV+1+I)=SMPSK%PIVNUL_LIST(I)
                  ENDDO
                CASE ('C')
                  DO I=1,INFO28
                    ZI(IPIV+1+I)=CMPSK%PIVNUL_LIST(I)
                  ENDDO
                CASE ('D')
                  DO I=1,INFO28
                    ZI(IPIV+1+I)=DMPSK%PIVNUL_LIST(I)
                  ENDDO
                CASE ('Z')
                  DO I=1,INFO28
                    ZI(IPIV+1+I)=ZMPSK%PIVNUL_LIST(I)
                  ENDDO
                END SELECT
              ENDIF
C ---   BCAST POUR COMMUNIQUER L'INFO AUX AUTRES PROCS
              CALL MPICM2('BCAST',KPIV)
            ENDIF
          ENDIF
C ---  AFFICHAGE DE CONTROLE
          IF (NIV.GE.2) THEN
            WRITE(IFM,*)
            WRITE(IFM,*)'*********************************************'
            WRITE(IFM,*)'<AMUMPU> TEST KPIV',ZI(IPIV),ZI(IPIV+1),
     &                   ZI(IPIV+2)
            WRITE(IFM,*)'*********************************************'
          ENDIF 
             
        ENDIF

C --- CASE SUR LA VARIABLE OPTION      
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
    
      CALL JEDEMA()
#endif
      END
