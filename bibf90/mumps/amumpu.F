      SUBROUTINE AMUMPU(OPTION,TYPE,KXMPS,USERSM,NPREC,LRESOL,KVERS)
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION
!& MODIF AMUMPU MUMPS  DATE 14/01/2013   AUTEUR COURTOIS M.COURTOIS 
! ==================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D              WWW.CODE-ASTER.ORG
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
!--------------------------------------------------------------
! BUT : UTILITAIRE POUR LES TRAITEMENTS CONNEXES LORS DU LANCEMENT
!       DES DIFFERENTES ETAPES DE MUMPS.
!
! OPTION=1 GESTION DE LA STRATEGIE MEMOIRE MUMPS (APRES ANALYSE)
!       CETTE ROUTINE DOIT ETRE APPELLEE ENTRE LA PHASE D'ANALYSE ET
!       CELLE DE FACTORISATION NUMERIQUE
! OPTION=2 DETECTION DES SINGULARITES (APRES FACTO) ET STOCKAGE DE CES
!          INFOS DS L'OBJET JEVEUX '&&AMUMP.PIVNUL' (V V I DIM=N+2)
!
! OPTION=3 RECUPERE LE NUMERO DE VERSION (OCCURENCE MUMPS EXISTE DEJA)
! OPTION=31 IDEM MAIS ON CREE UNE OCCURENCE MUMPS TEMPORAIRE. OPERATION
!    UN PEU COUTEUSE A NE FAIRE QU'UNE FOIS PAR OPERATEUR(SD_SOLVEUR).
! DANS CES DEUX MODES, ON CONTROLE LE CARACTERE LICITE DU NUMERO DE
! VERSIONS: 4.9.2 OU 4.10.0 SINON UTMESS_F
!
! OPTION=4 RECUPERE LE DETERMINANT ET ON LE STOCKE DS L'OBJET JEVEUX
!          '&&AMUMP.DETERMINANT' (V V R DIM=3)
!
! IN  KXMPS  :   IN   : INDICE DE L'INSTANCE MUMPS DANS XMPS
!                       (INUTILE POUR OPTION=31)
! IN  TYPE   :   K1   : TYPE DU POINTEUR R OU C
!
! SI OPTION=1
! IN  USERSM :   K8   : STRATEGIE MEMOIRE DE L'UTILISATEUR
!                 (INFORMATION SOUVENT ISSUE DE SD_SOLVEUR.SLVK(8))
! SI OPTION=2
! IN  NPREC  :   IN   : NBRE DE DIGITS POUR DETECTION DE SINGULARITE
! IN LRESOL  :  LOG   : .TRUE. SI ON FAIT LE SOLVE, .FALSE. SINON
!
! SI OPTION=3 OU 31
! OUT KVERS  :  K24   : NUMERO DE VERSION DE MUMPS LICITE
!
! SI OPTION=4
! RAS
!---------------------------------------------------------------
! RESPONSABLE BOITEAU O.BOITEAU

      INTEGER      OPTION,KXMPS,NPREC
      CHARACTER*1  TYPE
      CHARACTER*12 USERSM
      CHARACTER*24 KVERS
      LOGICAL      LRESOL

#ifdef _HAVE_MUMPS

!============================================================
      INCLUDE 'mpif.h'
      INCLUDE 'smumps_struc.h'
      INCLUDE 'cmumps_struc.h'
      INCLUDE 'dmumps_struc.h'
      INCLUDE 'zmumps_struc.h'
      INCLUDE 'jeveux.h'
!============================================================
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
      REAL*8        RVAL(3),RVAL1,RVAL2,RVAL3,RVAL1B,RVAL2B,RVAL3B,
     &              RINF12,RINF13
      INTEGER       INFO16,INFO26,VALI(10),ICOEFM,ICN22,ICN23,RANG,N,
     &              IAUX1,IAUX2,INFO3,NBPROC,IFM,NIV,IBID,IPIV,INFO28,
     &              INFO12,I,IERR,TMAX,TMAXB,LTOT,IRET,ISIZEMU,NSIZEMU,
     &              NSIZEMA,EXECMU,INFO34,ICNT33
c      INTEGER*4     getpid
      INTEGER*4     PID
      LOGICAL       LPARA,LPEAK,LPB1
      CHARACTER*1   ROUCS(NMXINS),PRECS(NMXINS)
      CHARACTER*2   FSTRING
      CHARACTER*4   ETAMS(NMXINS)
      CHARACTER*8   K8TAB(3)
      CHARACTER*10  STRPID
      CHARACTER*14  NONUS(NMXINS)
      CHARACTER*19  NOMATS(NMXINS),NOSOLS(NMXINS)
      CHARACTER*24  KPIV,VALK(2),KSIZEMU
      CHARACTER*50  STR,BUFFER
      CHARACTER*80  NVERS
      CHARACTER*256 JNOM(2)
      COMMON /MUMPSS/ SMPS
      COMMON /MUMPSC/ CMPS
      COMMON /MUMPSD/ DMPS
      COMMON /MUMPSZ/ ZMPS
      COMMON /MUMPSH/ NONUS,NOMATS,NOSOLS,ETAMS,ROUCS,PRECS

      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)

!       ------------------------------------------------
! ---   INITS
!       ------------------------------------------------
      NVERS(1:80)=''
      IF (OPTION.NE.31) THEN
! --- OCCURENCE DE MUMPS EXISTE DEJA DS UN VECTEUR XMPS
        SELECT CASE(TYPE)
        CASE ('S')
          SMPSK=>SMPS(KXMPS)
          LPARA=(SMPSK%NPROCS.GT.1)
          NBPROC=SMPSK%NPROCS
          RANG=SMPSK%MYID
          NVERS=SMPSK%VERSION_NUMBER
          N=SMPSK%N
        CASE ('C')
          CMPSK=>CMPS(KXMPS)
          LPARA=(CMPSK%NPROCS.GT.1)
          NBPROC=CMPSK%NPROCS
          RANG=CMPSK%MYID
          NVERS=CMPSK%VERSION_NUMBER
          N=CMPSK%N
        CASE ('D')
          DMPSK=>DMPS(KXMPS)
          LPARA=(DMPSK%NPROCS.GT.1)
          NBPROC=DMPSK%NPROCS
          RANG=DMPSK%MYID
          NVERS=DMPSK%VERSION_NUMBER
          N=DMPSK%N
        CASE ('Z')
          ZMPSK=>ZMPS(KXMPS)
          LPARA=(ZMPSK%NPROCS.GT.1)
          NBPROC=ZMPSK%NPROCS
          RANG=ZMPSK%MYID
          NVERS=ZMPSK%VERSION_NUMBER
          N=ZMPSK%N
        CASE DEFAULT
          CALL ASSERT(.FALSE.)
        END SELECT
      ELSE
! ---- ON CREE PUIS DETRUIT UNE OCCURENCE MUMPS TEMPORAIRE
        KXMPS=1
        SELECT CASE(TYPE)
        CASE ('S')
          SMPSK=>SMPS(KXMPS)
          SMPSK%COMM=MPI_COMM_WORLD
          SMPSK%SYM=0
          SMPSK%PAR=1
          SMPSK%JOB=-1
          CALL SMUMPS(SMPSK)
          NVERS=SMPSK%VERSION_NUMBER
          SMPSK%JOB=-2
          CALL SMUMPS(SMPSK)
        CASE ('C')
          CMPSK=>CMPS(KXMPS)
          CMPSK%COMM=MPI_COMM_WORLD
          CMPSK%SYM=0
          CMPSK%PAR=1
          CMPSK%JOB=-1
          CALL CMUMPS(CMPSK)
          NVERS=CMPSK%VERSION_NUMBER
          CMPSK%JOB=-2
          CALL CMUMPS(CMPSK)
        CASE ('D')
          DMPSK=>DMPS(KXMPS)
          DMPSK%COMM=MPI_COMM_WORLD
          DMPSK%SYM=0
          DMPSK%PAR=1
          DMPSK%JOB=-1
          CALL DMUMPS(DMPSK)
          NVERS=DMPSK%VERSION_NUMBER
          DMPSK%JOB=-2
          CALL DMUMPS(DMPSK)
        CASE ('Z')
          ZMPSK=>ZMPS(KXMPS)
          ZMPSK%COMM=MPI_COMM_WORLD
          ZMPSK%SYM=0
          ZMPSK%PAR=1
          ZMPSK%JOB=-1
          CALL ZMUMPS(ZMPSK)
          NVERS=ZMPSK%VERSION_NUMBER
          ZMPSK%JOB=-2
          CALL ZMUMPS(ZMPSK)
        CASE DEFAULT
          CALL ASSERT(.FALSE.)
        END SELECT
      ENDIF

!       ------------------------------------------------
! ---   GESTION STRATEGIE MEMOIRE MUMPS (APRES ANALYSE)
!       ------------------------------------------------
      IF (OPTION.EQ.1) THEN

! ---   INITS. PROPRE A L'OPTION
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

! ---   NSIZEMA: TAILLE CUMULEE EN MO DES OBJETS MUMPS A,IRN,RHS..
! ---   EXECMU:  TAILLE EN MO DE L'EXECUTABLE MUMPS
        EXECMU=30
        KSIZEMU='&&TAILLE_OBJ_MUMPS'
        CALL JEVEUO(KSIZEMU,'L',ISIZEMU)
        NSIZEMA=-999
        DO I=1,NBPROC
          NSIZEMU=ZI(ISIZEMU+I-1)
          IF (NSIZEMU.GT.NSIZEMA) NSIZEMA=NSIZEMU
        ENDDO

! ---   MARGES POUR LES ESTIMATIONS (EN %) DE MUMPS IC ET OOC PLUS
! ---   CORRECTION POUR PRENDRE EN COMPTE MAX(A/IRN/JCN,PROC) ET
! ---   EXECUTABLE MUMPS.
        IF (LPARA) THEN
! ---     MARGE DU AU PARALLELISME
          ICOEFM=30
        ELSE
! ---     MARGE DU AU SEQUENTIEL
          ICOEFM=10
        ENDIF
! ---   MARGE POUR LES TRES PETITS CAS
        IF (N.LT.100) ICOEFM=50

! ---   CONSOS MUMPS IC ET OOC MAX SUR TOUS LES PROCS
        INFO16=INT(INFO16*((ICOEFM+100)*1.D0/100.D0))+NSIZEMA+EXECMU
        INFO26=INT(INFO26*((ICOEFM+100)*1.D0/100.D0))+NSIZEMA+EXECMU
! ---
! ---   TMAX: MAX DE LA RAM DISPO =
! ---       MEM_JOB - COURANT_JEVEUX - RELIQUAT (PYTHON, EXEC ASTER...)
! ---
        TMAX=-999
        RVAL1=-999
        RVAL2=-999
        RVAL3=-999
        TMAXB=-999
        RVAL1B=-999
        RVAL2B=-999
        RVAL3B=-999
        ICN22=-999
        ICN23=-999

! ---   SI LA MESURE DE VMPEAK N'EST PAS DISPO, ON A MOINS CONFIANCE
! ---   DANS LA MESURE DE TMAX. DU COUP CERTAINS MSGS NE SONT PAS
! ---   AFFICHES.
! ---   SI GESTION_MEMOIRE='AUTO', ON BASCULE AUTOMATIQUEMENT
! ---   EN MODE OOC
! ---   SI GESTION_MEMOIRE='EVAL', ON ALERTE L'UTILISATEUR

        LPB1=.FALSE.
        CALL UTGTME(1,'VMPEAK  ',RVAL,IRET)
        IF (RVAL(1).LE.0) THEN
          LPEAK=.FALSE.
          IF (USERSM(1:4).EQ.'AUTO') THEN
            LPB1=.TRUE.
            USERSM='OUT_OF_CORE'
            IF (NIV.GE.2) CALL U2MESS('I','FACTOR_82')
          ENDIF
        ELSE
          LPEAK=.TRUE.
        ENDIF
        K8TAB(1)='MEM_TOTA'
        K8TAB(2)='COUR_JV '
        K8TAB(3)='RLQ_MEM '
        CALL UTGTME(3,K8TAB,RVAL,IRET)
        RVAL1=RVAL(1)
        RVAL2=RVAL(2)
        RVAL3=RVAL(3)
        IF (IRET.EQ.0) THEN
          TMAX=INT(RVAL1-(RVAL2+RVAL3))
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF

        SELECT CASE(USERSM)
        CASE('IN_CORE')
! --------------
! ---   IN-CORE
! --------------
          ICN22=0
          ICN23=0
          IF ((TMAX.LT.INFO16).AND.(LPEAK)) THEN
            VALI(1)=INFO16
            VALI(2)=ICOEFM
            VALI(3)=TMAX
            CALL U2MESI('A','FACTOR_74',3,VALI)
          ENDIF
        CASE ('OUT_OF_CORE')
! ------------------
! ---   OUT-OF-CORE
!-------------------
          ICN22=1
          ICN23=0
          IF ((TMAX.LT.INFO26).AND.(LPEAK)) THEN
            VALI(1)=INFO26
            VALI(2)=ICOEFM
            VALI(3)=TMAX
            CALL U2MESI('A','FACTOR_75',3,VALI)
          ENDIF
        CASE ('AUTO')
! -----------------------------------------------------------------
! ----- STRATEGIE DECIDEE EN FONCTION DES CAPACITES MACHINES ET DES
! ----- CONSOMMATIONS REQUISES PAR MUMPS
! -----------------------------------------------------------------
          IF (TMAX.GE.INFO16) THEN
            ICN22=0
            ICN23=MAX(INT(0.95*TMAX),INFO16)-(NSIZEMA+EXECMU)
          ELSE
            CALL JJLDYN(0,-1,LTOT)
            K8TAB(1)='MEM_TOTA'
            K8TAB(2)='COUR_JV'
            K8TAB(3)='RLQ_MEM'
            CALL UTGTME(3,K8TAB,RVAL,IRET)
            RVAL1B=RVAL(1)
            RVAL2B=RVAL(2)
            RVAL3B=RVAL(3)
            IF (IRET.EQ.0) THEN
              TMAXB=INT(RVAL1B-(RVAL2B+RVAL3B))
            ELSE
              CALL ASSERT(.FALSE.)
            ENDIF
            IF (NIV.GE.2) THEN
              VALI(1)=INT(RVAL1B-RVAL1)
              CALL U2MESI('I','FACTOR_51',1,VALI)
            ENDIF
            IF (TMAXB.GE.INFO16) THEN
              ICN22=0
              ICN23=MAX(INT(0.95*TMAXB),INFO16)-(NSIZEMA+EXECMU)
            ELSE IF ((TMAXB.GE.INFO26).AND.(TMAXB.LT.INFO16)) THEN
              ICN22=1
              ICN23=MAX(INT(0.95*TMAXB),INFO26)-(NSIZEMA+EXECMU)
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
! --------------------------------------------------
! ---   OPTION DE PRE-EVALUATION DES CONSOS MEMOIRE
! --------------------------------------------------
          ICN22=-1
          ICN23=-1
          K8TAB(1)='CUSE_JV'
          K8TAB(2)='RLQ_MEM'
          CALL UTGTME(2,K8TAB,RVAL,IRET)
          IAUX1=INT(RVAL(1)+RVAL(2))
          VALI(1)=N
          VALI(2)=MAX(IAUX1,1)
          VALI(3)=MAX(INFO16,1)
          VALI(4)=MAX(INFO26,1)
          VALI(5)=MAX(INFO3,1)
          VALI(6)=VALI(2)+VALI(3)
          VALI(7)=VALI(2)+VALI(4)
          CALL U2MESG('I','FACTOR_81',0,VALK,7,VALI,0,0.D0)
          IF (.NOT.LPEAK) CALL U2MESS('A','FACTOR_83')
        CASE DEFAULT
          CALL ASSERT(.FALSE.)
        END SELECT
! --- CORRECTIF POUR BENEFICIER DES BOUCLES DE RATTRAPAGE SI VMPEAK
! --- NON EVALUABLE ET GESTION_MEMOIRE='AUTO'
        IF (LPB1) USERSM='AUTO'

! ---  MODIFICATION DU PARAMETRAGE MUMPS POUR LA SUITE DU PROCESSUS
! ---- (FACTORISATION NUMERIQUE + SOLVE)
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

        IF (NIV.GE.2) THEN
! ---  NIVEAU DEVELOPPEUR
! ---  AFFICHAGE DE CONTROLE POUR DIAGNOSTIC MEMOIRE FIN
! ---  RECUPERATION DE L'AFFICHAGE DES CONSOS SYSTEMES
! ---  (VMPEAK, VMSIZE, VMDATA) + FREE DS LE FICHIER FORT.11
! ---  SI ON DECOMMENTARISE LES LIGNES 'CALL SYSTEM()' + 'GETPID'
          PID=0
!          PID=getpid()
          IF ( ABS(PID) < 10 ) THEN
            FSTRING = 'I1'
          ELSEIF ( PID < 100 ) THEN
            FSTRING = 'I2'
          ELSEIF ( PID < 1000 ) THEN
            FSTRING = 'I3'
          ELSEIF ( PID < 10000 ) THEN
            FSTRING = 'I4'
          ELSEIF ( PID < 100000 ) THEN
            FSTRING = 'I5'
          ELSEIF( PID < 1000000 ) THEN
            FSTRING = 'I6'
          ELSE
            WRITE(6,*)'READ_VMPEAK : PB FORMAT CHOICE !'
          ENDIF
          WRITE(STRPID,'('//FSTRING//')')PID
!          str=""
!          str="/proc/"//trim(adjustl(strpid))//"/status"
!          CALL SYSTEM("cat "//str//" > fort.11")
!          CALL SYSTEM('free -m >> fort.11')
          WRITE(IFM,*)
          WRITE(IFM,*)'*********************************************'
          WRITE(IFM,*)'<AMUMPU> GESTION MEMOIRE USERSM/ICN22/ICN23: ',
     &      USERSM,ICN22,ICN23
          WRITE(IFM,*)'<AMUMPU> CONSO MUMPS EXEC/OBJET_AIRNJCN/IC/OOC ',
     &                 EXECMU,NSIZEMA,INFO16-(EXECMU+NSIZEMA),
     &                                INFO26-(EXECMU+NSIZEMA)
          WRITE(IFM,*)'<AMUMPU> 1ERE ESTIMATION JEVEUX/RELIQUAT/TMAX: ',
     &                 RVAL2,RVAL3,TMAX
          WRITE(IFM,*)'<AMUMPU> 2NDE ESTIMATION JEVEUX/RELIQUAT/TMAX: ',
     &                 RVAL2B,RVAL3B,TMAXB
          WRITE(IFM,*)'*********************************************'
        ENDIF

!       ------------------------------------------------
! ---   DETECTION DES SINGULARITES (APRES FACTO)
!       ------------------------------------------------
      ELSE IF (OPTION.EQ.2) THEN

! ---   INITS. PROPRE A L'OPTION
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
! ---   KPIV(1)= NOMBRE DE PIVOTS QUASI NULS (TOUS LE PROCS)
              IF (INFO28.GT.N) THEN
                CALL ASSERT(.FALSE.)
              ELSE
                ZI(IPIV)=INFO28
              ENDIF
! ---   KPIV(2)= NOMBRE DE PIVOTS NEGATIFS (TOUS LE PROCS)
              IF (INFO12.GT.N) THEN
                CALL ASSERT(.FALSE.)
              ELSE
                ZI(IPIV+1)=INFO12
              ENDIF
              IF (RANG.EQ.0) THEN
! ---   KPIV(3..) LES PIVOTS QUASI NULS (ONLY PROC 0)
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
! ---   BCAST POUR COMMUNIQUER L'INFO AUX AUTRES PROCS
              CALL MPICM2('BCAST',KPIV)
            ENDIF
          ENDIF
! ---  AFFICHAGE DE CONTROLE
          IF (NIV.GE.2) THEN
            WRITE(IFM,*)
            WRITE(IFM,*)'*********************************************'
            WRITE(IFM,*)'<AMUMPU> TEST KPIV',ZI(IPIV),ZI(IPIV+1),
     &                   ZI(IPIV+2)
            WRITE(IFM,*)'*********************************************'
          ENDIF

        ENDIF

!       ------------------------------------------------
! ---   NUMERO DE VERSION DE MUMPS (DETECTION ET CONTROLE)
!       ------------------------------------------------
      ELSE IF ((OPTION.EQ.3).OR.(OPTION.EQ.31)) THEN

        KVERS=''
        KVERS=TRIM(ADJUSTL(NVERS))
        SELECT CASE(KVERS)
        CASE('4.9.2','4.10.0')
        CASE DEFAULT
          CALL U2MESK('F','FACTOR_72',1,KVERS)
        END SELECT

!       ------------------------------------------------
! ---   CALCUL DE DETERMINANT (APRES FACTO)
!       ------------------------------------------------
      ELSE IF (OPTION.EQ.4) THEN

! ---   INITS. PROPRE A L'OPTION
        SELECT CASE(TYPE)
        CASE ('S')
          RINF12=SMPSK%RINFOG(12)
          RINF13=SMPSK%RINFOG(13)
          INFO34=SMPSK%INFOG(34)
          ICNT33=SMPSK%ICNTL(33)
        CASE ('C')
          RINF12=CMPSK%RINFOG(12)
          RINF13=CMPSK%RINFOG(13)
          INFO34=CMPSK%INFOG(34)
          ICNT33=CMPSK%ICNTL(33)
        CASE ('D')
          RINF12=DMPSK%RINFOG(12)
          RINF13=DMPSK%RINFOG(13)
          INFO34=DMPSK%INFOG(34)
          ICNT33=DMPSK%ICNTL(33)
        CASE ('Z')
          RINF12=ZMPSK%RINFOG(12)
          RINF13=ZMPSK%RINFOG(13)
          INFO34=ZMPSK%INFOG(34)
          ICNT33=ZMPSK%ICNTL(33)
        END SELECT
        IF (ICNT33.EQ.1) THEN
          KPIV='&&AMUMP.DETERMINANT'
          CALL JEEXIN(KPIV,IBID)
          IF (IBID.NE.0) THEN
            CALL JEVEUO(KPIV,'E',IPIV)
          ELSE
            CALL WKVECT(KPIV,'V V R',3,IPIV)
          ENDIF
! --- ON STOCKE LE CALCUL DU DET: MANTISSE * (2**EXP)
! --- MANTISSE=DCMPLX(RINF12,RINF13)
! --- EXP     =INFO34
          ZR(IPIV)  =RINF12
          ZR(IPIV+1)=RINF13
          ZR(IPIV+2)=INFO34
        ENDIF

! --- CASE SUR LA VARIABLE OPTION
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF

      CALL JEDEMA()
#endif
      END
