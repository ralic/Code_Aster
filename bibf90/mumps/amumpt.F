
      SUBROUTINE  AMUMPT(OPTION,KMONIT,TEMPS,RANG,NBPROC,KXMPS,
     &                   LQUALI,RDESQ,TYPE,IETDEB,IETRAT,RCTDEB,LDIST)
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION                  
!& MODIF AMUMPT MUMPS  DATE 09/03/2010   AUTEUR DESOZA T.DESOZA 
! ================================================================== 
! COPYRIGHT (C) 1991 - 2008  EDF R&D              WWW.CODE-ASTER.ORG 
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
C BUT : ROUTINE DE MONITORING POUR AMUMPS/C/D/Z.
C
C IN  OPTION :   IN   : OPTION D'UTILISATION.
C IN/OUT KMONIT: K24  : VECTEUR DE NOMS DES OBJ JEVEUX
C IN/OUT TEMPS : R8   : VECTEUR POUR UTTCPU
C IN     RANG  : IN   : RANG DU PROCESSEUR
C IN     NBPROC: IN   : NBRE DE PROCESEURS
C IN  KXMPS  :   IN   : INDICE DE L'INSTANCE MUMPS DANS DMPS
C IN  LQUALI :  LOG   : LOGICAL EN CAS DE CRITERE DE QUALITE
C IN  RDESQ  :  R8    : PARAMETRE POUR LE DESEQUILIBRAGE MAX
C IN  TYPE   :   K1   : TYPE DU POINTEUR R OU C
C IN  LDIST  :  LOG   : LOGICAL MUMPS DISTRIBUE OR NOT
C---------------------------------------------------------------
C RESPONSABLE BOITEAU
 
      INTEGER      OPTION,RANG,NBPROC,KXMPS,IETDEB,IETRAT
      CHARACTER*1  TYPE
      CHARACTER*24 KMONIT(12)
      REAL*8       TEMPS(6),RDESQ,RCTDEB
      LOGICAL      LQUALI,LDIST

#ifdef _HAVE_MUMPS

C============================================================
      INCLUDE 'mpif.h'
      INCLUDE 'smumps_struc.h'
      INCLUDE 'cmumps_struc.h'
      INCLUDE 'dmumps_struc.h'
      INCLUDE 'zmumps_struc.h'
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
      INTEGER      IFM,NIV,IBID,IAUX1,IAUX2,IAUX3,K,I,N,INFO(100),IRET,
     &             MONIT(12),IETFIN,IETMAX
      CHARACTER*1  ROUCS(NMXINS),PRECS(NMXINS)
      CHARACTER*4  ETAMS(NMXINS)
      CHARACTER*14 NONUS(NMXINS)
      CHARACTER*19 NOMATS(NMXINS),NOSOLS(NMXINS),KTEMP
      CHARACTER*80 NVERS
      REAL*8       RMONIT(18),RAUX,VALR(2),RINFOG(100),RCTFIN,RETFIN
      LOGICAL      LDEBUG
      COMMON /MUMPSS/ SMPS
      COMMON /MUMPSC/ CMPS
      COMMON /MUMPSD/ DMPS
      COMMON /MUMPSZ/ ZMPS
      COMMON /MUMPSH/ NONUS,NOMATS,NOSOLS,ETAMS,ROUCS,PRECS
      
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C-----------------------------------------------------------------------
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)
      
C --- PARAMETRE POUR DEBUGGAGE (ATTENTION IL FAUT INFO=2)
      LDEBUG=.TRUE.
      LDEBUG=.FALSE.
      IF (LDEBUG.AND.(NIV.NE.2)) CALL ASSERT(.FALSE.)
      
C       ------------------------------------------------
C        INITS
C       ------------------------------------------------
C --- REMPLISSAGE DE DIFFERENTS OBJETS SUIVANT LE TYPE DU POINTEUR
C --- DE MUMPS: DMUMPS_STRUC OU ZMUMPS_STRUC
      IF (TYPE.EQ.'S') THEN
        SMPSK=>SMPS(KXMPS)
        N=SMPSK%N
        INFO(9)=SMPSK%INFO(9)
        INFO(15)=SMPSK%INFO(15)
        INFO(16)=SMPSK%INFO(16)
        INFO(17)=SMPSK%INFO(17)
        RINFOG(7)=SMPSK%RINFOG(7)
        RINFOG(9)=SMPSK%RINFOG(9)
        RINFOG(10)=SMPSK%RINFOG(10)
        NVERS(1:12)='SMUMPS '//SMPSK%VERSION_NUMBER
      ELSE IF (TYPE.EQ.'C') THEN
        CMPSK=>CMPS(KXMPS)
        N=CMPSK%N
        INFO(9)=CMPSK%INFO(9)
        INFO(15)=CMPSK%INFO(15)
        INFO(16)=CMPSK%INFO(16)
        INFO(17)=CMPSK%INFO(17)
        RINFOG(7)=CMPSK%RINFOG(7)
        RINFOG(9)=CMPSK%RINFOG(9)
        RINFOG(10)=CMPSK%RINFOG(10)
        NVERS(1:12)='CMUMPS '//CMPSK%VERSION_NUMBER
      ELSE IF (TYPE.EQ.'D') THEN
        DMPSK=>DMPS(KXMPS)
        N=DMPSK%N
        INFO(9)=DMPSK%INFO(9)
        INFO(15)=DMPSK%INFO(15)
        INFO(16)=DMPSK%INFO(16)
        INFO(17)=DMPSK%INFO(17)
        RINFOG(7)=DMPSK%RINFOG(7)
        RINFOG(9)=DMPSK%RINFOG(9)
        RINFOG(10)=DMPSK%RINFOG(10)
        NVERS(1:12)='DMUMPS '//DMPSK%VERSION_NUMBER
      ELSE IF (TYPE.EQ.'Z') THEN
        ZMPSK=>ZMPS(KXMPS)
        N=ZMPSK%N
        INFO(9)=ZMPSK%INFO(9)
        INFO(15)=ZMPSK%INFO(15)
        INFO(16)=ZMPSK%INFO(16)
        INFO(17)=ZMPSK%INFO(17)
        RINFOG(7)=ZMPSK%RINFOG(7)
        RINFOG(9)=ZMPSK%RINFOG(9)
        RINFOG(10)=ZMPSK%RINFOG(10)
        NVERS(1:12)='ZMUMPS '//ZMPSK%VERSION_NUMBER
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF

C --- TEST POUR EVITER LE MONITORING DES CMDES ECLATEES
C     LES OBJETS TEMPORAIRES DE MONITORING SONT EFFACES A CHAQUE
C     FIN DE COMMANDE (NUM_DDL/FACTORISER/RESOUDRE)
      CALL JEEXIN('&MUMPS.INFO.MAILLE',IRET)
      IF (IRET.EQ.0) GOTO 999

C --- VECTEURS DE MONITORING
      KMONIT(1)='&MUMPS.INFO.MAILLE'
      KMONIT(2)='&MUMPS.INFO.MEMOIRE'
      KMONIT(9)='&MUMPS.NB.MAILLE'
      KMONIT(10)='&MUMPS.INFO.MEM.EIC'
      KMONIT(11)='&MUMPS.INFO.MEM.EOC'
      KMONIT(12)='&MUMPS.INFO.MEM.USE'
      CALL JEVEUO(KMONIT(1),'E',MONIT(1))
      CALL JEVEUO(KMONIT(2),'E',MONIT(2))
      CALL JEVEUO(KMONIT(9),'E',MONIT(9))
      CALL JEVEUO(KMONIT(10),'E',MONIT(10))
      CALL JEVEUO(KMONIT(11),'E',MONIT(11))
      CALL JEVEUO(KMONIT(12),'E',MONIT(12))
          
C       ------------------------------------------------
C       TRAITEMENTS PROPREMENT DIT
C       ------------------------------------------------
      IF (NIV.GE.2) THEN
        IF (OPTION.EQ.0) THEN
C --      ON NE FAIT RIEN DE PLUS !
        ELSE IF (OPTION.EQ.1) THEN
          IF (LDEBUG) THEN
            CALL SYSTEM_CLOCK(IETDEB,IETRAT,IETMAX)
            CALL CPU_TIME(RCTDEB)
          ENDIF
        ELSE IF (OPTION.EQ.2) THEN
          CALL UTTCPU('CPU.AMUMPT','INIT ',' ')
          CALL UTTCPU('CPU.AMUMPT','DEBUT',' ')
          IF (LDEBUG) THEN
            CALL SYSTEM_CLOCK(IETFIN)
            RETFIN=REAL(IETFIN-IETDEB)/REAL(IETRAT)
            CALL CPU_TIME(RCTFIN)
            WRITE(IFM,*)'REMPLISSAGE MATRICE + 2*JJLDYN '
     &          //'TEMPS CPU/ELAPSED ',RCTFIN-RCTDEB,RETFIN
            CALL SYSTEM_CLOCK(IETDEB,IETRAT,IETMAX)
            CALL CPU_TIME(RCTDEB)
          ENDIF
C       -- ON INTERROMPT LA MESURE CPU.RESO.4 PENDANT CPU.RESO.3 :
          CALL UTTCPU('CPU.RESO.4','FIN',' ')
          CALL UTTCPU('CPU.RESO.3','DEBUT',' ')
        ELSE IF (OPTION.EQ.4) THEN
          CALL UTTCPU('CPU.RESO.3','FIN',' ')
          CALL UTTCPU('CPU.RESO.4','DEBUT',' ')
          CALL UTTCPU('CPU.AMUMPT','FIN',' ')
          CALL UTTCPR('CPU.AMUMPT',6,TEMPS)
          ZI(MONIT(10)+RANG)=INFO(15)
          ZI(MONIT(11)+RANG)=INFO(17)
          CALL UTTCPU('CPU.AMUMPT','INIT ',' ')
          CALL UTTCPU('CPU.AMUMPT','DEBUT',' ')
          IF (LDEBUG) THEN
            CALL SYSTEM_CLOCK(IETFIN)
            RETFIN=REAL(IETFIN-IETDEB)/REAL(IETRAT)
            CALL CPU_TIME(RCTFIN)
            WRITE(IFM,*)'ANALYSE MUMPS '
     &          //'TEMPS CPU/ELAPSED ',RCTFIN-RCTDEB,RETFIN
            CALL SYSTEM_CLOCK(IETDEB,IETRAT,IETMAX)
            CALL CPU_TIME(RCTDEB)
          ENDIF
        ELSE IF (OPTION.EQ.6) THEN
          CALL UTTCPU('CPU.AMUMPT','FIN',' ')
          CALL UTTCPR('CPU.AMUMPT',6,TEMPS)
          ZI(MONIT(2)+RANG)=INFO(9)
          ZI(MONIT(12)+RANG)=INFO(16)
          CALL MPICM2('REDUCE',KMONIT(1))
          CALL MPICM2('REDUCE',KMONIT(2))
          IF (LDEBUG) THEN
            CALL SYSTEM_CLOCK(IETFIN)
            RETFIN=REAL(IETFIN-IETDEB)/REAL(IETRAT)
            CALL CPU_TIME(RCTFIN)
            WRITE(IFM,*)'FACTO NUMERIQUE MUMPS '
     &          //'TEMPS CPU/ELAPSED ',RCTFIN-RCTDEB,RETFIN
          ENDIF
        ELSE IF (OPTION.EQ.7) THEN
          IF (LDEBUG) THEN
            CALL SYSTEM_CLOCK(IETDEB,IETRAT,IETMAX)
            CALL CPU_TIME(RCTDEB)
          ENDIF
        ELSE IF (OPTION.EQ.8) THEN
          CALL UTTCPU('CPU.AMUMPT','INIT ',' ')
          CALL UTTCPU('CPU.AMUMPT','DEBUT',' ')
          IF (LDEBUG) THEN
            CALL SYSTEM_CLOCK(IETFIN)
            RETFIN=REAL(IETFIN-IETDEB)/REAL(IETRAT)
            CALL CPU_TIME(RCTFIN)
            WRITE(IFM,*)'PRETRAITEMENTS RHS '
     &          //'TEMPS CPU/ELAPSED ',RCTFIN-RCTDEB,RETFIN
            CALL SYSTEM_CLOCK(IETDEB,IETRAT,IETMAX)
            CALL CPU_TIME(RCTDEB)
          ENDIF
        ELSE IF (OPTION.EQ.10) THEN
          CALL UTTCPU('CPU.AMUMPT','FIN',' ')
          CALL UTTCPR('CPU.AMUMPT',6,TEMPS)
          IF (LDEBUG) THEN
            CALL SYSTEM_CLOCK(IETFIN)
            RETFIN=REAL(IETFIN-IETDEB)/REAL(IETRAT)
            CALL CPU_TIME(RCTFIN)
            WRITE(IFM,*)'DESCENTE-REMONTEE MUMPS '
     &          //'TEMPS CPU/ELAPSED ',RCTFIN-RCTDEB,RETFIN
            CALL SYSTEM_CLOCK(IETDEB,IETRAT,IETMAX)
            CALL CPU_TIME(RCTDEB)
          ENDIF
        ELSE IF (OPTION.EQ.12) THEN
          IF (LDEBUG) THEN
            CALL SYSTEM_CLOCK(IETFIN)
            RETFIN=REAL(IETFIN-IETDEB)/REAL(IETRAT)
            CALL CPU_TIME(RCTFIN)
            WRITE(IFM,*)'POST-TRAITEMENTS SOLUTION '
     &          //'TEMPS CPU/ELAPSED ',RCTFIN-RCTDEB,RETFIN
          ENDIF
C -- COMMUNICATION DES DONNEES DU MONITORING
          CALL MPICM2('REDUCE',KMONIT(10))
          CALL MPICM2('REDUCE',KMONIT(11))
          CALL MPICM2('REDUCE',KMONIT(12))
C -- AFFICHAGE
           IF (RANG.EQ.0) THEN
             WRITE(IFM,*)
             WRITE(IFM,*)'*********************************************'
     &                  //'*********************************'
             WRITE(IFM,*)'<MONITORING '//NVERS(1:12)//' >'
             WRITE(IFM,'(A19,I9)')' TAILLE DU SYSTEME ',N
             IF (LQUALI) THEN
               WRITE(IFM,'(A28,1PD11.4,1PD11.4)')'CONDITIONNEMENT'//
     &           '/ERREUR ALGO ',RINFOG(10),RINFOG(7)
               WRITE(IFM,'(A23,1PD11.4)')'ERREUR SUR LA SOLUTION ',
     &           RINFOG(9)
             ENDIF
             IAUX1=0
             IAUX2=0
             IAUX3=0
             WRITE(IFM,*)'RANG    '//' NBRE MAILLES    '//
     &         ' NBRE TERMES K    '//' LU FACTEURS'
             DO K=0,NBPROC-1
               WRITE(IFM,1001)K,ZI(MONIT(9)+K),ZI(MONIT(1)+K),
     &                        ZI(MONIT(2)+K)
               IAUX1=IAUX1+ZI(MONIT(9)+K)
               IAUX2=IAUX2+ZI(MONIT(1)+K)
               IAUX3=IAUX3+ZI(MONIT(2)+K)
             ENDDO
C -- EN CENTRALISE ON NE FAIT PAS LA SOMME
             IF (.NOT.LDIST) THEN
               IAUX1=IAUX1/NBPROC
               CALL ASSERT(IAUX1.EQ.ZI(MONIT(9)))
             ENDIF
C
             WRITE(IFM,*)'--------------------------------------------'
     &          //'---------------'
             WRITE(IFM,1003)IAUX1,IAUX2,IAUX3
             WRITE(IFM,*)
             DO I=1,18
               RMONIT(I)=0.D0
             ENDDO

C            MONITORING MEMOIRE
             IF (TYPE.EQ.'S') THEN             
               IBID=SMPSK%ICNTL(22)
             ELSE IF (TYPE.EQ.'C') THEN
               IBID=CMPSK%ICNTL(22)
             ELSE IF (TYPE.EQ.'D') THEN             
               IBID=DMPSK%ICNTL(22)
             ELSE IF (TYPE.EQ.'Z') THEN
               IBID=ZMPSK%ICNTL(22)
	     ELSE
	       CALL ASSERT(.FALSE.)
             ENDIF
             IF (IBID.EQ.0) THEN 
               KTEMP='IN-CORE'
             ELSE 
               KTEMP='OUT-OF-CORE'
             ENDIF
             WRITE(IFM,*)' MEMOIRE RAM ESTIMEE ET REQUISE 
     &             PAR MUMPS EN MO(FAC_NUM + RESOL)'
            
             WRITE(IFM,*)'RANG ASTER : '//
     &            'ESTIM IN-CORE | ESTIM OUT-OF-CORE | RESOL. '//KTEMP  
             
C            POUR LE CALCUL DES MOYENNES            
             RMONIT(1)=0.0
             RMONIT(2)=0.0
             RMONIT(3)=0.0
C            POUR LE CALCUL DES MAX
             RMONIT(4)=ZI(MONIT(10))
             RMONIT(5)=ZI(MONIT(11))
             RMONIT(6)=ZI(MONIT(12))
C            POUR LES CALCULS DES MIN
             RMONIT(7)=ZI(MONIT(10))
             RMONIT(8)=ZI(MONIT(11))
             RMONIT(9)=ZI(MONIT(12))
             DO K=0,NBPROC-1
                WRITE(IFM,1001) K,ZI(MONIT(10)+K),ZI(MONIT(11)+K),
     &               ZI(MONIT(12)+K)
                RMONIT(1)=RMONIT(1)+ZI(MONIT(10)+K)
                RMONIT(2)=RMONIT(2)+ZI(MONIT(11)+K)
                RMONIT(3)=RMONIT(3)+ZI(MONIT(12)+K)
                IF (RMONIT(4) .GT. ZI(MONIT(10)+K))
     &               RMONIT(4)=ZI(MONIT(10)+K)
                IF (RMONIT(5) .GT. ZI(MONIT(11)+K))
     &               RMONIT(5)=ZI(MONIT(11)+K)  
                IF (RMONIT(6) .GT. ZI(MONIT(12)+K))
     &               RMONIT(6)=ZI(MONIT(12)+K)
                IF (RMONIT(7) .LT. ZI(MONIT(10)+K))
     &               RMONIT(7)=ZI(MONIT(10)+K)
                IF (RMONIT(8) .LT. ZI(MONIT(11)+K))
     &               RMONIT(8)=ZI(MONIT(11)+K)  
                IF (RMONIT(9) .LT. ZI(MONIT(12)+K))
     &               RMONIT(9)=ZI(MONIT(12)+K)
                
             ENDDO
             RMONIT(1)=RMONIT(1)/NBPROC
             RMONIT(2)=RMONIT(2)/NBPROC
             RMONIT(3)=RMONIT(3)/NBPROC
             WRITE(IFM,*)'------------------------------------------'//
     &            '-----------------------------------'
             WRITE(IFM,1007) RMONIT(1),RMONIT(2),RMONIT(3)
             WRITE(IFM,*)'------------------------------------------'//
     &            '-----------------------------------'
   
             WRITE(IFM,1008) RMONIT(4),RMONIT(5),RMONIT(6)
             WRITE(IFM,*)'------------------------------------------'//
     &            '-----------------------------------'
         
             WRITE(IFM,1009) RMONIT(7),RMONIT(8),RMONIT(9)
             WRITE(IFM,*)'------------------------------------------'//
     &            '-----------------------------------'
                   
      
             WRITE(IFM,*)'*********************************************'
     &                  //'*********************************'
 1001        FORMAT(' N ',I4,' :    ',I12,'    ',I12,'    ',I12)
 1002        FORMAT(' N ',I4,' :',1PD10.2,' ',1PD10.2,' ',1PD10.2,7X,
     &              1PD10.2,' ',1PD10.2,' ',1PD10.2)
 1003        FORMAT('TOTAL   : ',I15,' ',I15,' ',I15)
 1004        FORMAT('MOYENNE :',1PD10.2,' ',1PD10.2,' ',1PD10.2,7X,
     &              1PD10.2,' ',1PD10.2,' ',1PD10.2)
 1005        FORMAT('        :',1PD10.2,' ',1PD10.2,' ',1PD10.2,7X,
     &              1PD10.2,' ',1PD10.2,' ',1PD10.2)
 1006        FORMAT('        :',1PD10.2,' ',1PD10.2,' ',1PD10.2,7X,
     &              1PD10.2,' ',1PD10.2,' ',1PD10.2)
           ENDIF

 1007      FORMAT('MOYENNE :      ',1PD10.2,'      ',1PD10.2,'      ',
     &            1PD10.2)
 1008      FORMAT('MINIMUM :      ',1PD10.2,'      ',1PD10.2,'      ',
     &            1PD10.2)
 1009      FORMAT('MAXIMUM :      ',1PD10.2,'      ',1PD10.2,'      ',
     &            1PD10.2)

           DO I=1,NBPROC
             ZI(MONIT(1)+I-1)=0
             ZI(MONIT(2)+I-1)=0
             ZI(MONIT(10)+I-1)=0
             ZI(MONIT(11)+I-1)=0
             ZI(MONIT(12)+I-1)=0             
           ENDDO
       
        ELSE
C --- OPTION IMPREVUE
          CALL ASSERT(.FALSE.)

        ENDIF
      ENDIF
  999 CALL JEDEMA()
#endif
      END
