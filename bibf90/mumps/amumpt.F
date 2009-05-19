      SUBROUTINE  AMUMPT(OPTION,KMONIT,TEMPS,RANG,NBPROC,KXMPS,
     &                   LQUALI,RDESQ,TYPE)
      IMPLICIT NONE
!           CONFIGURATION MANAGEMENT OF EDF VERSION                  
!& MODIF AMUMPC MUMPS  DATE 18/05/2009   AUTEUR LEFEBVRE J-P.LEFEBVRE 
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
C BUT : ROUTINE DE MONITORING POUR AMUMPR/C.
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
C---------------------------------------------------------------
 
      INTEGER      OPTION,RANG,NBPROC,KXMPS
      CHARACTER*1  TYPE
      CHARACTER*24 KMONIT(12)
      REAL*8       TEMPS(6),RDESQ
      LOGICAL      LQUALI

#ifdef _HAVE_MUMPS

C============================================================
      INCLUDE 'mpif.h'
      INCLUDE 'dmumps_struc.h'
      INCLUDE 'zmumps_struc.h'
C============================================================
      INTEGER    NMXINS
      PARAMETER (NMXINS=5)
      TYPE (DMUMPS_STRUC) , TARGET  :: DMPS(NMXINS)
      TYPE (ZMUMPS_STRUC) , TARGET  :: ZMPS(NMXINS)
      TYPE (DMUMPS_STRUC) , POINTER :: XMPSK
      TYPE (ZMUMPS_STRUC) , POINTER :: YMPSK
      INTEGER      IFM,NIV,IBID,IAUX1,IAUX2,IAUX3,K,I,N,INFO(100),IRET,
     &             MONIT(12)
      CHARACTER*1  ROUCS(NMXINS)
      CHARACTER*4  ETAMS(NMXINS)
      CHARACTER*14 NONUS(NMXINS)
      CHARACTER*19 NOMATS(NMXINS),NOSOLS(NMXINS),KTEMP
      REAL*8       RMONIT(18),RAUX,VALR(2),RINFOG(100)
      COMMON /SMUMPS1/ DMPS
      COMMON /SMUMPS2/ ZMPS
      COMMON /SMUMPS/  NONUS,NOMATS,NOSOLS,ETAMS,ROUCS
      
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

C       ------------------------------------------------
C        INITS
C       ------------------------------------------------
C --- REMPLISSAGE DE DIFFERENTS OBJETS SUIVANT LE TYPE DU POINTEUR
C --- DE MUMPS: DMUMPS_STRUC OU ZMUMPS_STRUC
      IF (TYPE.EQ.'R') THEN
        XMPSK=>DMPS(KXMPS)
        N=XMPSK%N
        INFO(9)=XMPSK%INFO(9)
        INFO(10)=XMPSK%INFO(10)
        INFO(15)=XMPSK%INFO(15)
        INFO(16)=XMPSK%INFO(16)
        INFO(17)=XMPSK%INFO(17)
        RINFOG(7)=XMPSK%RINFOG(7)
        RINFOG(9)=XMPSK%RINFOG(9)
        RINFOG(10)=XMPSK%RINFOG(10)
      ELSE IF (TYPE.EQ.'C') THEN
        YMPSK=>ZMPS(KXMPS)
        N=YMPSK%N
        INFO(9)=YMPSK%INFO(9)
        INFO(10)=YMPSK%INFO(10)
        INFO(15)=YMPSK%INFO(15)
        INFO(16)=YMPSK%INFO(16)
        INFO(17)=YMPSK%INFO(17)
        RINFOG(7)=YMPSK%RINFOG(7)
        RINFOG(9)=YMPSK%RINFOG(9)
        RINFOG(10)=YMPSK%RINFOG(10)
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
      KMONIT(3)='&MUMPS.INFO.CPU.FACS'
      KMONIT(4)='&MUMPS.INFO.CPU.ANAL'
      KMONIT(5)='&MUMPS.INFO.CPU.FACN'
      KMONIT(6)='&MUMPS.INFO.CPU.CAEL'
      KMONIT(7)='&MUMPS.INFO.CPU.ASSE'
      KMONIT(8)='&MUMPS.INFO.CPU.SOLV'
      KMONIT(9)='&MUMPS.NB.MAILLE'
      KMONIT(10)='&MUMPS.INFO.MEM.EIC'
      KMONIT(11)='&MUMPS.INFO.MEM.EOC'
      KMONIT(12)='&MUMPS.INFO.MEM.USE'
      CALL JEVEUO(KMONIT(1),'E',MONIT(1))
      CALL JEVEUO(KMONIT(2),'E',MONIT(2))
      CALL JEVEUO(KMONIT(3),'E',MONIT(3))
      CALL JEVEUO(KMONIT(4),'E',MONIT(4))
      CALL JEVEUO(KMONIT(5),'E',MONIT(5))
      CALL JEVEUO(KMONIT(6),'E',MONIT(6))
      CALL JEVEUO(KMONIT(7),'E',MONIT(7))
      CALL JEVEUO(KMONIT(8),'E',MONIT(8))
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
        ELSE IF (OPTION.EQ.2) THEN
          CALL UTTCPU(70,'INIT ',6,TEMPS)
          CALL UTTCPU(70,'DEBUT',6,TEMPS)

        ELSE IF (OPTION.EQ.4) THEN
          CALL UTTCPU(70,'FIN  ',6,TEMPS)
          ZR(MONIT(4)+RANG)=TEMPS(5)+TEMPS(6)
          ZI(MONIT(10)+RANG)=INFO(15)
          ZI(MONIT(11)+RANG)=INFO(17)
          CALL UTTCPU(70,'INIT ',6,TEMPS)
          CALL UTTCPU(70,'DEBUT',6,TEMPS)

        ELSE IF (OPTION.EQ.6) THEN
          CALL UTTCPU(70,'FIN  ',6,TEMPS)
          ZR(MONIT(5)+RANG)=TEMPS(5)+TEMPS(6)
          ZI(MONIT(2)+RANG)=INFO(9)
          ZI(MONIT(12)+RANG)=INFO(16)
          CALL MUMMPI(4,IFM,NIV,KMONIT(1),NBPROC,IBID)
          CALL MUMMPI(4,IFM,NIV,KMONIT(2),NBPROC,IBID)

        ELSE IF (OPTION.EQ.8) THEN
          CALL UTTCPU(70,'INIT ',6,TEMPS)
          CALL UTTCPU(70,'DEBUT',6,TEMPS)

        ELSE IF (OPTION.EQ.10) THEN
          CALL UTTCPU(70,'FIN  ',6,TEMPS)
          ZR(MONIT(8)+RANG)=TEMPS(5)+TEMPS(6)

        ELSE IF (OPTION.EQ.12) THEN
C -- COMMUNICATION DES DONNEES DU MONITORING
           CALL MUMMPI(4,IFM,NIV,KMONIT(3),NBPROC,IBID)
           CALL MUMMPI(4,IFM,NIV,KMONIT(4),NBPROC,IBID)
           CALL MUMMPI(4,IFM,NIV,KMONIT(5),NBPROC,IBID)
           CALL MUMMPI(4,IFM,NIV,KMONIT(6),NBPROC,IBID)
           CALL MUMMPI(4,IFM,NIV,KMONIT(7),NBPROC,IBID)
           CALL MUMMPI(4,IFM,NIV,KMONIT(8),NBPROC,IBID)
           CALL MUMMPI(4,IFM,NIV,KMONIT(10),NBPROC,IBID)
           CALL MUMMPI(4,IFM,NIV,KMONIT(11),NBPROC,IBID)
           CALL MUMMPI(4,IFM,NIV,KMONIT(12),NBPROC,IBID)
C -- AFFICHAGE
           IF (RANG.EQ.0) THEN
             WRITE(IFM,*)
             WRITE(IFM,*)'*********************************************'
     &                  //'*********************************'
             WRITE(IFM,*)'<MONITORING MUMPS >'
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
             WRITE(IFM,*)'--------------------------------------------'
     &          //'---------------'
             WRITE(IFM,1003)IAUX1,IAUX2,IAUX3
             WRITE(IFM,*)
             DO I=1,18
               RMONIT(I)=0.D0
             ENDDO
             WRITE(IFM,*)'TEMPS CPU+SYSTEME'
             WRITE(IFM,*)'RANG '//
     &        ' ASTER: FACS CALCUL_ELEM  ASSEMBLAGE '//
     &        '| MUMPS: ANALYSE  FACTO_NUM  RESOL'
             DO K=0,NBPROC-1
               WRITE(IFM,1002)K,ZR(MONIT(3)+K),ZR(MONIT(6)+K),
     &           ZR(MONIT(7)+K),ZR(MONIT(4)+K),ZR(MONIT(5)+K),
     &           ZR(MONIT(8)+K)
                 DO I=1,6
                   RMONIT(I)=RMONIT(I)+(ZR(MONIT(I+2)+K)/NBPROC)
                 ENDDO
             ENDDO
             WRITE(IFM,*)'------------------------------------------'//
     &                   '-----------------------------------'
             RAUX=0.D0
             DO I=1,6
               RAUX=RAUX+RMONIT(I)
             ENDDO
             DO I=1,6
               IF (RAUX.NE.0.D0)
     &           RMONIT(6+I)=100.D0*RMONIT(I)/RAUX
             ENDDO
             DO K=0,NBPROC-1
               DO I=1,6
                 RAUX=ZR(MONIT(I+2)+K)-RMONIT(I)
                 IF (DABS(RAUX).GT.DABS(RMONIT(12+I)))
     &             RMONIT(12+I)=RAUX
               ENDDO
             ENDDO
             DO I=1,6
               IF (RMONIT(I).NE.0.D0)
     &           RMONIT(12+I)=100.D0*RMONIT(12+I)/RMONIT(I)
             ENDDO
             WRITE(IFM,1004)RMONIT(1),RMONIT(4),RMONIT(5),RMONIT(2),
     &                   RMONIT(3),RMONIT(6)
             WRITE(IFM,*)'EN % : VALEUR RELATIVE ET DESEQUILIBRAGE MAX'
             WRITE(IFM,1005)RMONIT(7),RMONIT(10),RMONIT(11),RMONIT(8),
     &                   RMONIT(9),RMONIT(12)
             WRITE(IFM,1006)RMONIT(13),RMONIT(16),RMONIT(17),RMONIT(14),
     &                   RMONIT(15),RMONIT(18)
             WRITE(IFM,*)
             IF (NBPROC.GT.1) THEN
               DO I=1,6
                 RAUX=ABS(RMONIT(I+6)*RMONIT(I+12))/100
                 VALR(1)=RDESQ
                 IF (RAUX.GE.RDESQ) THEN
                   CALL U2MESR('I','FACTOR_63',1,VALR)
                   GOTO 998
                 ENDIF
               ENDDO
             ENDIF
  998        CONTINUE
C            MONITORING MEMOIRE             
             IF (DMPS(KXMPS)%ICNTL(22).EQ. 0) THEN 
                 KTEMP='IN-CORE'
              ELSE 
                 KTEMP='OUT-OF-CORE'
              END IF
              WRITE(IFM,*)' MEMOIRE RAM ESTIMEE ET REQUISE 
     &             PAR MUMPS EN MO(FAC_NUM + RESOL)'
            
              WRITE(IFM,*)'RANG ASTER : '//
     &             'ESTIM IN-CORE | ESTIM OUT-OF-CORE | RESOL. '//KTEMP  
             
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
C ON REINITIALISE LES VECTEURS DE MONITORING POUR L'EVENTUELLE
C RESOLUTION SUIVANTE (SURTOUT POUR MONIT(6) ET MONIT(7)
           DO I=1,NBPROC
             ZR(MONIT(3)+I-1)=0.D0
             ZR(MONIT(4)+I-1)=0.D0
             ZR(MONIT(5)+I-1)=0.D0
             ZR(MONIT(6)+I-1)=0.D0
             ZR(MONIT(7)+I-1)=0.D0
             ZR(MONIT(8)+I-1)=0.D0
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
