      SUBROUTINE FETMON(INFOFE,NBI2,NBI,NBTOT,NBSD,DIMGI,IFM,MAMOY,
     &                  LSTOGI,IFET1,RANG,ITPS,LPARA,OPTION)
C-----------------------------------------------------------------------
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
C-----------------------------------------------------------------------
C    - FONCTION REALISEE:  MONITORING DE ALFETI
C----------------------------------------------------------------------
C RESPONSABLE BOITEAU O.BOITEAU
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      INCLUDE 'jeveux.h'
      INTEGER      NBI2,NBTOT,NBSD,DIMGI,IFM,MAMOY,NBI,IFET1,RANG,ITPS,
     &             OPTION
      LOGICAL      LSTOGI,LPARA
      CHARACTER*24 INFOFE
      
      
C DECLARATION VARIABLES LOCALES
      INTEGER      IFET2,IFET3,IFET4,IFET5,IFET6,IFET7,I,NBPROC,IEXIST,
     &             IMON
      CHARACTER*8  K8BID
      CHARACTER*20 NOMOPT(11)
      CHARACTER*24 NOMMON
      REAL*8       R1,R2,R3,R1M,R2M,R3M,RAUX2

      CALL JEMARQ()      
      IF ((INFOFE(11:11).EQ.'T').AND.(RANG.EQ.0).AND.(OPTION.EQ.1)) THEN
C MONITORING INITIAL DE FETI
        WRITE(IFM,*)
        WRITE(IFM,*)'**************************************************'
        WRITE(IFM,'(A13,I4,A1)')'<FETI/ALFETI ',RANG,'>'
        WRITE(IFM,'(A20,I4)')'NUMERO D''INCREMENT  ',ITPS
        WRITE(IFM,'(A20,I4)')'NB SOUS-DOMAINES    ',NBSD
        WRITE(IFM,'(A20,I4)')'NB DE MODES RIGIDES ',DIMGI
        WRITE(IFM,*)'POINTS INTERFACE / MAILLAGE / RAPPORT'
        WRITE(IFM,1081)NBI2,NBTOT,100.D0* NBI2/NBTOT
        IF (MAMOY.NE.0) THEN
          IF (LSTOGI) THEN
            RAUX2=(100.D0*DIMGI*(NBI+DIMGI))/MAMOY
          ELSE
            RAUX2=(100.D0*DIMGI*DIMGI)/MAMOY    
          ENDIF
        ENDIF
        WRITE(IFM,1082)RAUX2
        CALL JEVEUO('&FETI.INFO.STOCKAGE.FVAF','L',IFET2)
        CALL JEVEUO('&FETI.INFO.STOCKAGE.FNBN','L',IFET3)
        CALL JEVEUO('&FETI.INFO.CPU.FACS','L',IFET4)
        CALL JEVEUO('&FETI.INFO.CPU.ASSE','L',IFET5)
        CALL JEVEUO('&FETI.INFO.CPU.FACN','L',IFET6)
        CALL JEVEUO('&FETI.INFO.CPU.ELEM','L',IFET7)
        CALL JELIRA('&FETI.INFO.CPU.ELEM','LONMAX',NBPROC,K8BID)
        WRITE(IFM,*)
        WRITE(IFM,*)'SOUS-DOMAINE   / MATRICE   / FACTORISEE   / NOEUDS'
        DO 15 I=1,NBSD
          WRITE(IFM,1075)I,ZI(IFET1+I-1),ZI(IFET2+I-1),ZI(IFET3+I-1)
   15   CONTINUE
        WRITE(IFM,*)'--------------------------------------------------'
        WRITE(IFM,1080)ZI(IFET1+NBSD),ZI(IFET2+NBSD),ZI(IFET3+NBSD)
        WRITE(IFM,1083)ZI(IFET1+NBSD)/NBSD,ZI(IFET2+NBSD)/NBSD,
     &                 ZI(IFET3+NBSD)/NBSD
        WRITE(IFM,*)
        WRITE(IFM,*)'SOUS-DOMAINE   / CPU FACSYM / CPU ASSE /'//
     &              ' CPU FACNUM'
        R1=0.D0
        R2=0.D0
        R3=0.D0
        R1M=-1.D0
        R2M=-1.D0
        R3M=-1.D0
        DO 16 I=0,NBSD
          WRITE(IFM,1084)I,ZR(IFET4+I),ZR(IFET5+I),ZR(IFET6+I)
          R1=R1+ZR(IFET4+I)
          R2=R2+ZR(IFET5+I)
          R3=R3+ZR(IFET6+I)
          IF (ZR(IFET4+I).GT.R1M) R1M=ZR(IFET4+I)
          IF (ZR(IFET5+I).GT.R2M) R2M=ZR(IFET5+I)
          IF (ZR(IFET6+I).GT.R3M) R3M=ZR(IFET6+I)         
   16   CONTINUE
        WRITE(IFM,*)'--------------------------------------------------'
        WRITE(IFM,1085)R1,R2,R3
        WRITE(IFM,1086)R1M,R2M,R3M

        R1=0.D0
        R1M=-1.D0
        WRITE(IFM,*)
        WRITE(IFM,*)'PROCESSEUR     / CPU CALCUL_ELEM '
        DO 20 I=1,NBPROC
          WRITE(IFM,1087)I,ZR(IFET7+I-1)
          R1=R1+ZR(IFET7+I-1)
          IF (ZR(IFET7+I-1).GT.R1M) R1M=ZR(IFET7+I-1)
   20   CONTINUE
        WRITE(IFM,*)'-----------------------------------'
        WRITE(IFM,1088)R1
        WRITE(IFM,1089)R1M
        WRITE(IFM,*)'**************************************************'
      ELSE IF ((INFOFE(10:10).EQ.'T').AND.(RANG.EQ.0).AND.LPARA.AND.
     &         (OPTION.EQ.2)) THEN
C PROFILING MPI
        WRITE(IFM,*)
        WRITE(IFM,*)'**************************************************'
        WRITE(IFM,*)' PROFILING MPI'
        NOMMON='&FETI.MONITORING.MPI'
        CALL JEEXIN(NOMMON,IEXIST)
C SI L OBJET NOMMON EXISTE DEJA : ARRET
        CALL ASSERT(IEXIST.EQ.0)
        CALL JEVEUO(NOMMON,'L',IMON)
C NBRE D'OPTION DE FETAM
        NOMOPT(1)='REPARTITION SD'
        NOMOPT(2)='MPI_COMM_RANK'
        NOMOPT(3)='MPI_COMM_SIZE'
        NOMOPT(4)='MPI_REDUCE ENTIER'
        NOMOPT(5)='MPI_REDUCE REEL'
        NOMOPT(6)='MPI_ALLREDUCE ENTIER'
        NOMOPT(7)='MPI_REDUCE 2 REEL'
        NOMOPT(8)='MPI_GATHERV'
        NOMOPT(9)='MPI_BCAST VECTEUR'
        NOMOPT(10)='MPI_BCAST SCALAIRE'
        NOMOPT(11)='MPI_ALLREDUCE REEL'
        
        R1M=0.D0
        WRITE(IFM,*)'APPELS MPI     /  TEMPS CPU / TEMPS SYS /   TOTAL'
        DO 30 I=1,11
          R1=ZR(IMON+2*(I-1))
          R2=ZR(IMON+2*(I-1))
          R3=R1+R2
          R1M=R1M+R3
          WRITE(IFM,1090)NOMOPT(I),R1,R2,R3
   30   CONTINUE
        WRITE(IFM,*)'--------------------------------------------------'
        WRITE(IFM,1091)R1M
        WRITE(IFM,*)'**************************************************'
        WRITE(IFM,*)
        CALL JEDETR(NOMMON)           
      ENDIF

C FORMAT            
 1075 FORMAT(' N ',I4,'     :',I12,' ',I12,' ',I12)
 1080 FORMAT('TOTAL       :',I15,' ',I15,' ',I15)
 1081 FORMAT(I12,' ',I12,'        ',1PD9.2,' %')
 1082 FORMAT('TAILLE (GI + GIT*GI)/MATRICE MOYENNE :',D11.4,' %')
 1083 FORMAT('MOYENNE     :',I12,' ',I12,' ',I12) 
 1084 FORMAT(' N ',I4,'     :     ',D11.4,' ',D11.4,' ',D11.4)
 1085 FORMAT('CPU + SYS TOTAL  :',D11.4,' ',D11.4,' ',D11.4) 
 1086 FORMAT('CPU + SYS LE PIRE:',D11.4,' ',D11.4,' ',D11.4)
 1087 FORMAT(' N ',I4,'     :     ',D11.4)
 1088 FORMAT('CPU + SYS TOTAL  :',D11.4) 
 1089 FORMAT('CPU + SYS LE PIRE:',D11.4)
 1090 FORMAT(A20,' ',D10.2,' ',D10.2,' ',D10.2)
 1091 FORMAT('TOTAL                :',D11.2)

      CALL JEDEMA()      
      END 
