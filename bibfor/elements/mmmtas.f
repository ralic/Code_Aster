      SUBROUTINE MMMTAS(NBDM  ,NDIM  ,NNL   ,NNE   ,NNM   ,
     &                  NBCPS ,LFROTT,MATRCC,MATREE,MATRMM,
     &                  MATREM,MATRME,MATRCE,MATRCM,MATRMC,
     &                  MATREC,MATRFF,MATRFE,MATRFM,MATRMF,
     &                  MATREF,MMAT  )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 22/12/2009   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_21
C
      IMPLICIT NONE
      INTEGER      NBDM,NDIM,NNL,NNE,NNM,NBCPS
      LOGICAL      LFROTT
      REAL*8       MATRCC(9,9)
      REAL*8       MATREE(27,27),MATRMM(27,27)
      REAL*8       MATREM(27,27),MATRME(27,27)
      REAL*8       MATRCE(9,27) ,MATRCM(9,27)
      REAL*8       MATREC(27,9) ,MATRMC(27,9)
      REAL*8       MATRFF(18,18)   
      REAL*8       MATRFE(18,27),MATRFM(18,27)  
      REAL*8       MATRMF(27,18),MATREF(27,18)
      REAL*8       MMAT(81,81)
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
C
C ASSEMBLAGE DES MATRICES
C
C ----------------------------------------------------------------------
C
C
C IN  NBDM   : NOMBRE DE COMPOSANTES/NOEUD DES DEPL+LAGR_C+LAGR_F
C IN  NDIM   : DIMENSION DU PROBLEME
C IN  NNL    : NOMBRE DE NOEUDS LAGRANGE DE CONTACT
C IN  MATRCC : MATRICE ELEMENTAIRE LAGR_C/LAGR_C
C OUT MMAT   : MATRICE ELEMENTAIRE DE CONTACT/FROTTEMENT
C
C ----------------------------------------------------------------------
C
      INTEGER   II,JJ,KK,LL
      INTEGER   NBCPF
      INTEGER   INOC,INOE,INOM,INOF,ICMP,IDIM
      INTEGER   INOE1,INOE2,INOM1,INOM2,IDIM1,IDIM2
      INTEGER   INOC1,INOC2,INOF1,INOF2,ICMP1,ICMP2
      LOGICAL   DEBUG
C
C ----------------------------------------------------------------------
C 
      NBCPF  = NBCPS - 1
      DEBUG  = .FALSE.
C      
C --- CONTACT/CONTACT
C
      DO 10 INOC1 = 1,NNL
        DO 11 INOC2 = 1,NNL
          II = NBDM*(INOC1-1)+NDIM+1
          JJ = NBDM*(INOC2-1)+NDIM+1
          KK = INOC1
          LL = INOC2
          MMAT(II,JJ) = MMAT(II,JJ)+MATRCC(KK,LL) 
          IF (DEBUG) CALL MMMTDB(MATRCC(KK,LL),'CC',II,JJ)          
 11     CONTINUE
 10   CONTINUE
C
C --- CONTACT/ESCL
C
      DO 20 INOC = 1,NNL
        DO 21 INOE = 1,NNE
          DO 22 IDIM = 1,NDIM        
            II  = NBDM*(INOC-1)+NDIM+1
            JJ  = NBDM*(INOE-1)+IDIM
            KK  = INOC
            LL  = NDIM*(INOE-1)+IDIM
            MMAT(II,JJ) = MMAT(II,JJ)+MATRCE(KK,LL)
            IF (DEBUG) CALL MMMTDB(MATRCE(KK,LL),'CE',II,JJ)   
 22       CONTINUE         
 21     CONTINUE
 20   CONTINUE
C
C --- ESCL/CONTACT
C
      DO 23 INOC = 1,NNL
        DO 24 INOE = 1,NNE
          DO 25 IDIM = 1,NDIM        
            II = NBDM*(INOC-1)+NDIM+1
            JJ = NBDM*(INOE-1)+IDIM
            KK = INOC
            LL = NDIM*(INOE-1)+IDIM            
            MMAT(JJ,II) = MMAT(JJ,II)+MATREC(LL,KK)
            IF (DEBUG) CALL MMMTDB(MATREC(LL,KK),'EC',JJ,II)
 25       CONTINUE         
 24     CONTINUE
 23   CONTINUE
C
C --- CONTACT/MAIT
C
      DO 30 INOC = 1,NNL
        DO 31 INOM = 1,NNM
          DO 32 IDIM = 1,NDIM        
            II = NBDM*(INOC-1)+NDIM+1
            JJ = NBDM*NNE+NDIM*(INOM-1)+IDIM 
            KK = INOC
            LL = NDIM*(INOM-1)+IDIM
            MMAT(II,JJ) = MMAT(II,JJ)+MATRCM(KK,LL)
            IF (DEBUG) CALL MMMTDB(MATRCM(KK,LL),'CM',II,JJ)
 32       CONTINUE         
 31     CONTINUE
 30   CONTINUE
C
C --- MAIT/CONTACT
C
      DO 33 INOC = 1,NNL
        DO 34 INOM = 1,NNM
          DO 35 IDIM = 1,NDIM        
            II = NBDM*(INOC-1)+NDIM+1
            JJ = NBDM*NNE+NDIM*(INOM-1)+IDIM 
            KK = NDIM*(INOM-1)+IDIM
            LL = INOC
            MMAT(JJ,II) = MMAT(JJ,II)+MATRMC(KK,LL)
            IF (DEBUG) CALL MMMTDB(MATRMC(KK,LL),'MC',JJ,II)
 35       CONTINUE         
 34     CONTINUE
 33   CONTINUE
C 
C --- ESCL/ESCL
C
      DO 40 INOE1 = 1,NNE
        DO 41 INOE2 = 1,NNE
          DO 42 IDIM2 = 1,NDIM 
            DO 43 IDIM1 = 1,NDIM
              II = NBDM*(INOE1-1)+IDIM1
              JJ = NBDM*(INOE2-1)+IDIM2              
              KK = NDIM*(INOE1-1)+IDIM1
              LL = NDIM*(INOE2-1)+IDIM2 
              MMAT(II,JJ) = MMAT(II,JJ)+MATREE(KK,LL)
              IF (DEBUG) CALL MMMTDB(MATREE(KK,LL),'EE',II,JJ)
 43         CONTINUE           
 42       CONTINUE         
 41     CONTINUE
 40   CONTINUE 
C
C --- MAIT/MAIT
C
      DO 50 INOM1 = 1,NNM
        DO 51 INOM2 = 1,NNM
          DO 52 IDIM2 = 1,NDIM 
            DO 53 IDIM1 = 1,NDIM    
              KK = NDIM*(INOM1-1)+IDIM1
              LL = NDIM*(INOM2-1)+IDIM2               
              II = NBDM*NNE+NDIM*(INOM1-1)+IDIM1
              JJ = NBDM*NNE+NDIM*(INOM2-1)+IDIM2 
              MMAT(II,JJ) = MMAT(II,JJ)+MATRMM(KK,LL)
              IF (DEBUG) CALL MMMTDB(MATRMM(KK,LL),'MM',II,JJ)
 53         CONTINUE
 52       CONTINUE
 51     CONTINUE
 50   CONTINUE
C
C --- ESCL/MAIT
C
      DO 60 INOE = 1,NNE
        DO 61 INOM = 1,NNM
          DO 62 IDIM2 = 1,NDIM 
            DO 63 IDIM1 = 1,NDIM
              KK = NDIM*(INOE-1)+IDIM1
              LL = NDIM*(INOM-1)+IDIM2
              II = NBDM*(INOE-1)+IDIM1
              JJ = NBDM*NNE+NDIM*(INOM-1)+IDIM2
              MMAT(II,JJ) = MMAT(II,JJ)+MATREM(KK,LL)
              IF (DEBUG) CALL MMMTDB(MATREM(KK,LL),'EM',II,JJ)
 63         CONTINUE
 62       CONTINUE
 61     CONTINUE
 60   CONTINUE
C
C --- MAIT/ESCL
C
      DO 70 INOE = 1,NNE
        DO 71 INOM = 1,NNM
          DO 72 IDIM2 = 1,NDIM 
            DO 73 IDIM1 = 1,NDIM
              II = NBDM*NNE+NDIM*(INOM-1)+IDIM2
              JJ = NBDM*(INOE-1)+IDIM1
              KK = NDIM*(INOM-1)+IDIM2 
              LL = NDIM*(INOE-1)+IDIM1
              MMAT(II,JJ) = MMAT(II,JJ)+MATRME(KK,LL)
              IF (DEBUG) CALL MMMTDB(MATRME(KK,LL),'ME',II,JJ)
 73         CONTINUE           
 72       CONTINUE         
 71     CONTINUE
 70   CONTINUE 
 
      IF (.NOT.LFROTT) GOTO 999
C
C --- FROTTEMENT/FROTTEMENT
C
      DO 110 INOF1 = 1,NNL
        DO 111 INOF2 = 1,NNL
          DO 112 ICMP1 = 1,NBCPF
            DO 113 ICMP2 = 1,NBCPF          
              II = NBDM*(INOF1-1)+NDIM+1+ICMP1
              JJ = NBDM*(INOF2-1)+NDIM+1+ICMP2
              KK = (NDIM-1)*(INOF1-1)+ICMP1
              LL = (NDIM-1)*(INOF2-1)+ICMP2              
              MMAT(II,JJ) = MMAT(II,JJ)+MATRFF(KK,LL)
             IF (DEBUG)  CALL MMMTDB(MATRFF(KK,LL),'FF',II,JJ)
 113        CONTINUE
 112      CONTINUE
 111    CONTINUE
 110  CONTINUE
C
C --- FROTTEMENT/ESCL
C
      DO 120 INOF = 1,NNL
        DO 121 INOE = 1,NNE
          DO 122 ICMP = 1,NBCPF
            DO 123 IDIM = 1,NDIM 
              KK = NBCPF*(INOF-1)+ICMP
              LL = NDIM*(INOE-1)+IDIM                   
              II = NBDM*(INOF-1)+NDIM+1+ICMP
              JJ = NBDM*(INOE-1)+IDIM
              MMAT(II,JJ) = MMAT(II,JJ)+MATRFE(KK,LL)
              IF (DEBUG) CALL MMMTDB(MATRFE(KK,LL),'FE',II,JJ)
 123        CONTINUE
 122      CONTINUE
 121    CONTINUE
 120  CONTINUE
C
C --- ESCL/FROTTEMENT
C
      DO 220 INOE = 1,NNE
        DO 221 INOF = 1,NNL
          DO 222 ICMP = 1,NBCPF
            DO 223 IDIM = 1,NDIM 
              KK = NDIM*(INOE-1)+IDIM
              LL = NBCPF*(INOF-1)+ICMP
              II = NBDM*(INOE-1)+IDIM          
              JJ = NBDM*(INOF-1)+NDIM+1+ICMP
              MMAT(II,JJ) = MMAT(II,JJ)+MATREF(KK,LL)
              IF (DEBUG) CALL MMMTDB(MATRFE(KK,LL),'FE',II,JJ)
 223        CONTINUE
 222      CONTINUE
 221    CONTINUE
 220  CONTINUE
C
C --- FROTTEMENT/MAITRE
C
      DO 320 INOF = 1,NNL
        DO 321 INOM = 1,NNM
          DO 322 ICMP = 1,NBCPF
            DO 323 IDIM = 1,NDIM 
              KK = NBCPF*(INOF-1)+ICMP
              LL = NDIM*(INOM-1)+IDIM                   
              II = NBDM*(INOF-1)+NDIM+1+ICMP
              JJ = NBDM*NNE+NDIM*(INOM-1)+IDIM
              MMAT(II,JJ) = MMAT(II,JJ)+MATRFM(KK,LL)
              IF (DEBUG) CALL MMMTDB(MATRFM(KK,LL),'FM',II,JJ)
 323        CONTINUE
 322      CONTINUE
 321    CONTINUE
 320  CONTINUE
C
C --- MAITRE/FROTTEMENT
C
      DO 420 INOM = 1,NNM
        DO 421 INOF = 1,NNL
          DO 422 ICMP = 1,NBCPF
            DO 423 IDIM = 1,NDIM 
              KK = NDIM*(INOM-1)+IDIM 
              LL = NBCPF*(INOF-1)+ICMP
              II = NBDM*NNE+NDIM*(INOM-1)+IDIM            
              JJ = NBDM*(INOF-1)+NDIM+1+ICMP
              MMAT(II,JJ) = MMAT(II,JJ)+MATRMF(KK,LL)
              IF (DEBUG) CALL MMMTDB(MATRMF(KK,LL),'MF',II,JJ)
 423        CONTINUE
 422      CONTINUE
 421    CONTINUE
 420  CONTINUE
 999  CONTINUE
C
      END
