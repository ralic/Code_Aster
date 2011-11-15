        SUBROUTINE LCDPEC(VIND,NBCOMM,NMAT,NDT,CPMONO,MATERF,ITER,NVI,
     &          ITMAX, TOLER, PGL, NFS,NSG,TOUTMS,HSR, DT,DY,YD,VINF,
     &          TAMPON,COMP,SIGF,DF,NR,MOD, CODRET)
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/11/2011   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_21 CRS_1404
C     POST-TRAITEMENTS POUR LE MONOCRISTAL
C     DEFORMATION PLASTIQUE EQUIVALENTE CUMULEE MACROSCOPIQUE
C     RECALCUL DES 3 VARIABLES INTERNES PAR SYSTEME
C  IN VIND   :  VARIABLE INTERNES A T
C     NBCOMM :  INCIDES DES COEF MATERIAU
C     NMAT   :  DIMENSION MATER ET DE NBCOMM
C     NDT    :  NOMBRE DE CMP DE SIG (6)
C     NBCOMM :  INCIDES DES COEF MATERIAU monocristal
C     MATERF :  COEF MATERIAU
C     ITER   :  NOMBRE D ITERATIONS POUR CONVERGER
C     NVI    :  NOMBRE DE VARIABLES INTERNES
C     ITMAX  :  ITER_INTE_MAXI
C     TOLER  :  RESI_INTE_RELA
C     PGL    :  MATRICE DE PASSAGE
C     TOUTMS :  TENSEURS D'ORIENTATION monocristal
C     HSR    :  MATRICE D'INTERACTION monocristal
C     DT     :  INCREMENT DE TEMPS
C     DY     :  INCREMENT DES VARIABLES Y
C     YD     :  VARIABLES A T   = ( SIGD  VARD  )
C     TAMPON :  DONNES GEOM SUIVANT LE TE APPELANT
C     COMP   :  COMPOR - LOI ET TYPE DE DEFORMATION
C     SIGF   :  CONRIANTES DE CAUCHY (HPP) OU KIRCHHOFF (GDEF)
C     DF     :  GRADIENT DF
C     NR     :  DIMENSION DECLAREE DRDY
C     MOD    :  TYPE DE MODELISATION
C     CODRET :  CODE RETOUR
C VAR VINF   :  VARIABLES INTERNES A L'INSTANT ACTUEL

C     ----------------------------------------------------------------
      INTEGER  NMAT,NDT,I,J,NBCOMM(NMAT,3),NBSYS,IFA,IS,NBFSYS,ITMAX
      INTEGER  NUVI,ITER,NVI,IRET,IR,NR,NS,NSFA,NSFV,IFL,NUECOU,CODRET
      INTEGER  NFS,NSG
      REAL*8   VIND(*),VINF(*),DY(*),MATERF(NMAT*2)
      REAL*8   LCNRTE, EPSEQ,PGL(3,3),MUS(6),NG(3),DGAMMA,DP,DALPHA
      REAL*8   DEVI(6),TOUTMS(NFS,NSG,6),TOLER,HSR(NSG,NSG)
      REAL*8   TAUS,FKOOH(6,6),MSNS(3,3),YD(*),IDEN(3,3)
      REAL*8   CRIT, SGNS, DT,OMP(3),QM(3,3),FP(3,3)
      REAL*8   SICL,LG(3),TAMPON(*),RP,R8MIEM
      REAL*8   PK2(6),DF(3,3),ID6(6),EXPBP(NSG)
      REAL*8   FETFE6(6),GAMSNS(3,3),FE(3,3),SIGF(6)
      CHARACTER*16 CPMONO(5*NMAT+1),NOMFAM,COMP(*)
      CHARACTER*8 MOD
      DATA IDEN/1.D0,0.D0,0.D0, 0.D0,1.D0,0.D0, 0.D0,0.D0,1.D0/
      DATA ID6/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/
C
      CODRET=0
      IRET=0
      SICL=-R8MIEM()
C     CAS MONO1 : ON RECALCULE LES VARIABLES INTERNES      
      CALL R8INIR(6, 0.D0, DEVI, 1)                        
      CALL R8INIR(3, 0.D0, OMP, 1)                        
                             
      NBFSYS=NBCOMM(NMAT,2)
                                     
C     NSFA : debut de la famille IFA dans DY et YD
      NSFA=6
C     NSFV : debut de la famille IFA dans les variables internes
      NSFV=6

      IF (NBCOMM(NMAT,1).GT.0) THEN
C        ROTATION RESEAU 
         IR=1
         DO 24 I = 1, 3
         DO 24 J=1,3
            QM(I,J)=VIND(NVI-19+3*(I-1)+J)+IDEN(I,J)
 24      CONTINUE
      ELSE
         IR=0
      ENDIF
      
      IF (COMP(3)(1:5).NE.'PETIT') THEN
         IF (MATERF(NMAT).EQ.0) THEN
            CALL LCOPIL  ( 'ISOTROPE' , MOD , MATERF(1) , FKOOH )
         ELSEIF (MATERF(NMAT).EQ.1) THEN
            CALL LCOPIL  ( 'ORTHOTRO' , MOD , MATERF(1) , FKOOH )
         ENDIF
         CALL LCPRMV(FKOOH,SIGF,FETFE6)
         CALL DSCAL(6,2.D0,FETFE6,1)
         CALL DAXPY(6,1.D0,ID6,1,FETFE6,1)
         CALL R8INIR(9,0.D0,GAMSNS,1)
      ENDIF

      DO 6 IFA=1,NBFSYS
      
         IFL=NBCOMM(IFA,1)           
         NUECOU=NINT(MATERF(NMAT+IFL))
         NOMFAM=CPMONO(5*(IFA-1)+1)       
         
         CALL LCMMSG(NOMFAM,NBSYS,0,PGL,MUS,NG,LG,0,QM)
         
         DO 7 IS=1,NBSYS
         
            CALL CALTAU(COMP,IFA,IS,SIGF,FKOOH,NFS,NSG,TOUTMS,
     &                  TAUS,MUS,MSNS)
            
            CALL LCMMLC(NMAT,NBCOMM,CPMONO,NFS,NSG,HSR,NSFV,NSFA,IFA,
     &       NBSYS,IS,DT,NVI,VIND,YD,DY,ITMAX,TOLER,MATERF,EXPBP,TAUS,
     &                  DALPHA,DGAMMA,DP,CRIT,SGNS,RP,IRET)

            IF (IRET.GT.0) GOTO 9999       
     
            IF (COMP(3)(1:5).EQ.'PETIT') THEN
               DO 19 I=1,6
                  DEVI(I)=DEVI(I)+MUS(I)*DGAMMA
 19            CONTINUE
            ELSE
               CALL DAXPY(9,DGAMMA,MSNS,1,GAMSNS,1)
            ENDIF
            
C STOCKAGE DES VARIABLES INTERNES PAR SYSTEME DE GLISSEMENT

            NUVI=NSFV+3*(IS-1)+3
            VINF(NUVI-2)=VIND(NUVI-2)+DALPHA
            VINF(NUVI-1)=VIND(NUVI-1)+DGAMMA
            VINF(NUVI ) =VIND(NUVI)+DP
            
            IF ((NUECOU.EQ.4).OR.(NUECOU.EQ.5)) THEN
                IF (VINF(NUVI-2).LT.0.D0) CODRET=1
            ENDIF

C CONTRAINTE DE CLIVAGE            
            CALL LCMCLI(COMP,NOMFAM,NBSYS,IS,PGL,SIGF,SICL)
            
            CALL LCMMSG(NOMFAM,NBSYS,IS,PGL,MUS,NG,LG,IR,QM)
            IF (IR.EQ.1) THEN
C              ROTATION RESEAU - CALCUL DE OMEGAP         
               OMP(1)=OMP(1)+DGAMMA*0.5D0*(NG(2)*LG(3)-NG(3)*LG(2))
               OMP(2)=OMP(2)+DGAMMA*0.5D0*(NG(3)*LG(1)-NG(1)*LG(3))
               OMP(3)=OMP(3)+DGAMMA*0.5D0*(NG(1)*LG(2)-NG(2)*LG(1))
            ENDIF
            
  7      CONTINUE
  
         NSFA=NSFA+NBSYS
         NSFV=NSFV+NBSYS*3
                                
  6   CONTINUE
            
C     ROTATION RESEAU DEBUT
      IF (IR.EQ.1) THEN
          CALL LCMMRO(TAMPON,OMP,NVI,VIND,VINF)
      ENDIF
C ROTATION RESEAU FIN      

      IF (COMP(3)(1:5).NE.'PETIT') THEN
         NS=NR-NDT
         CALL CALCFE(NR,NDT,VIND,DF,GAMSNS,FE,FP,IRET)             
         IF (IRET.GT.0) GOTO 9999       
         
C        CALCUL DES CONTRAINTES DE KIRCHOFF
         CALL DCOPY(6,SIGF,1,PK2,1)
         CALL DSCAL(3,SQRT(2.D0),PK2(4),1)
         CALL PK2SIG(3,FE,1.D0,PK2,SIGF,1)
         
C les racine(2) attendues par NMCOMP :-)       
         CALL DSCAL(3,SQRT(2.D0),SIGF(4),1)

         CALL DAXPY(9,-1.D0,IDEN,1,FE,1)
         CALL DCOPY(9,FE,1,VINF(6+3*NS+1),1)

         CALL LCGRLA ( FP,DEVI)
         CALL DCOPY(6,DEVI,1,VINF,1)
         CALL DSCAL(3,SQRT(2.D0),DEVI(4),1)
         
         CALL DAXPY(9,-1.D0,IDEN,1,FP,1)
         CALL DCOPY(9,FP,1,VINF(6+3*NS+10),1)
         
         EPSEQ = LCNRTE(DEVI)
         VINF (NVI-1) = EPSEQ
         
      ELSE                              
         DO 8 I=1,6                                           
            VINF(I)=VIND(I)+DEVI(I)                           
  8      CONTINUE                                             
         EPSEQ = LCNRTE(DEVI)
         VINF (NVI-1) = VIND (NVI-1) + EPSEQ
      ENDIF

      
      VINF(NVI-2) = SICL
                                               
      VINF (NVI) = ITER
      
 9999 CONTINUE
      CODRET=MAX(CODRET,IRET)
      END
