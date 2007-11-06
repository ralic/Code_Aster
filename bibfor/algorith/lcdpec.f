        SUBROUTINE LCDPEC(VIND,NBCOMM,NMAT,NDT,CPMONO,MATERF,ITER,NVI,
     &          ITMAX, TOLER, PGL, TOUTMS,DY, YF, VINF,EPSEQ)
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/11/2007   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     DEFORMATION PLASTIQUE EQUIVALENTE CUMULEE MACROSCOPIQUE
C     POUR LE MONOCRISTAL
C     IN  VIND   :  VARIABLES INTERNES A T
C     IN  VINF   :  VARIABLES INTERNES A T+DT
C     ----------------------------------------------------------------
      INTEGER  NMAT,NDT,I,NBCOMM(NMAT,3),NBSYS,IEC,IFA,IS,NBFSYS,ITMAX
      INTEGER  NUV1,NUVI,MONO1,ITER,NVI,IRET,IFL,IU
      REAL*8   VIND(*),VINF(*),DVIN(6),DY(*),YF(*),MATERF(NMAT*2)
      REAL*8   LCNRTE, EPSEQ,PGL(3,3),D,MS(6),NG(3),DGAMMA,DP,DALPHA
      REAL*8   ALPHAM,DEVI(6),TOUTMS(5,24,6),TOLER
      REAL*8   BSD,GCB,KDCS, RACR,SOM, AUX,DAL
C JOUM      
      INTEGER  J
      REAL*8   SI(3,3)
      REAL*8   SING(3)
      REAL*8   SICL, P
C JOUM      
      CHARACTER*16 CPMONO(5*NMAT+1),NOMFAM,NECRCI,NECOUL
      COMMON/KRDAL/DAL(24)
C

C      MONO1=NBCOMM(NMAT,1)
      
C     CAS MONO1 : ON RECALCULE LES VARIABLES INTERNES      
      CALL R8INIR(6, 0.D0, DEVI, 1)                        
      NBFSYS=NBCOMM(NMAT,2)                                
      NUVI=6                                               
      NUV1=NDT                                             
      SICL = 0.D0
 
      DO 6 IFA=1,NBFSYS                                    
 
         NOMFAM=CPMONO(5*(IFA-1)+1)                        
         NECOUL=CPMONO(5*(IFA-1)+3)
         NECRCI=CPMONO(5*(IFA-1)+5)
         CALL LCMMSG(NOMFAM,NBSYS,0,PGL,MS,NG)                
         DO 7 IS=1,NBSYS                                   
            DO 101 I=1,6                                    
              MS(I)=TOUTMS(IFA,IS,I)                       
 101        CONTINUE                                       
            NUVI=NUVI+3                                           
            NUV1=NUV1+1                                           
            DGAMMA=DY(NUV1)                                       
            DP=ABS(DGAMMA)                                        
            ALPHAM=VIND(NUVI-2) 
            
            IF(NECOUL.EQ.'KOCKS_RAUCH') THEN
                DALPHA=DAL(IS)
            ELSE
C           ECROUISSAGE CINEMATIQUE - CALCUL DE DALPHA  
            CALL LCMMFC( MATERF(NMAT+1),IFA,NMAT,NBCOMM,NECRCI,   
     &                ITMAX, TOLER,ALPHAM,DGAMMA,DALPHA, IRET)
            
            ENDIF


            DO 19 I=1,6                                           
               DEVI(I)=DEVI(I)+MS(I)*DGAMMA                       
 19         CONTINUE                                              
 
            VINF(NUVI-2)=VIND(NUVI-2)+DALPHA                      
            VINF(NUVI-1)=VIND(NUVI-1)+DGAMMA                      
            VINF(NUVI ) =VIND(NUVI)+DP
       
C           faire call lcmmsg(NOMFAM,NBSYS,IS,PGL,MS,N) Ns(3)       
C           calcul du max de Ns.(SIGMA.Ns)
C            
            CALL LCMMSG(NOMFAM,NBSYS,IS,PGL,MS,NG) 

C           SIGMA (3,3)

            SI(1,1) = YF(1)
            SI(1,2) = YF(4)/SQRT(2.D0)
            SI(1,3) = YF(5)/SQRT(2.D0)

            SI(2,1) = SI(2,1)
            SI(2,2) = YF(2)
            SI(2,3) = YF(6)/SQRT(2.D0)
      
            SI(3,1) = SI(1,3)
            SI(3,2) = SI(2,3)
            SI(3,3) = YF(3)
           
            DO 9 I = 1,3
               SING(I) = 0.D0
  9         CONTINUE

            DO 11 I = 1 , 3
            DO 10 J = 1 , 3
               SING(I) = SING(I) + SI(I,J) * NG(J)
 10         CONTINUE
 11      CONTINUE

            P = 0.D0
            DO 1 I = 1 , 3
            P = P + SING(I)*NG(I)
 1       CONTINUE
      
         SICL = MAX(SICL, P)
         VINF(NVI-2) = SICL
  7     CONTINUE                                            
 
  6   CONTINUE                                             
      DO 8 I=1,6                                           
         VINF(I)=VIND(I)+DEVI(I)                           
8     CONTINUE                                             
       
      CALL LCDIVE(VINF,VIND,DVIN)
      EPSEQ = LCNRTE(DVIN)
      VINF (NVI-1) = VIND (NVI-1) + EPSEQ
      VINF (NVI) = ITER

      END
