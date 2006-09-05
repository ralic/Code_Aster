        SUBROUTINE LCDPEC(VIND,NBCOMM,NMAT,NDT,CPMONO,MATERF,ITER,NVI,
     &                   PGL, TOUTMS,DY, YF, VINF,EPSEQ )
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/09/2006   AUTEUR JOUMANA J.EL-GHARIB 
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
      INTEGER  NMAT,NDT,I,NBCOMM(NMAT,3),NBSYS,IEC,IFA,IS,NBFSYS
      INTEGER  NUV1,NUVI,MONO1,ITER,NVI
      REAL*8   VIND(*),VINF(*),DVIN(6),DY(*),YF(*),MATERF(NMAT*2)
      REAL*8   LCNRTE, EPSEQ,PGL(3,3),D,MS(6),DGAMMA,DP,DALPHA
      REAL*8   ALPHAM,DEVI(6)
        REAL*8 TOUTMS(5,24,6)
      CHARACTER*16    CPMONO(5*NMAT+1),NOMFAM
C

      MONO1=NBCOMM(NMAT,1)
      
      IF (MONO1.EQ.1) THEN

C         CAS MONO1 : ON RECALCULE LES VARIABLES INTERNES
          CALL R8INIR(6, 0.D0, DEVI, 1)
          NBFSYS=NBCOMM(NMAT,2)
          NUVI=6
          NUV1=NDT
 
          DO 6 IFA=1,NBFSYS
 
             NOMFAM=CPMONO(5*(IFA-1)+1)
             IEC=NBCOMM(IFA,2)
             D=MATERF(NMAT+IEC)
 
             CALL LCMMSG(NOMFAM,NBSYS,0,PGL,MS)
             DO 7 IS=1,NBSYS
C            CALL LCMMSG(NOMFAM,NBSYS,IS,PGL,MS)
               DO 101 I=1,6
                  MS(I)=TOUTMS(IFA,IS,I)
 101            CONTINUE  
                IF (MONO1.EQ.1) THEN
                   NUVI=NUVI+3
                   NUV1=NUV1+1
                   DGAMMA=DY(NUV1)
                   DP=ABS(DGAMMA)
                   ALPHAM=VIND(NUVI-2)
                   DALPHA=(DGAMMA-D*ALPHAM*DP)/(1.0D0+D*DP)
                   DO 19 I=1,6
                      DEVI(I)=DEVI(I)+MS(I)*DGAMMA
 19                CONTINUE
 
                   VINF(NUVI-2)=VIND(NUVI-2)+DALPHA
                   VINF(NUVI-1)=VIND(NUVI-1)+DGAMMA
                   VINF(NUVI ) =VIND(NUVI)+DP
 
                ENDIF

  7         CONTINUE
 
  6       CONTINUE
          DO 8 I=1,6
             VINF(I)=VIND(I)+DEVI(I)
8         CONTINUE
       
      ENDIF
      
      CALL LCDIVE(VINF,VIND,DVIN)
      EPSEQ = LCNRTE(DVIN)
      VINF (NVI-1) = VIND (NVI-1) + EPSEQ
      VINF (NVI) = ITER

      END
