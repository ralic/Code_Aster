      SUBROUTINE TANMGL(T,VMP,VFP,DSPDEP,DSIDEP)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 07/11/2006   AUTEUR MARKOVIC D.MARKOVIC 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
      REAL*8             DSPDEP(6,*),DSIDEP(6,*),RAC2,VMP(2,2),VFP(2,2)
      INTEGER            T(2,2),I,J,K,L,N,M
C ----------------------------------------------------------------------
C
C      LOI GLOBALE POUR LES PLAQUES/COQUES DKT - GLRC_DM
C
C IN:
C       LAMBDA  : PARAMETRE D ELASTICITE - MEMBRANE
C       DEUXMU  : PARAMETRE D ELASTICITE - MEMBRANE
C       LAMF    : PARAMETRE D ELASTICITE - FLEXION
C       DEUMUF  : PARAMETRE D ELASTICITE - FLEXION 
C       GMT     : PARAMETRE GAMMA POUR LA MEMBRANE EN TRACTION 
C       GMC     : PARAMETRE GAMMA POUR LA MEMBRANE EN COMPRESSION
C       GF      : PARAMETRE GAMMA POUR LA FLEXION
C       SEUIL   : INITIAL MEMBRANE
C       ALF     : PARAMETRE DE SEUIL FLEXION
C       VIM     : VARIABLES INTERNES EN T-
C       OPTION  : TOUTE
C OUT:
C       SIG     : CONTRAINTE
C       VIP     : VARIABLES INTERNES EN T+
C       DSIDEP  : MATRICE TANGENTE
C       D2      : ET DE L AUTRE 
C ----------------------------------------------------------------------

      REAL*8   RTEMP4,RTEMP3,RTP4F,RTP3F,RTEMP2,RTP2F,RTMF4,RTMF2
      REAL*8   RTFM4,RTFM2
C ----------------------------------------------------------------------

      RAC2=SQRT(2.D0)
      
      
        DO 20 I=1,2
          DO 21 J=I,2          
            IF (I.EQ.J) THEN
              RTEMP3  = 1.D0
              RTP3F = 1.D0
            ELSE
              RTEMP3  = RAC2
              RTP3F = RAC2
            ENDIF
            DO 22 K=1,2
              DO 23 L=1,2
                IF (T(I,J).GE.T(K,L)) THEN
                  IF (K.EQ.L) THEN
                    RTEMP4  = RTEMP3
                    RTP4F = RTP3F
                    
                    RTMF4   = RTEMP3
                    RTFM4   = RTP3F
                  ELSE
                    RTEMP4  = RTEMP3/RAC2
                    RTP4F = RTP3F/RAC2
                    
                    RTMF4   = RTEMP3/RAC2
                    RTFM4   = RTP3F/RAC2
                  ENDIF
                RTEMP2  = 0.D0                
                RTP2F   = 0.D0                
                RTMF2   = 0.D0                
                RTFM2   = 0.D0                
                DO 24 M=1,2
                  DO 25 N=1,2
        RTEMP2=RTEMP2+VMP(K,M)*
     &        VMP(I,N)*VMP(J,N)*VMP(L,M)*DSPDEP(N,M)
     
     
        RTP2F=RTP2F+VFP(K,M)*
     &        VFP(I,N)*VFP(J,N)*VFP(L,M)*DSPDEP(N+3,M+3)

        RTMF2 = RTMF2 + VFP(K,M)*
     &        VMP(I,N)*VMP(J,N)*VFP(L,M)*DSPDEP(N,M+3)
        RTFM2 = RTFM2 + VMP(K,M)*
     &        VFP(I,N)*VFP(J,N)*VMP(L,M)*DSPDEP(N+3,M)

     
25                CONTINUE
24              CONTINUE
       RTEMP2=RTEMP2+VMP(I,1)*VMP(J,2)*VMP(K,1)*VMP(L,2)*DSPDEP(3,3)
       RTEMP2=RTEMP2+VMP(I,2)*VMP(J,1)*VMP(K,2)*VMP(L,1)*DSPDEP(3,3)

       RTP2F = RTP2F + VFP(I,1)*VFP(J,2)*VFP(K,1)*VFP(L,2)
     &                     * DSPDEP(6,6)
       RTP2F = RTP2F + VFP(I,2)*VFP(J,1)*VFP(K,2)*VFP(L,1)
     &                     * DSPDEP(6,6)

              DSIDEP(T(I,J),T(K,L)) = DSIDEP(T(I,J),T(K,L))
     &                              + RTEMP2*RTEMP4
              DSIDEP(T(I,J)+3,T(K,L)+3) = DSIDEP(T(I,J)+3,T(K,L)+3)
     &                              + RTP2F*RTP4F
     
              DSIDEP(T(I,J)+3,T(K,L)) = DSIDEP(T(I,J)+3,T(K,L))
     &                                + RTFM2*RTFM4
              IF(T(I,J) .NE. T(K,L)) THEN
                DSIDEP(T(K,L)+3,T(I,J)) = DSIDEP(T(K,L)+3,T(I,J))
     &                                + RTMF2*RTMF4
              ENDIF

               ENDIF
23            CONTINUE
22          CONTINUE
21        CONTINUE
20      CONTINUE

        DO 200 J=1,2
          DSIDEP(3  ,J)   = DSIDEP(3  ,J)  /RAC2
          DSIDEP(6  ,J)   = DSIDEP(6  ,J)  /RAC2
          DSIDEP(6  ,J+3) = DSIDEP(6  ,J+3)/RAC2
          DSIDEP(J+3,3) = DSIDEP(J+3,3)/RAC2
200     CONTINUE

        DSIDEP(3,3) = DSIDEP(3,3)/2.0D0
        DSIDEP(6,6) = DSIDEP(6,6)/2.0D0
        DSIDEP(6,3) = DSIDEP(6,3)/2.0D0

        DO 26 I=1,6 
          DO 27 J=I+1,6
            DSIDEP(I,J)=DSIDEP(J,I)
27        CONTINUE
26      CONTINUE

      END
