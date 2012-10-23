      SUBROUTINE TANMGL(VMP,VFP,DSPDEP,DSIDEP)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 22/10/2012   AUTEUR IDOUX L.IDOUX 
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
      IMPLICIT NONE
      REAL*8             DSPDEP(6,*),DSIDEP(6,*),VMP(2,2),VFP(2,2)
      INTEGER            T(2,2),I,J,K,L,N,M
C ----------------------------------------------------------------------
C
C   ROUTINE DE CHANGEMENT DE REPERE DE LA MATRICE TANGENTE POUR LCGLDM
C
C IN:
C       VMP(2,2)    : MATRICE DES VECTEURS PROPRES DE MEMBRANE
C       VFP(2,2)    : MATRICE DES VECTEURS PROPRES DE FLEXION
C       DSPDEP(4,4) : MATRICE TANGENTE DANS LE REPERE PROPRE
C
C OUT:
C       DSIDEP(6,6) : MATRICE TANGENTE DANS LE REPERE GLOBAL
C ----------------------------------------------------------------------
      REAL*8   RTEMP,RTPF,RTMF,RTFM
C ----------------------------------------------------------------------

C MATRICE DE PASSAGE TENSEUR D'ORDRE 4 >> TENSEUR D'ORDRE 2
        T(1,1)=1
        T(1,2)=3
        T(2,1)=3
        T(2,2)=2

        DO 20 I=1,2
          DO 21 J=I,2
            DO 22 K=1,2
              DO 23 L=1,2
                IF (T(I,J).GE.T(K,L)) THEN
                  RTEMP  = 0.D0
                  RTPF   = 0.D0
                  RTMF   = 0.D0
                  RTFM   = 0.D0
                  DO 24 M=1,2
                    DO 25 N=1,2
                      RTEMP = RTEMP + DSPDEP(N,M)
     &                      * VMP(K,M) * VMP(I,N) * VMP(J,N) * VMP(L,M)

                      RTPF  = RTPF + DSPDEP(N+3,M+3)
     &                      * VFP(K,M) * VFP(I,N) * VFP(J,N) * VFP(L,M)

                      RTMF  = RTMF + DSPDEP(N,M+3)
     &                      * VFP(K,M) * VMP(I,N) * VMP(J,N) * VFP(L,M)

                      RTFM  = RTFM + DSPDEP(N+3,M)
     &                      * VMP(K,M) * VFP(I,N) * VFP(J,N) * VMP(L,M)
25                  CONTINUE
24                CONTINUE

                  RTEMP = RTEMP + DSPDEP(3,3)
     &                  * VMP(I,1) * VMP(J,2) * VMP(K,1) * VMP(L,2)
                  RTEMP = RTEMP + DSPDEP(3,3)
     &                  * VMP(I,2) * VMP(J,1) * VMP(K,2) * VMP(L,1)

                  RTPF = RTPF + DSPDEP(6,6)
     &                 * VFP(I,1) * VFP(J,2) * VFP(K,1) * VFP(L,2)
                  RTPF = RTPF + DSPDEP(6,6)
     &                 * VFP(I,2) * VFP(J,1) * VFP(K,2) * VFP(L,1)

                  DSIDEP(T(I,J),T(K,L)) = DSIDEP(T(I,J),T(K,L))
     &                                  + RTEMP
                  DSIDEP(T(I,J)+3,T(K,L)+3) = DSIDEP(T(I,J)+3,T(K,L)+3)
     &                                      + RTPF
                  DSIDEP(T(I,J)+3,T(K,L)) = DSIDEP(T(I,J)+3,T(K,L))
     &                                    + RTFM
                  IF(T(I,J) .NE. T(K,L)) THEN
                    DSIDEP(T(K,L)+3,T(I,J)) = DSIDEP(T(K,L)+3,T(I,J))
     &                                      + RTMF
                  ENDIF
                ENDIF
23            CONTINUE
22          CONTINUE
21        CONTINUE
20      CONTINUE

        DSIDEP(3,3) = DSIDEP(3,3)/2.0D0
        DSIDEP(6,6) = DSIDEP(6,6)/2.0D0
        DSIDEP(6,3) = DSIDEP(6,3)/2.0D0

        DO 26 I=1,6
          DO 27 J=I+1,6
            DSIDEP(I,J)=DSIDEP(J,I)
27        CONTINUE
26      CONTINUE

      END
