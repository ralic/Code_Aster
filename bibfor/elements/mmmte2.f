      SUBROUTINE MMMTE2(NDIM  ,NNL   ,NNE   ,NNM   ,NBCPF ,
     &                  NDEXCL,MATRFF,MATRFE,MATRFM,MATREF,
     &                  MATRMF)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 18/04/2011   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      INTEGER      NDIM,NNE,NNL,NNM,NBCPF
      INTEGER      NDEXCL(10)
      REAL*8       MATRFF(18,18)
      REAL*8       MATREF(27,18),MATRFE(18,27)
      REAL*8       MATRMF(27,18),MATRFM(18,27)              
C
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - CALCUL)
C
C CALCUL DES MATRICES - MODIFICATIONS EXCL_FROT
C
C ----------------------------------------------------------------------
C
C
C IN  NDIM   : DIMENSION DU PROBLEME
C IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
C IN  NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
C IN  NNL    : NOMBRE DE NOEUDS DE LAGRANGE
C IN  NBCPF  : NOMBRE DE COMPOSANTES/NOEUD DES LAGR_F
C IN  NDEXCL : TABLEAU DES NOEUDS CONCERNES
C OUT MATRFF : MATRICE ELEMENTAIRE LAGR_F/LAGR_F
C OUT MATRFE : MATRICE ELEMENTAIRE LAGR_F/DEPL_E
C OUT MATRFM : MATRICE ELEMENTAIRE LAGR_F/DEPL_M
C OUT MATREF : MATRICE ELEMENTAIRE DEPL_E/LAGR_F
C OUT MATRMF : MATRICE ELEMENTAIRE DEPL_M/LAGR_F
C
C ----------------------------------------------------------------------
C
      INTEGER   INOE,INOM,INOF,IDIM,JJ,II,L,ICMP,I
C
C ----------------------------------------------------------------------
C
      DO 10 I = 1,NNL
        IF (NDEXCL(I).EQ.1) THEN
          DO 11 L = 1,NBCPF
            IF ((L.EQ.2).AND.(NDEXCL(10).EQ.0)) THEN
              GOTO 11
            ENDIF
            II = NBCPF*(I-1)+L
            MATRFF(II,II) = 1.D0
 11       CONTINUE
        ENDIF
 10   CONTINUE
C
      DO 20 INOF = 1,NNL
        IF (NDEXCL(INOF).EQ.1) THEN
          DO 21 INOE = 1,NNE
            DO 22 ICMP = 1,NBCPF
              IF ((ICMP.EQ.2).AND.(NDEXCL(10).EQ.0)) THEN
                GOTO 22
              ENDIF
              DO 23 IDIM = 1,NDIM
                II = NBCPF*(INOF-1)+ICMP
                JJ = NDIM*(INOE-1)+IDIM
                MATRFE(II,JJ) = 0.D0
 23           CONTINUE
 22         CONTINUE
 21       CONTINUE
        ENDIF
 20   CONTINUE
C
      DO 30 INOF = 1,NNL
        IF (NDEXCL(INOF).EQ.1) THEN
          DO 31 INOM = 1,NNM
            DO 32 ICMP = 1,NBCPF
              IF ((ICMP.EQ.2).AND.(NDEXCL(10).EQ.0)) THEN
                GOTO 32
              ENDIF
              DO 33 IDIM = 1,NDIM
                II     = NBCPF*(INOF-1)+ICMP
                JJ     = NDIM*(INOM-1)+IDIM
                MATRFM(II,JJ) = 0.D0
 33           CONTINUE
 32         CONTINUE
 31      CONTINUE
        ENDIF
 30   CONTINUE
C
      DO 40 INOF = 1,NNL
        IF (NDEXCL(INOF).EQ.1) THEN
          DO 41 INOE = 1,NNE
            DO 42 ICMP = 1,NBCPF
              IF ((ICMP.EQ.2).AND.(NDEXCL(10).EQ.0)) THEN
                GOTO 42
              ENDIF
              DO 43 IDIM = 1,NDIM
                JJ = NBCPF*(INOF-1)+ICMP
                II = NDIM*(INOE-1)+IDIM
                MATREF(II,JJ) = 0.D0
 43           CONTINUE
 42         CONTINUE
 41       CONTINUE
        ENDIF
 40   CONTINUE
C
      DO 50 INOF = 1,NNL
        IF (NDEXCL(INOF).EQ.1) THEN
          DO 51 INOM = 1,NNM
            DO 52 ICMP = 1,NBCPF
              IF ((ICMP.EQ.2).AND.(NDEXCL(10).EQ.0)) THEN
                GOTO 52
              ENDIF
              DO 53 IDIM = 1,NDIM
                JJ     = NBCPF*(INOF-1)+ICMP
                II     = NDIM*(INOM-1)+IDIM
                MATRMF(II,JJ) = 0.D0
 53           CONTINUE
 52         CONTINUE
 51       CONTINUE
        ENDIF
 50   CONTINUE 
C
      END
