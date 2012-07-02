      SUBROUTINE AMPCPR(CMAT,NB1,NB2,BMAT,N1,N2,I,J,FAC,NPAR,NSYM)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
      IMPLICIT NONE
C
C***********************************************************************
C    P. RICHARD     DATE 12/03/91
C-----------------------------------------------------------------------
C  BUT:  AJOUTER UNE MATRICE PLEINE REELLE A UNE MATRICE PLEINE
C       COMPLEXE (SOIT A LA PARTIE REELLE SOIT A LA PARTIE IMAGINAIRE)
C            MULTIPLICATION POSSIBLE PAR UN FACTEUR
C-----------------------------------------------------------------------
C
C CMAT     /M/: MATRICE RECEPTRICE COMPLEXE
C NB1      /I/: NB DE LIGNES DE LA MATRICE RECEPTRICE
C NB2      /I/: NB DE COLONNES DE LA MATRICE RECEPTRICE
C BMAT     /M/: MATRICE PLEINE RELLE, A AJOUTER
C N1       /I/: NB DE LIGNE DE LA MATRICE A AJOUTER
C N2       /I/: NB DE COLONNE DE LA MATRICE A AJOUTER
C I        /I/: INDICE DU PREMIER TERME DANS RECEPTRICE
C J        /I/: INDICE DE COLONNE TERME  DANS RECEPTRICE
C FAC      /I/: FACTEUR MULTIPLICATIF DE LA MATRICE RELLE AVANT ASSEMBLA
C NPAR     /I/: INDICATEUR PARTIE RELLE (1) OU IMAGINAIRE(2)
C NSYM     /I/: INDICATEUR TRANSPOSITION (-1) MATRICE REELLE OU NON(1)
C
C-----------------------------------------------------------------------
C
      REAL*8 BMAT(N1,N2)
      COMPLEX*16    CMAT(*)
C
C-----------------------------------------------------------------------
C
C   CAS SANS TRANSPOSITION
C
C-----------------------------------------------------------------------
      INTEGER I ,ICOL ,IDEB ,IFIN ,II ,IIDEB ,IIFIN 
      INTEGER ILIG ,ITERME ,J ,JDEB ,JFIN ,JJ ,JJDEB 
      INTEGER JJFIN ,N1 ,N2 ,NB1 ,NB2 ,NPAR ,NSYM 

      REAL*8 FAC 
C-----------------------------------------------------------------------
      IF(NSYM.EQ.1) THEN
C
        JDEB=J
        JFIN=MIN(J+N2-1,NB2)
        IF((J+N2-1).GT.NB2) THEN
          CALL U2MESG('F', 'ALGORITH11_88',0,' ',0,0,0,0.D0)
        ENDIF
        IF(JFIN.LT.JDEB) GOTO 9999
        JJDEB=JDEB-J+1
        JJFIN=JFIN-J+1
C
        IDEB=I
        IF((I+N1-1).GT.NB1) THEN
          CALL U2MESG('F', 'ALGORITH11_88',0,' ',0,0,0,0.D0)
        ENDIF
        IFIN=MIN(I+N1-1,NB1)
        IF(IFIN.LT.IDEB) GOTO 9999
        IIDEB=IDEB-I+1
        IIFIN=IFIN-I+1
C
C    PARTIE RELLE
C
        IF(NPAR.EQ.1) THEN
C
          DO 10 II=IIDEB,IIFIN
            DO 20 JJ=JJDEB,JJFIN
              ILIG = I+II-1
              ICOL = J+JJ-1
              IF (ICOL .GE. ILIG) THEN
                 ITERME = ICOL*(ICOL-1)/2+1+ICOL-ILIG
                 CMAT(ITERME)=CMAT(ITERME)+DCMPLX(BMAT(II,JJ)*FAC,0.D0)
              ENDIF
 20         CONTINUE
 10       CONTINUE
C
        ELSE
C
C    PARTIE IMAGINAIRE
C
          DO 30 II=IIDEB,IIFIN
            DO 40 JJ=JJDEB,JJFIN
              ILIG = I+II-1
              ICOL = J+JJ-1
              IF (ICOL .GE. ILIG) THEN
                 ITERME = ICOL*(ICOL-1)/2+1+ICOL-ILIG
                 CMAT(ITERME)=CMAT(ITERME)+DCMPLX(0.D0,BMAT(II,JJ)*FAC)
              ENDIF
 40         CONTINUE
 30       CONTINUE
C
        ENDIF
      ENDIF
C
C
C     CAS AVEC TRANSPOSITION
C
      IF(NSYM.EQ.-1) THEN
C
        JDEB=J
        JFIN=MIN(J+N1-1,NB2)
        IF((J+N1-1).GT.NB2) THEN
          CALL U2MESG('F', 'ALGORITH11_90',0,' ',0,0,0,0.D0)
        ENDIF
        IF(JFIN.LT.JDEB) GOTO 9999
        JJDEB=JDEB-J+1
        JJFIN=JFIN-J+1
C
        IDEB=I
        IFIN=MIN(I+N2-1,NB1)
        IF((I+N2-1).GT.NB1) THEN
          CALL U2MESG('F', 'ALGORITH11_88',0,' ',0,0,0,0.D0)
        ENDIF
        IF(IFIN.LT.IDEB) GOTO 9999
        IIDEB=IDEB-I+1
        IIFIN=IFIN-I+1
C
C    PARTIE RELLE
C
        IF(NPAR.EQ.1) THEN
C
          DO 50 II=IIDEB,IIFIN
            DO 60 JJ=JJDEB,JJFIN
              ILIG = I+II-1
              ICOL = J+JJ-1
              IF (ICOL .GE. ILIG) THEN
                 ITERME = ICOL*(ICOL-1)/2+1+ICOL-ILIG
                 CMAT(ITERME)=CMAT(ITERME)+DCMPLX(BMAT(JJ,II)*FAC,0.D0)
              ENDIF
 60         CONTINUE
 50       CONTINUE
C
        ELSE
C
C    PARTIE IMAGINAIRE
C
          DO 70 II=IIDEB,IIFIN
            DO 80 JJ=JJDEB,JJFIN
              ILIG = I+II-1
              ICOL = J+JJ-1
              IF (ICOL .GE. ILIG) THEN
                 ITERME = ICOL*(ICOL-1)/2+1+ICOL-ILIG
                 CMAT(ITERME)=CMAT(ITERME)+DCMPLX(0.D0,BMAT(JJ,II)*FAC)
              ENDIF
 80         CONTINUE
 70       CONTINUE
C
        ENDIF
      ENDIF
C
C
 9999 CONTINUE
      END
