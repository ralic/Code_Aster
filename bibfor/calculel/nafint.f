      SUBROUTINE NAFINT(INT,NINT,LIMA,BDIM,BPAN,DIM,CINE,TMA,
     &                  NO,NRM,CNX,CNXC,NTM,DICO,CNO,NNO,NOP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 08/11/2004   AUTEUR DURAND C.DURAND 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ----------------------------------------------------------------------
C          NOEUDS, ARETES, FACES DE LA FRONTIERE D'UN DOMAINE
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C INTEGER      INT(2,*)      : FACES DE LA FRONTIERE (CF ELBORD) 
C                            ( MAILLE SUPPORT, NUMERO DE LA FACE, ... )
C                              INDEX DANS LIMA
C INTEGER      NINT          : NOMBRE DE FACES DE LA FRONTIERE
C INTEGER      LIMA(*)       : NUMERO DES MAILLES
C INTEGER      BDIM(*)       : STRUCTURE BOITE.DIME (CF BOITE)
C REAL*8       BPAN(DIM+2,*) : STRUCTURE BOITE.PAN (CF BOITE)
C INTEGER      DIM           : DIMENSION DE L'ESPACE
C CHARACTER*8  CINE          : CINEMATIQUE (COQUE OU SOLIDE) 
C INTEGER      TMA(*)        : NUMERO DE TYPE DES MAILLES DU MAILLAGE
C REAL*8       NO(3,*)       : COORDONNEES DES NOEUDS DU MAILLAGE
C REAL*8       NRM(DIM,*)    : NORMALE LISSEE (CF. LISNOR)
C INTEGER      CNX(*)        : CONNECTIVITE DES MAILLES
C INTEGER      CNXC(*)       : INDEX DANS CNX 
C CHARACTER*8  NTM(*)        : VECTEUR NOMS TYPES DE MAILLE
C
C VECTEUR DE TRAVAIL
C INTEGER      DICO : 1 * NOMBRE DE NOEUDS DU MAILLAGE 
C 
C VARIABLES DE SORTIE
C REAL*8       CNO(DIM,*)    : COORDONNEES DES NOEUDS DE LA FRONTIERE
C INTEGER      NNO           : NOMBRE DE NOEUDS DE LA FRONTIERE
C INTEGER      NOP(*)        : CONNECTIVITE DES FACES DE LA FRONTIERE
C                             (NB NOEUDS FACE.1,NOEUD.1.FACE.1, 
C                              NOEUD.2.FACE.1,...,NB NOEUDS FACE.2, ...)
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- PARAMETRES
      REAL*8      PREC0
      PARAMETER   (PREC0 = 1.D-2)

C --- FONCTION
      REAL*8      PROVE2
 
C --- VARIABLES
      CHARACTER*8 NTM(*),CINE,TYPE
      INTEGER     INT(2,*),TMA(*),DICO(*),BDIM(2,*),CNX(*),CNXC(*),NNO
      INTEGER     LIMA(*),NOP(*),DIM,PAN(60),CQN(2,18),IPAN,INO,MA,MA0
      INTEGER     CNXMA(27),NINT,NNP,P0,P1,P2,P3,Q0,I,J,K
      REAL*8      BPAN(DIM+2,*),NO(3,*),NRM(DIM,*),CNO(DIM,*),PREC,R

C --- CAS DES MAILLAGES SOLIDES
     
      MA0 = 0
      NNO = 0
      Q0 = 0

      IF (CINE.EQ.'SOLIDE  ') THEN
       
        DO 10 I = 1, NINT

          MA = INT(1,I)
          IPAN = INT(2,I) 

          IF (MA.NE.MA0) THEN

            MA0 = MA
            MA = LIMA(MA)
            TYPE = NTM(TMA(MA))
            CALL NOPAN(TYPE,PAN,J)

C --------- COORDONNEES ET RENUMEROTATION DES NOEUDS

            P0 = CNXC(MA)
            P1 = CNXC(MA+1) - 1

            DO 20 J = P0, P1

              INO = CNX(J)
              IF (DICO(INO).NE.0) GOTO 20

              NNO = NNO + 1
              DICO(INO) = NNO
              CALL R8COPY(DIM,NO(1,INO),1,CNO(1,NNO),1)

 20         CONTINUE

C --------- REORIENTATION POUR DE LA MAILLE
            
            CALL ORIEM3(MA,TYPE,NO,CNX,CNXC,CNXMA)

          ENDIF

C ------- CONNECTIVITE DES PANS

          P3 = 1
          DO 30 J = 2, IPAN
            P3 = P3 + ABS(PAN(P3)) + 1
 30       CONTINUE

          Q0 = Q0 + 1
          NNP = PAN(P3)
          NOP(Q0) = NNP

          PREC = 0.D0
          P2 = BDIM(1,MA0+1) - 1 + IPAN

          DO 40 J = 1, DIM
            R = BPAN(J,P2)
            PREC = PREC + R*R
 40       CONTINUE

          PREC = PREC0/PREC 
          
          DO 10 J = 1, ABS(NNP)

            P3 = P3 + 1
            INO = DICO(CNXMA(PAN(P3)))

            Q0 = Q0 + 1
            NOP(Q0) = INO

            DO 10 K = 1, DIM

              CNO(K,INO) = CNO(K,INO) + PREC*BPAN(K,P2)

 10     CONTINUE

C --- CAS DES MAILLAGES COQUE

      ELSE 
 
        DO 50 I = 1, NINT

          MA = INT(1,I)
          IPAN = INT(2,I)

          IF (MA.NE.MA0) THEN 

            MA0 = MA 
            MA = LIMA(MA)
            TYPE = NTM(TMA(MA))
            CALL TMACOQ(TYPE,DIM,J)
            CALL NOPAN(TYPE,PAN,J)

C --------- COORDONNEES ET RENUMEROTATION DES NOEUDS

            P0 = CNXC(MA)
            P1 = CNXC(MA+1) - 1
            CALL COQUNO(DIM,P1-P0+1,CQN)

            DO 60 J = P0, P1

              INO = CNX(J)
              IF (DICO(INO).NE.0) GOTO 60

              DO 70 K = 1, DIM
                CNO(K,NNO+1) = NO(K,INO) - NRM(K,INO)
                CNO(K,NNO+2) = NO(K,INO) + NRM(K,INO)
 70           CONTINUE

              DICO(INO) = NNO+1
              NNO = NNO+2

 60         CONTINUE

          ENDIF

C ------- CONNECTIVITE DES PANS

          P3 = 1
          DO 80 J = 2, IPAN
            P3 = P3 + ABS(PAN(P3)) + 1
 80       CONTINUE

          Q0 = Q0 + 1
          NNP = PAN(P3)
          NOP(Q0) = NNP

          PREC = 0.D0
          P2 = BDIM(1,MA0+1) - 1 + IPAN
          
          DO 90 J = 1, DIM
            R = BPAN(J,P2)
            PREC = PREC + R*R
 90       CONTINUE

          PREC = PREC0/PREC

          DO 50 J = 1, ABS(NNP)
             
            P3 = P3 + 1
            INO = PAN(P3)
            INO = DICO(CNX(P0-1+CQN(1,INO)))+CQN(2,INO)
            
            Q0 = Q0 + 1
            NOP(Q0) = INO

            DO 50 K = 1, DIM

              CNO(K,INO) = CNO(K,INO) + PREC*BPAN(K,P2)

 50     CONTINUE
        
      ENDIF
      
      END
