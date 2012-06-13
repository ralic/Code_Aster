      SUBROUTINE FOEC2N(IUNI,VECPRO,VALPAR,CHVAL,NBFONC,IMPR)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'jeveux.h'
      INTEGER           IUNI,                    NBFONC,IMPR
      REAL*8                        VALPAR(NBFONC)
      CHARACTER*(*)          VECPRO(*),    CHVAL
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C     ECRITURE DES VALEURS (PARAMETRE, FONCTION) D'UNE NAPPE,
C     DE LA N1-IEME A LA N2-IEME
C     ------------------------------------------------------------------
C IN  IUNI  : NUMERO D'UNITE LOGIQUE D'ECRITURE
C IN  VECPRO: VECTEUR DE DESCRIPTION DE LA NAPPE
C IN  VALPAR: VECTEUR DES VALEURS DES PARAMETRES
C IN  CHVAL : NOM JEVEUX DE LA COLLECTION DES VALEURS
C IN  NBFONC: NOMBRE DE FONCTIONS
C     ------------------------------------------------------------------
C LOC    N1, N2: NUMEROS DE DEBUT ET FIN DE LA LISTE
C     OBJETS SIMPLES LUS:
C        JEXNUM(CHVAL,I)
C     ------------------------------------------------------------------
C
C     ------------------------------------------------------------------
      INTEGER       N1,N2,JV,N
      CHARACTER*8   NOMPAN,NOMRES,NOMPAF
      CHARACTER*24  CHBID
      CHARACTER*8   TPROL(3),PROLGD,INTERP
      DATA TPROL/'CONSTANT','LINEAIRE','EXCLU'/
C
      CALL JEMARQ()
      NOMPAF = VECPRO(7)
      NOMPAN = VECPRO(3)
      NOMRES = VECPRO(4)
      N1     = MIN( 1,NBFONC)
      N2     = MIN(10,NBFONC)
      IF (IMPR.GE.3)  N2=NBFONC
C
C     --- NAPPE DONT LES FONCTIONS SONT DEFINIES AUX MEME INSTANTS ? ---
      NDOM = 1
      CALL JELIRA(JEXNUM(CHVAL,N1),'LONMAX',N0,CHBID)
      CALL JEVEUO(JEXNUM(CHVAL,N1),'L',LR)
      DO 10 I=N1+1, N2
         CALL JELIRA(JEXNUM(CHVAL,I),'LONMAX',N,CHBID)
         IF ( N0 .NE. N ) THEN
             NDOM = NDOM + 1
             GOTO 12
         ELSE
            CALL JEVEUO(JEXNUM(CHVAL,I),'L',LF)
            DO 11 J=0, N/2 - 1
               IF ( ZR(LF+J) .NE. ZR(LR+J) ) THEN
                  NDOM = NDOM + 1
                  GOTO 12
               ENDIF
  11        CONTINUE
         ENDIF
  10  CONTINUE
  12  CONTINUE
C
C
      IF ( NDOM.EQ.1 .AND. N1.NE.N2 ) THEN
         N = N/2
         NF1 = 1
         NF2 = MIN(10,N)
         IF (IMPR.GE.3) NF2=N
         NPAS = 5
         CALL JEVEUO(JEXNUM(CHVAL,1),'L',LVAR)
         LFON = LVAR + N
         DO 100 I=N1,N2,NPAS
            NN = MIN(I+NPAS-1,N2)
            WRITE( IUNI,'(/,1X,A8,4X,9(1X,1PE12.5),1X)' ) NOMPAN,
     +                                           ( VALPAR(K) , K=I,NN )
            WRITE( IUNI,'(1X,A)' ) NOMPAF
            DO 110 IK = NF1, NF2
              WRITE(IUNI,'(1X,1PE12.5,9(1X,1PE12.5))') ZR(LVAR+IK-1),
     +                             ( ZR(LFON+(J-1)*2*N+IK-1) , J=I,NN )
  110       CONTINUE
  100    CONTINUE
C
      ELSE
C
         DO 200 I=N1,N2
            WRITE(IUNI,'(///)' )
            WRITE(IUNI,*) ' FONCTION NUMERO: ',I
            WRITE(IUNI,*) '    PARAMETRE : ',NOMPAN,' = ',VALPAR(I)
            CALL FOPRO1(VECPRO,I,PROLGD,INTERP)
            WRITE(IUNI,*) '    INTERPOLATION         : ',INTERP
            DO 210 J=1,3
               IF (PROLGD(1:1).EQ.TPROL(J)(1:1)) THEN
                  WRITE(IUNI,*) '    PROLONGEMENT A GAUCHE : ',TPROL(J)
               ENDIF
               IF (PROLGD(2:2).EQ.TPROL(J)(1:1)) THEN
                  WRITE(IUNI,*) '    PROLONGEMENT A DROITE : ',TPROL(J)
               ENDIF
  210       CONTINUE
            CALL JEVEUO(JEXNUM(CHVAL,I),'L',JV)
            CALL JELIRA(JEXNUM(CHVAL,I),'LONMAX',N,CHBID)
            N   = N/2
            NF1 = 1
            NF2 = MIN(10,N)
            IF (IMPR.GE.3) NF2=N
            CALL FOEC2F(IUNI,ZR(JV),N,NF1,NF2,NOMPAF,NOMRES)
  200    CONTINUE
      ENDIF
      CALL JEDEMA()
      END
