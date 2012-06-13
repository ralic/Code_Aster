      SUBROUTINE MECHPO ( SOUCHE, CHARGE, MODELE, CHDEP2,
     +                    CHDYNR, SUROPT, LPAIN, LCHIN, NBOPT,
     +                    TYPCOE, ALPHA,  CALPHA )
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'jeveux.h'
      CHARACTER*(*)       SOUCHE, CHARGE, MODELE, CHDEP2,
     +                    CHDYNR, SUROPT, LPAIN(*), LCHIN(*),
     +                    TYPCOE
      INTEGER                                           NBOPT
      REAL*8                      ALPHA
      COMPLEX*16                          CALPHA
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C     CREE UNE CARTE SPECFIQUE POUTRE A LA POUX
C     ------------------------------------------------------------------
C IN  : MODELE : NOM DU MODELE
C IN  : TYPCOE : TYPE DU COEFFICIENT MULTIPLICATIF DE LA CHARGE REPARTIE
C                SI TYPE = R ON CREE UNE CARTE AVEC LE COEFFICIENT REEL
C                   ALPHA
C                SI TYPE = C ALORS ON CREE UNE CARTE DE COEFFICIENT
C                    COMPLEXE CALPHA
C     ------------------------------------------------------------------
C
C
      REAL*8       TPS(11)
      CHARACTER*5  CH5
      CHARACTER*8  K8B, NCMPPE(4),NCMPFO(11),TPF(11)
      CHARACTER*19 CH19
      CHARACTER*24  LIGRMO,CHDEPL
      COMPLEX*16   C16B, TPC(11)
      DATA         NCMPPE/ 'G' , 'AG' , 'BG' , 'CG' /
      DATA         NCMPFO/ 'FX' , 'FY' , 'FZ' , 'MX' , 'MY' , 'MZ' ,
     +                     'BX' , 'REP' , 'ALPHA' , 'BETA' , 'GAMMA' /
C    -------------------------------------------------------------------
      CALL JEMARQ()
      ZERO = 0.D0
      DO 10 I = 1,11
         TPS(I) = ZERO
         TPF(I) = '&FOZERO'
         TPC(I) = ( 0.D0 , 0.D0 )
 10   CONTINUE
      LIGRMO = MODELE(1:8)//'.MODELE'
      CHDEPL = CHDEP2
      CH5 = '.    '
C
      NBOPT = 0
      IF (TYPCOE.EQ.'R') THEN
        NBOPT = NBOPT+1
        LPAIN(NBOPT) = 'PCOEFFR'
        LCHIN(NBOPT) = SOUCHE(1:8)//CH5//'.COEFF'
        CALL MECACT('V',LCHIN(NBOPT),'MODELE',LIGRMO,'IMPE_R',1,'IMPE',
     +                                             IBID,ALPHA,C16B,K8B)
      ELSEIF (TYPCOE.EQ.'C') THEN
        NBOPT = NBOPT+1
        LPAIN(NBOPT) = 'PCOEFFC'
        LCHIN(NBOPT) = SOUCHE(1:8)//CH5//'.COEFF'
        CALL MECACT('V',LCHIN(NBOPT),'MODELE',LIGRMO,'IMPE_C',1,'IMPE',
     +                                             IBID,R8B,CALPHA,K8B)
      ENDIF
C
      NBOPT = NBOPT+1
      LPAIN(NBOPT) = 'PPESANR'
      LCHIN(NBOPT) = CHARGE(1:8)//'.CHME.PESAN.DESC'
      CALL JEEXIN(LCHIN(NBOPT),IRET)
      IF (IRET.EQ.0) THEN
         CALL CODENT(NBOPT,'D0',CH5(2:5))
         LCHIN(NBOPT) = SOUCHE(1:8)//CH5//'.PESAN.DESC'
         CALL MECACT('V',LCHIN(NBOPT),'MODELE',LIGRMO,'PESA_R  ',4,
     +                                        NCMPPE,IBID,TPS,C16B,K8B)
      ENDIF
C
      NBOPT = NBOPT+1
      LCHIN(NBOPT) = CHARGE(1:8)//'.CHME.F1D1D.DESC'
      CALL JEEXIN(LCHIN(NBOPT),IRET)
      IF (IRET.EQ.0) THEN
         LPAIN(NBOPT) = 'PFF1D1D'
         CALL CODENT(NBOPT,'D0',CH5(2:5))
         LCHIN(NBOPT) = SOUCHE(1:8)//CH5//'.P1D1D.DESC'
         CALL FOZERO(TPF(1))
         CALL MECACT('V',LCHIN(NBOPT),'MODELE',LIGRMO,'FORC_F  ',11,
     +                                       NCMPFO,IBID,RBID,C16B,TPF)
C
         NBOPT = NBOPT+1
         LPAIN(NBOPT) = 'PFR1D1D'
         CALL CODENT(NBOPT,'D0',CH5(2:5))
         LCHIN(NBOPT) = SOUCHE(1:8)//CH5//'.P1D1D.DESC'
         CALL MECACT('V',LCHIN(NBOPT),'MODELE',LIGRMO,'FORC_R  ',11,
     +                                       NCMPFO,IBID,TPS,C16B,K8B)
C
         NBOPT = NBOPT+1
         LPAIN(NBOPT) = 'PFC1D1D'
         CALL CODENT(NBOPT,'D0',CH5(2:5))
         LCHIN(NBOPT) = SOUCHE(1:8)//CH5//'.P1D1D.DESC'
         CALL MECACT('V',LCHIN(NBOPT),'MODELE',LIGRMO,'FORC_C  ',11,
     +                                       NCMPFO,IBID,RBID,TPC,K8B)
C
      ELSE
         CALL DISMOI('F','TYPE_CHARGE',CHARGE,'CHARGE',IBID,K8B,IER)
         IF ( K8B(5:7) .EQ. '_FO' ) THEN
            LPAIN(NBOPT) = 'PFF1D1D'
C
            NBOPT = NBOPT+1
            LPAIN(NBOPT) = 'PFR1D1D'
            CALL CODENT(NBOPT,'D0',CH5(2:5))
            LCHIN(NBOPT) = SOUCHE(1:8)//CH5//'.P1D1D.DESC'
            CALL MECACT('V',LCHIN(NBOPT),'MODELE',LIGRMO,'FORC_R  ',
     +                                     11,NCMPFO,IBID,TPS,C16B,K8B)
C
            NBOPT = NBOPT+1
            LPAIN(NBOPT) = 'PFC1D1D'
            CALL CODENT(NBOPT,'D0',CH5(2:5))
            LCHIN(NBOPT) = SOUCHE(1:8)//CH5//'.P1D1D.DESC'
            CALL MECACT('V',LCHIN(NBOPT),'MODELE',LIGRMO,'FORC_C  ',
     +                                    11,NCMPFO,IBID,RBID,TPC,K8B)
         ELSEIF ( K8B(5:6) .EQ. '_RI' ) THEN
            LPAIN(NBOPT) = 'PFC1D1D'
C
            NBOPT = NBOPT+1
            LPAIN(NBOPT) = 'PFR1D1D'
            CALL CODENT(NBOPT,'D0',CH5(2:5))
            LCHIN(NBOPT) = SOUCHE(1:8)//CH5//'.P1D1D.DESC'
            CALL MECACT('V',LCHIN(NBOPT),'MODELE',LIGRMO,'FORC_R  ',
     +                                     11,NCMPFO,IBID,TPS,C16B,K8B)
C
            NBOPT = NBOPT+1
            LPAIN(NBOPT) = 'PFF1D1D'
            CALL CODENT(NBOPT,'D0',CH5(2:5))
            LCHIN(NBOPT) = SOUCHE(1:8)//CH5//'.P1D1D.DESC'
            CALL FOZERO(TPF(1))
            CALL MECACT('V',LCHIN(NBOPT),'MODELE',LIGRMO,'FORC_F  ',
     +                                     11,NCMPFO,IBID,RBID,C16B,TPF)
         ELSE
            LPAIN(NBOPT) = 'PFR1D1D'
C
            NBOPT = NBOPT+1
            LPAIN(NBOPT) = 'PFF1D1D'
            CALL CODENT(NBOPT,'D0',CH5(2:5))
            LCHIN(NBOPT) = SOUCHE(1:8)//CH5//'.P1D1D.DESC'
            CALL FOZERO(TPF(1))
            CALL MECACT('V',LCHIN(NBOPT),'MODELE',LIGRMO,'FORC_F  ',
     +                                     11,NCMPFO,IBID,RBID,C16B,TPF)
C
            NBOPT = NBOPT+1
            LPAIN(NBOPT) = 'PFC1D1D'
            CALL CODENT(NBOPT,'D0',CH5(2:5))
            LCHIN(NBOPT) = SOUCHE(1:8)//CH5//'.P1D1D.DESC'
            CALL MECACT('V',LCHIN(NBOPT),'MODELE',LIGRMO,'FORC_C  ',
     +                                    11,NCMPFO,IBID,RBID,TPC,K8B)
         ENDIF
      ENDIF
C
      NBOPT = NBOPT+1
      LPAIN(NBOPT) = 'PCHDYNR'
      CH19         = CHDYNR
      LCHIN(NBOPT) = CH19//'.VALE'
      CALL JEEXIN(LCHIN(NBOPT),IRET)
      IF (IRET.EQ.0) THEN
         CALL CODENT(NBOPT,'D0',CH5(2:5))
         LCHIN(NBOPT) = SOUCHE(1:8)//CH5//'.PCHDY'

         CALL COPISD('CHAMP_GD','V',CHDEPL,LCHIN(NBOPT))
      ENDIF
C
      NBOPT = NBOPT+1
      LPAIN(NBOPT) = 'PSUROPT'
      CALL CODENT(NBOPT,'D0',CH5(2:5))
      LCHIN(NBOPT) = SOUCHE(1:8)//CH5//'.SUR_OPTION'
      CALL MECACT('V',LCHIN(NBOPT),'MODELE',LIGRMO,'NEUT_K24',
     +                                  1,'Z1',IBID,RBID,C16B,SUROPT)
C
      CALL JEDEMA()
      END
