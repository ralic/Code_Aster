      SUBROUTINE EXTMOD(BASEMO,NUMDDL,NUME,NBNUMO,DMODE,NBEQ,NBNOE,
     &                  IDDL,NBDDL)
      IMPLICIT REAL*8 (A-H,O-Z)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 14/10/96   AUTEUR KXBADNG N.GAY 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C-----------------------------------------------------------------------
C EXTRAIRE D'UN CONCEPT MODE_MECA LA DEFORMEE POUR UN OU PLUSIEURS DDL
C LES LAGRANGES SONT SUPPRIMES.
C-----------------------------------------------------------------------
C IN  :BASEMO : CONCEPT DE TYPE MODE_MECA
C IN  :NUMDDL : PERMET D'ACCEDER AU PROFIL DU CHAMP_NO EXTRAIT
C IN  :NUME   : LISTE DES NUMEROS D'ORDRE DES MODES CONSIDERES
C IN  :NBNUMO : NB DE MODES CONSIDERES
C IN  :NBEQ   : NB D'EQUATIONS
C IN  :NBNOE  : NB DE NOEUDS DU MAILLAGE
C IN  :IDDL   : LISTE DES INDICES DES DDL A EXTRAIRE
C IN  :NBDDL  : NB DE DDLS A EXTRAIRE
C OUT :DMODE  : VECTEUR => DEFORMEES MODALES
C-----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER      NUME(NBNUMO),IDDL(NBDDL)
      REAL*8       DMODE(NBDDL*NBNOE*NBNUMO)
      CHARACTER*8  BASEMO
      CHARACTER*14 NUMDDL
      CHARACTER*24 DEEQ, NOMCHA
C-----------------------------------------------------------------------
C
      CALL JEMARQ()
        DEEQ = NUMDDL//'.NUME.DEEQ'
        CALL JEVEUO(DEEQ,'L',IDEEQ)
        IPM = 0
        ICM = 0
        DO 10 I = 1,NBNUMO
          INUMO = NUME(I)
          CALL RSEXCH(BASEMO,'DEPL',INUMO,NOMCHA,IRET)
          NOMCHA = NOMCHA(1:19)//'.VALE'
          CALL JEVEUO(NOMCHA,'L',IADMOD)
          IPM = IPM + ICM
          ICM = 0
          DO 20 J = 1,NBEQ
            DO 21 K = 1,NBDDL
              IF (ZI(IDEEQ+(2*J)-1) .EQ. IDDL(K)) THEN
                ICM = ICM + 1
                DMODE(IPM+ICM) = ZR(IADMOD+J-1)
                GOTO 22
              ENDIF
  21        CONTINUE
  22        CONTINUE
  20      CONTINUE
  10    CONTINUE
C
      CALL JEDEMA()
        END
