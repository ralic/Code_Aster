      SUBROUTINE ACEAGR(NOMU,NOMA,LMAX,NBOCC)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 04/04/2006   AUTEUR VABHHTS J.PELLET 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           LMAX,NBOCC
      CHARACTER*8       NOMU,NOMA
C ----------------------------------------------------------------------

C ----------------------------------------------------------------------
C     AFFE_CARA_ELEM
C     AFFECTATION DES CARACTERISTIQUES POUR LE MOT CLE ASSE_GRIL
C ----------------------------------------------------------------------
C IN  : NOMU   : NOM UTILISATEUR DE LA COMMANDE
C IN  : NOMA   : NOM DU MAILLAGE
C IN  : LMAX   : LONGUEUR
C IN  : NBOCC  : NOMBRE D'OCCURENCES DU MOT CLE ASSE_GRIL
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32     JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      PARAMETER  ( NCD = 3, NBVAL = 20, NBEX = 5)
      INTEGER      I,IJKL,IJ,K,J,L,NVALGR(NCD),NVALEX(NCD)
      REAL*8       VAL(NBVAL),RAPP,PAST,PASN,ANG(2)
      CHARACTER*10 CAR(NCD), CARGR(NCD)
      CHARACTER*16 TOU
      CHARACTER*19 CARTGR
      CHARACTER*24 TMPNGR, TMPVGR
      DATA CARGR  /'K_TR_D_N  ','K_TR_D_L_T','K_TR_D_L_N'/
      DATA NVALGR /6,6,6/
      DATA NVALEX /3,0,0/
C     ------------------------------------------------------------------
C     SIGNIFICATION DES CONSTANTES :
C
C     NCD : NOMBRE DE MOTS CLES DANS "CARA"
C     NBVAL : NOMBRE MAXIMAL DE VALEURS DANS "VALE"
C     NBEX  : NOMBRE DE VALEURS A EXCLURE DANS "VALE"
C     CARGR : LISTE DES MOTS CLES DE "CARA"
C     NVALGR(I) : NOMBRE DE VALEURS ASSOCIE A CARGR(I)
C     NVALEX(I) : NOMBRE DE VALEURS A EXCLURE DE CARGR(I)
C
C --- CONSTRUCTION DES CARTES ET ALLOCATION
      CALL JEMARQ()
      CARTGR = NOMU//'.CARCOQUE'
      TMPNGR = CARTGR//'.NCMP'
      TMPVGR = CARTGR//'.VALV'
      CALL ALCAR2('G',CARTGR,NOMA,'CACOQU')
      CALL JEVEUO(TMPNGR,'E',JDCC)
      CALL JEVEUO(TMPVGR,'E',JDVC)
C
      CALL WKVECT('&&TMPGRILLE','V V K8',LMAX,JDLS)
C
      ZK8(JDCC) = 'ALPHA'
      ZK8(JDCC+1) = 'BETA'
      ZK8(JDCC+2) = 'PAS_T'
      ZK8(JDCC+3) = 'PAS_N'
      ZK8(JDCC+4) = 'COEF_ECH'
      ZK8(JDCC+5)  = 'K_DRL'
      ZK8(JDCC+6)  = 'K_DRT'
      ZK8(JDCC+7)  = 'K_DRN'
      ZK8(JDCC+8)  = 'KT_DL'
      ZK8(JDCC+9)  = 'KT_DT'
      ZK8(JDCC+10)  = 'KT_DN'
      ZK8(JDCC+11)  = 'KT_DRL'
      ZK8(JDCC+12)  = 'KT_DRT'
      ZK8(JDCC+13)  = 'KT_DRN'
      ZK8(JDCC+14)  = 'KN_DL'
      ZK8(JDCC+15) = 'KN_DT'
      ZK8(JDCC+16) = 'KN_DN'
      ZK8(JDCC+17) = 'KN_DRL'
      ZK8(JDCC+18) = 'KN_DRT'
      ZK8(JDCC+19) = 'KN_DRN'
C
C --- LECTURE DES VALEURS ET AFFECTATION DANS LA CARTE CARTPF
      DO 10 IOC = 1 , NBOCC
        DO 11 I = 1, NBVAL
           VAL(I) = 0.D0
 11     CONTINUE
        PAST   = 0.D0
        PASN   = 0.D0
        RAPP   = 0.D0
        ANG(1) = 0.D0
        ANG(2) = 0.D0
        CALL GETVEM(NOMA,'GROUP_MA','ASSE_GRIL','GROUP_MA',
     +              IOC,1,LMAX,ZK8(JDLS),NG)
        CALL GETVEM(NOMA,'MAILLE','ASSE_GRIL','MAILLE',
     +            IOC,1,LMAX,ZK8(JDLS),NM)
        CALL GETVTX('ASSE_GRIL','CARA'      ,IOC,1,NCD ,CAR      ,NCAR)
        CALL GETVR8('ASSE_GRIL','VALE'      ,IOC,1,NBVAL,VAL     ,NVAL)
        CALL GETVR8('ASSE_GRIL','PAS_T'     ,IOC,1,1    ,PAST    ,NPT )
        CALL GETVR8('ASSE_GRIL','PAS_N'     ,IOC,1,1    ,PASN    ,NPN )
        CALL GETVR8('ASSE_GRIL','COEF_ECHELLE',IOC,1,1  ,RAPP    ,NR  )
        CALL GETVR8('ASSE_GRIL','ANGL_REP',IOC,1,2  ,ANG    ,NR  )
        ALPHA  = ANG(1)
        BETA   = ANG(2)
C
        DO 30 I = 1, NBVAL
          ZR(JDVC+I-1) = 0.D0
 30     CONTINUE
        IJKL = 0
        DO 12 I = 1, NCAR
          DO 13 J = 1, NCD
            IF (CAR(I).EQ.CARGR(J)) THEN
                  IJ = 0
                  DO 14 K = 1, J-1
                     IJ = IJ + NVALGR(K) - NVALEX(K)
 14               CONTINUE
                  DO 15 L = 1, NVALGR(J)
                     ZR(JDVC+NBEX+IJ+L-1) = VAL(IJKL+L+NVALEX(J))
 15               CONTINUE
               IJKL = IJKL + NVALGR(I)
               ENDIF
 13         CONTINUE
 12      CONTINUE
C
         IF (PAST*PASN .LT. 1.D-10) THEN
            CALL UTMESS('F','ACEAGR','VALEUR INVALIDE DE PAS_T'//
     +                               ' OU DE PAS_N')
         ENDIF
         ZR(JDVC)   = ALPHA
         ZR(JDVC+1) = BETA
         ZR(JDVC+2) = PAST
         ZR(JDVC+3) = PASN
         ZR(JDVC+4) = RAPP

C
C ---    "GROUP_MA" = TOUTES LES MAILLES DE LA LISTE DE GROUPES MAILLES
         IF (NG.GT.0) THEN
            DO 20 I = 1 , NG
               CALL NOCAR2(CARTGR,2,ZK8(JDLS+I-1),' ',0,' ',0,' ',20)
 20         CONTINUE
         ENDIF
C
C ---    "MAILLE" = TOUTES LES MAILLES DE LA LISTE DE MAILLES
C
         IF (NM.GT.0) THEN
            CALL NOCAR2(CARTGR,3,' ','NOM',NM,ZK8(JDLS),0,' ',20)
         ENDIF
C
 10   CONTINUE
C
      CALL JEDETR('&&TMPGRILLE')
      CALL JEDETR(TMPNGR)
      CALL JEDETR(TMPVGR)
C
      CALL JEDEMA()
      END
