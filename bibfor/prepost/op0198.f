      SUBROUTINE OP0198 ( IER )
      IMPLICIT   NONE
      INTEGER    IER
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 08/03/2004   AUTEUR REZETTE C.REZETTE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                                                                       
C                                                                       
C ======================================================================
C ======================================================================
C --- BUT : COMMANDE POST_K_BETA ---------------------------------------
C ======================================================================
C --------- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C --------- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C ======================================================================
      INTEGER      NDIM, NK1D, IK1D, JNOGN, ITIME, NBVAL, JTBINT, NBVAL2
      INTEGER      NOREV, NOMDB, NOTOT, JTHRTO, IBID
      REAL*8       LREV, DEKLAG, PRODEF, LONDEF, TEMPS, R8MIEM, K1ACP
      REAL*8       DKMA, DKMB, KAL, KBL, K1A, K1B, TEMPA, TEMPB, K1BCP
      REAL*8       RNOM(7)
      COMPLEX*16   C16B
      CHARACTER*8  RESULT, K8B, NOMA, MATREV, ORIDEF
      CHARACTER*8  TABREV, TABMDB, TABTHR, TYPPAR(8)
      CHARACTER*10 NOMTAB(8)
      CHARACTER*16 NOMCMD, NMGRNO
      CHARACTER*19 TBINST, TBSCRV, TBSCMB, SIGMRV, SIGMDB, TBINTH
      CHARACTER*32 KNOM
C
      DATA  NOMTAB / 'GROUP_NO', 'INST', 'K1_REV', 'KCP_REV',  
     +               'TEMPPF_REV', 'K1_MDB',  'KCP_MDB', 'TEMPPF_MDB' /
      DATA  TYPPAR / 'K32', 'R', 'R', 'R', 'R', 'R', 'R', 'R' /
C ======================================================================
      CALL JEMARQ()
      CALL INFMAJ()
C ======================================================================
      IER = 0
      CALL GETRES ( RESULT, K8B, NOMCMD )
C ======================================================================
C --- DEFINITIONS ------------------------------------------------------
C ======================================================================
      NMGRNO = '&&OP0198.NMGRNO'
      TABREV = '        '
      TABMDB = '        '
      TABTHR = '        '
      TBINST = '&&OP0198.TBINST'
      TBINTH = '&&OP0198.TBINTH'
      TBSCRV = '&&OP0198.TBSCRV'
      TBSCMB = '&&OP0198.TBSCMB'
      DKMA   =  0.0D0
      DKMB   =  0.0D0
      K1ACP  =  0.0D0
      K1BCP  =  0.0D0
      KAL    =  0.0D0
      KBL    =  0.0D0
C ======================================================================
C --- RECUPERATION DES DONNEES AUTRE QUE K1D ---------------------------
C ======================================================================
      CALL RECUPE( NOMA, NDIM, NK1D, LREV, MATREV, DEKLAG, PRODEF,
     +             LONDEF, ORIDEF )
C ======================================================================
C --- VERIFICATION DES DONNEES -----------------------------------------
C ======================================================================
      CALL VERITB ( NK1D, NDIM, ORIDEF )
C ======================================================================
C --- RECUPERATION DES TABLES D'INSTANT, D'ABSCISSES CURVILIGNES / -----
C --- COTE REVETEMENT / COTE METAL DE BASE -----------------------------
C ======================================================================
      CALL RECUVL ( NBVAL, TBINST, NBVAL2, TBINTH, NOREV, TBSCRV,
     +              NOMDB, TBSCMB )
C ======================================================================
C --- CREATION DE LA TABLE RESULTAT ------------------------------------
C ======================================================================
      CALL TBCRSD ( RESULT, 'G' )
      CALL TBAJPA ( RESULT, 8, NOMTAB, TYPPAR )
C ======================================================================
C --- CREATION DES VECTEURS NECESSAIRES --------------------------------
C ======================================================================
      CALL WKVECT ( NMGRNO, 'V V K32', NK1D , JNOGN  )
      CALL JEVEUO ( TBINST , 'L', JTBINT )
C ======================================================================
C --- ITERATIONS SUR LES DIFFERENTES OCCURENCES DE K1D -----------------
C ======================================================================
      DO 10 IK1D = 1,NK1D
C ======================================================================
C --- INITIALISATIONS --------------------------------------------------
C ======================================================================
         DKMA   =  0.0D0
         DKMB   =  0.0D0
         K1ACP  =  0.0D0
         K1BCP  =  0.0D0
         KAL    =  0.0D0
         KBL    =  0.0D0
C ======================================================================
C --- RECUPERATION DES DONNEES ASSOCIEES A LA IK1D OCCURENCE DE K1D ----
C ======================================================================
         CALL RECUTB (IK1D, ZK32(JNOGN-1+IK1D), TABREV, TABMDB, TABTHR )
C ======================================================================
C --- ITERATIONS SUR LES INSTANTS MECANIQUES ---------------------------
C ======================================================================
         DO 20 ITIME = 1, NBVAL
            TEMPS = ZR(JTBINT-1+ITIME)
C ======================================================================
C --- RECUPERATION DES CHAMPS MECANIQUES -------------------------------
C ======================================================================
            SIGMRV = '&&OP0198.SIGMRV'
            SIGMDB = '&&OP0198.SIGMDB'
            CALL RECHMC( NDIM, TEMPS, ORIDEF, TABREV, TABMDB, 
     +                   NOREV, SIGMRV, NOMDB, SIGMDB )
C ======================================================================
C --- CALCUL DES FACTEURS D'INTENSITE DE CONTRAINTES ELASTIQUES --------
C ======================================================================
            CALL CALCK1( NOREV, NOMDB, SIGMRV, SIGMDB, TBSCRV, TBSCMB,
     +                   PRODEF, LONDEF, DEKLAG, LREV, K1A, K1B )
C ======================================================================
C --- RECUPERATION DES TEMPERATURES AUX POINTES DE LA FISSURE ----------
C ======================================================================
            CALL RECHTH ( TEMPS, NBVAL2, TBINTH, TABTHR, TEMPA, TEMPB )
C ======================================================================
C --- AJOUT DE CORRECTION PLASTIQUE AU CALCUL DES FACTEURS -------------
C --- D'INTENSITE DE CONTRAINTES ---------------------------------------
C ======================================================================
            CALL COPLAS( TEMPA, K1A, K1B, MATREV, LREV, DEKLAG,
     +                   KAL, KBL, DKMA, DKMB, K1ACP, K1BCP )
C ======================================================================
C --- RECUPERATION DES TEMPERATURES AUX POINTES DE LA FISSURE ----------
C ======================================================================
            RNOM(1)   = TEMPS
            RNOM(2)   = K1A
            RNOM(3)   = K1ACP
            RNOM(4)   = TEMPA
            RNOM(5)   = K1B
            RNOM(6)   = K1BCP
            RNOM(7)   = TEMPB
            KNOM      = ZK32(JNOGN+IK1D-1)
            CALL TBAJLI (RESULT,8,NOMTAB,IBID,RNOM,C16B,KNOM,0)
C ======================================================================
C --- DESTRUCTION DES CHAMPS DE CONTRAINTES ----------------------------
C ======================================================================
            CALL JEDETR (SIGMRV)
            CALL JEDETR (SIGMDB)
 20      CONTINUE
 10   CONTINUE
C ======================================================================
      CALL JEDETC('V','&&',1)
      CALL JEDEMA()
C ======================================================================
      END
