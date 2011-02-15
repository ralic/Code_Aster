      SUBROUTINE OP0114 ()
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ECHANGE  DATE 14/02/2011   AUTEUR GREFFET N.GREFFET 
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
C RESPONSABLE GREFFET N.GREFFET
C TOLE CRP_4
      IMPLICIT   NONE
C  ----- OPERATEUR RECU_PARA_YACS             --------------------------
C  RECUPERATION DE VALEURS D'INITIALISATION POUR COUPLAGE IFS VIA YACS
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER IFM,NIV,NBVALE,NDIM,JPAS,JNBP,JBOR,JVAL,I
      CHARACTER*19 RESU
      CHARACTER*16 NOMCMD,CONCEP
C     ------------------------------------------------------------------
C     COUPLAGE =>
      INTEGER*4          LENVAR,CPITER,NUMPA4,TAILLE,EYACS,IBID4
      INTEGER            ICOMPO,IBID,NUMPAS,IADR
      PARAMETER (LENVAR = 144)
      PARAMETER (CPITER= 41)
      REAL*4             TI4,TF4
      REAL*8             DT,RYACS,TI,TF
      CHARACTER*16       OPTION,VALK(3)
      CHARACTER*24       AYACS   
      CHARACTER*(LENVAR) NOMVAR
C     COUPLAGE <=
C
      CALL JEMARQ()
      CALL INFMAJ
      CALL INFNIV(IFM,NIV)
C
C     ASSIGNATION DES NOMS POUR LES ADRESSES DANS LES COMMON ASTER
C     ------------------------------------------------------------
      AYACS='&ADR_YACS'
C
C     RECUPERATION DE L'ADRESSE YACS
C     ------------------------------        
      CALL JEVEUO(AYACS,'L',IADR)
      ICOMPO=ZI(IADR) 

      CALL GETRES(RESU,CONCEP,NOMCMD)
      
      CALL GETVTX(' ','DONNEES',  0,1,1,OPTION,IBID)
      IF (NIV.EQ.2) THEN
        VALK(1) = 'OP0114'
        VALK(2) = 'DONNEES'
        VALK(3) = OPTION     
        CALL U2MESK('I+','COUPLAGEIFS_11',3,VALK)
      ENDIF
C
      NBVALE = 7
      IF ( ( OPTION(1:3) .EQ. 'FIN' ) 
     & .OR. ( OPTION(1:4) .EQ. 'CONV' ) 
     & .OR. ( OPTION(1:3) .EQ. 'PAS' ) ) NBVALE = 1

      NDIM = MAX(1,NBVALE-1)
      CALL WKVECT(RESU//'.LPAS','G V R',NDIM,JPAS)
      CALL WKVECT(RESU//'.NBPA','G V I',NDIM,JNBP)
      CALL WKVECT(RESU//'.BINT','G V R',NBVALE,JBOR)
      CALL WKVECT(RESU//'.VALE','G V R',NBVALE,JVAL)

      IF ( OPTION(1:4) .EQ. 'INIT' ) THEN
        CALL GETVR8(' ','PAS',0,1,1,DT,IBID)
C reception des parametres utilisateurs a l iteration 0
        NUMPA4 = 0
        NUMPAS = 0
        TI = 0.D0
        TF = 0.D0
        TI4 = 0.D0
        TF4 = 0.D0
C
        DO 10 I = 1,NBVALE-1
          ZR(JPAS+I-1) = 0.1D0
          ZI(JNBP+I-1) = 1
   10   CONTINUE
        ZR(JPAS) = 0.1D0
        ZI(JNBP) = 1
C  RECEPTION NOMBRE DE PAS DE TEMPS
        NOMVAR = 'NBPDTM'
        CALL CPLEN(ICOMPO,CPITER,TI4,TF4,NUMPA4,NOMVAR,
     &             1,TAILLE,EYACS,IBID4)
        ZR(JBOR) = EYACS
        ZR(JVAL) = EYACS
C  RECEPTION NOMBRE DE SOUS-ITERATIONS
        NOMVAR = 'NBSSIT'
        CALL CPLEN(ICOMPO,CPITER,TI4,TF4,NUMPA4,NOMVAR,
     &             1,TAILLE,EYACS,IBID4)
        ZR(JBOR+1) = EYACS
        ZR(JVAL+1) = EYACS
C  RECEPTION EPSILON
        NOMVAR = 'EPSILO'
        CALL CPLDB(ICOMPO,CPITER,TI,TF,NUMPA4,NOMVAR,
     &             1,TAILLE,RYACS,IBID4)
        ZR(JBOR+2) = RYACS
        ZR(JVAL+2) = RYACS
        NOMVAR = 'ISYNCP'
        CALL CPLEN(ICOMPO,CPITER,TI4,TF4,NUMPA4,NOMVAR,
     &             1,TAILLE,EYACS,IBID4)
        ZR(JBOR+3) = EYACS
        ZR(JVAL+3) = EYACS
        NOMVAR = 'NTCHRO'
        CALL CPLEN(ICOMPO,CPITER,TI4,TF4,NUMPA4,NOMVAR,
     &             1,TAILLE,EYACS,IBID4)
        ZR(JBOR+4) = EYACS
        ZR(JVAL+4) = EYACS
        NOMVAR = 'TTINIT'
        CALL CPLDB(ICOMPO,CPITER,TI,TF,NUMPA4,NOMVAR,
     &             1,TAILLE,RYACS,IBID4)
        ZR(JBOR+5) = RYACS
        ZR(JVAL+5) = RYACS
C  RECEPTION PAS DE TEMPS DE REFERENCE
        NOMVAR = 'PDTREF'
        CALL CPLDB(ICOMPO,CPITER,TI,TF,NUMPA4,NOMVAR,
     &             1,TAILLE,RYACS,IBID4)
        ZR(JBOR+6) = RYACS
        ZR(JVAL+6) = RYACS
      ELSE
        CALL GETVR8(' ','INST',0,1,1,TF,IBID)
        CALL GETVIS(' ','NUME_ORDRE_YACS',0,1,1,NUMPAS,IBID)
        NUMPA4 = NUMPAS
        CALL GETVR8(' ','PAS',0,1,1,DT,IBID)
        TI = TF
        IF ( OPTION(1:3) .EQ. 'FIN' ) THEN
          NOMVAR = 'END'
          RYACS = 0.D0
        ELSE
          IF ( OPTION(1:4) .EQ. 'CONV' ) THEN
            NOMVAR = 'ICVAST'
            CALL CPLEN(ICOMPO,CPITER,TI4,TF4,NUMPA4,NOMVAR,
     &                  1,TAILLE,EYACS,IBID4)
            RYACS = EYACS           
          ELSE
            NOMVAR = 'DTAST'
            CALL CPEDB(ICOMPO,CPITER,TI,NUMPA4,NOMVAR,1,DT,IBID4)
            NOMVAR = 'DTCALC'
            CALL CPLDB(ICOMPO,CPITER,TI,TF,NUMPA4,NOMVAR,
     &                  1,TAILLE,RYACS,IBID4)
          ENDIF
        ENDIF
        ZR(JBOR) = RYACS
        ZR(JVAL) = RYACS        
        ZR(JPAS) = 0.1D0
        ZI(JNBP) = 1
        
      ENDIF
C     --- TITRE ---
      CALL TITRE
C     --- IMPRESSION ---
      IF (NIV.GT.1) CALL LIIMPR(RESU,NIV,'MESSAGE')

      CALL JEDEMA()
      END
