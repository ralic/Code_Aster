      SUBROUTINE NMDOMC (MODELE,MATE,CARELE,FOMULT,INFCHA)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 12/09/2005   AUTEUR NICOLAS O.NICOLAS 
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
      CHARACTER*19       INFCHA
      CHARACTER*24       MODELE,CARELE,CHARGE,FOMULT, MATE
C ----------------------------------------------------------------------
C     SAISIE ET VERIFICATION DE LA COHERENCE DES DONNEES MECANIQUE DU
C     PROBLEME, CHARGE COMPLEXE.
C     ( COPIE DE LA ROUTINE NMDOME )
C
C VAR MODELE  : NOM DU MODELE
C OUT MATE    : NOM DU CHAMP DE MATERIAU
C OUT CARELE  : CARACTERISTIQUES DES POUTRES ET COQUES
C OUT FOMULT  : LISTE DES FONCTIONS MULTIPLICATIVES
C OUT INFCHA  : CONTIENT LA LISTE DES CHARGES ET DES INFOS
C               SUR LES CHARGES
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER            N1,N11,JNOMA,NCHAR,IALICH,IBID,IERD,NFCTS
      CHARACTER*4        KNUM
      CHARACTER*8        CARA,MODE,K8BID,AFFCHA,PARCHA,TYPCHA, MATERI
      CHARACTER*19       CH19
      CHARACTER*24       LIGRCH,LCHIN,NOMFCT,CH24
      COMPLEX*16         CCOEF
      LOGICAL            EXITHE
C
C --- NOMBRE MAXIMUM DE RESU_ELEM POUR LES FORCES DE LAPLACE : NBCHMX
C --- NOMBRE MAXIMUM DE TYPE DE CHARGE : NBTYCH
C
      INTEGER            NBCHMX,NBTYCH
      PARAMETER        ( NBCHMX = 99 , NBTYCH = 14 )
      CHARACTER*6        NOMLIG(NBTYCH)
      DATA NOMLIG/'.FORNO','.F3D3D','.F2D3D','.F1D3D','.F2D2D','.F1D2D',
     &            '.F1D1D','.PESAN','.ROTAT','.PRESS','.FELEC','.FCO3D',
     &            '.FCO2D','.EPSIN'/
      CALL JEMARQ()
      EXITHE = .FALSE.
      MATERI = ' '
      CALL GETVID(' ','CHAM_MATER',0,1,1,MATERI,N1)
      IF ( N1 .NE. 0 ) THEN
         CALL RCMFMC ( MATERI , MATE )
      ELSE
         MATE = ' '
      ENDIF
C
      IF(MODELE.EQ.' ') THEN
        CALL GETVID(' ','MODELE'    ,0,1,1,MODE,N1)
        MODELE = MODE
      ENDIF
      CALL GETVID(' ','CARA_ELEM' ,0,1,1,CARA,N1)
      IF (N1.LE.0) CARA = '        '
      CARELE = CARA
C
      CALL GETFAC('EXCIT',NCHAR)
      IF ( NCHAR .EQ. 0 ) GOTO 9999
C
C --- LISTE DES CHARGES
C
      CALL WKVECT(INFCHA//'.LCHA','V V K24',NCHAR,IALICH)
      CALL WKVECT(INFCHA//'.INFC','V V IS',4*NCHAR+3,JINF)
      ZI(JINF) = NCHAR
      INFMAX = 0
      FOMULT = '&&NMDOMC.LIFCTS'
      CALL WKVECT(FOMULT,'V V K24',NCHAR,IALIFC)
      DO 100 ICH=1,NCHAR
        CALL GETVID('EXCIT','CHARGE',ICH,1,1,ZK24(IALICH+ICH-1),N1)
C
C ----- TYPES DE CHARGES UTILISEES
C
        CALL GETVTX('EXCIT','TYPE_CHARGE',ICH,1,1,TYPCHA,N1)
C
        CALL DISMOI('F','TYPE_CHARGE',ZK24(IALICH-1+ICH),'CHARGE',IBID,
     &               AFFCHA,IERD)
        IF ((AFFCHA(1:5).NE.'MECA_').AND.(AFFCHA(1:5).NE.'CIME_')) THEN
          CALL UTMESS('E','NMDOMC_01','LA CHARGE '
     &              //ZK24(IALICH-1+ICH)(1:8)//' N''EST PAS MECANIQUE')
        ENDIF
C
C ----- ON REGARDE LES CHARGES DU TYPE DIRICHLET PROVENANT D'UN
C       AFFE_CHAR_CINE
        IF ( AFFCHA(1:5).EQ.'CIME_') THEN
          IF( TYPCHA(1:4) .EQ. 'SUIV' ) THEN
            CALL UTMESS('F','NMDOMC_02','LA CHARGE '
     &              //ZK24(IALICH-1+ICH)(1:8)//' NE PEUT ETRE SUIVEUSE')
          ELSE IF( TYPCHA(1:4) .EQ. 'DIDI' ) THEN
            CALL UTMESS('F','NMDOMC_03','LA CHARGE '
     &        //ZK24(IALICH-1+ICH)(1:8)//' NE PEUT ETRE DIFFERENTIELLE')
          ELSE
            IF (AFFCHA(5:7).EQ.'_FT') THEN
              ZI(JINF+ICH) = -3
            ELSE IF (AFFCHA(5:7).EQ.'_FO') THEN
              ZI(JINF+ICH) = -2
            ELSE
              ZI(JINF+ICH) = -1
            ENDIF
          ENDIF
        ENDIF
        LIGRCH =  ZK24(IALICH-1+ICH)(1:8)//'.CHME.LIGRE'
C
C ----- ON REGARDE LES CHARGES DU TYPE DIRICHLET PROVENANT D'UN
C       AFFE_CHAR_MECA OU AFFE_CHAR_MECA_F
        LCHIN = LIGRCH(1:13)//'.CIMPO.DESC'
        CALL JEEXIN(LCHIN,IRET)
        IF ( IRET .NE. 0 ) THEN
          IF( TYPCHA(1:4) .EQ. 'SUIV' ) THEN
            CALL UTMESS('F','NMDOMC_04','LA CHARGE '
     &              //ZK24(IALICH-1+ICH)(1:8)//' NE PEUT ETRE SUIVEUSE')
          ELSE
            IF (AFFCHA(5:7).EQ.'_FO') THEN
              ZI(JINF+ICH) = 2
              CALL DISMOI('F','PARA_INST',LCHIN(1:19),'CARTE',IBID,
     &                     PARCHA,IERD)
              IF ( PARCHA(1:3) .EQ. 'OUI' ) ZI(JINF+ICH) = 3
            ELSE
              ZI(JINF+ICH) = 1
            ENDIF
            IF( TYPCHA(1:4) .EQ. 'DIDI' ) THEN
              ZI(JINF+3*NCHAR+2+ICH) = 1
            ENDIF
          ENDIF
        ENDIF
C
C ----- ON REGARDE LES CHARGES DU TYPE MECANIQUE
C
        DO 7 K = 1,NBTYCH
          LCHIN = LIGRCH(1:13)//NOMLIG(K)//'.DESC'
          CALL JEEXIN(LCHIN,IRET)
          IF ( IRET .NE. 0 ) THEN
            IF( TYPCHA(1:4) .EQ. 'SUIV' ) THEN
              ZI(JINF+NCHAR+ICH) = MAX(4,ZI(JINF+NCHAR+ICH))
            ELSE
              IF (AFFCHA(5:7).EQ.'_FO') THEN
                ZI(JINF+NCHAR+ICH) = MAX(2,ZI(JINF+NCHAR+ICH))
                CALL DISMOI('F','PARA_INST',LCHIN(1:19),'CARTE',IBID,
     &                      PARCHA,IERD)
                IF ( PARCHA(1:3) .EQ. 'OUI' ) THEN
                  ZI(JINF+NCHAR+ICH) = MAX(3,ZI(JINF+NCHAR+ICH))
                ENDIF
              ELSE
                ZI(JINF+NCHAR+ICH) = MAX(1,ZI(JINF+NCHAR+ICH))
              ENDIF
            ENDIF
          ENDIF
 7      CONTINUE
C
C ----- ON REGARDE LES CHARGES DU TYPE THERMIQUE
C
        LCHIN = LIGRCH(1:13)//'.TEMPE.TEMP'
        CALL JEEXIN(LCHIN,IRET)
        IF ( IRET .NE. 0 ) THEN
          IF ( EXITHE ) THEN
            CALL UTMESS('F','NMDOMC_05','IL Y A PLUSIEURS CHARGES'
     &                  //' THERMIQUES ')
          ENDIF
          EXITHE = .TRUE.
          ZI(JINF+2*NCHAR+1) = ICH
        ENDIF
C
C --- NOMBRE MAXIMUM DE CHARGES DE TYPE FORCE DE LAPLACE
C
        INFC = 0
        DO 10 J =1,NBCHMX
          LCHIN(1:17) = LIGRCH(1:13)//'.FL1'
          CALL CODENT(J,'D0',LCHIN(18:19))
          LCHIN = LCHIN(1:19)//'.DESC'
          CALL JEEXIN(LCHIN,IRET)
          IF ( IRET .NE. 0 ) THEN
            INFC = INFC + 1
          ELSE
            GOTO 11
          ENDIF
 10     CONTINUE
 11     CONTINUE
        INFMAX = MAX (INFMAX,INFC)
        ZI(JINF+2*NCHAR+2) = INFMAX
C
C     ----- CHARGES DE TYPES CONTACT 1 SI PRESENT 0 SINON
C
        LCHIN = LIGRCH(1:13)//'.CONI'
        CALL JEEXIN(LCHIN,IRET)
        IF (IRET.EQ.0) THEN
           ZI(JINF+2*NCHAR+2+ICH) = 0
        ELSE
           ZI(JINF+2*NCHAR+2+ICH) = 1
        ENDIF
C
C ----- FONCTIONS MULTIPLICATIVES DES CHARGES
C
        CALL GETVID('EXCIT','FONC_MULT_C',ICH,1,1,NOMFCT,N1)
        CALL GETVID('EXCIT','FONC_MULT',ICH,1,1,NOMFCT,N11)
        IF ((N1.EQ.0).AND.(N11.EQ.0)) THEN
           CALL CODENT( ICH , 'D0' , KNUM  )
           NOMFCT = '&&NC'//KNUM
           CALL GETVC8('EXCIT','COEF_MULT_C',ICH,1,1,CCOEF,N2)
           IF ( N2 .EQ. 0 ) THEN
              CALL GETVR8('EXCIT','COEF_MULT',ICH,1,1,COEF,N3)
              CALL WKVECT(NOMFCT(1:19)//'.PROL','V V K16',5,JPRO)
              ZK16(JPRO)   = 'CONSTANT'
              ZK16(JPRO+1) = 'LIN LIN '
              ZK16(JPRO+2) = 'TOUTPARA'
              ZK16(JPRO+3) = 'TOUTRESU'
              ZK16(JPRO+4) = 'CC      '
              CALL WKVECT(NOMFCT(1:19)//'.VALE','V V R',3,JVAL)
              ZR(JVAL)   = 1.0D0
              ZR(JVAL+1) = COEF
              ZR(JVAL+2) = 0.D0
           ELSE
              CALL WKVECT(NOMFCT(1:19)//'.PROL','V V K16',5,JPRO)
              ZK16(JPRO)   = 'CONSTANT'
              ZK16(JPRO+1) = 'LIN LIN '
              ZK16(JPRO+2) = 'TOUTPARA'
              ZK16(JPRO+3) = 'TOUTRESU'
              ZK16(JPRO+4) = 'CC      '
              CALL WKVECT(NOMFCT(1:19)//'.VALE','V V R',3,JVAL)
              ZR(JVAL)   = 1.0D0
              ZR(JVAL+1) = DBLE( CCOEF )
              ZR(JVAL+2) = DIMAG( CCOEF )
           ENDIF
        ENDIF
        ZK24(IALIFC+ICH-1) = NOMFCT
 100  CONTINUE
 9999 CONTINUE
C FIN ------------------------------------------------------------------
      CALL JEDEMA()
      END
