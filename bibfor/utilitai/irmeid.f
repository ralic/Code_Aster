      SUBROUTINE IRMEID ( MATREL, FICH, VERSIO, NMSUP, TABIMA, TABELE )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER                          VERSIO,NMSUP,TABIMA(*),TABELE(*)
      CHARACTER*8         MATREL, FICH
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 30/09/97   AUTEUR VABHHTS J.PELLET 
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
C     IMPRESSION D'UN CONCEPT MATR_ELEM AU FORMAT "IDEAS"
C
C     ATTENTION: AFIN D'AVOIR UNE PLUS GRANDE PRECISION, LES VALEURS
C                SONT IMPRIMES AU FORMAT E20.12 AU LIEU DE E13.5
C     ------------------------------------------------------------------
C
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
      CHARACTER*32      JEXNOM, JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER      NBELEM, DIGDEL, ADDMC
      CHARACTER*1  K1B
      CHARACTER*8  K8B, GRAN, SCAL, SCALAI, NOMA, NOMMAI, NOMAIL
      CHARACTER*16 OPTION, OPTIO2
      CHARACTER*19 MATR, LIGREL
      CHARACTER*24 NOLI, DESC
      CHARACTER*80 TITMAI
      LOGICAL      LMASU
C     ------------------------------------------------------------------
C
      CALL JEMARQ ( )
      IFC = IUNIFI(FICH)
      IFM = IUNIFI('MESSAGE')
C
      CALL JEVEUO(MATREL//'.LISTE_RESU','L',JLRESU)
      CALL JELIRA(MATREL//'.LISTE_RESU','LONMAX',NBRESU,K8B)
      CALL JEVEUO(MATREL//'.REFE_RESU','L',JRRESU)
      OPTION = ZK24(JRRESU+1)
      IF ( OPTION .EQ. 'RIGI_MECA' ) THEN
        IMAT = 9
      ELSEIF ( OPTION .EQ. 'MASS_MECA' ) THEN
        IMAT = 6
      ELSEIF ( OPTION .EQ. 'AMOR_MECA' ) THEN
        IMAT = 7
      ELSE
        IMAT = 0
      ENDIF
C
      MATR = ZK24(JLRESU)
      NOLI = MATR//'.NOLI'
      CALL JEVEUO(NOLI,'L',JNOLI)
      LIGREL = ZK24(JNOLI)
      CALL DISMOI('F','NOM_MAILLA'  ,LIGREL,'LIGREL',IBID  ,NOMA,IE)
      CALL DISMOI('F','NB_MA_MAILLA',LIGREL,'LIGREL',NBELET,K8B ,IE)
C
      LMASU = .FALSE.
      CALL JEEXIN(NOMA//'           .TITR',IRET)
      IF (IRET.NE.0) THEN
         CALL JEVEUO(NOMA//'           .TITR','L',JTITR)
         CALL JELIRA(NOMA//'           .TITR','LONMAX',NBTITR,K1B)
         IF ( NBTITR .GE. 1 ) THEN
            TITMAI = ZK80(JTITR-1+1)
            IF(TITMAI(10:31).EQ.'AUTEUR=INTERFACE_IDEAS') LMASU =.TRUE.
         ENDIF
      ENDIF
C
      IF ( VERSIO .EQ. 5 ) THEN
         WRITE (IFC,'(A)') '    -1'
         WRITE (IFC,'(A)') '   233'
      ENDIF
      IIII = 0
      DO 10 IMEL = 1 , NBRESU
        MATR = ZK24(JLRESU+IMEL-1)
        DESC = MATR//'.DESC'
        NOLI = MATR//'.NOLI'
        CALL JEEXIN(DESC,IRET)
        IF ( IRET .EQ. 0 ) GOTO 10
        CALL JEVEUO(DESC,'L',JDESC)
        CALL JEVEUO(NOLI,'L',JNOLI)
        LIGREL = ZK24(JNOLI)
        OPTIO2 = ZK24(JNOLI+1)
        IF ( OPTIO2 .NE. OPTION ) GOTO 10
        CALL JELIRA(LIGREL//'.LIEL','NMAXOC',NBGREL,K8B)
C
        NUMGD = ZI(JDESC)
        SCAL  = SCALAI(NUMGD)
C
        DO 20 IGREL = 1 , NBGREL
          MODE = ZI(JDESC-1+2+IGREL)
          IF (MODE.EQ.0) GOTO 20
          CALL JEVEUO(JEXNUM(MATR//'.RESL',IGREL),'L',JVALE)
          CALL JEVEUO(JEXNUM(LIGREL//'.LIEL',IGREL),'L',JGR)
          NCMPEL = DIGDEL(MODE)
C          ICOEF  = ZI(JDESC-1 +2 +2*NBGREL +IGREL)
C          NCMPEL = NCMPEL * ICOEF
          NEL    = NBELEM(LIGREL,IGREL)
          M = ( NINT ( SQRT( DBLE(1 + 8*NCMPEL) ) ) - 1 ) / 2
          IVERI = M * ( M + 1 ) / 2
          IF ( NCMPEL .NE. IVERI ) THEN
            CALL UTMESS('F','IRMEID_1','IL Y UN BUG.')
          ENDIF
          M2 = M * M
          INBM = 1
C
C         ALLOCATION DU TABLEAU DE RECOPIE DE LA MATRICE ELEM
          CALL WKVECT('MC','G V R',M2,ADDMC)
C
          DO 22 IEL = 1 , NEL
            IMAIL = ZI(JGR+IEL-1)
            IF ( IMAIL .LT. 0 ) THEN
              DO 24 IMA = 1 , NMSUP
                IF ( IMAIL .EQ. TABIMA(IMA) ) GOTO 26
 24           CONTINUE
              CALL UTMESS('F','IRMEID','MANQUE UNE MAILLE TARDIVE')
 26           CONTINUE
              NUMEMA = TABELE(IMA)
            ELSE
               IF ( LMASU ) THEN
                  CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',IMAIL),NOMAIL)
                  CALL LXLIIS(NOMAIL(3:8),NUMEMA,IER)
               ELSE
                  NUMEMA = IMAIL
               ENDIF
            ENDIF
C
            IF (SCAL(1:1).EQ.'I') THEN
              CALL UTMESS('F','IRMEID_2','DEVELOPPEMENT NON REALISE.')
            ELSE IF (SCAL(1:1).EQ.'R') THEN
              IJ = 0
              DO 110 I1 = 0 , M-1
                DO 112 I2 = 0 , M-1
                  IJ = IJ + 1
                  ZR(ADDMC+I1+M*I2) = ZR(JVALE+(IEL-1)*NCMPEL-1+IJ)
                  ZR(ADDMC+I2+M*I1) = ZR(JVALE+(IEL-1)*NCMPEL-1+IJ)
                  IF ( I2 .EQ. I1 ) GOTO 110
 112            CONTINUE
 110          CONTINUE
C
              IF ( VERSIO .EQ. 5 ) THEN
                IIII = IIII + 1
                WRITE (IFC,'(2I10)') NUMEMA , INBM
                WRITE (IFC,'(2I10)') IMAT, M
                WRITE (IFC ,'(1P,4D20.12)')( ZR(ADDMC+I) , I=0,M2-1  )
              ENDIF
            ELSE IF (SCAL(1:1).EQ.'C') THEN
              CALL UTMESS('F','IRMEID_3','DEVELOPPEMENT NON REALISE.')
            ELSE IF (SCAL(1:1).EQ.'K') THEN
              CALL UTMESS('F','IRMEID_4','DEVELOPPEMENT NON REALISE.')
            ENDIF
 22       CONTINUE
          CALL JEDETR('MC')
 20     CONTINUE
C
 10   CONTINUE
      IF ( VERSIO .EQ. 5 ) THEN
         WRITE (IFC,'(A)') '    -1'
      ENDIF
      WRITE(IFM,*)'OPTION DE CALCUL DES MATRICES ELEMENTAIRES : ',OPTION
      WRITE(IFM,*)'NOMBRE DE MATRICES ELEMENTAIRES IMPRIMEES  : ',IIII
      WRITE(IFM,*) ' '
C
      CALL JEDEMA ( )
      END
