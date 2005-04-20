      SUBROUTINE IRCHSU ( NBCHAR, CHARG , IFC , VERSIO )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             NBCHAR,         IFC , VERSIO
      CHARACTER*8                 CHARG(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 18/04/2005   AUTEUR VABHHTS J.PELLET 
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
C TOLE CRP_20
C     IMPRESSION D'UN CONCEPT CHARGE AU FORMAT "IDEAS"
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
      CHARACTER*1  BL
      CHARACTER*4  TYPVAL
      CHARACTER*8  K8B, CHARGE, TYPE, GRAN
      CHARACTER*16 TYPELM
      CHARACTER*19 LIGREL, CIMPO, CMULT
      CHARACTER*24 NEMA
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      IFM = IUNIFI('MESSAGE')
      BL = ' '
C
      DO 10 IC = 1 , NBCHAR
         CHARGE = CHARG(IC)
         LIGREL = CHARGE//'.CHME.LIGRE'
         NEMA   = CHARGE//'.CHME.LIGRE.NEMA'
         CIMPO  = CHARGE//'.CHME.CIMPO'
         CMULT  = CHARGE//'.CHME.CMULT'
C

C
         CALL JEVEUO(CHARGE//'.TYPE','L',JTYPE)
         TYPE = ZK8(JTYPE)
         IF ( TYPE .NE. 'MECA_RE ' ) THEN
            CALL UTMESS('A','IRCHSU','IMPRESSION NON PREVUE POUR LA '//
     +                      'CHARGE "'//CHARGE//'", VRAIMENT DESOLE !')
            GOTO 10
         ENDIF
C
         CALL JEEXIN(CIMPO//'.NOMA',IRE1)
         CALL JEEXIN(CMULT//'.NOMA',IRE2)
         IRET = IRE1 + IRE2
         IF ( IRET .EQ. 0 ) THEN
            CALL UTMESS('A','IRCHSU','IMPRESSION DE CHARGE A DDL '//
     +                      'IMPOSE "'//CHARGE//'", VRAIMENT DESOLE !')
            GOTO 10
         ENDIF
C
C        --- NOMBRE DE RELATIONS ---
C
         CALL JEVEUO(CIMPO//'.DESC','L',JDESC)
         NUMGD = ZI(JDESC)
         CALL JENUNO(JEXNUM('&CATA.GD.NOMGD',NUMGD),GRAN)
         CALL JELIRA(JEXNUM('&CATA.GD.NOMCMP',NUMGD),'LONMAX',NCMPI,BL)
         NBRELA = ZI(JDESC+2)
         CALL JEVEUO(CMULT//'.DESC','L',JDESC)
         NUMGD = ZI(JDESC)
         CALL JENUNO(JEXNUM('&CATA.GD.NOMGD',NUMGD),GRAN)
         CALL JELIRA(JEXNUM('&CATA.GD.NOMCMP',NUMGD),'LONMAX',NCMPM,BL)
C
C        --- NOMBRE DE LAGRANGES ---
C
         CALL JEVEUO(LIGREL//'.NBNO','L',JNBNO)
         NBLAGR = ZI(JNBNO)
C
C        --- ON SEPARE DDL_IMPO ET LIAISON_DDL ---
C
         CALL WKVECT('&&IRCHSU.LAGRANGE','V V I',NBLAGR,JLAGR)
         DO 20 IMAIL = 1 , NBRELA
           CALL JEVEUO(JEXNUM(NEMA,IMAIL),'L',JGR)
           LAGR1 = -ZI(JGR+1)
           LAGR2 = -ZI(JGR+2)
           ZI(JLAGR+LAGR1-1) = ZI(JLAGR+LAGR1-1) + 1
           ZI(JLAGR+LAGR2-1) = ZI(JLAGR+LAGR2-1) + 1
 20      CONTINUE
         CALL WKVECT('&&IRCHSU.DDL_IMPO','V V I',NBRELA,JDDLI)
         CALL WKVECT('&&IRCHSU.LIAISON' ,'V V I',NBRELA,JLIAI)
         NBDDLI = 0
         NBLIAI = 0
         DO 22 IMAIL = 1 , NBRELA
           CALL JEVEUO(JEXNUM(NEMA,IMAIL),'L',JGR)
           IF ( ZI(JLAGR-ZI(JGR+1)-1) .EQ. 1 ) THEN
             ZI(JDDLI+NBDDLI) = IMAIL
             NBDDLI = NBDDLI + 1
           ELSE
             ZI(JLIAI+NBLIAI) = IMAIL
             NBLIAI = NBLIAI + 1
           ENDIF
 22      CONTINUE
         IF ( NBLIAI .NE. 0 )
     +                   WRITE(IFM,*)'NOMBRE DE LIAISON_DDL : ',NBLIAI
         IF ( NBDDLI .NE. 0 )
     +                   WRITE(IFM,*)'NOMBRE DE DDL_IMPO : ',NBDDLI
C
C======================================================================
C
C                 --- TRAITEMENT DE DDL_IMPO ---
C
C======================================================================
         IF ( NBDDLI .NE. 0 ) THEN
C
C        --- NOMBRE DE NOEUDS CONCERNES ---
C
         CALL WKVECT('&&IRCHSU.NOE','V V I',NBDDLI,JNOE)
         CALL JEVEUO(JEXNUM(NEMA,ZI(JDDLI)),'L',JGR)
         NBNOEU = 1
         ZI(JNOE+NBNOEU-1) = ZI(JGR)
         DO 100 I = 2 , NBDDLI
           NUMAIL = ZI(JDDLI+I-1)
           CALL JEVEUO(JEXNUM(NEMA,NUMAIL),'L',JGR)
           INOE = ZI(JGR)
           DO 102 J = 1 , NBNOEU
             IF ( INOE .EQ. ZI(JNOE+J-1) ) GOTO 100
 102       CONTINUE
           NBNOEU = NBNOEU + 1
           ZI(JNOE+NBNOEU-1) = INOE
 100     CONTINUE
C
C        --- LES COEFFICIENTS IMPOSES ---
C
         CALL JEVEUO(CIMPO//'.VALE','L',JCOEFI)
         CALL JELIRA(CIMPO//'.VALE','TYPE',IBKD,TYPVAL)
         IF ( TYPVAL(1:1) .EQ. 'R' ) THEN
         ELSEIF ( TYPVAL(1:1) .EQ. 'C' ) THEN
            CALL UTMESS('A','IRCHSU','TYPVAL "'//TYPVAL//'" DES COEF'//
     +                      'FICIENTS IMPOSES DE LA CHARGE "'//CHARGE//
     +                      '" NON TRAITE, VRAIMENT DESOLE !')
            GOTO 10
         ELSE
            CALL UTMESS('F','IRCHSU','TYPVAL "'//TYPVAL//'" DES COEF'//
     +                      'FICIENTS IMPOSES DE LA CHARGE "'//CHARGE//
     +                      '" NON TRAITE, VRAIMENT DESOLE !')
            GOTO 10
         ENDIF
C
         CALL JELIRA(LIGREL//'.LIEL','NMAXOC',NBGREL,K8B)
C
        IF ( VERSIO .EQ. 5 ) THEN
          WRITE (IFC,'(A)') '    -1'
          WRITE (IFC,'(A)') '   755'
          IND  = 1
          WRITE (IFC,'(2I10)') IC, IND
          WRITE (IFC,'(A)') 'RESTRAINT SETS'
          ICOL = 7
          KDEP = 2
          I9   = 1
          DO 30 IN = 1 , NBNOEU
            INOE = ZI(JNOE+IN-1)
            IDX  = 0
            IDY  = 0
            IDZ  = 0
            IDRX = 0
            IDRY = 0
            IDRZ = 0
            XDX  = 0.D0
            XDY  = 0.D0
            XDZ  = 0.D0
            XDRX = 0.D0
            XDRY = 0.D0
            XDRZ = 0.D0
            DO 32 I = 1 , NBDDLI
              NUMAIL = ZI(JDDLI+I-1)
              CALL JEVEUO(JEXNUM(NEMA,NUMAIL),'L',JNM)
              IF ( ZI(JNM) .EQ. INOE ) THEN
C               --- TYPE DE MAILLE ? ---
                DO 40 IG = 1 , NBGREL
                  CALL JEVEUO(JEXNUM(LIGREL//'.LIEL',IG),'L',JGR)
               CALL JELIRA(JEXNUM(LIGREL//'.LIEL',IG),'LONMAX',NEL,K8B)
                  DO 42 IE = 1 , NEL-1
                    IF ( ZI(JGR+IE-1) .EQ. -NUMAIL) GOTO 44
 42               CONTINUE
 40             CONTINUE
           CALL UTMESS('F','IRCHSU','ERREUR TROP GRAVE POUR CONTINUER')
 44             CONTINUE
                ITYPEL = ZI(JGR+NEL-1)
                CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',ITYPEL),TYPELM)
                IF ( TYPELM(10:13) .EQ. 'DX ' ) THEN
                  IDX = 1
                  XDX = ZR(JCOEFI+NCMPI*(NUMAIL-1)+1-1)
                ELSEIF ( TYPELM(10:13) .EQ. 'DY ' ) THEN
                  IDY = 1
                  XDY = ZR(JCOEFI+NCMPI*(NUMAIL-1)+1-1)
                ELSEIF ( TYPELM(10:13) .EQ. 'DZ ' ) THEN
                  IDZ = 1
                  XDZ = ZR(JCOEFI+NCMPI*(NUMAIL-1)+1-1)
                ELSEIF ( TYPELM(10:13) .EQ. 'DRX' ) THEN
                  IDRX = 1
                  XDRX = ZR(JCOEFI+NCMPI*(NUMAIL-1)+1-1)
                ELSEIF ( TYPELM(10:13) .EQ. 'DRY' ) THEN
                  IDRY = 1
                  XDRY = ZR(JCOEFI+NCMPI*(NUMAIL-1)+1-1)
                ELSEIF ( TYPELM(10:13) .EQ. 'DRZ' ) THEN
                  IDRZ = 1
                  XDRZ = ZR(JCOEFI+NCMPI*(NUMAIL-1)+1-1)
                ENDIF
              ENDIF
 32         CONTINUE
            WRITE (IFC,'(2I10,7I2)')
     +             INOE, ICOL, IDX, IDY, IDZ, IDRX, IDRY, IDRZ, I9
            WRITE (IFC,'(1P,6D13.5)')
     +             XDX, XDY, XDZ, XDRX, XDRY, XDRZ
 30       CONTINUE
          WRITE (IFC,'(A)') '    -1'
        ENDIF
        ENDIF
C
C======================================================================
C
C                 --- TRAITEMENT DE LIAISON_DDL ---
C
C======================================================================
        IF ( NBLIAI .NE. 0 ) THEN
C
C       --- NOMBRE DE RELATIONS, NOMBRE DE TERMES DE LA RELATION ---
C
        CALL WKVECT('&&IRCHSU.RELA','V V I',NBLAGR,JRELA)
        CALL WKVECT('&&IRCHSU.TERM','V V I',NBLAGR,JTERM)
        NUMAIL = ZI(JLIAI)
        NBEQUA = 1
        ZI(JRELA+NUMAIL-1) = NBEQUA
        ZI(JTERM+NBEQUA-1) = 1
        DO 200 I = 2 , NBLIAI
          NUMAIL = ZI(JLIAI+I-1)
          CALL JEVEUO(JEXNUM(NEMA,NUMAIL),'L',JGR1)
          LAGR1 = ZI(JGR1+1)
          DO 202 J = 1 , I-1
            CALL JEVEUO(JEXNUM(NEMA,ZI(JLIAI+J-1)),'L',JGR2)
            LAGR2 = ZI(JGR2+1)
            IF ( LAGR1 .EQ. LAGR2 ) THEN
              IRELA = ZI(JRELA+ZI(JLIAI+J-1)-1)
              ZI(JTERM+IRELA-1) = ZI(JTERM+IRELA-1) + 1
              ZI(JRELA+NUMAIL-1) = IRELA
              GOTO 200
            ENDIF
 202      CONTINUE
          NBEQUA = NBEQUA + 1
          ZI(JTERM+NBEQUA-1) = 1
          ZI(JRELA+NUMAIL-1) = NBEQUA
 200    CONTINUE
C
C        --- LES COEFFICIENTS IMPOSES ---
C
         CALL JEVEUO(CIMPO//'.VALE','L',JCOEFI)
         CALL JELIRA(CIMPO//'.VALE','TYPE',IBKD,TYPVAL)
         IF ( TYPVAL(1:1) .EQ. 'R' ) THEN
            ITYPI = 0
         ELSEIF ( TYPVAL(1:1) .EQ. 'C' ) THEN
            ITYPI = 1
         ELSE
            CALL UTMESS('A','IRCHSU','TYPVAL "'//TYPVAL//'" DES COEF'//
     +                      'FICIENTS IMPOSES DE LA CHARGE "'//CHARGE//
     +                      '" NON TRAITE, VRAIMENT DESOLE !')
            GOTO 10
         ENDIF
C
C        --- LES COEFFICIENTS MULTIPLICATEURS ---
C
         CALL JEVEUO(CMULT//'.VALE','L',JCOEFM)
         CALL JELIRA(CMULT//'.VALE','TYPE',IBKD,TYPVAL)
         IF ( TYPVAL(1:1) .EQ. 'R' ) THEN
            ITYPM = 0
         ELSEIF ( TYPVAL(1:1) .EQ. 'C' ) THEN
            ITYPM = 1
         ELSE
            CALL UTMESS('A','IRCHSU','TYPVAL "'//TYPVAL//'" DES COEF'//
     +                      'FICIENTS IMPOSES DE LA CHARGE "'//CHARGE//
     +                      '" NON TRAITE, VRAIMENT DESOLE !')
            GOTO 10
         ENDIF
C
        IF ( VERSIO .EQ. 5 ) THEN
          WRITE (IFC,'(A)') '    -1'
          WRITE (IFC,'(A)') '   754'
          IND  = 2
          WRITE (IFC,'(2I10)') IC, IND
          WRITE (IFC,'(A)') 'CONSTRAINT SETS'
          ICOL = 7
          KDEP = 2
          I9   = 1
          DO 210 IN = 1 , NBEQUA
            ITERM = ZI(JTERM+IN-1)
            XREAL = 0.D0
            XIMAG = 0.D0
            DO 220 J = 1 , NBLIAI
              NUMAIL = ZI(JLIAI+J-1)
              IF ( ZI(JRELA+NUMAIL-1) .EQ. IN ) THEN
                IF ( ITYPI .EQ. 0 ) THEN
                  XREAL = XREAL + ZR(JCOEFI+NCMPI*(NUMAIL-1)+1-1)
                ELSE
                  XREAL = XREAL + DBLE(ZC(JCOEFI+NCMPI*(NUMAIL-1)+1-1))
                  XIMAG = XIMAG + DIMAG(ZC(JCOEFI+NCMPI*(NUMAIL-1)+1-1))
                ENDIF
              ENDIF
 220        CONTINUE
            WRITE (IFC,'(4I10,1P,2D13.5,I10)')
     +             IN, ITERM, KDEP, ICOL, XREAL, XIMAG, ITYPI
            DO 212 J = 1 , NBLIAI
              NUMAIL = ZI(JLIAI+J-1)
              IF ( ZI(JRELA+NUMAIL-1) .EQ. IN ) THEN
                NUMAIL = ZI(JLIAI+J-1)
                CALL JEVEUO(JEXNUM(NEMA,NUMAIL),'L',JGR)
                INOE = ZI(JGR)
C               --- TYPE DE MAILLE ? ---
                DO 240 IG = 1 , NBGREL
                  CALL JEVEUO(JEXNUM(LIGREL//'.LIEL',IG),'L',JGR)
               CALL JELIRA(JEXNUM(LIGREL//'.LIEL',IG),'LONMAX',NEL,K8B)
                  DO 242 IE = 1 , NEL-1
                    IF ( ZI(JGR+IE-1) .EQ. -NUMAIL) GOTO 244
 242              CONTINUE
 240            CONTINUE
           CALL UTMESS('F','IRCHSU','ERREUR TROP GRAVE POUR CONTINUER')
 244            CONTINUE
                ITYPEL = ZI(JGR+NEL-1)
                CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',ITYPEL),TYPELM)
                IF ( TYPELM(10:13) .EQ. 'DX ' ) THEN
                  IDDL = 1
                ELSEIF ( TYPELM(10:13) .EQ. 'DY ' ) THEN
                  IDDL = 2
                ELSEIF ( TYPELM(10:13) .EQ. 'DZ ' ) THEN
                  IDDL = 3
                ELSEIF ( TYPELM(10:13) .EQ. 'DRX' ) THEN
                  IDDL = 4
                ELSEIF ( TYPELM(10:13) .EQ. 'DRY' ) THEN
                  IDDL = 5
                ELSEIF ( TYPELM(10:13) .EQ. 'DRZ' ) THEN
                  IDDL = 6
                ENDIF
                IF ( ITYPM .EQ. 0 ) THEN
                  XREAL = ZR(JCOEFM+NCMPM*(NUMAIL-1)+1-1)
                  XIMAG = 0.D0
                ELSE
                  XREAL =  DBLE( ZC(JCOEFM+NCMPM*(NUMAIL-1)+1-1) )
                  XIMAG = DIMAG( ZC(JCOEFM+NCMPM*(NUMAIL-1)+1-1) )
                ENDIF
                WRITE (IFC,'(I10,I2,1P,2D13.5)') INOE,IDDL,XREAL,XIMAG
              ENDIF
 212        CONTINUE
 210      CONTINUE
          WRITE (IFC,'(A)') '    -1'
        ENDIF
        ENDIF
C
        CALL JEDETR('&&IRCHSU.LAGRANGE')
        CALL JEDETR('&&IRCHSU.DDL_IMPO')
        CALL JEDETR('&&IRCHSU.LIAISON' )
        CALL JEDETR('&&IRCHSU.NOE')
        CALL JEDETR('&&IRCHSU.RELA')
        CALL JEDETR('&&IRCHSU.TERM')
 10   CONTINUE
C
      CALL JEDEMA()
      END
