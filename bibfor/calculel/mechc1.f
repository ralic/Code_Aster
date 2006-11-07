      SUBROUTINE MECHC1 (  MODELE, MATE, EXICAR, CHCARA )
      IMPLICIT  NONE
      LOGICAL                            EXICAR
      CHARACTER*(*)        MODELE, MATE,         CHCARA(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 07/11/2006   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     SURCHARGE LA CARTE '.CARCOQUE'
C     ------------------------------------------------------------------
C IN  : NOMA   : NOM DU MAILLAGE
C IN  : EXICAR : LE CARA EXISTE OU N'EXISTE PAS
C VAR : CHCARA : CARTE DE CARACTERISTIQUES COQUE
C     ------------------------------------------------------------------
C     ----- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER        ZI
      COMMON /IVARJE/ZI(1)
      REAL*8         ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16     ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL        ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8    ZK8
      CHARACTER*16          ZK16
      CHARACTER*24                  ZK24
      CHARACTER*32                          ZK32
      CHARACTER*80                                  ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     ----- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER      IBID, IOC, N1, N2, NA, NVEC, JNOMA, IRET, NREP,
     +             NBMA, NBMAIL, JMAIL, IALPHA, IBETA, IAD1, IAD2, IMA,
     +             NUMMA, NCMAX, ICESK, ICESL, ICESV, ICESC, ICESD,
     +             INDIK8, IE, NNCP
      REAL*8       R8B, ANG(2), VECT(3), R8PI
      COMPLEX*16   C16B
      LOGICAL      AFAIRE, LTOUT
      CHARACTER*3  OUINON
      CHARACTER*8  K8B, NOMA, MOTCLS(2), TYPMCL(2)
      CHARACTER*16 MOTCLE, PHENO, OPTION, NOMPAR
      CHARACTER*19 CARTE, CARTCO, CHELMS
      CHARACTER*24 MESMAI, LIGRMO
C DEB-------------------------------------------------------------------
C
      IF ( .NOT. EXICAR ) GOTO 9999
C
      CARTE = CHCARA(7)
      MOTCLE = 'REPE_COQUE'
C
      CALL GETFAC ( MOTCLE, NREP ) 
      IF ( NREP .EQ. 0 ) GOTO 9999
C
C --- POUR LES ELEMENTS TUYAUX, LA CARTE .CARCOQUE N'EXISTE PAS
C
      CALL EXISD ( 'CARTE', CARTE, IRET )
      IF ( IRET .EQ. 0 ) GOTO 9999
C
      CALL DISMOI('F','EXI_ANISO',MATE,'CHAM_MATER',IBID,OUINON,IE)
      CALL DISMOI('F','NOM_LIGREL',MODELE,'MODELE',IBID,LIGRMO,IE)
      CALL DISMOI('F','PHENOMENE',MODELE,'MODELE',IBID,PHENO,IE)
      IF ( PHENO .EQ. 'MECANIQUE' ) THEN
         OPTION = 'RIGI_MECA'
         NOMPAR = 'PCACOQU'
      ELSEIF ( PHENO .EQ. 'THERMIQUE' ) THEN
         OPTION = 'RIGI_THER'
         NOMPAR = 'PCACOQU'
      ELSEIF ( PHENO .EQ. 'ACOUSTIQUE' ) THEN
         OPTION = 'RIGI_ACOU'
         NOMPAR = ' '
      ELSE
         OPTION = ' '
         NOMPAR = ' '
      ENDIF
C
C --- ON VERIFIE SI ON DOIT SURCHARGER LA CARTE
C
      AFAIRE = .FALSE.
      DO 100 IOC = 1 , NREP
         CALL GETVR8 ( MOTCLE, 'ANGL_REP', IOC,1,0, ANG , NA   )
         CALL GETVR8 ( MOTCLE, 'VECTEUR' , IOC,1,0, VECT, NVEC )
         IF ( NA+NVEC .NE. 0 )  AFAIRE = .TRUE.
 100  CONTINUE
C
C --- DANS LE CAS DE L'ORTHOTROPIE, LE POST-TRAITEMENT EST A FAIRE
C     DANS SON REPERE, ON NE TRAITE PAS LES MOTS CLES 
C
      IF ( AFAIRE .AND. OUINON.EQ.'OUI' ) THEN
         CALL UTMESS('A','MECHC1','LES MOTS CLES DE "REPE_COQUE" '//
     &                            'NE SONT PAS TRAITES')
         GOTO 9999
      ENDIF
      IF ( OUINON.EQ.'OUI' ) GOTO 9999
C
      CARTCO = '&&MECHC1.CARCOQUE'
      CALL EXISD ( 'CHAM_ELEM', CARTCO, IRET )
      IF (IRET.NE.0)  CALL DETRSD ( 'CHAM_ELEM', CARTCO )
C
      K8B = ' '
      CHELMS = '&&MECHC1.ELEM_S  '
      CALL CARCES ( CARTE, 'ELEM', K8B, 'V', CHELMS, IRET )
C
      CALL JEVEUO ( CHELMS//'.CESK', 'L', ICESK )
      CALL JEVEUO ( CHELMS//'.CESC', 'L', ICESC )
      CALL JEVEUO ( CHELMS//'.CESD', 'L', ICESD )
      CALL JEVEUO ( CHELMS//'.CESL', 'L', ICESL )
      CALL JEVEUO ( CHELMS//'.CESV', 'L', ICESV )
C
      NOMA   = ZK8(ICESK)
      NBMAIL =  ZI(ICESD)
      NCMAX  =  ZI(ICESD+1)
C
C --- ON SURCHARGE 'ALPHA' ET 'BETA' ---
C
      IALPHA = INDIK8 ( ZK8(ICESC), 'ALPHA   ', 1, NCMAX )
      IBETA  = INDIK8 ( ZK8(ICESC), 'BETA    ', 1, NCMAX )
C
      MOTCLS(1) = 'GROUP_MA'
      MOTCLS(2) = 'MAILLE'
      TYPMCL(1) = 'GROUP_MA'
      TYPMCL(2) = 'MAILLE'
      MESMAI = '&&MECHN2.MES_MAILLES'
C
      DO 10 IOC = 1 , NREP
C
         CALL GETVID ( MOTCLE, 'MAILLE'  , IOC,1,0, K8B, N1 )
         CALL GETVID ( MOTCLE, 'GROUP_MA', IOC,1,0, K8B, N2 )
         IF ( N1+N2 .EQ. 0 ) THEN
            LTOUT = .TRUE. 
            NBMA  = NBMAIL
         ELSE
            CALL RELIEM(' ', NOMA, 'NU_MAILLE', MOTCLE, IOC, 2, 
     +                                  MOTCLS, TYPMCL, MESMAI, NBMA )
            IF ( NBMA.NE.0 )  CALL JEVEUO ( MESMAI, 'L', JMAIL )
            LTOUT = .FALSE. 
         ENDIF
C
         ANG(1) = 0.D0
         ANG(2) = 0.D0
         AFAIRE = .FALSE.
         CALL GETVR8 ( MOTCLE, 'ANGL_REP', IOC,1,2, ANG , NA   )
         CALL GETVR8 ( MOTCLE, 'VECTEUR' , IOC,1,3, VECT, NVEC )
         IF ( NA+NVEC .NE. 0 )  AFAIRE = .TRUE.
         IF (NVEC.NE.0) THEN
            CALL ANGVX ( VECT, ANG(1), ANG(2) )
            ANG(1)=  ANG(1)*180.D0/R8PI()
            ANG(2)= -ANG(2)*180.D0/R8PI()
         ENDIF
C
         DO 30 IMA = 1 , NBMA
            IF ( LTOUT ) THEN
               NUMMA = IMA
            ELSE
               NUMMA = ZI(JMAIL+IMA-1)
            ENDIF
C
            CALL CESEXI ( 'C', ICESD, ICESL, NUMMA, 1, 1, IALPHA, IAD1 )
            IF ( IAD1 .GT. 0 ) THEN
               IF ( ZR(ICESV-1+IAD1).NE.0.D0 ) THEN
                  CALL UTMESS('A','MECHC1',
     &         'LE REPERE DE POST_TRAITEMENT A ETE DEFINI DANS LA '//
     &         'COMMANDE AFFE_CARA_ELEM / COQUE, IL EST SOUHAITABLE '//
     &         'DE DEFINIR CE REPERE PAR LE MOT CLE FACTEUR  '//
     &             '"REPE_COQUE" DE LA COMMANDE CALC_ELEM.')
               ENDIF
               IF ( AFAIRE )  ZR(ICESV-1+IAD1) = ANG(1)
            ELSEIF ( IAD1 .LT. 0 ) THEN
               IAD1 = -IAD1
               ZL(ICESL-1+IAD1) = .TRUE.
               ZR(ICESV-1+IAD1) = ANG(1)
            ENDIF
C
            CALL CESEXI ( 'C', ICESD, ICESL, NUMMA, 1, 1, IBETA, IAD2 )
            IF ( IAD2 .GT. 0 ) THEN
               IF ( ZR(ICESV-1+IAD2).NE.0.D0 ) THEN
                  CALL UTMESS('A','MECHC1',
     &         'LE REPERE DE POST_TRAITEMENT A ETE DEFINI DANS LA '//
     &         'COMMANDE AFFE_CARA_ELEM / COQUE, IL EST SOUHAITABLE '//
     &         'DE DEFINIR CE REPERE PAR LE MOT CLE FACTEUR  '//
     &            '"REPE_COQUE" DE LA COMMANDE CALC_ELEM.')
               ENDIF
               IF ( AFAIRE )  ZR(ICESV-1+IAD2) = -ANG(2)
            ELSEIF  ( IAD2 .LT. 0 ) THEN
               IAD2 = -IAD2
               ZL(ICESL-1+IAD2) = .TRUE.
               ZR(ICESV-1+IAD2) = -ANG(2)
            ENDIF
C
 30      CONTINUE
C
         IF (.NOT. LTOUT)  CALL JEDETR ( MESMAI )
C
 10   CONTINUE
C
      CALL CESCEL ( CHELMS, LIGRMO, OPTION, NOMPAR, 'NON',
     +              NNCP, 'V', CARTCO )
C
      CALL DETRSD ( 'CHAM_ELEM_S', CHELMS )
C
      CHCARA(7) = CARTCO
C
 9999 CONTINUE
C
      END
