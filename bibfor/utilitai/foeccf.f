      SUBROUTINE FOECCF( NOMF , IUNI , IND , FONINS )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)      NOMF ,              FONINS
      INTEGER                   IUNI , IND
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 30/03/2001   AUTEUR MCOURTOI M.COURTOIS 
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
C     ECRITURE D'UNE FONCTION AU FORMAT "LANGAGE DE COMMANDE"
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
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32 JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      CHARACTER*1   QUOTE,  PRGC  , PRDR
      CHARACTER*8   NOMPAR, NOMRES, INTER
      CHARACTER*16  TYPFON
      CHARACTER*19  NOMFON
      CHARACTER*24  PROL  , VALE, PARA,  TITR
      CHARACTER*8 K8BID
C     ------------------------------------------------------------------
      CALL JEMARQ()
      IF (IND.NE.0) THEN
         CALL UTMESS('E','FOECCF','DEVELOPPEMENT NON IMPLANTE.')
      ENDIF
      NOMFON = NOMF
      PROL   = NOMFON//'.PROL'
      VALE   = NOMFON//'.VALE'
      PARA   = NOMFON//'.PARA'
      TITR   = NOMFON//'.TITR'
C
C     --- INITIALISATIONS DIVERSES ET VARIEES ---
      QUOTE = ''''
C
C     --- ECRITURE DU TITRE ----
      CALL JEEXIN(TITR,IRET)
      IF (IRET .NE. 0 ) THEN
         CALL JEVEUO(TITR,'L',LTITR)
         CALL JELIRA(TITR,'LONMAX',NBTITR,K8BID)
         DO 10 I = 1, NBTITR
            WRITE(IUNI,'(2A)') '#',ZK80(LTITR+I-1)
 10      CONTINUE
      ENDIF
C
C     --- INFORMATIONS COMPLEMENTAIRES POUR L'EDITION ---
      CALL JEVEUO(PROL,'L',LPROL)
      TYPFON = ZK8(LPROL)
      IF (TYPFON .EQ. 'CONSTANT ')  TYPFON = 'CONSTANTE'
      NOMPAR = ZK8(LPROL+2)
      NOMRES = ZK8(LPROL+3)
      PRGC   = ZK8(LPROL+4)(1:1)
      PRDR   = ZK8(LPROL+4)(2:2)
C
      WRITE(IUNI,'(9A)') NOMF,' = ',' DEFI_',TYPFON,'('
C
      LG = LXLGUT(NOMRES)
      WRITE(IUNI,'(10X,9A)') 'NOM_RESU= ',QUOTE,NOMRES(1:LG),QUOTE,','
C
      IF (TYPFON .EQ. 'CONSTANTE' ) GOTO 100
      LG = LXLGUT(NOMPAR)
      WRITE(IUNI,'(10X,9A)') 'NOM_PARA= ',QUOTE,NOMPAR(1:LG),QUOTE,','
C
      IF (PRGC.EQ.'E') THEN
         WRITE(IUNI,'(10X,9A)') 'PROL_GAUCHE= ''EXCLU'','
      ELSEIF (PRGC.EQ.'C') THEN
         WRITE(IUNI,'(10X,9A)') 'PROL_GAUCHE= ''CONSTANT'','
      ELSEIF (PRGC.EQ.'L') THEN
         WRITE(IUNI,'(10X,9A)') 'PROL_GAUCHE= ''LINEAIRE'','
      ELSE
C     JE NE COMPRENDS DANS QUEL CAS ON PASSE ICI
         WRITE(IUNI,'(10X,9A)') 'PROL_GAUCHE= % ',PRGC
      ENDIF
      IF (PRDR.EQ.'E') THEN
         WRITE(IUNI,'(10X,9A)') 'PROL_DROITE= ''EXCLU'','
      ELSEIF (PRDR.EQ.'C') THEN
         WRITE(IUNI,'(10X,9A)') 'PROL_DROITE= ''CONSTANT'','
      ELSEIF (PRDR.EQ.'L') THEN
         WRITE(IUNI,'(10X,9A)') 'PROL_DROITE= ''LINEAIRE'','
      ELSE
C     JE NE COMPRENDS DANS QUEL CAS ON PASSE ICI
         WRITE(IUNI,'(10X,9A)') 'PROL_DROITE= % ',PRDR
      ENDIF
C
C     --- ECRITURE DES VALEURS ---
  100 CONTINUE
      IF ( TYPFON .EQ. 'CONSTANTE' ) THEN
         CALL JEVEUO(VALE,'L',LVAR)
         WRITE(IUNI,'(10X,A,1PE13.5,A)') 'VALE= ',ZR(LVAR-1+2),','
      ELSEIF ( TYPFON .EQ. 'FONCTION' ) THEN
         CALL JELIRA(VALE,'LONUTI',NBVAL,K8BID)
         NBVAL = NBVAL/2
         CALL JEVEUO(VALE,'L',LVAR)
         LFON = LVAR + NBVAL
         WRITE(IUNI,'(10X,9A)') 'VALE= ('
         IF (MOD(NBVAL,2).EQ.0) THEN
            DO 110 IVAL= 0, NBVAL-1 , 2
               WRITE(IUNI,'(4X,4(1PE13.5,1A))')
     +                   (ZR(LVAR+IVAL+J),',',ZR(LFON+IVAL+J),',',J=0,1)
 110        CONTINUE
         ELSE
            DO 112 IVAL= 0, NBVAL-2 , 2
               WRITE(IUNI,'(4X,4(1PE13.5,1A))')
     +                   (ZR(LVAR+IVAL+J),',',ZR(LFON+IVAL+J),',',J=0,1)
 112        CONTINUE
            WRITE(IUNI,'(4X,4(1PE13.5,1A))')
     +                    ZR(LVAR+NBVAL-1),',',ZR(LFON+NBVAL-1),','
         ENDIF
         WRITE(IUNI,'(10X,9A)') '     )'
      ELSE
         NOMPAR = ZK8(LPROL+5)
         LG = LXLGUT(NOMPAR)
         WRITE(IUNI,'(10X,9A)')
     +                 'NOM_PARA_FONC= ',QUOTE,NOMPAR(1:LG),QUOTE,','
C
         CALL JELIRA(PARA,'LONUTI',NBPARA,K8BID)
         CALL JEVEUO(PARA,'L',LPARA)
         WRITE(IUNI,'(10X,9A)') ' PARA=('
         WRITE(IUNI,'(4X,4(1PE13.5,1A))') (ZR(LPARA+I),',',I=0,NBPARA-1)
         WRITE(IUNI,'(10X,9A)') '      ),'
C
            WRITE(IUNI,'(10X,18A)') 'DEFI_FONCTION = ( '
         DO 120 IPARA= 1, NBPARA
            CALL JELIRA(JEXNUM(VALE,IPARA),'LONUTI',NBVAL,K8BID)
            NBVAL = NBVAL/2
            CALL JEVEUO(JEXNUM(VALE,IPARA),'L',LVAR)
            LFON = LVAR + NBVAL
C            WRITE(IUNI,'(10X,20A)') 'DEFI_FONCTION = _F( '
            WRITE(IUNI,'(11X,3A)') '_F( '
            PRGC   = ZK8(LPROL+5+2*IPARA)(1:1)
            PRDR   = ZK8(LPROL+5+2*IPARA)(2:2)
            IF (PRGC.EQ.'E') THEN
               WRITE(IUNI,'(14X,9A)') 'PROL_GAUCHE= ''EXCLU'','
            ELSEIF (PRGC.EQ.'C') THEN
               WRITE(IUNI,'(14X,9A)') 'PROL_GAUCHE= ''CONSTANT'','
            ELSEIF (PRGC.EQ.'L') THEN
               WRITE(IUNI,'(14X,9A)') 'PROL_GAUCHE= ''LINEAIRE'','
            ELSE
C     JE NE COMPRENDS DANS QUEL CAS ON PASSE ICI
               WRITE(IUNI,'(14X,9A)') 'PROL_GAUCHE= % ',PRGC
            ENDIF
            IF (PRDR.EQ.'E') THEN
               WRITE(IUNI,'(14X,9A)') 'PROL_DROITE= ''EXCLU'','
            ELSEIF (PRDR.EQ.'C') THEN
               WRITE(IUNI,'(14X,9A)') 'PROL_DROITE= ''CONSTANT'','
            ELSEIF (PRDR.EQ.'L') THEN
               WRITE(IUNI,'(14X,9A)') 'PROL_DROITE= ''LINEAIRE'','
            ELSE
C     JE NE COMPRENDS DANS QUEL CAS ON PASSE ICI
               WRITE(IUNI,'(14X,9A)') 'PROL_DROITE= % ',PRDR
            ENDIF
            WRITE(IUNI,'(14X,9A)') 'VALE= ('
            IF (MOD(NBVAL,2).EQ.0) THEN
               DO 121 IVAL= 0, NBVAL-1 , 2
                  WRITE(IUNI,'(4X,4(1PE13.5,1A))')
     +                  (ZR(LVAR+IVAL+J),',',ZR(LFON+IVAL+J),',',J=0,1)
 121           CONTINUE
            ELSE
               DO 122 IVAL= 0, NBVAL-2 , 2
                  WRITE(IUNI,'(4X,4(1PE13.5,1X))')
     +                  (ZR(LVAR+IVAL+J),',',ZR(LFON+IVAL+J),',',J=0,1)
 122           CONTINUE
               WRITE(IUNI,'(4X,4(1PE13.5,1X))')
     +                    ZR(LVAR+NBVAL-1),',',ZR(LFON+NBVAL-1),','
            ENDIF
C PARENTHESE FERMANTE DE VALE
            WRITE(IUNI,'(13X,3A)')   ' ),'
C PARENTHESE FERMANTE DU MCF
            WRITE(IUNI,'(11X,3A)') '  ), '
 120     CONTINUE
C PARENTHESE FERMANTE DU MCF DEFI_FONCTION
         WRITE(IUNI,'(11X,1A)') '),'
      ENDIF
      WRITE(IUNI,'(9X,1A)')  ')'
      CALL JEDEMA()
      END
