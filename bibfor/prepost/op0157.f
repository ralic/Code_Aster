      SUBROUTINE OP0157 ( IER )
      IMPLICIT REAL*8 (A-H,O-Z)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 14/05/2002   AUTEUR DURAND C.DURAND 
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
C     PROCEDURE IMPR_GENE
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
      CHARACTER*32      JEXNUM, JEXNOM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      CHARACTER*3  TOUCHA, TOUCMP, TOUPAR, INTERP
      CHARACTER*4  MOTFAC
      CHARACTER*8  K8B, FORM, FICH
      CHARACTER*16 NOMCMD, TYPCON, CRIT, K16BID
      CHARACTER*19 GENE, KNUM, KINST, KRANG
      CHARACTER*80 TITRE
      LOGICAL      LHIST
C     ------------------------------------------------------------------
      CALL JEMARQ()
      CALL GETRES(K8B,K8B,NOMCMD)
      MOTFAC = 'GENE'
      CALL GETFAC(MOTFAC,NOCC)
C
      DO 10 IOCC = 1,NOCC
C
C        --- FORMAT ---
C
         CALL GETVTX(MOTFAC,'FORMAT',IOCC,1,1,FORM,N)
C
C        --- FICHIER ---
C
         FICH = '        '
         CALL GETVTX(MOTFAC,'FICHIER',IOCC,1,1,FICH,N)
         IF ( FICH .EQ. '        ' ) FICH = FORM
         IFI = IUNIFI(FICH)
C
C        --- SEPARATION DES DIFFERENTES OCCURENCES---
C
         IF ( FORM .EQ. 'RESULTAT' ) WRITE(IFI,'(/,1X,80(''-''))')
C
         CALL GETVID(MOTFAC,'RESU_GENE',IOCC,1,1,GENE,NR)
         CALL GETTCO( GENE , TYPCON )
C
C        --- ECRITURE DU TITRE ---
C
         K8B = '        '
         CALL IRTITR(GENE,K8B,K8B,FORM,FICH,TITRE)
C
C        --- IMPRESSION DE LA STRUCTURE DU RESU_GENE ---
C
         IF (TYPCON.EQ.'MODE_GENE' .OR. TYPCON.EQ.'HARM_GENE') THEN
            CALL GETVTX(MOTFAC,'INFO_GENE',IOCC,1,1,K8B,N01)
            IF( K8B(1:3) .EQ. 'OUI' ) CALL RSINFO ( GENE , FICH )
         ENDIF
C
C        --- QUELS SONT LES NOM_CHAMP A IMPRIMER ---
C
         TOUCHA = 'OUI'
         CALL GETVTX(MOTFAC,'TOUT_CHAM',IOCC,1,1,TOUCHA,N21)
         CALL GETVTX(MOTFAC,'NOM_CHAM' ,IOCC,1,0,K16BID,N22)
         IF ( N22 .LT. 0 ) THEN
            NBNOSY = - N22
            CALL WKVECT('&&OP0157.NOM_SYMB','V V K16',NBNOSY,JNOSY)
            CALL GETVTX(MOTFAC,'NOM_CHAM',IOCC,1,NBNOSY,ZK16(JNOSY),N)
         ELSEIF ( TOUCHA .EQ. 'OUI' ) THEN
            IF (TYPCON.EQ.'MODE_GENE' .OR. TYPCON.EQ.'HARM_GENE') THEN
              CALL JELIRA(GENE//'.DESC','NOMUTI',NBNOSY,K8B)
              CALL WKVECT('&&OP0157.NOM_SYMB','V V K16',NBNOSY,JNOSY)
              DO 20 ISY = 1,NBNOSY
                CALL JENUNO(JEXNUM(GENE//'.DESC',ISY),ZK16(JNOSY-1+ISY))
 20           CONTINUE
            ELSE
              NBNOSY = 3
              CALL WKVECT('&&OP0157.NOM_SYMB','V V K16',NBNOSY,JNOSY)
              ZK16(JNOSY+1-1) = 'DEPL'
              ZK16(JNOSY+2-1) = 'VITE'
              ZK16(JNOSY+3-1) = 'ACCE'
            ENDIF
         ELSEIF ( TOUCHA .EQ. 'NON' ) THEN
            NBNOSY = 0
            JNOSY  = 1
         ENDIF
C
C        --- QUELS SONT LES CMP_GENE A IMPRIMER ---
C
         NBCMPG = -1
         JCMPG  =  1
         TOUCMP = '   '
         CALL GETVTX(MOTFAC,'TOUT_CMP_GENE',IOCC,1,1,TOUCMP,N21)
         CALL GETVIS(MOTFAC,'NUME_CMP_GENE',IOCC,1,0,IBID  ,N22)
         IF ( TOUCMP .EQ. 'NON' ) THEN
            NBCMPG = 0
         ELSEIF ( N22 .LT. 0 ) THEN
            NBCMPG = -N22
            CALL WKVECT('&&OP0157.NOM_CMPG','V V I',NBCMPG,JCMPG)
           CALL GETVIS(MOTFAC,'NUME_CMP_GENE',IOCC,1,NBCMPG,ZI(JCMPG),N)
         ENDIF
C
C        --- ON RECHERCHE LES PARAMETRES A ECRIRE ---
C
         NBPARA = -1
         JPARA  =  1
         TOUPAR = '   '
         CALL GETVTX(MOTFAC,'TOUT_PARA',IOCC,1,1,TOUPAR,N11)
         CALL GETVTX(MOTFAC,'NOM_PARA' ,IOCC,1,0,K8B   ,N10)
         IF ( TOUPAR .EQ. 'NON' ) THEN
            NBPARA = 0
         ELSEIF ( N10 .NE. 0 ) THEN
            NBPARA = -N10
            CALL WKVECT('&&OP0157.NOMUTI_PARA','V V K16',NBPARA,JPARA)
            CALL GETVTX(MOTFAC,'NOM_PARA',IOCC,1,NBPARA,ZK16(JPARA),N)
         ENDIF
C
C        --- LES ACCES ---
C
         NBORDR = 0
         JORDR  = 1
         NBINST = 0
         JINST  = 1
         JRANG  = 1
         IF ( TYPCON.EQ.'MODE_GENE' .OR. TYPCON.EQ.'HARM_GENE' ) THEN
            KNUM = '&&OP0157.NUME_ORDRE'
            CALL GETVR8(MOTFAC,'PRECISION',IOCC,1,1,PREC,NP)
            CALL GETVTX(MOTFAC,'CRITERE'  ,IOCC,1,1,CRIT,NC)
            CALL RSUTNU(GENE,MOTFAC,IOCC,KNUM,NBORDR,PREC,CRIT,IRET)
            IF (IRET.NE.0) GOTO 12
            CALL JEVEUO ( KNUM, 'L', JORDR )
         ELSEIF ( TYPCON.EQ.'TRAN_GENE' ) THEN
            KINST = '&&OP0157.INSTANT'
            KRANG = '&&OP0157.NUME_ORDRE'
            INTERP = 'NON'
            CALL RSTRAN(INTERP,GENE,MOTFAC,IOCC,KINST,KRANG,NBINST,IRET)
            IF (IRET.NE.0) GOTO 12
            CALL JEEXIN(KINST,IRE2)
            IF (IRE2.GT.0) THEN
              CALL JEVEUO(KINST,'E',JINST)
              CALL JEVEUO(KRANG,'E',JRANG)
            END IF
         ENDIF
C
C        --- HISTOR ---
C
         LHIST = .TRUE.
         CALL GETVTX(MOTFAC,'INFO_CMP_GENE',IOCC,1,1,K8B,N)
         IF ( K8B(1:3) .EQ. 'NON' ) LHIST = .FALSE.
C
         CALL IRGENE (IOCC, GENE, FORM,FICH, TITRE, NBNOSY,ZK16(JNOSY),
     +                NBCMPG,ZI(JCMPG), NBPARA,ZK16(JPARA),
     +            NBORDR,ZI(JORDR), NBINST,ZR(JINST),ZI(JRANG), LHIST )
C
 12      CONTINUE
         CALL JEDETR ( '&&OP0157.NOM_SYMB')
         CALL JEDETR ( '&&OP0157.NOM_CMPG')
         CALL JEDETR ( '&&OP0157.NOMUTI_PARA')
         CALL JEDETR ( '&&OP0157.NUME_ORDRE')
         CALL JEDETR ( '&&OP0157.INSTANT')
 10   CONTINUE
C
      CALL JEDEMA()
      END
