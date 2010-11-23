      SUBROUTINE ECRESU(RESIN,VECTOT,NBVA,GRAND,RESOU,IER)
      IMPLICIT NONE
      INTEGER NPARA,NBVA
      CHARACTER*(*) RESIN,RESOU,GRAND
      CHARACTER*19 VECTOT
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 22/11/2010   AUTEUR GREFFET N.GREFFET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     REALISATION N.GREFFET
C     OPERATEUR "ECRIRE RESULTAT"
C     IN:
C       RESIN : SD_RESULTAT INITIALE HARMONIQUE
C               (VENANT DE DYNA_LINE_HARM)
C       NPARA : POINTEUR DU TABLEAU DE DONNEE VENANT DE LA FFT
C       NBVA  : NOMBRE D'INSTANTS
C       GRAND : GRANDEUR PHYSIQUE (DEPL, VITE, ACCE)
C
C     OUT:
C       RESOU   : SD_RESULTAT FINALE TRANSITOIRE
C
C
C
C
C
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
      INTEGER      NBORDR,LTPS,JORDR,IBID,I,LFREQ,NBNOEU,NBCMP,LBIG,II
      INTEGER      LTPS2,IEQ,IER,NEQ,LVAL,LVALS,JDEP,JREFE,IRET,NBVA2
      INTEGER      NBSAUV,IARCHI,ISTO1,ISTO2,ISTO3,JDEPS,JVITS,JACCS
      INTEGER      JPASS,JORD,JINST,JFCHO,JDCHO,JVCHO,JICHO,JREDC,JREDD
      INTEGER      IRES,N1,JDESC,NBMODE,LVALV,LVALA,J,LV1,LV2,LV3
      INTEGER      JREFAM,JVINT
      REAL*8       R8B,R1,RBID
      REAL*8       DT
      COMPLEX*16   C16B
      LOGICAL      LPSTO
      CHARACTER*1  K1B,KTYP
      CHARACTER*4  GRANDE
      CHARACTER*8  K8B
      CHARACTER*8  MASGEN,RIGGEN,AMOGEN,BASEMO
      CHARACTER*16 K16B,TYPOUT
      CHARACTER*19 K19B,CHDEP,CHDEPS,KREFE
      CHARACTER*24 TYPRES,CHDEP2,REFE
      CHARACTER*24 RAIDE
C
      DATA  REFE  /'                   .REFD'/
C     ------------------------------------------------------------------
      CALL JEMARQ()
      GRANDE = GRAND
      CALL JEVEUO(VECTOT,'L',NPARA)
      IER = 0
C   Recuperation type RESU
      CALL GETTCO(RESIN,TYPRES)
      IF (TYPRES(1:10).EQ.'DYNA_HARMO') THEN
         TYPOUT='DYNA_TRANS'
         NBVA2=NBVA
      ELSEIF (TYPRES(1:9).EQ.'HARM_GENE') THEN
         TYPOUT='TRAN_GENE'
         NBVA2=NBVA
      ELSEIF (TYPRES(1:10).EQ.'DYNA_TRANS') THEN
         TYPOUT='DYNA_HARMO'
         NBVA2=2*NBVA
      ELSEIF (TYPRES(1:9).EQ.'TRAN_GENE') THEN
         TYPOUT='HARM_GENE'
         NBVA2=2*NBVA
         CALL JEVEUO(RESIN(1:8)//'           .REFD','L',JREFE)
         RIGGEN = ZK24(JREFE)
         MASGEN = ZK24(JREFE+1)
         AMOGEN = ZK24(JREFE+2)
      ENDIF
C
C  Creation objet de stockage en LTPS pour les valeurs d'instants
C
C  Champs
      IF ( TYPRES(1:9).NE.'TRAN_GENE') THEN
         CALL RSEXCH(RESIN,GRANDE,1,CHDEP,IRET)
         CALL JEVEUO(CHDEP//'.VALE','L',LVAL)
C  Nombre d'equations : NEQ
         CHDEP2 = CHDEP(1:19)//'.VALE'
         CALL JELIRA(CHDEP2,'LONMAX',NEQ,K1B)
         NBORDR = NBVA
      ELSE
         CALL JELIRA(RESIN(1:19)//'.ORDR','LONUTI',NBORDR,K1B)
         CALL JEVEUO(RESIN(1:19)//'.'//GRANDE ,'L',LVAL)
         CHDEP2 = RESIN(1:19)//'.'//GRANDE
         CALL JELIRA(CHDEP2,'LONMAX',NEQ,K1B)
         NEQ = NEQ / NBORDR         
      ENDIF
      CALL WKVECT('&&ECRESU.PARAMACC','V V R',NBVA,LTPS)
C
C  Creation objet resultat en sortie si non existence
C
C      NBORDR = NBVA
      CALL JEEXIN ( RESOU(1:8)//'           .DESC', IRES )
      IF ( (IRES.EQ.0).AND.(TYPOUT(1:9).NE.'TRAN_GENE')) 
     &     CALL RSCRSD('G',RESOU,TYPOUT,NBORDR)
C
      REFE(1:8) = RESIN
      CALL JEVEUO(REFE, 'L', JREFE )
      RAIDE = ZK24(JREFE)

C
      IF ( (TYPOUT(1:10).EQ.'DYNA_HARMO').OR.
     &     (TYPOUT(1:9).EQ.'HARM_GENE') ) THEN
         DO  10 I = 1,NBVA
            ZR(LTPS+I-1) =  DBLE(ZC(NPARA+(NEQ*NBVA)+I-1))
 10      CONTINUE
         DO 20 I = 1,NBORDR
C  Temps
            CALL RSADPA(RESOU,'E',1,'FREQ',I,0,LTPS2,K8B)
            ZR(LTPS2) = ZR(LTPS+I-1)
            CALL RSEXCH (RESOU,GRANDE,I,CHDEPS,IRET)
            IF (TYPOUT(1:10).EQ.'DYNA_HARMO') THEN
              IF (RAIDE(1:1).NE.' ') THEN
                CALL VTCREM (CHDEPS, RAIDE, 'G', 'C' )
              ELSE
                CALL VTCREB(CHDEPS,ZK24(JREFE+3),'G','C',N1)
                CALL ASSERT(N1.EQ.NEQ)
              ENDIF
            ELSE
               CALL VTCREM (CHDEPS, MASGEN, 'G', 'C' )
               CALL JEECRA(CHDEPS//'.DESC','DOCU',0,'VGEN')
            ENDIF
            CALL JEVEUO(CHDEPS//'.VALE', 'E', LVALS )
            DO 30 IEQ = 1,NEQ
               ZC(LVALS+IEQ-1) = ZC(NPARA+NBVA*(IEQ-1)+I-1)
 30         CONTINUE
            CALL RSNOCH(RESOU,GRANDE,I,' ')
 20      CONTINUE
      ELSEIF ( TYPOUT(1:10).EQ.'DYNA_TRANS' ) THEN
         DO  100 I = 1,NBVA
            ZR(LTPS+I-1) =  ZR(NPARA+(NEQ*NBVA2)+I-1)
 100     CONTINUE
         DO 200 I = 1,NBORDR
C  Temps
            CALL RSADPA(RESOU,'E',1,'INST',(I-1),0,LTPS2,K8B)
            ZR(LTPS2) = ZR(LTPS+I-1)
            CALL RSEXCH (RESOU,GRANDE,(I-1),CHDEPS,IRET)
            IF (RAIDE(1:1).NE.' ') THEN
              CALL VTCREM (CHDEPS, RAIDE, 'G', 'R' )
            ELSE
              CALL VTCREB(CHDEPS,ZK24(JREFE+3),'G','R',N1)
              CALL ASSERT(N1.EQ.NEQ)
            ENDIF
            
            CALL JEVEUO(CHDEPS//'.VALE', 'E', LVALS )
            CALL JELIRA(CHDEPS//'.VALE', 'LONMAX', N1,K1B )
            CALL ASSERT(N1.EQ.NEQ)
            CALL JELIRA(CHDEPS//'.VALE', 'TYPE', IBID,KTYP)
            CALL ASSERT(KTYP.EQ.'R')
            DO 300 IEQ = 1,NEQ
               R1 = ZR(NPARA+NBVA*(IEQ-1)+I-1)
               ZR(LVALS+IEQ-1) = R1
 300        CONTINUE
            CALL RSNOCH(RESOU,GRANDE,(I-1),' ')
 200     CONTINUE
      ELSEIF ( TYPOUT(1:9).EQ.'TRAN_GENE' ) THEN
         IF ( IRES .EQ. 0 ) THEN
            DO  400 I = 1,NBVA
               ZR(LTPS+I-1) =  ZR(NPARA+(NEQ*NBVA2)+I-1)
 400        CONTINUE
            ISTO1 = 0
            ISTO2 = 0
            ISTO3 = 0
            LPSTO = .FALSE.
            JVINT = 1
 
C         CALL MDGENE(BASEMO,NBMODE,K14B,MASGEN,RIGGEN,AMOGEN,NEXCIT,
C     &            JVEC,IRET)
C         IF (IRET.NE.0) CALL U2MESS('F','ALGORITH5_24')
         
            RIGGEN = ZK24(JREFE)(1:8)
            MASGEN = ZK24(JREFE+1)(1:8)
            AMOGEN = ZK24(JREFE+2)(1:8)
            NBSAUV = NBORDR
            DT = ZR(LTPS+1) - ZR(LTPS)
            CALL JEVEUO(MASGEN(1:8)//'           .DESC','L',JDESC)
            NBMODE = ZI(JDESC+1)
            CALL JEVEUO(MASGEN(1:8)//'           .REFA','L',JREFAM)
            BASEMO = ZK24(JREFAM-1+1)(1:8)
C Bizarre !!!!
C            NBSAUV = NBSAUV + 1

         
            K8B = '        '
            CALL MDALLO(RESOU(1:8),BASEMO,MASGEN,RIGGEN,AMOGEN,NBMODE,
     &            DT,NBSAUV,0,K8B,K8B,0,K8B,0,
     &            JDEPS,JVITS,JACCS,JPASS,JORDR,JINST,JFCHO,JDCHO,JVCHO,
     &            JICHO,JREDC,JREDD,.FALSE.,'EULER   ')
            CALL WKVECT('&&ECRESU.DEPL','V V R',NEQ,LVALS)
            CALL WKVECT('&&ECRESU.VITE','V V R',NEQ,LVALV)
            CALL WKVECT('&&ECRESU.ACCE','V V R',NEQ,LVALA)
            LV1 = LVALS
            LV2 = LVALV
            LV3 = LVALA
            IF (GRANDE.EQ.'VITE') THEN
               LV1 = LVALV
               LV2 = LVALS
            ELSEIF (GRANDE.EQ.'ACCE') THEN
               LV1 = LVALA
               LV3 = LVALS
            ENDIF
         
C            DO 500 I = 1,NBORDR+1
            DO 500 I = 1,NBORDR
               J = I - 1
               IARCHI = J
               ISTO1 = J
               DO 600 IEQ = 1,NEQ
                  R1 = ZR(NPARA+NBVA*(IEQ-1)+I-1)
                  ZR(LV1+IEQ-1) = R1
                  ZR(LV2+IEQ-1)=0.D0
                  ZR(LV3+IEQ-1)=0.D0
 600           CONTINUE
           
               CALL MDARCH(ISTO1,IARCHI,ZR(LTPS+I-1),DT,NEQ,ZR(LVALS),
     &            ZR(LVALV),ZR(LVALA),
     &            ISTO2,0,0.D0,0, ISTO3,0,0.D0,
     &            0, ZR(JDEPS),ZR(JVITS),ZR(JACCS),RBID,LPSTO,ZI(JORDR),
     &            ZR(JINST),ZR(JFCHO),ZR(JDCHO),ZR(JVCHO),ZI(JICHO),
     &            ZR(JVINT),ZI(JREDC),ZR(JREDD))
 500        CONTINUE
         ELSE
            CALL JEVEUO(RESOU(1:8)//'           .'//GRANDE ,'E',LVALS)
            CALL JELIRA(RESOU(1:8)//'           .'//GRANDE,'LONMAX',
     &              IBID,K1B)
            DO 700 I = 1,NBORDR
               J = I - 1
               IARCHI = J
               ISTO1 = J
               DO 800 IEQ = 1,NEQ
                  R1 = ZR(NPARA+NBVA*(IEQ-1)+I-1)
                  ZR(LVALS+(NEQ*ISTO1)+IEQ-1) = R1
 800           CONTINUE
 700        CONTINUE
         ENDIF
      ENDIF
C  On finalise le RESOU
      IF ( (IRES.EQ.0) .AND. (TYPOUT(1:9).NE.'TRAN_GENE') ) THEN
         KREFE = RESOU(1:8)
         CALL WKVECT(KREFE//'.REFD','G V K24',7,JORDR)
         ZK24(JORDR) = ZK24(JREFE)
         ZK24(JORDR+1) = ZK24(JREFE+1)
         ZK24(JORDR+2) = ZK24(JREFE+2)
         ZK24(JORDR+3) = ZK24(JREFE+3)
         ZK24(JORDR+4) = ZK24(JREFE+4)
         ZK24(JORDR+5) = ZK24(JREFE+5)
         ZK24(JORDR+6) = ZK24(JREFE+6)
         CALL JELIBE(KREFE//'.REFD')
      ENDIF

      CALL JEDETR('&&ECRESU.PARAMACC')
      CALL JEDEMA()
      END
