      SUBROUTINE OP0036 ()
      IMPLICIT   NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 25/06/2012   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ----- OPERATEUR CREA_TABLE              --------------------------
      INCLUDE 'jeveux.h'
      INTEGER      IOCC,IBID,NI,NR,NK,I,J,IR,JVALE,JP,NDIM,JT
      INTEGER      NOCC,NOCC2,NINDI,III,DIMMAX,JY,JLNG,JPROL,JD
      INTEGER      JTRAV1,JTRAV2,JTRAV3,JTRAV4,JTRAV5,LXLGUT,NPAR
      INTEGER      LONGCO,NOCC3,IARG
      REAL*8       RBID
      COMPLEX*16   CBID
      CHARACTER*1  KBID
      CHARACTER*3  NTYP
      CHARACTER*8  RESU,TYPARR(2),TYPARC(3)
      CHARACTER*16 CONCEP,NOMCMD,NMPAR,NMPAR1,NMPARF(2),NMPARC(3)
      CHARACTER*19 NFCT
      CHARACTER*24 TRAV,LDBL,INDIC,LTYP,WORK
      CHARACTER*24 VECTCR,VECTCI
      INTEGER      IVCR,IVCI
      DATA TYPARR / 'R'       , 'R'       /
      DATA TYPARC / 'R'       , 'R'       , 'R'       /

C     ------------------------------------------------------------------

      CALL JEMARQ()

      CALL GETRES(RESU,CONCEP,NOMCMD)
      CALL GETFAC('LISTE',NOCC)
      CALL GETFAC('FONCTION',NOCC2)
      CALL GETFAC('RESU',NOCC3)

      INDIC  ='&&OP0036.IND'
      TRAV   ='&&OP0036.VAL'
      LDBL   ='&&OP0036.DBL'
      LTYP   ='&&OP0036.TYP'
      WORK   ='&&OP0036.WOR'

C
C     ==========
C --- CAS: LISTE
C     ==========
      IF(NOCC.NE.0)THEN
         CALL WKVECT(WORK,'V V I'  ,NOCC,JLNG)
         CALL WKVECT(LDBL,'V V K16',NOCC,JD)
         CALL WKVECT(LTYP,'V V K8' ,NOCC,JY)
         DIMMAX=0

         DO 50 IOCC=1,NOCC
            CALL GETVTX('LISTE','PARA',IOCC,IARG,1,NMPAR,JP)
            ZK16(JD+IOCC-1)=NMPAR
            CALL GETVIS('LISTE','LISTE_I',  IOCC,IARG,0,IBID,NI)
            CALL GETVIS('LISTE','NUME_LIGN',IOCC,IARG,0,IBID,NINDI)
            CALL GETVR8('LISTE','LISTE_R',  IOCC,IARG,0,RBID,NR)
            CALL GETVTX('LISTE','LISTE_K',  IOCC,IARG,0,KBID,NK)
            CALL GETVTX('LISTE','TYPE_K',   IOCC,IARG,1,NTYP,JT)

            IF ( NINDI.NE.0 ) THEN
              IF ( (-NI-NR-NK).NE.(-NINDI)) THEN
               CALL U2MESS('F','UTILITAI2_75')
              ENDIF
              CALL WKVECT(INDIC,'V V I',-NINDI,III)
              LONGCO=0
              CALL GETVIS('LISTE','NUME_LIGN',IOCC,IARG,-NINDI,
     &                    ZI(III),IR)
              DO 55 I=1,-NINDI
                 LONGCO=MAX(LONGCO,ZI(III+I-1))
 55           CONTINUE
              CALL JEDETR(INDIC)
              ZI(JLNG+IOCC-1)=LONGCO
            ELSE
              ZI(JLNG+IOCC-1)=-NI-NR-NK
            ENDIF
            DIMMAX=MAX(DIMMAX,ZI(JLNG+IOCC-1))

            IF ( NI.NE.0 ) THEN
               ZK8(JY+IOCC-1)='I'
            ELSE IF (NR.NE.0) THEN
               ZK8(JY+IOCC-1)='R'
            ELSE IF (NK.NE.0) THEN
               IF (NTYP(2:2).EQ.'8') THEN
                  ZK8(JY+IOCC-1)='K8'
               ELSE IF (NTYP(2:2).EQ.'1') THEN
                  ZK8(JY+IOCC-1)='K16'
               ELSE IF (NTYP(2:2).EQ.'2') THEN
                  ZK8(JY+IOCC-1)='K24'
               ENDIF
            ENDIF

 50      CONTINUE

C       ---CREATION DE LA TABLE
         CALL TBCRSV(RESU,'G',NOCC,ZK16(JD),ZK8(JY),DIMMAX)
C
         DO 200 IOCC=1,NOCC
            CALL GETVIS('LISTE','LISTE_I',IOCC,IARG,0,IBID,NI)
            CALL GETVIS('LISTE','NUME_LIGN',IOCC,IARG,0,IBID,NINDI)
            CALL GETVR8('LISTE','LISTE_R',IOCC,IARG,0,RBID,NR)
            CALL GETVTX('LISTE','LISTE_K',IOCC,IARG,0,KBID,NK)
            CALL GETVTX('LISTE','PARA',IOCC,IARG,1,NMPAR,JP)
            DO 150 J=1,NOCC
               NMPAR1=ZK16(JD+J-1)
               IF ((NMPAR.EQ.NMPAR1).AND.(J.NE.IOCC)) THEN
                  CALL U2MESS('F','UTILITAI2_76')
               ENDIF
 150        CONTINUE
C
            IF (NINDI.NE.0)THEN
               NINDI=-NINDI
               CALL WKVECT(INDIC,'V V I',NINDI,III)
               CALL GETVIS('LISTE','NUME_LIGN',IOCC,IARG,NINDI,
     &                     ZI(III),IR)
            ELSE
               CALL WKVECT(INDIC,'V V I',(-NI-NR-NK),III)
               DO 175 I=1,(-NI-NR-NK)
                    ZI(III+I-1)=I
 175           CONTINUE
            ENDIF

C           LISTE D'ENTIERS :
C           ---------------
            IF (NI.NE.0)THEN
               NI=-NI
               CALL WKVECT(TRAV,'V V I',NI,JTRAV1)
               CALL GETVIS('LISTE','LISTE_I',IOCC,IARG,NI,ZI(JTRAV1),IR)
               CALL TBAJCO(RESU,NMPAR,'I',NI,ZI(JTRAV1),
     &                     RBID,CBID,KBID,'R',ZI(III))
            ENDIF

C           LISTE DE REELS :
C           --------------
            IF (NR.NE.0)THEN
               NR=-NR
               CALL WKVECT(TRAV,'V V R',NR,JTRAV2)
               CALL GETVR8('LISTE','LISTE_R',IOCC,IARG,NR,ZR(JTRAV2),IR)
               CALL TBAJCO(RESU,NMPAR,'R',NR,IBID,ZR(JTRAV2),
     &                     CBID,KBID,'R',ZI(III))
            ENDIF

C           LISTE DE CHAINE DE CARACTERES :
C           -----------------------------
            IF (NK.NE.0)THEN
               NK=-NK
               CALL GETVTX('LISTE','TYPE_K',IOCC,IARG,1,NTYP,JT)
C              CHAINES DE 8 CARACTERES
               IF(NTYP(2:2).EQ.'8')THEN
                  CALL WKVECT(TRAV,'V V K8',NK,JTRAV3)
                 CALL GETVTX('LISTE','LISTE_K',IOCC,IARG,NK,
     &                       ZK8(JTRAV3),IR)
                  CALL TBAJCO(RESU,NMPAR,'K8',NK,IBID,RBID,CBID,
     &                        ZK8(JTRAV3),'R',ZI(III))

C              CHAINES DE 16 CARACTERES
               ELSEIF(NTYP(2:2).EQ.'1')THEN
                  CALL WKVECT(TRAV,'V V K16',NK,JTRAV4)
                CALL GETVTX('LISTE','LISTE_K',IOCC,IARG,NK,
     &                      ZK16(JTRAV4),IR)
                  CALL TBAJCO(RESU,NMPAR,'K16',NK,IBID,RBID,CBID,
     &                        ZK16(JTRAV4),'R',ZI(III))

C              CHAINES DE 24 CARACTERES
               ELSEIF(NTYP(2:2).EQ.'2')THEN
                  CALL WKVECT(TRAV,'V V K24',NK,JTRAV5)
                CALL GETVTX('LISTE','LISTE_K',IOCC,IARG,NK,
     &                      ZK24(JTRAV5),IR)
                  CALL TBAJCO(RESU,NMPAR,'K24',NK,IBID,RBID,CBID,
     &                        ZK24(JTRAV5),'R',ZI(III))
               ENDIF
            ENDIF
            CALL JEDETR(TRAV)
            CALL JEDETR(INDIC)
 200     CONTINUE

C     ==============
C --- CAS : FONCTION
C     ==============
      ELSEIF(NOCC2.NE.0)THEN
          CALL GETVID('FONCTION','FONCTION',1,IARG,1,NFCT,IR)

C
          CALL JELIRA(NFCT//'.VALE','LONMAX',NDIM,KBID)
          CALL JEVEUO(NFCT//'.VALE','L',JVALE)
          CALL JEVEUO(NFCT//'.PROL','L',JPROL)
C
          IF(ZK24(JPROL).NE.'FONCTION' .AND.
     &      ZK24(JPROL).NE.'CONSTANT'.AND.
     &      ZK24(JPROL).NE.'FONCT_C')
     &      CALL U2MESK('F','UTILITAI2_78',1,NOMCMD)
          CALL GETVTX('FONCTION','PARA',1,IARG,2,NMPARF,IR)
          IF(IR.EQ.0)THEN
            NMPARF(1)=ZK24(JPROL+2)(1:16)
            NMPARF(2)=ZK24(JPROL+3)(1:16)
          ENDIF
          IF(NMPARF(1).EQ.NMPARF(2)) CALL U2MESS('F','UTILITAI2_79')
C
C       ---CAS CREATION D UNE NOUVELLE TABLE
C       ---
          IF (ZK24(JPROL).EQ.'FONCT_C') THEN
            NMPARC(1)=NMPARF(1)
            NPAR     =LXLGUT(NMPARF(2))
            NMPARC(2)=NMPARF(2)(1:NPAR)//'_R'
            NMPARC(3)=NMPARF(2)(1:NPAR)//'_I'
C
            CALL TBCRSV(RESU,'G',3,NMPARC,TYPARC,NDIM/3)
            CALL TBAJPA(RESU,3,NMPARC,TYPARC)
            VECTCR='&&OP0036.VCR'
            VECTCI='&&OP0036.VCI'
            CALL WKVECT(VECTCR,'V V R',NDIM/3,IVCR)
            CALL WKVECT(VECTCI,'V V R',NDIM/3,IVCI)
            DO 301 I=1,NDIM/3
               ZR(IVCR+I-1)= ZR(JVALE-1+NDIM/3+2*I-1)
               ZR(IVCI+I-1)= ZR(JVALE-1+NDIM/3+2*I)
 301        CONTINUE
            CALL TBAJCO(RESU,NMPARC(1),'R',NDIM/3,IBID,
     &                  ZR(JVALE),CBID,KBID,'R',-1)
            CALL TBAJCO(RESU,NMPARC(2),'R',NDIM/3,IBID,
     &                  ZR(IVCR),CBID,KBID,'R',-1)
            CALL TBAJCO(RESU,NMPARC(3),'R',NDIM/3,IBID,
     &                  ZR(IVCI),CBID,KBID,'R',-1)
            CALL JEDETR ( VECTCR )
            CALL JEDETR ( VECTCI )
          ELSE
            CALL TBCRSV(RESU,'G',2,NMPARF,TYPARR,NDIM/2)
            CALL TBAJPA(RESU,2,NMPARF,TYPARR)
            CALL TBAJCO(RESU,NMPARF(1),'R',NDIM/2,IBID,
     &                  ZR(JVALE)       ,CBID,KBID,'R',-1)
            CALL TBAJCO(RESU,NMPARF(2),'R',NDIM/2,IBID,
     &                  ZR(JVALE+NDIM/2),CBID,KBID,'R',-1)
          ENDIF
C
C     ==============
C --- CAS : RESU
C     ==============
      ELSEIF(NOCC3.NE.0)THEN
C
          CALL CTRESU(RESU)
C
      ENDIF


      CALL TITRE
      CALL JEDEMA()

      END
