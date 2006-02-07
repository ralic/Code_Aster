      SUBROUTINE OP0036 ( IER )
      IMPLICIT   NONE
      INTEGER             IER

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 07/02/2006   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      INTEGER      IOCC,JF,IBID,NI,NR,NK,I,J,IR,JVALE,JP,NDIM,NDIM1,JT
      INTEGER      NOCC,IL,NOCC2,NINDI,III,DIMMAX,JY,JLNG,JPROL,JD
      INTEGER      NBLIGN,JTRAV1,JTRAV2,JTRAV3,JTRAV4,JTRAV5
      REAL*8       RBID,VR(2)
      COMPLEX*16   CBID,VC(2)
      CHARACTER*1  KBID
      CHARACTER*3  NTYP
      CHARACTER*8  RESULT,TYPARR(2),TYPARC(2)
      CHARACTER*16 CONCEP,NOMCMD,NMPAR,NMPAR1,NMPARF(2)
      CHARACTER*19 NFCT
      CHARACTER*24 TRAV,LDBL,INDIC,LTYP,WORK
C SENSIBILITE
      INTEGER      NRPASS,NBPASS,ADRECG,IAUX,IRET
      CHARACTER*19 LATAB1,NOPASE,LERES0
      CHARACTER*24 NORECG
      CHARACTER*38 TITRES
C SENSIBILITE
      DATA TYPARR / 'R'       , 'R'       /   
      DATA TYPARC / 'R'       , 'C'       /   
C     ------------------------------------------------------------------

      CALL JEMARQ()

      IER=0

      CALL GETRES(RESULT,CONCEP,NOMCMD)
      CALL GETFAC('LISTE',NOCC)
      CALL GETFAC('FONCTION',NOCC2)
      INDIC='&&OP0036.IND'
      TRAV='&&OP0036.VAL'
      LDBL='&&OP0036.DBL'
      LTYP='&&OP0036.TYP'
      WORK='&&OP0036.WOR'
C
      NORECG = '&&'//'OP0135'//'_RESULTA_GD     '
C
C
C     ==========
C --- CAS: LISTE 
C     ==========

      IF(NOCC.NE.0)THEN

         CALL WKVECT(WORK,'V V I'  ,NOCC,JLNG)
         CALL WKVECT(LDBL,'V V K16',NOCC,JD)
         CALL WKVECT(LTYP,'V V K8' ,NOCC,JY)
         DIMMAX=0
         NBLIGN=0
         
         DO 50 IOCC=1,NOCC
            CALL GETVID('LISTE','PARA',IOCC,1,1,NMPAR,JP)
            ZK16(JD+IOCC-1)=NMPAR
            CALL GETVIS('LISTE','LISTE_I',  IOCC,1,0,IBID,NI)
            CALL GETVIS('LISTE','NUME_LIGN',IOCC,1,0,IBID,NINDI)
            CALL GETVR8('LISTE','LISTE_R',  IOCC,1,0,RBID,NR)
            CALL GETVTX('LISTE','LISTE_K',  IOCC,1,0,KBID,NK)
            CALL GETVTX('LISTE','TYPE_K',   IOCC,1,1,NTYP,JT)
            
            IF ( NINDI.NE.0 ) THEN
              IF ( (-NI-NR-NK).NE.(-NINDI)) THEN
               CALL UTMESS('F','OP0036','LES LISTES NUME_LIGN'//
     &         ' ET LISTE_X DOIVENT CONTENIR LE MEME NOMBRE DE TERMES')
              ENDIF
              CALL WKVECT(INDIC,'V V I',-NINDI,III)
              DIMMAX=0
              CALL GETVIS('LISTE','NUME_LIGN',IOCC,1,-NINDI,ZI(III),IR)
              DO 55 I=1,-NINDI
                 DIMMAX=MAX(DIMMAX,ZI(III+I-1))
 55           CONTINUE
              CALL JEDETR(INDIC)
              ZI(JLNG+IOCC-1)=DIMMAX
            ELSE
              ZI(JLNG+IOCC-1)=-NI-NR-NK
            ENDIF

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

C       ---DIMENSIONNEMENT DE LA TABLE

         NBLIGN=0
         DO 70 I=1,NOCC
            NBLIGN=MAX(NBLIGN,ZI(JLNG+I-1))
 70      CONTINUE
         
C        ---CREATION DE LA TABLE

         CALL TBCRSV(RESULT,'G',NOCC,ZK16(JD),ZK8(JY),NBLIGN)


         DO 200 IOCC=1,NOCC
            CALL GETVIS('LISTE','LISTE_I',IOCC,1,0,IBID,NI)
            CALL GETVIS('LISTE','NUME_LIGN',IOCC,1,0,IBID,NINDI)
            CALL GETVR8('LISTE','LISTE_R',IOCC,1,0,RBID,NR)
            CALL GETVTX('LISTE','LISTE_K',IOCC,1,0,KBID,NK)
            CALL GETVID('LISTE','PARA',IOCC,1,1,NMPAR,JP)
            DO 150 J=1,NOCC
               NMPAR1=ZK16(JD+J-1)
               IF ((NMPAR.EQ.NMPAR1).AND.(J.NE.IOCC)) THEN 
                  CALL UTMESS('F','OP0036','LES NOMS'//
     &                 ' DES PARAMETRES DOIVENT ETRE DIFFERENTS')
               ENDIF
 150        CONTINUE       

            IF (NINDI.NE.0)THEN
               NINDI=-NINDI
               CALL WKVECT(INDIC,'V V I',NINDI,III)
               CALL GETVIS('LISTE','NUME_LIGN',IOCC,1,NINDI,ZI(III),IR)
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
               CALL GETVIS('LISTE','LISTE_I',IOCC,1,NI,ZI(JTRAV1),IR)
               CALL TBAJCO(RESULT,NMPAR,'I',NI,ZI(JTRAV1),
     &                     RBID,CBID,KBID,'R',ZI(III))
            ENDIF

C           LISTE DE REELS :
C           --------------
            IF (NR.NE.0)THEN
               NR=-NR
               CALL WKVECT(TRAV,'V V R',NR,JTRAV2)
               CALL GETVR8('LISTE','LISTE_R',IOCC,1,NR,ZR(JTRAV2),IR)
               CALL TBAJCO(RESULT,NMPAR,'R',NR,IBID,ZR(JTRAV2),
     &                     CBID,KBID,'R',ZI(III))
            ENDIF

C           LISTE DE CHAINE DE CARACTERES :
C           -----------------------------
            IF (NK.NE.0)THEN
               NK=-NK
               CALL GETVTX('LISTE','TYPE_K',IOCC,1,1,NTYP,JT)
C              CHAINES DE 8 CARACTERES
               IF(NTYP(2:2).EQ.'8')THEN
                  CALL WKVECT(TRAV,'V V K8',NK,JTRAV3)
                 CALL GETVTX('LISTE','LISTE_K',IOCC,1,NK,ZK8(JTRAV3),IR)
                  CALL TBAJCO(RESULT,NMPAR,'K8',NK,IBID,RBID,CBID,
     &                        ZK8(JTRAV3),'R',ZI(III))

C              CHAINES DE 16 CARACTERES
               ELSEIF(NTYP(2:2).EQ.'1')THEN
                  CALL WKVECT(TRAV,'V V K16',NK,JTRAV4)
                CALL GETVTX('LISTE','LISTE_K',IOCC,1,NK,ZK16(JTRAV4),IR)
                  CALL TBAJCO(RESULT,NMPAR,'K16',NK,IBID,RBID,CBID,
     &                        ZK16(JTRAV4),'R',ZI(III))

C              CHAINES DE 24 CARACTERES
               ELSEIF(NTYP(2:2).EQ.'2')THEN
                  CALL WKVECT(TRAV,'V V K24',NK,JTRAV5)
                CALL GETVTX('LISTE','LISTE_K',IOCC,1,NK,ZK24(JTRAV5),IR)
                  CALL TBAJCO(RESULT,NMPAR,'K24',NK,IBID,RBID,CBID,
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
C        --- NOMBRE DE PASSAGES POUR LA SENSIBILITE ---
C
        CALL PSRESE ( 'FONCTION', 1, 1, RESULT, 1,
     >              NBPASS, NORECG, IRET )
C
        IF ( IRET.EQ.0 ) THEN
          CALL JEVEUO ( NORECG, 'L', ADRECG )
        ENDIF
C
        DO 60 , NRPASS = 1 , NBPASS
          LATAB1 = '                   '
          LATAB1(1:8) = ZK24(ADRECG+2*NRPASS-2)(1:8)
          NOPASE = ZK24(ADRECG+2*NRPASS-1)(1:8)
          CALL GETVID('FONCTION','FONCTION',1,1,1,NFCT,IR)
          IF (NOPASE.EQ.' ') THEN
            LERES0 = NFCT
          ELSE
            CALL PSRENC ( NFCT, NOPASE, LERES0, IRET )
            IF ( IRET.NE.0 ) THEN
              CALL UTMESS ('F', 'OP0036','IMPOSSIBLE DE TROUVER'//
     >   ' LE RESULTAT DERIVE ASSOCIE A LA FONCTION '//
     >  NFCT//' ET AU PARAMETRE SENSIBLE '//NOPASE)
            ENDIF
          ENDIF

          CALL TBCRSD(LATAB1,'G')
          CALL JEVEUO(LERES0//'.PROL','L',JPROL)
          IF(ZK16(JPROL).NE.'FONCTION' .AND.
     &      ZK16(JPROL).NE.'CONSTANT'.AND.
     &      ZK16(JPROL).NE.'FONCT_C')
     &      CALL UTMESS('F','OP0036','FONCTION'//
     &        ' INCOMPATIBLE AVEC '//NOMCMD)  
          CALL GETVTX('FONCTION','PARA',1,1,2,NMPARF,IR)
          IF(IR.EQ.0)THEN
            NMPARF(1)=ZK16(JPROL+2)
            NMPARF(2)=ZK16(JPROL+3) 
          ENDIF
          IF(NMPARF(1).EQ.NMPARF(2)) CALL UTMESS('F','OP0036',
     &    'LES NOMS DE CHAQUE PARAMETRE DOIVENT ETRE DIFFERENTS') 
          IF (ZK16(JPROL).EQ.'FONCT_C') THEN
            CALL TBAJPA(LATAB1,2,NMPARF,TYPARC)
          ELSE
            CALL TBAJPA(LATAB1,2,NMPARF,TYPARR)
          ENDIF
          CALL JELIRA(LERES0//'.VALE','LONMAX',NDIM,KBID)
          CALL JEVEUO(LERES0//'.VALE','L',JVALE)
          IF (ZK16(JPROL).EQ.'FONCT_C') THEN
            DO 301 I=1,NDIM/3
              VR(1)=ZR(JVALE-1+I)
           VC(1)=DCMPLX(ZR(JVALE-1+NDIM/3+2*I-1),ZR(JVALE-1+NDIM/3+2*I))
              CALL TBAJLI(LATAB1,2,NMPARF,IBID,VR,VC,KBID,0)
 301        CONTINUE
          ELSE
            DO 300 I=1,NDIM/2
              VR(1)=ZR(JVALE-1+I)
              VR(2)=ZR(JVALE-1+NDIM/2+I)
              CALL TBAJLI(LATAB1,2,NMPARF,IBID,VR,CBID,KBID,0)
 300        CONTINUE
          ENDIF
  60    CONTINUE
        CALL JEDETR ( NORECG )
      ENDIF

      CALL TITRE
      CALL JEDEMA()

      END
