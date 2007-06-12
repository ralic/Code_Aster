      SUBROUTINE IRCNRL(IFI,NBNO,PRNO,NUEQ,NEC,DG,NCMPMX,VALE,
     +      NOMCMP,NOMNOE,LCOR,NDIM,COOR,NUMNOE,NBCMPT,NUCMPU,
     +      LSUP,BORSUP,LINF,BORINF,LMAX,LMIN,FORMR)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER           IFI,NBNO,PRNO(*),NUEQ(*),NEC,DG(*),NCMPMX
      INTEGER                         NDIM,NUMNOE(*),NBCMPT,NUCMPU(*)
      REAL*8            BORSUP,BORINF,      COOR(*),VALE(*)
      CHARACTER*(*)         NOMCMP(*),NOMNOE(*), FORMR
      LOGICAL                         LCOR,LSUP,LINF,     LMAX,LMIN
C
C----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 09/05/2001   AUTEUR CIBHHLV L.VIVAN 
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
C TOLE CRP_21
C        ECRITURE D'UN CHAM_NO SUR FICHIER IFI AU FORMAT 'RESULTAT'
C        A VALEURS REELLES
C      ENTREE:
C         IFI   : UNITE LOGIQUE DU FICHIER UNIVERSEL
C         NBNO  : NOMBRE DE NOEUDS A IMPRIMER
C         PRNO  : OBJET .PRNO(ILIGREL) D'UN PROF_CHNO
C         NUEQ  : OBJET .NUEQ D'UN PROF_CHNO
C         NEC   : NOMBRE D'ENTIERS-CODES
C         DG    : TABLEAU DES ENTIERS CODES
C         NCMPMX: NOMBRE MAXI DE CMP DE LA GRANDEUR NOMGD
C         VALE  : VALEURS DU CHAM_NO
C         NOMCMP: NOMS DES CMP
C         NOMNOE: NOMS DES NOEUDS
C         LCOR  : IMPRESSION DES COORDONNEES .TRUE. IMPRESSION
C         NDIM  : DIMENSION DU MAILLAGE
C         COOR  : COORDONNEES D'UN MAILLAGE
C         NUMNOE: NUMEROS DES NOEUDS A IMPRIMER
C         NBCMPT: NOMBRE DE COMPOSANTES A IMPRIMER
C         NOCMPU: NUMEROS DES COMPOSANTES A IMPRIMER
C         LSUP  : =.TRUE. INDIQUE PRESENCE D'UNE BORNE SUPERIEURE
C         BORSUP: VALEUR DE LA BORNE SUPERIEURE
C         LINF  : =.TRUE. INDIQUE PRESENCE D'UNE BORNE INFERIEURE
C         BORINF: VALEUR DE LA BORNE INFERIEURE
C         LMAX  : =.TRUE. INDIQUE IMPRESSION VALEUR MAXIMALE
C         LMIN  : =.TRUE. INDIQUE IMPRESSION VALEUR MINIMALE
C         FORMR : FORMAT D'ECRITURE DES REELS SUR "RESULTAT"
C     ------------------------------------------------------------------
C     ATTENTION EN CAS DE MODIFICATION DE CE SS-PGME, PENSER A IRCNC8
C     ------------------------------------------------------------------
      COMMON  /IVARJE/ ZI(1)
      COMMON  /RVARJE/ ZR(1)
      COMMON  /CVARJE/ ZC(1)
      COMMON  /LVARJE/ ZL(1)
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER      ZI
      REAL*8       ZR
      COMPLEX*16   ZC
      LOGICAL      ZL
      CHARACTER*8  ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
C
      REAL*8  RUNDF
      INTEGER IMPRE
      LOGICAL EXISDG
      CHARACTER*8 NOMCOR(3), FORCMP
      CHARACTER*10 FORMAT
      CHARACTER*50 FMT, FORM1
C
      CALL JEMARQ()
      RUNDF = R8VIDE()
      NOMCOR(1) = 'X'
      NOMCOR(2) = 'Y'
      NOMCOR(3) = 'Z'
      FORMAT = FORMR
      LGR = LXLGUT( FORMAT )
      ID = 0
      IF = 0
      DO 2 I = 1 , LGR-1
         IF ( FORMAT(I:I) .EQ. 'D' .OR. FORMAT(I:I) .EQ. 'E' .OR.
     +        FORMAT(I:I) .EQ. 'F' .OR. FORMAT(I:I) .EQ. 'G' ) THEN
            ID = I+1
            GOTO 2
         ENDIF
         IF ( FORMAT(I:I) .EQ. '.' ) THEN
            IF = I-1
            GOTO 2
         ENDIF
 2    CONTINUE
      IF ( ID.NE.0 .AND. IF.GE.ID ) THEN
         FORCMP = 'A'//FORMAT(ID:IF)
      ELSE
         FORCMP = 'A12'
      ENDIF
C
C -- ALLOCATION DES TABLEAUX DE TRAVAIL ---
C
      CALL JEDETR('&&IRCNRL.VAL')
      CALL WKVECT('&&IRCNRL.VAL','V V R',NCMPMX,IRVAL)
      CALL JEDETR('&&IRCNRL.POS')
      CALL WKVECT('&&IRCNRL.POS','V V I',NCMPMX,IPOS)
      IF(NEC.GT.0) THEN
        CALL JEDETR('&&IRCNRL.ENT')
        CALL WKVECT('&&IRCNRL.ENT','V V I',NEC,INEC)
        DO 16 IEC=1,NEC
        ZI(INEC-1+IEC)=0
 16     CONTINUE
      END IF
      IF(LMAX) THEN
        CALL JEDETR('&&IRCNRL.MAX')
        CALL WKVECT('&&IRCNRL.MAX','V V R',NCMPMX,IMAX)
        CALL JEDETR('&&IRCNRL.NOEMAX')
        CALL WKVECT('&&IRCNRL.NOEMAX','V V K8',NCMPMX,INMAX)
        CALL JEDETR('&&IRCNRL.NBVMAX')
        CALL WKVECT('&&IRCNRL.NBVMAX','V V I',NCMPMX,IVMAX)
        DO 70 I=1,NCMPMX
          ZR(IMAX-1+I)=RUNDF
 70     CONTINUE
      ENDIF
      IF(LMIN) THEN
        CALL JEDETR('&&IRCNRL.MIN')
        CALL WKVECT('&&IRCNRL.MIN','V V R',NCMPMX,IMIN)
        CALL JEDETR('&&IRCNRL.NOEMIN')
        CALL WKVECT('&&IRCNRL.NOEMIN','V V K8',NCMPMX,INMIN)
        CALL JEDETR('&&IRCNRL.NBVMIN')
        CALL WKVECT('&&IRCNRL.NBVMIN','V V I',NCMPMX,IVMIN)
        DO 71 I=1,NCMPMX
          ZR(IMIN-1+I)=RUNDF
 71     CONTINUE
      ENDIF
C
      DO 11 INNO = 1,NBNO
        INO = NUMNOE(INNO)
        DO 17 IEC=1,NEC
           DG(IEC)= PRNO((INO-1)*(NEC+2)+2+IEC)
 17     CONTINUE
C
C        NCMP : NOMBRE DE CMPS SUR LE NOEUD INO
C        IVAL : ADRESSE DU DEBUT DU NOEUD INO DANS .NUEQ
C
          IVAL = PRNO((INO-1)* (NEC+2)+1)
          NCMP = PRNO((INO-1)* (NEC+2)+2)
          IF (NCMP.EQ.0) GO TO 11
C
          DO 21 I=1,NCMPMX
            ZI(IPOS-1+I) = 0
 21       CONTINUE
          ICOMPT = 0
          IMPRE  = 0
          IPRES  = 0
          DO 12 ICMP = 1,NCMPMX
              IF (EXISDG(DG,ICMP)) THEN
                  IPRES  = IPRES  + 1
                  IEQ = NUEQ(IVAL-1+IPRES)
                  IF(NBCMPT.NE.0) THEN
                    DO 13 ICM=1,NBCMPT
                      ICMP2=NUCMPU(ICM)
                      IF(ICMP.EQ.ICMP2) THEN
                        ZR(IRVAL-1+ICM) = VALE(IEQ)
                        ZI(IPOS-1+ICM) = ICMP
                        GOTO 12
                      ENDIF
   13               CONTINUE
                  ELSE
                    ICOMPT=IPRES
                    ZR(IRVAL-1+ICOMPT) = VALE(IEQ)
                    ZI(IPOS-1+ICOMPT) = ICMP
                  ENDIF
              END IF
   12     CONTINUE
C
C --- RETASSAGE POUR IMPRIMER COMPOSANTES ORDRE UTILISATEUR---
C
          IF(NBCMPT.NE.0) THEN
             ICOMPT=0
             DO 14 I=1,NBCMPT
               IF(ZI(IPOS-1+I).NE.0) THEN
                 ICOMPT=ICOMPT+1
                 ZI(IPOS-1+ICOMPT)=ZI(IPOS-1+I)
                 ZR(IRVAL-1+ICOMPT)=ZR(IRVAL-1+I)
               ENDIF
   14        CONTINUE
          ENDIF
          DO 15 IEC =1,NEC
              IF(DG(IEC).NE.ZI(INEC-1+IEC)) THEN
                 IMPRE=1
                 ZI(INEC-1+IEC) =DG(IEC)
              END IF
   15     CONTINUE
C
C --  TRI DES COMPOSANTES DANS L'INTERVALLE BORINF,BORSUP
C
          IF(LSUP.OR.LINF) THEN
            DO 35 IVA=1,ICOMPT
               IF(LSUP) THEN
                 IF((ZR(IRVAL-1+IVA)-BORSUP).GT.0.D0)
     +               ZI(IPOS-1+IVA)=0
               ENDIF
               IF(LINF) THEN
                 IF((ZR(IRVAL-1+IVA)-BORINF).LT.0.D0)
     +               ZI(IPOS-1+IVA)=0
               ENDIF
 35         CONTINUE
C
C --- RETASSAGE POUR IMPRIMER COMPOSANTES PRESENTES DANS L'INTERVALLE --
C
            ICOMP2=0
            DO 36 I=1,ICOMPT
              IF(ZI(IPOS-1+I).NE.0) THEN
                 ICOMP2=ICOMP2+1
                 ZI(IPOS-1+ICOMP2)=ZI(IPOS-1+I)
                 ZR(IRVAL-1+ICOMP2)=ZR(IRVAL-1+I)
              ENDIF
   36       CONTINUE
            ICOMPT=ICOMP2
          ENDIF
          IF(ICOMPT.EQ.0) THEN
            GOTO 11
          ENDIF
C
C -- RECHERCHE DE LA VALEURE MAXIMALE ---
C
          IF(LMAX) THEN
             DO 90 I=1,ICOMPT
               IF(ZR(IMAX-1+ZI(IPOS-1+I)).EQ.RUNDF) THEN
                  ZR(IMAX-1+ZI(IPOS-1+I)) = ZR(IRVAL-1+I)
                  ZK8(INMAX-1+ZI(IPOS-1+I)) = NOMNOE(INNO)
                  ZI(IVMAX-1+ZI(IPOS-1+I)) = 1
               ELSEIF(ZR(IRVAL-1+I).GT.ZR(IMAX-1+ZI(IPOS-1+I))) THEN
                  ZR(IMAX-1+ZI(IPOS-1+I))= ZR(IRVAL-1+I)
                  ZK8(INMAX-1+ZI(IPOS-1+I)) = NOMNOE(INNO)
                  ZI(IVMAX-1+ZI(IPOS-1+I)) = 1
               ELSEIF(ZR(IRVAL-1+I).EQ.ZR(IMAX-1+ZI(IPOS-1+I))) THEN
                  ZI(IVMAX-1+ZI(IPOS-1+I))=ZI(IVMAX-1+ZI(IPOS-1+I))+1
               ENDIF
  90         CONTINUE
          ENDIF
C
C -- RECHERCHE DE LA VALEURE MINIMALE ---
C
          IF(LMIN) THEN
             DO 91 I=1,ICOMPT
               IF(ZR(IMIN-1+ZI(IPOS-1+I)).EQ.RUNDF) THEN
                  ZR(IMIN-1+ZI(IPOS-1+I)) = ZR(IRVAL-1+I)
                  ZK8(INMIN-1+ZI(IPOS-1+I)) = NOMNOE(INNO)
                  ZI(IVMIN-1+ZI(IPOS-1+I)) = 1
               ELSEIF(ZR(IRVAL-1+I).LT.ZR(IMIN-1+ZI(IPOS-1+I))) THEN
                  ZR(IMIN-1+ZI(IPOS-1+I))= ZR(IRVAL-1+I)
                  ZK8(INMIN-1+ZI(IPOS-1+I)) = NOMNOE(INNO)
                  ZI(IVMIN-1+ZI(IPOS-1+I)) = 1
               ELSEIF(ZR(IRVAL-1+I).EQ.ZR(IMIN-1+ZI(IPOS-1+I))) THEN
                  ZI(IVMIN-1+ZI(IPOS-1+I))=ZI(IVMIN-1+ZI(IPOS-1+I))+1
               ENDIF
  91         CONTINUE
          ENDIF
C
C - IMPRESSION DES VALEURS ---
C
          IF (.NOT.LMAX.AND..NOT.LMIN.AND.LCOR) THEN
            ILIGN=(ICOMPT+NDIM)/6
            IREST=(ICOMPT+NDIM)-ILIGN*6
            IF (IMPRE.EQ.1.OR.LSUP.OR.LINF) THEN
              FMT = ' '
              IF (IREST.NE.0) THEN
                FMT = '(1X,A,6(1X,'//FORCMP//'),30(/,9X,6(1X,'//
     +                               FORCMP//')))'
              ELSEIF (IREST.EQ.0.AND.ILIGN.EQ.1) THEN
                FMT = '(1X,A,6(1X,'//FORCMP//'))'
              ELSE
                WRITE(FMT,'(A,A8,A,I2,A,A8,A)') '(1X,A,6(1X,', FORCMP,
     +                     '),', (ILIGN-1), '(/,9X,6(1X,', FORCMP, ')))'
              ENDIF
              WRITE (IFI,FMT) 'NOEUD   ', (NOMCOR(I),I=1,NDIM),
     +                        (NOMCMP(ZI(IPOS-1+I)),I=1,ICOMPT)
            ENDIF
            FMT = ' '
            IF (IREST.NE.0) THEN
              FMT = '(1X,A,6(1X,'//FORMAT//
     +                          '),30(/,9X,6(1X,'//FORMAT//')))'
            ELSEIF (IREST.EQ.0.AND.ILIGN.EQ.1) THEN
              FMT = '(1X,A,6(1X,'//FORMAT//'))'
            ELSE
              WRITE(FMT,'(A,A10,A,I2,A,A10,A)') '(1X,A,6(1X,',FORMAT,
     +                     '),',(ILIGN-1),'(/,9X,6(1X,', FORMAT, ')))'
            ENDIF
            WRITE (IFI,FMT) NOMNOE(INNO),
     +       (COOR((INO-1)*3+I),I=1,NDIM),(ZR(IRVAL-1+I),I=1,ICOMPT)
          ELSE IF (.NOT.LMAX.AND..NOT.LMIN) THEN
            ILIGN=(ICOMPT)/6
            IREST=(ICOMPT)-ILIGN*6
            IF(IMPRE.EQ.1.OR.LSUP.OR.LINF) THEN
              FMT = ' '
              IF (IREST.NE.0) THEN
                FMT = '(1X,A,6(1X,'//FORCMP//'),30(/,9X,6(1X,'//
     +                               FORCMP//')))'
              ELSEIF (IREST.EQ.0.AND.ILIGN.EQ.1) THEN
                FMT = '(1X,A,6(1X,'//FORCMP//'))'
              ELSE
                WRITE(FMT,'(A,A8,A,I2,A,A8,A)') '(1X,A,6(1X,',FORCMP,
     +                    '),', (ILIGN-1), '(/,9X,6(1X,', FORCMP, ')))'
              ENDIF
              WRITE (IFI,FMT) 'NOEUD   ',
     +                        (NOMCMP(ZI(IPOS-1+I)),I=1,ICOMPT)
            ENDIF
            FMT = ' '
            IF (IREST.NE.0) THEN
              FMT = '(1X,A,6(1X,'//FORMAT//
     +                          '),30(/,9X,6(1X,'//FORMAT//')))'
            ELSEIF (IREST.EQ.0.AND.ILIGN.EQ.1) THEN
              FMT = '(1X,A,6(1X,'//FORMAT//'))'
            ELSE
              WRITE(FMT,'(A,A10,A,I2,A,A10,A)') '(1X,A,6(1X,', FORMAT,
     +                    '),',(ILIGN-1),'(/,9X,6(1X,', FORMAT, ')))'
            ENDIF
            WRITE (IFI,FMT) NOMNOE(INNO), (ZR(IRVAL-1+I),I=1,ICOMPT)
          END IF
   11 CONTINUE
      WRITE (IFI,'(A)') ' '
C
C --- IMPRESSION DE LA VALEUR MAXIMALE ---
C
      IF(LMAX) THEN
        DO 95 I=1,NCMPMX
          IF(ZR(IMAX-1+I).NE.RUNDF) THEN
           FORM1 = '(1X,3A,1X,'//FORMAT//',A,I4,A,A8)'
           WRITE(IFI,FORM1) 'LA VALEUR MAXIMALE DE ', NOMCMP(I),
     +       ' EST',ZR(IMAX-1+I),
     +       ' EN ',ZI(IVMAX-1+I),' NOEUD(S) : ',ZK8(INMAX-1+I)
          ENDIF
 95     CONTINUE
      ENDIF
C
C --- IMPRESSION DE LA VALEUR MINIMALE ---
C
      IF(LMIN) THEN
        DO 96 I=1,NCMPMX
          IF(ZR(IMIN-1+I).NE.RUNDF) THEN
           FORM1 = '(1X,3A,1X,'//FORMAT//',A,I4,A,A8)'
           WRITE(IFI,FORM1) 'LA VALEUR MINIMALE DE ',NOMCMP(I),
     +       ' EST',ZR(IMIN-1+I),
     +       ' EN ',ZI(IVMIN-1+I),' NOEUD(S) : ',ZK8(INMIN-1+I)
          ENDIF
 96     CONTINUE
      ENDIF
C
      CALL JEDETR('&&IRCNRL.VAL')
      CALL JEDETR('&&IRCNRL.POS')
      CALL JEDETR('&&IRCNRL.ENT')
      CALL JEDETR('&&IRCNRL.MAX')
      CALL JEDETR('&&IRCNRL.NOEMAX')
      CALL JEDETR('&&IRCNRL.NBVMAX')
      CALL JEDETR('&&IRCNRL.MIN')
      CALL JEDETR('&&IRCNRL.NOEMIN')
      CALL JEDETR('&&IRCNRL.NBVMIN')
      CALL JEDEMA()
      END
