      SUBROUTINE IRCNC8(IFI,NBNO,PRNO,NUEQ,NEC,DG,NCMPMX,VALE,
     +      NOMCMP,NOMNOE,LCOR,NDIM,COOR,NUMNOE,NBCMPT,NUCMPU,
     +      LSUP,BORSUP,LINF,BORINF,LMAX,LMIN,FORMR)
      IMPLICIT NONE
C
      INCLUDE 'jeveux.h'
      INTEGER           IFI,NBNO,PRNO(*),NUEQ(*),NEC,DG(*),NCMPMX
      INTEGER                      NDIM, NUMNOE(*),NBCMPT,NUCMPU(*)
      CHARACTER*(*)          NOMCMP(*),NOMNOE(*),FORMR
      REAL*8            BORSUP,BORINF,               COOR(*)
      COMPLEX*16                                        VALE(*)
      LOGICAL           LSUP,LINF,         LCOR,              LMAX,LMIN
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C TOLE CRP_21
C     ------------------------------------------------------------------
C     ECRITURE D'UN CHAM_NO SUR FICHIER IFI AU FORMAT 'RESULTAT' A
C     VALEURS COMPLEXES
C     ------------------------------------------------------------------
C      ENTREE:
C         IFI   : UNITE LOGIQUE DU FICHIER
C         NBNO  : NOMBRE DE NOEUDS A IMPRIMER
C         PRNO  : OBJET .PRNO(ILIGREL) D'UN PROF_CHNO
C         NUEQ  : OBJET .NUEQ D'UN PROF_CHNO
C         NEC   : NOMBRE D'ENTIERS-CODES
C         DG    : ENTIERS CODES       ES
C         NCMPMX: NOMBRE MAXI DE CMP DE LA GRANDEUR NOMGD
C         VALE  : VALEURS DU CHAM_NO
C         NOMCMP: NOMS DES CMP
C         NOMNOE: NOMS DES NOEUDS
C         LCOR  : IMPRESSION DES COORDONNES .TRUE. IMPRESSION
C         NDIM  : DIMENSION DU MAILLAGE
C         COOR  : COORDONNES DES NOEUDS
C         NUMNOE: NUMEROS DES NOEUDS A IMPRIMER
C         NBCMPT: NOMBRE DE COMPOSANTES A IMPRIMER
C         NUCMPU: NUMEROS DES COMPOSANTES A IMPRIMER
C         LSUP  : =.TRUE.  INDIQUE PRESENCE BORNE SUPERIEURE
C         BORSUP: VALEUR DE LA BORNE SUPERIEURE
C         LINF  : =.TRUE.  INDIQUE PRESENCE BORNE INFERIEURE
C         BORINF: VALEUR DE LA BORNE INFERIEURE
C         LMAX  : =.TRUE.  INDIQUE IMPRESSION VALEUR MAXIMALE
C         LMIN  : =.TRUE.  INDIQUE IMPRESSION VALEUR MINIMALE
C         FORMR : FORMAT D'ECRITURE DES REELS SUR "RESULTAT"
C     ------------------------------------------------------------------
C     ATTENTION EN CAS DE MODIFICATION DE CE SS-PGME, PENSER A IRCNRL
C     ------------------------------------------------------------------
      LOGICAL      EXISDG
      CHARACTER*1  NOMCOR(3)
      CHARACTER*8  FORCMP
      CHARACTER*10 FORMAT
      CHARACTER*50 FMT, FORM1
      REAL*8       VALUE,VALMIN,VALMAX
C
C-----------------------------------------------------------------------
      INTEGER I ,ICM ,ICMAX ,ICMIN ,ICMP ,ICMP2 ,ICOMP2 
      INTEGER ICOMPT ,ID ,IEC ,IEQ ,IF ,IIVAL ,ILIGN 
      INTEGER IMPRE ,INEC ,INMAX ,INMIN ,INNO ,INO ,IPOS 
      INTEGER IPRES ,IREST ,IRMAX ,IRMIN ,IRVAL ,IVA ,IVAL 
      INTEGER IVMAX ,IVMIN ,LGR ,LXLGUT ,NCMP 
      REAL*8 R8VIDE ,RUNDF 
C-----------------------------------------------------------------------
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
      CALL JEDETR('&&IRCNC8.VALR')
      CALL WKVECT('&&IRCNC8.VALR','V V R',NCMPMX,IRVAL)
      CALL JEDETR('&&IRCNC8.VALI')
      CALL WKVECT('&&IRCNC8.VALI','V V R',NCMPMX,IIVAL)
      CALL JEDETR('&&IRCNC8.POS')
      CALL WKVECT('&&IRCNC8.POS','V V I',NCMPMX,IPOS)
      IF(NEC.GT.0) THEN
        CALL JEDETR('&&IRCNC8.ENT')
        CALL WKVECT('&&IRCNC8.ENT','V V I',NEC,INEC)
        DO 16 IEC=1,NEC
        ZI(INEC-1+IEC) =0
 16     CONTINUE
      ENDIF
      IF(LMAX) THEN
        CALL JEDETR('&&IRCNC8.MAXR')
        CALL WKVECT('&&IRCNC8.MAXR','V V R',NCMPMX,IRMAX)
        CALL JEDETR('&&IRCNC8.MAXC')
        CALL WKVECT('&&IRCNC8.MAXC','V V R',NCMPMX,ICMAX)
        CALL JEDETR('&&IRCNC8.NOEMAX')
        CALL WKVECT('&&IRCNC8.NOEMAX','V V K8',NCMPMX,INMAX)
        CALL JEDETR('&&IRCNC8.NBVMAX')
        CALL WKVECT('&&IRCNC8.NBVMAX','V V I',NCMPMX,IVMAX)
        DO 70 I=1,NCMPMX
          ZR(IRMAX-1+I)=RUNDF
 70     CONTINUE
      ENDIF
      IF(LMIN) THEN
        CALL JEDETR('&&IRCNC8.MINR')
        CALL WKVECT('&&IRCNC8.MINR','V V R',NCMPMX,IRMIN)
        CALL JEDETR('&&IRCNC8.MINC')
        CALL WKVECT('&&IRCNC8.MINC','V V R',NCMPMX,ICMIN)
        CALL JEDETR('&&IRCNC8.NOEMIN')
        CALL WKVECT('&&IRCNC8.NOEMIN','V V K8',NCMPMX,INMIN)
        CALL JEDETR('&&IRCNC8.NBVMIN')
        CALL WKVECT('&&IRCNC8.NBVMIN','V V I',NCMPMX,IVMIN)
        DO 71 I=1,NCMPMX
          ZR(IRMIN-1+I)=RUNDF
 71     CONTINUE
      ENDIF
C
      DO 11 INNO = 1,NBNO
         INO = NUMNOE(INNO)
C
C        NCMP : NOMBRE DE CMPS SUR LE NOEUD INO
C        IVAL : ADRESSE DU DEBUT DU NOEUD INO DANS .NUEQ
C
          DO 17 IEC=1,NEC
          DG(IEC) = PRNO((INO-1)*(NEC+2)+2+IEC)
 17       CONTINUE
C
          IVAL = PRNO((INO-1)* (NEC+2)+1)
          NCMP = PRNO((INO-1)* (NEC+2)+2)
          IF (NCMP.EQ.0) GO TO 11
C
          DO 21  I=1,NCMPMX
            ZI(IPOS-1+I) = 0
 21       CONTINUE
          ICOMPT = 0
          IMPRE  = 0
          IPRES = 0
          DO 12 ICMP = 1,NCMPMX
              IF (EXISDG(DG,ICMP)) THEN
                  IPRES  = IPRES  + 1
                  IEQ = NUEQ(IVAL-1+IPRES)
                  IF(NBCMPT.NE.0) THEN
                    DO 13 ICM=1,NBCMPT
                      ICMP2=NUCMPU(ICM)
                      IF(ICMP.EQ.ICMP2)  THEN
                        ZR(IRVAL-1+ICM) = DBLE(VALE(IEQ))
                        ZR(IIVAL-1+ICM) = DIMAG(VALE(IEQ))
                        ZI(IPOS-1+ICM) = ICMP
                        GO TO 12
                      ENDIF
   13                CONTINUE
                  ELSE
                     ICOMPT=IPRES
                     ZR(IRVAL-1+ICOMPT) = DBLE(VALE(IEQ))
                     ZR(IIVAL-1+ICOMPT) = DIMAG(VALE(IEQ))
                     ZI(IPOS-1+ICOMPT) = ICMP
                  ENDIF
              END IF
   12     CONTINUE
C
C --- RETASSAGE POUR IMPRIMER COMPOSANTES ORDRE UTILISATEUR ----
C
          IF(NBCMPT.NE.0) THEN
            ICOMPT=0
            DO 14 I=1,NBCMPT
              IF(ZI(IPOS-1+I).NE.0)  THEN
                 ICOMPT=ICOMPT+1
                 ZI(IPOS-1+ICOMPT) = ZI(IPOS-1+I)
                 ZR(IRVAL-1+ICOMPT) = ZR(IRVAL-1+I)
                 ZR(IIVAL-1+ICOMPT) = ZR(IIVAL-1+I)
              ENDIF
   14       CONTINUE
          ENDIF
          DO 15 IEC=1,NEC
          IF (DG(IEC).NE.ZI(INEC-1+IEC)) THEN
             IMPRE=1
             ZI(INEC-1+IEC) = DG(IEC)
          ENDIF
   15     CONTINUE
C
C --  TRI DES COMPOSANTES DANS L'INTERVALLE BORINF,BORSUP
C
          IF(LSUP.OR.LINF) THEN
            DO 35 IVA=1,ICOMPT
               VALUE= SQRT(ZR(IRVAL-1+IVA)**2+ZR(IIVAL-1+IVA)**2)
               IF(LSUP) THEN
                 IF((VALUE-BORSUP).GT.0.D0)
     +               ZI(IPOS-1+IVA)=0
               ENDIF
               IF(LINF) THEN
                 IF((VALUE-BORINF).LT.0.D0)
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
                 ZR(IIVAL-1+ICOMP2)=ZR(IIVAL-1+I)
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
               IF(ZR(IRMAX-1+ZI(IPOS-1+I)).EQ.RUNDF) THEN
                  ZR(IRMAX-1+ZI(IPOS-1+I)) = ZR(IRVAL-1+I)
                  ZR(ICMAX-1+ZI(IPOS-1+I)) = ZR(IIVAL-1+I)
                  ZK8(INMAX-1+ZI(IPOS-1+I)) = NOMNOE(INNO)
                  ZI(IVMAX-1+ZI(IPOS-1+I)) = 1
               ELSE
                 VALMAX = SQRT(ZR(IRMAX-1+ZI(IPOS-1+I))**2 +
     +                        ZR(ICMAX-1+ZI(IPOS-1+I))**2 )
                 VALUE  = SQRT(ZR(IRVAL-1+I)**2 +
     +                        ZR(IIVAL-1+I)**2 )
                 IF(VALUE.GT.VALMAX) THEN
                   ZR(IRMAX-1+ZI(IPOS-1+I))= ZR(IRVAL-1+I)
                   ZR(ICMAX-1+ZI(IPOS-1+I))= ZR(IIVAL-1+I)
                   ZK8(INMAX-1+ZI(IPOS-1+I)) = NOMNOE(INNO)
                   ZI(IVMAX-1+ZI(IPOS-1+I)) = 1
                 ELSEIF(VALUE.EQ.VALMAX) THEN
                   ZI(IVMAX-1+ZI(IPOS-1+I))=ZI(IVMAX-1+ZI(IPOS-1+I))+1
                 ENDIF
               ENDIF
  90         CONTINUE
          ENDIF
C
C -- RECHERCHE DE LA VALEURE MINIMALE ---
C
          IF(LMIN) THEN
             DO 91 I=1,ICOMPT
               IF(ZR(IRMIN-1+ZI(IPOS-1+I)).EQ.RUNDF) THEN
                  ZR(IRMIN-1+ZI(IPOS-1+I)) = ZR(IRVAL-1+I)
                  ZR(ICMIN-1+ZI(IPOS-1+I)) = ZR(IIVAL-1+I)
                  ZK8(INMIN-1+ZI(IPOS-1+I)) = NOMNOE(INNO)
                  ZI(IVMIN-1+ZI(IPOS-1+I)) = 1
               ELSE
                 VALMIN = SQRT(ZR(IRMIN-1+ZI(IPOS-1+I))**2 +
     +                        ZR(ICMIN-1+ZI(IPOS-1+I))**2 )
                 VALUE  = SQRT(ZR(IRVAL-1+I)**2 +
     +                        ZR(IIVAL-1+I)**2 )
                 IF(VALUE.LT.VALMIN) THEN
                   ZR(IRMIN-1+ZI(IPOS-1+I))= ZR(IRVAL-1+I)
                   ZR(ICMIN-1+ZI(IPOS-1+I))= ZR(IIVAL-1+I)
                   ZK8(INMIN-1+ZI(IPOS-1+I)) = NOMNOE(INNO)
                   ZI(IVMIN-1+ZI(IPOS-1+I)) = 1
                 ELSEIF(VALUE.EQ.VALMIN) THEN
                   ZI(IVMIN-1+ZI(IPOS-1+I))=ZI(IVMIN-1+ZI(IPOS-1+I))+1
                 ENDIF
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
     +                    '),', (ILIGN-1), '(/,9X,6(1X,', FORCMP, ')))'
              ENDIF
              WRITE (IFI,FMT) 'NOEUD   ', (NOMCOR(I),I=1,NDIM),
     +                       (NOMCMP(ZI(IPOS-1+I)),I=1,ICOMPT)
            ENDIF
            FMT = ' '
            IF (IREST.NE.0) THEN
              FMT = '(1X,A,6(1X,'//FORMAT//'),30(/,9X,6(1X,'//
     +                             FORMAT//')))'
            ELSEIF (IREST.EQ.0.AND.ILIGN.EQ.1) THEN
              FMT = '(1X,A,6(1X,'//FORMAT//'))'
            ELSE
              WRITE(FMT,'(A,A10,A,I2,A,A10,A)') '(1X,A,6(1X,', FORMAT,
     +                  '),', (ILIGN-1), '(/,9X,6(1X,', FORMAT, ')))'
            ENDIF
            WRITE (IFI,FMT) NOMNOE(INNO), (COOR((INO-1)*3+I),I=1,NDIM),
     +                      (ZR(IRVAL-1+I),I=1,ICOMPT)
            WRITE (IFI,FMT) '        ',   (COOR((INO-1)*3+I),I=1,NDIM),
     +                      (ZR(IIVAL-1+I),I=1,ICOMPT)
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
                WRITE(FMT,'(A,A8,A,I2,A,A8,A)') '(1X,A,6(1X,', FORCMP,
     +                    '),', (ILIGN-1), '(/,9X,6(1X,', FORCMP, ')))'
              ENDIF
              WRITE (IFI,FMT) 'NOEUD   ',
     +                        (NOMCMP(ZI(IPOS-1+I)),I=1,ICOMPT)
            ENDIF
            FMT = ' '
            IF (IREST.NE.0) THEN
              FMT = '(1X,A,6(1X,'//FORMAT//'),30(/,9X,6(1X,'//
     +                             FORMAT//')))'
            ELSEIF (IREST.EQ.0.AND.ILIGN.EQ.1) THEN
              FMT = '(1X,A,6(1X,'//FORMAT//'))'
            ELSE
              WRITE(FMT,'(A,A10,A,I2,A,A10,A)') '(1X,A,6(1X,', FORMAT,
     +                  '),', (ILIGN-1), '(/,9X,6(1X,', FORMAT, ')))'
            ENDIF
            WRITE (IFI,FMT) NOMNOE(INNO),
     +                     (ZR(IRVAL-1+I),I=1,ICOMPT)
            WRITE (IFI,FMT) '        ',
     +                     (ZR(IIVAL-1+I),I=1,ICOMPT)
          END IF
   11 CONTINUE
      WRITE (IFI,'(A)') ' '
C
C --- IMPRESSION DE LA VALEUR MAXIMALE ---
C
      IF(LMAX) THEN
        DO 95 I=1,NCMPMX
          IF(ZR(IRMAX-1+I).NE.RUNDF) THEN
           FORM1 = '(1X,3A,1X,'//FORMAT//',1X,'//FORMAT//',A,I4,A,A8)'
           WRITE(IFI,FORM1) 'LA VALEUR MAXIMALE DE ', NOMCMP(I),
     +       ' EST',ZR(IRMAX-1+I),ZR(ICMAX-1+I),
     +       ' EN ',ZI(IVMAX-1+I),' NOEUD(S) : ',ZK8(INMAX-1+I)
          ENDIF
 95     CONTINUE
      ENDIF
C
C --- IMPRESSION DE LA VALEUR MINIMALE ---
C
      IF(LMIN) THEN
        DO 96 I=1,NCMPMX
          IF(ZR(IRMIN-1+I).NE.RUNDF) THEN
           FORM1 = '(1X,3A,1X,'//FORMAT//',1X,'//FORMAT//',A,I4,A,A8)'
           WRITE(IFI,FORM1) 'LA VALEUR MINIMALE DE ', NOMCMP(I),
     +       ' EST',ZR(IRMIN-1+I),ZR(ICMIN-1+I),
     +       ' EN ',ZI(IVMIN-1+I),' NOEUD(S) : ',ZK8(INMIN-1+I)
          ENDIF
 96     CONTINUE
      ENDIF
C
      CALL JEDETR('&&IRCNC8.VALR')
      CALL JEDETR('&&IRCNC8.VALI')
      CALL JEDETR('&&IRCNC8.POS')
      CALL JEDETR('&&IRCNC8.ENT')
      CALL JEDETR('&&IRCNC8.MAXR')
      CALL JEDETR('&&IRCNC8.MAXC')
      CALL JEDETR('&&IRCNC8.NOEMAX')
      CALL JEDETR('&&IRCNC8.NBVMAX')
      CALL JEDETR('&&IRCNC8.MINR')
      CALL JEDETR('&&IRCNC8.MINC')
      CALL JEDETR('&&IRCNC8.NOEMIN')
      CALL JEDETR('&&IRCNC8.NBVMIN')
      CALL JEDEMA()
      END
