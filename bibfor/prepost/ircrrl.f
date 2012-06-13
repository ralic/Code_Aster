      SUBROUTINE IRCRRL(IFI,NBNO,DESC,NEC,DG,NCMPMX,VALE,
     +      NOMCMP,NOMNOE,LCOR,NDIM,COOR,NUMNOE,NBCMPT,NUCMPU,
     +      LSUP,BORSUP,LINF,BORINF,LMAX,LMIN,FORMR)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INCLUDE 'jeveux.h'
      INTEGER           IFI,NBNO,DESC(*),NEC,DG(*),NCMPMX
      INTEGER                         NDIM,NUMNOE(*),NBCMPT,NUCMPU(*)
      REAL*8            BORSUP,BORINF,      COOR(*),VALE(*)
      CHARACTER*(*)         NOMCMP(*),NOMNOE(*),FORMR
      LOGICAL                         LCOR,LSUP,LINF,     LMAX,LMIN
C
C----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C        ECRITURE D'UN CHAM_NO A REPRESENTATION CONSTANTE
C        SUR FICHIER IFI AU FORMAT 'RESULTAT' A VALEURS REELLES
C      ENTREE:
C         IFI   : UNITE LOGIQUE DU FICHIER UNIVERSEL
C         NBNO  : NOMBRE DE NOEUDS A IMPRIMER
C         DESC  :
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
      CALL JEDETR('&&IRCRRL.VAL')
      CALL WKVECT('&&IRCRRL.VAL','V V R',NCMPMX,IRVAL)
      CALL JEDETR('&&IRCRRL.POS')
      CALL WKVECT('&&IRCRRL.POS','V V I',NCMPMX,IPOS)
      CALL JEDETR('&&IRCRRL.POSL')
      CALL WKVECT('&&IRCRRL.POSL','V V I',NCMPMX,IPOL)
      IF(NEC.GT.0) THEN
        DO 16 IEC=1,NEC
        DG(IEC)=DESC(3+IEC-1)
 16     CONTINUE
      END IF
      IF(LMAX) THEN
        CALL JEDETR('&&IRCRRL.MAX')
        CALL WKVECT('&&IRCRRL.MAX','V V R',NCMPMX,IMAX)
        CALL JEDETR('&&IRCRRL.NOEMAX')
        CALL WKVECT('&&IRCRRL.NOEMAX','V V K8',NCMPMX,INMAX)
        CALL JEDETR('&&IRCRRL.NBVMAX')
        CALL WKVECT('&&IRCRRL.NBVMAX','V V I',NCMPMX,IVMAX)
        DO 70 I=1,NCMPMX
          ZR(IMAX-1+I)=RUNDF
 70     CONTINUE
      ENDIF
      IF(LMIN) THEN
        CALL JEDETR('&&IRCRRL.MIN')
        CALL WKVECT('&&IRCRRL.MIN','V V R',NCMPMX,IMIN)
        CALL JEDETR('&&IRCRRL.NOEMIN')
        CALL WKVECT('&&IRCRRL.NOEMIN','V V K8',NCMPMX,INMIN)
        CALL JEDETR('&&IRCRRL.NBVMIN')
        CALL WKVECT('&&IRCRRL.NBVMIN','V V I',NCMPMX,IVMIN)
        DO 71 I=1,NCMPMX
          ZR(IMIN-1+I)=RUNDF
 71     CONTINUE
      ENDIF
C
      NCMP = -DESC(2)
      DO 21 I=1,NCMPMX
        ZI(IPOS-1+I) = 0
 21   CONTINUE
      ICOMPT = 0
      IMPRE  = 1
      IPRES  = 0
      DO 12 ICMP = 1,NCMPMX
        IF (EXISDG(DG,ICMP)) THEN
          IPRES  = IPRES  + 1
          IF(NBCMPT.NE.0) THEN
            DO 13 ICM=1,NBCMPT
              ICMP2=NUCMPU(ICM)
              IF(ICMP.EQ.ICMP2) THEN
                ZI(IPOS-1+ICM) = ICMP
                GOTO 12
              ENDIF
   13       CONTINUE
          ELSE
            ZI(IPOS-1+IPRES) = ICMP
          ENDIF
        END IF
   12 CONTINUE
C
C --- RETASSAGE POUR IMPRIMER COMPOSANTES ORDRE UTILISATEUR---
C
      IF(NBCMPT.NE.0) THEN
        ICOMPT=0
        DO 14 I=1,NBCMPT
          IF(ZI(IPOS-1+I).NE.0) THEN
            ICOMPT=ICOMPT+1
            ZI(IPOS-1+ICOMPT)=ZI(IPOS-1+I)
          ENDIF
   14   CONTINUE
      ELSE
        ICOMPT=NCMP
      ENDIF
C
C --- BOUCLE SUR LES NOEUDS
C
      DO 11 INNO = 1,NBNO
        INO = NUMNOE(INNO)
        IVAL= (INO-1)*NCMP
        DO 22 IVA=1,ICOMPT
          IC= ZI(IPOS-1+IVA)
          ZR(IRVAL-1+IVA) = VALE(IVAL+IC)
          ZI(IPOL-1+IVA) = ZI(IPOS-1+IVA)
C
C --  TRI DES COMPOSANTES DANS L'INTERVALLE BORINF,BORSUP
C
          IF(LSUP.OR.LINF) THEN
            IF(LSUP) THEN
              IF((ZR(IRVAL-1+IVA)-BORSUP).GT.0.D0)
     +               ZI(IPOL-1+IVA)=0
            ENDIF
            IF(LINF) THEN
              IF((ZR(IRVAL-1+IVA)-BORINF).LT.0.D0)
     +               ZI(IPOL-1+IVA)=0
            ENDIF
          ENDIF
C
C --- RETASSAGE POUR IMPRIMER COMPOSANTES PRESENTES DANS L'INTERVALLE --
C
   22   CONTINUE
        ICOMP2 = ICOMPT
        IF(LSUP.OR.LINF) THEN
          ICOMP2=0
          DO 36 I=1,ICOMPT
            IF(ZI(IPOL-1+I).NE.0) THEN
              ICOMP2=ICOMP2+1
              ZI(IPOL-1+ICOMP2)=ZI(IPOL-1+I)
              ZR(IRVAL-1+ICOMP2)=ZR(IRVAL-1+I)
            ENDIF
   36     CONTINUE
        ENDIF
        IF(ICOMP2.EQ.0) THEN
          GOTO 11
        ENDIF
C
C -- RECHERCHE DE LA VALEURE MAXIMALE ---
C
        IF(LMAX) THEN
          DO 90 I=1,ICOMP2
            IF(ZR(IMAX-1+ZI(IPOL-1+I)).EQ.RUNDF) THEN
                ZR(IMAX-1+ZI(IPOL-1+I)) = ZR(IRVAL-1+I)
                ZK8(INMAX-1+ZI(IPOL-1+I)) = NOMNOE(INNO)
                ZI(IVMAX-1+ZI(IPOL-1+I)) = 1
            ELSEIF(ZR(IRVAL-1+I).GT.ZR(IMAX-1+ZI(IPOL-1+I))) THEN
                ZR(IMAX-1+ZI(IPOL-1+I))= ZR(IRVAL-1+I)
                ZK8(INMAX-1+ZI(IPOL-1+I))= NOMNOE(INNO)
                ZI(IVMAX-1+ZI(IPOL-1+I))=1
            ELSEIF(ZR(IRVAL-1+I).EQ.ZR(IMAX-1+ZI(IPOL-1+I))) THEN
                ZI(IVMAX-1+ZI(IPOL-1+I))=ZI(IVMAX-1+ZI(IPOL-1+I))+1
            ENDIF
  90      CONTINUE
        ENDIF
C
C -- RECHERCHE DE LA VALEURE MINIMALE ---
C
        IF(LMIN) THEN
          DO 91 I=1,ICOMP2
            IF(ZR(IMIN-1+ZI(IPOL-1+I)).EQ.RUNDF) THEN
                ZR(IMIN-1+ZI(IPOL-1+I)) = ZR(IRVAL-1+I)
                ZK8(INMIN-1+ZI(IPOL-1+I)) = NOMNOE(INNO)
                ZI(IVMIN-1+ZI(IPOL-1+I)) = 1
            ELSEIF(ZR(IRVAL-1+I).LT.ZR(IMIN-1+ZI(IPOL-1+I))) THEN
                ZR(IMIN-1+ZI(IPOL-1+I))= ZR(IRVAL-1+I)
                ZK8(INMIN-1+ZI(IPOL-1+I))= NOMNOE(INNO)
                ZI(IVMIN-1+ZI(IPOL-1+I))=1
            ELSEIF(ZR(IRVAL-1+I).EQ.ZR(IMIN-1+ZI(IPOL-1+I))) THEN
                ZI(IVMIN-1+ZI(IPOL-1+I))=ZI(IVMIN-1+ZI(IPOL-1+I))+1
            ENDIF
  91      CONTINUE
        ENDIF
C
C - IMPRESSION DES VALEURS ---
C
        IF (.NOT.LMAX.AND..NOT.LMIN.AND.LCOR) THEN
          ILIGN=(ICOMP2+NDIM)/6
          IREST=(ICOMP2+NDIM)-ILIGN*6
          IF (IMPRE.EQ.1.OR.LSUP.OR.LINF) THEN
            FMT = ' '
            IF (IREST.NE.0) THEN
              FMT = '(1X,A,6(1X,'//FORCMP//'),30(/,9X,6(1X,'//
     +                             FORCMP//')))'
            ELSEIF (IREST.EQ.0.AND.ILIGN.EQ.1) THEN
              FMT = '(1X,A,6(1X,'//FORCMP//'))'
            ELSE
              WRITE(FMT,'(A,A8,A,I2,A,A8,A)') '(1X,A,6(1X,', FORCMP,
     +                  '), ',(ILIGN-1), '(/,9X,6(1X,', FORCMP, ')))'
            ENDIF
            WRITE (IFI,FMT) 'NOEUD   ', (NOMCOR(I),I=1,NDIM),
     +                      (NOMCMP(ZI(IPOL-1+I)),I=1,ICOMP2)
          ENDIF
          FMT = ' '
          IF (IREST.NE.0) THEN
            WRITE(FMT,'(A,A10,A,A10,A)') '(1X,A,6(1X,', FORMAT,
     +                '),30(/,9X,6(1X,', FORMAT, ')))'
          ELSEIF (IREST.EQ.0.AND.ILIGN.EQ.1) THEN
            WRITE(FMT,'(A,A10,A)') '(1X,A,6(1X,', FORMAT, '))'
          ELSE
            WRITE(FMT,'(A,A10,A,I2,A,A10,A)') '(1X,A,6(1X,', FORMAT,
     +                    '),' ,(ILIGN-1), '(/,9X,6(1X,', FORMAT, ')))'
          ENDIF
          WRITE (IFI,FMT) NOMNOE(INNO), (COOR((INO-1)*3+I),I=1,NDIM),
     +                    (ZR(IRVAL-1+I),I=1,ICOMP2)
        ELSE IF (.NOT.LMAX.AND..NOT.LMIN)   THEN
          ILIGN=(ICOMP2)/6
          IREST=(ICOMP2)-ILIGN*6
          IF(IMPRE.EQ.1.OR.LSUP.OR.LINF) THEN
            FMT = ' '
            IF (IREST.NE.0) THEN
              FMT = '(1X,A,6(1X,'//FORCMP//'),30(/,9X,6(1X,'//
     +                             FORCMP//')))'
            ELSEIF (IREST.EQ.0.AND.ILIGN.EQ.1) THEN
              FMT = '(1X,A,6(1X,'//FORCMP//'))'
            ELSE
              WRITE(FMT,'(A,A8,A,I2,A,A8,A)') '(1X,A,6(1X,', FORCMP,
     +                  '),', (ILIGN-1),'(/,9X,6(1X,', FORCMP, ')))'
            ENDIF
            WRITE (IFI,FMT) 'NOEUD   ',
     +                      (NOMCMP(ZI(IPOL-1+I)),I=1,ICOMP2)
          ENDIF
          FMT = ' '
          IF (IREST.NE.0) THEN
            WRITE(FMT,'(A,A10,A,A10,A)') '(1X,A,6(1X,', FORMAT,
     +                      '),30(/,9X,6(1X,', FORMAT, ')))'
          ELSEIF (IREST.EQ.0.AND.ILIGN.EQ.1) THEN
            WRITE(FMT,'(A,A10,A)') '(1X,A,6(1X,', FORMAT, '))'
          ELSE
            WRITE(FMT,'(A,A10,A,I2,A,A10,A)') '(1X,A,6(1X,', FORMAT,
     +                      '),',(ILIGN-1),'(/,9X,6(1X,',FORMAT,')))'
          ENDIF
          WRITE (IFI,FMT) NOMNOE(INNO),(ZR(IRVAL-1+I),I=1,ICOMP2)
        END IF
        IMPRE = 0
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
      CALL JEDETR('&&IRCRRL.VAL')
      CALL JEDETR('&&IRCRRL.POS')
      CALL JEDETR('&&IRCRRL.POSL')
      CALL JEDETR('&&IRCRRL.MAX')
      CALL JEDETR('&&IRCRRL.NOEMAX')
      CALL JEDETR('&&IRCRRL.NBVMAX')
      CALL JEDETR('&&IRCRRL.MIN')
      CALL JEDETR('&&IRCRRL.NOEMIN')
      CALL JEDETR('&&IRCRRL.NBVMIN')
      CALL JEDEMA()
      END
