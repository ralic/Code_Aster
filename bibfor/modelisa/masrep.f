      SUBROUTINE MASREP(NOMA,IOC,RIGI,LVALE,NBGR,LIGRMA,NBNO,
     &                  TABNOE,RIGNOE,RIGTO,NDIM)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM,JEXNOM
      INTEGER      IOC, NBGR, NBNO,NDIM
      CHARACTER*8  NOMA, TABNOE(*)
      CHARACTER*24 LIGRMA(NBGR)
      REAL*8       RIGNOE(*), RIGTO(*)
      LOGICAL      LVALE
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 18/12/2012   AUTEUR SELLENET N.SELLENET 
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
C
      CHARACTER*8  K8B
      CHARACTER*8   NOMNOE,TYPM
      CHARACTER*8  NOMPAR(3)
      CHARACTER*24 MANONO, MAGRMA, MANOMA, MATYMA
      REAL*8        ZERO, X(9), Y(9), Z(9), RIGI(6)
      REAL*8       A(3), B(3), C(3), U(3)
      LOGICAL      LFONC
      INTEGER      IARG,APPUI
C
C-----------------------------------------------------------------------
      INTEGER I ,ICOEF  ,IDNO ,IER ,IFONGR ,II, IUNITE
      INTEGER IJ ,IM ,IN ,INOE ,IRET ,ISURMA ,JCOOR
      INTEGER LDGM  ,LDNM ,LTYP ,NB ,NBMA
      INTEGER NFG  ,NM ,NN  ,NOEMAX ,NTOPO, NUMA
      INTEGER ISURM1, ISURM2, ISURM3, ISURM4, ISURM5, ISURM6
      INTEGER ICOEXX, ICOEXY, ICOEXZ, ICOEYY, ICOEYZ, ICOEZZ
      REAL*8 COEF ,DDOT  ,HC ,R1 ,R2 ,R3, R4 ,R5 ,R6
      REAL*8 SURF ,SURTOT ,XC
      REAL*8 YC    , Z0
      REAL*8 SURTXX, SURTXY, SURTXZ, SURTYY, SURTYZ, SURTZZ
C-----------------------------------------------------------------------
      CALL JEMARQ()
      ZERO = 0.D0
      LFONC = .FALSE.
      IUNITE = 6

C
C     --- ON RECUPERE LES POINTS D'ANCRAGE ---
C
C
C        --- ON ECLATE LE GROUP_NO EN NOEUDS ---
      CALL COMPMA(NOMA,NBGR,LIGRMA,NBMA)
      MANONO = NOMA//'.NOMNOE'
      MAGRMA = NOMA//'.GROUPEMA'
      MANOMA = NOMA//'.CONNEX'
      MATYMA = NOMA//'.TYPMAIL'

      NOEMAX = 0

C
C     --- DESCRIPTION NOEUDS STRUCTURE ---
      CALL JEVEUO(NOMA//'.COORDO    .VALE','L',JCOOR)

C
C       RECUPERATION DES COEFS OU FONCTIONS DE GROUPE
C
C      CALL GETVR8('MASS_AJOU','Z0',IOC,IARG,1,Z0,NCG)
      Z0 = ZERO
      CALL GETVID('MASS_AJOU','FONC_GROUP',IOC,IARG,0,K8B,NFG)
      IF (NFG.NE.0) THEN
        CALL WKVECT('&&MASREP.FONGRO','V V K8',NBGR,IFONGR)
        LFONC = .TRUE.
        CALL GETVID('MASS_AJOU','FONC_GROUP',IOC,IARG,NBGR,
     &              ZK8(IFONGR),NFG)
      ENDIF
C


      IF (NDIM.EQ.2)THEN
        APPUI=1
      ELSE
C     LA DIMENSION DE L'APPUI N'EST PAS ENCORE DETERMINEE
        APPUI=-1
      ENDIF

      CALL JEVEUO(MATYMA,'L',LTYP)
      DO 20 I = 1,NBGR
         CALL JELIRA(JEXNOM(MAGRMA,LIGRMA(I)),'LONUTI',NB,K8B)
         CALL JEVEUO(JEXNOM(MAGRMA,LIGRMA(I)),'L',LDGM)
         DO 22 IN = 0,NB-1
           NUMA=ZI(LDGM+IN)
           CALL ASSERT(NUMA.GT.0)
           CALL JELIRA(JEXNUM(MANOMA,NUMA),'LONMAX',NM,K8B)
           CALL JEVEUO(JEXNUM(MANOMA,NUMA),'L',LDNM)

           CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(LTYP-1+NUMA)),TYPM)
           CALL DISMOI('F','DIM_TOPO',TYPM,'TYPE_MAILLE',NTOPO,K8B,IER)

           IF (APPUI.EQ.-1)THEN
C            LA DIMENSION DE LA PREMIERE MAILLE DEFINIT L'APPUI
             APPUI=NTOPO
           ELSEIF ((APPUI.EQ.1).OR.(APPUI.EQ.2))THEN
             IF (APPUI.NE.NTOPO)THEN
               CALL U2MESS('F','MODELISA6_35')
             ENDIF
           ELSE
             CALL U2MESS('F','MODELISA6_29')
           ENDIF
           DO 24 NN = 1, NM
              INOE = ZI(LDNM+NN-1)
              NOEMAX = MAX(NOEMAX,INOE)
 24        CONTINUE
 22      CONTINUE
 20   CONTINUE
      CALL ASSERT(APPUI.NE.-1)

      CALL WKVECT('&&MASREP.COENO','V V R',NOEMAX,ICOEF)
      CALL WKVECT('&&MASREP.COENXX','V V R',NOEMAX,ICOEXX)
      CALL WKVECT('&&MASREP.COENXY','V V R',NOEMAX,ICOEXY)
      CALL WKVECT('&&MASREP.COENXZ','V V R',NOEMAX,ICOEXZ)
      CALL WKVECT('&&MASREP.COENYY','V V R',NOEMAX,ICOEYY)
      CALL WKVECT('&&MASREP.COENYZ','V V R',NOEMAX,ICOEYZ)
      CALL WKVECT('&&MASREP.COENZZ','V V R',NOEMAX,ICOEZZ)
C
C        TABLEAU DE PARTICIPATION DES NOEUDS DE L INTERFACE
C
      CALL WKVECT('&&MASREP.PARNO','V V I',NOEMAX,IDNO)
C
C
C     CALCUL DES SURFACES ELEMENTAIRES ET DE LA SURFACE TOTALE
C
      CALL WKVECT('&&MASREP.SURMAI','V V R',NBMA,ISURMA)
      CALL WKVECT('&&MASREP.SURMA1','V V R',NBMA,ISURM1)
      CALL WKVECT('&&MASREP.SURMA2','V V R',NBMA,ISURM2)
      CALL WKVECT('&&MASREP.SURMA3','V V R',NBMA,ISURM3)
      CALL WKVECT('&&MASREP.SURMA4','V V R',NBMA,ISURM4)
      CALL WKVECT('&&MASREP.SURMA5','V V R',NBMA,ISURM5)
      CALL WKVECT('&&MASREP.SURMA6','V V R',NBMA,ISURM6)
      IM = 0
      SURTOT = ZERO
      SURTXX = ZERO
      SURTXY = ZERO
      SURTXZ = ZERO
      SURTYY = ZERO
      SURTYZ = ZERO
      SURTZZ = ZERO
      DO 21 I = 1,NBGR
         CALL JELIRA(JEXNOM(MAGRMA,LIGRMA(I)),'LONUTI',NB,K8B)
         CALL JEVEUO(JEXNOM(MAGRMA,LIGRMA(I)),'L',LDGM)
         DO 23 IN = 0,NB-1
           IM = IM + 1
           CALL JELIRA(JEXNUM(MANOMA,ZI(LDGM+IN)),'LONMAX',NM,K8B)
           CALL JEVEUO(JEXNUM(MANOMA,ZI(LDGM+IN)),'L',LDNM)
           XC = ZERO
           YC = ZERO
           HC = ZERO
           DO 25 NN = 1, NM
             INOE = ZI(LDNM+NN-1)
             ZI(IDNO+INOE-1) = ZI(IDNO+INOE-1) + 1
             X(NN) = ZR(JCOOR+3*(INOE-1)+1-1)
             Y(NN) = ZR(JCOOR+3*(INOE-1)+2-1)
             Z(NN) = ZR(JCOOR+3*(INOE-1)+3-1)
             XC = XC + X(NN)
             YC = YC + Y(NN)
             HC = HC + Z(NN)
 25        CONTINUE
            XC = XC/NM
            YC = YC/NM
            HC = HC/NM

           IF (APPUI.EQ.1) THEN
              CALL ASSERT(.FALSE.)
           ELSEIF (APPUI.EQ.2)THEN
             A(1) = X(3) - X(1)
             A(2) = Y(3) - Y(1)
             A(3) = Z(3) - Z(1)
             IF (NM.EQ.3.OR.NM.EQ.6.OR.NM.EQ.7) THEN
               B(1) = X(2) - X(1)
               B(2) = Y(2) - Y(1)
               B(3) = Z(2) - Z(1)
             ELSEIF (NM.EQ.4.OR.NM.EQ.8.OR.NM.EQ.9) THEN
               B(1) = X(4) - X(2)
               B(2) = Y(4) - Y(2)
               B(3) = Z(4) - Z(2)
             ELSE
               CALL ASSERT(.FALSE.)
             ENDIF
             CALL PROVEC(A,B,C)
             SURF=DDOT(3,C,1,C,1)
             C(1)=C(1)/SQRT(SURF)
             C(2)=C(2)/SQRT(SURF)
             C(3)=C(3)/SQRT(SURF)
             ZR(ISURMA+IM-1) = SQRT(SURF)*0.5D0
           ELSE
             CALL ASSERT(.FALSE.)
           ENDIF
           IF (.NOT.LVALE) SURTOT = SURTOT + ZR(ISURMA+IM-1)
           IF (LFONC) THEN
             U(1) = XC
             U(2) = YC
             U(3) = HC
             NOMPAR(1) = 'X'
             NOMPAR(2) = 'Y'
             NOMPAR(3) = 'Z'
             CALL FOINTE('F ',ZK8(IFONGR+I-1),3,NOMPAR,U,COEF,IRET)
             ZR(ISURMA+IM-1) = ZR(ISURMA+IM-1)*COEF
           ELSE
             ZR(ISURMA+IM-1) = ZR(ISURMA+IM-1)*1.0D3*(Z0-HC)
           ENDIF
           IF (LVALE) THEN
             SURTOT = SURTOT + ZR(ISURMA+IM-1)
             ZR(ISURMA+IM-1) = ZR(ISURMA+IM-1)/NM
           ELSE
             SURTXX = SURTXX + ZR(ISURMA+IM-1)*C(1)*C(1)
             SURTXY = SURTXY + ZR(ISURMA+IM-1)*C(1)*C(2)
             SURTXZ = SURTXZ + ZR(ISURMA+IM-1)*C(1)*C(3)
             SURTYY = SURTYY + ZR(ISURMA+IM-1)*C(2)*C(2)
             SURTYZ = SURTYZ + ZR(ISURMA+IM-1)*C(2)*C(3)
             SURTZZ = SURTZZ + ZR(ISURMA+IM-1)*C(3)*C(3)
             ZR(ISURM1+IM-1) = ZR(ISURMA+IM-1)*C(1)*C(1)/NM
             ZR(ISURM2+IM-1) = ZR(ISURMA+IM-1)*C(1)*C(2)/NM
             ZR(ISURM4+IM-1) = ZR(ISURMA+IM-1)*C(1)*C(3)/NM
             ZR(ISURM3+IM-1) = ZR(ISURMA+IM-1)*C(2)*C(2)/NM
             ZR(ISURM5+IM-1) = ZR(ISURMA+IM-1)*C(2)*C(3)/NM
             ZR(ISURM6+IM-1) = ZR(ISURMA+IM-1)*C(3)*C(3)/NM
           ENDIF
 23      CONTINUE
 21   CONTINUE

      WRITE(IUNITE,1010) SURTXX,SURTXY,SURTXZ,SURTYY,SURTYZ,SURTZZ

C
C     CALCUL DES PONDERATIONS ELEMENTAIRES
C
      IM = 0
      DO 31 I = 1,NBGR
         CALL JELIRA(JEXNOM(MAGRMA,LIGRMA(I)),'LONUTI',NB,K8B)
         CALL JEVEUO(JEXNOM(MAGRMA,LIGRMA(I)),'L',LDGM)
         DO 33 IN = 0,NB-1
           IM = IM + 1
           CALL JELIRA(JEXNUM(MANOMA,ZI(LDGM+IN)),'LONMAX',NM,K8B)
           CALL JEVEUO(JEXNUM(MANOMA,ZI(LDGM+IN)),'L',LDNM)
           DO 35 NN = 1, NM
             DO 37 IJ = 1, NOEMAX
               IF (ZI(IDNO+IJ-1).EQ.0) GOTO 37
               IF (ZI(LDNM+NN-1).EQ.IJ) THEN
                 IF (LVALE) THEN
                   ZR(ICOEF+IJ-1)=ZR(ICOEF+IJ-1)+ZR(ISURMA+IM-1)/SURTOT
                 ELSE
                   ZR(ICOEXX+IJ-1) = ZR(ICOEXX+IJ-1) + ZR(ISURM1+IM-1)
                   ZR(ICOEXY+IJ-1) = ZR(ICOEXY+IJ-1) + ZR(ISURM2+IM-1)
                   ZR(ICOEXZ+IJ-1) = ZR(ICOEXZ+IJ-1) + ZR(ISURM4+IM-1)
                   ZR(ICOEYY+IJ-1) = ZR(ICOEYY+IJ-1) + ZR(ISURM3+IM-1)
                   ZR(ICOEYZ+IJ-1) = ZR(ICOEYZ+IJ-1) + ZR(ISURM5+IM-1)
                   ZR(ICOEZZ+IJ-1) = ZR(ICOEZZ+IJ-1) + ZR(ISURM6+IM-1)
                 ENDIF
               ENDIF
 37          CONTINUE
 35        CONTINUE
 33      CONTINUE
 31   CONTINUE
      NBMA = IM
C
      II = 0
      DO 51 IJ = 1, NOEMAX
         IF (ZI(IDNO+IJ-1).EQ.0) GOTO 51
         II = II + 1
         IF (LVALE) THEN
           R1 = RIGI(1)*ZR(ICOEF+IJ-1)
           R2 = RIGI(2)*ZR(ICOEF+IJ-1)
           R3 = RIGI(3)*ZR(ICOEF+IJ-1)
         ELSE
           R1 = ZR(ICOEXX+IJ-1)
           R2 = ZR(ICOEXY+IJ-1)
           R3 = ZR(ICOEYY+IJ-1)
         ENDIF
         IF (NDIM.EQ.3) THEN
           IF (LVALE) THEN
             R4 = RIGI(4)*ZR(ICOEF+IJ-1)
             R5 = RIGI(5)*ZR(ICOEF+IJ-1)
             R6 = RIGI(6)*ZR(ICOEF+IJ-1)
           ELSE
             R4 = ZR(ICOEXZ+IJ-1)
             R5 = ZR(ICOEYZ+IJ-1)
             R6 = ZR(ICOEZZ+IJ-1)
           ENDIF
         ELSE
           R4 = ZERO
           R5 = ZERO
           R6 = ZERO
         ENDIF
         CALL JENUNO(JEXNUM(MANONO,IJ),NOMNOE)
         RIGTO(6*(IJ-1)+1) = R1 + RIGTO(6*(IJ-1)+1)
         RIGTO(6*(IJ-1)+2) = R2 + RIGTO(6*(IJ-1)+2)
         RIGTO(6*(IJ-1)+3) = R3 + RIGTO(6*(IJ-1)+3)
         RIGTO(6*(IJ-1)+4) = R4 + RIGTO(6*(IJ-1)+4)
         RIGTO(6*(IJ-1)+5) = R5 + RIGTO(6*(IJ-1)+5)
         RIGTO(6*(IJ-1)+6) = R6 + RIGTO(6*(IJ-1)+6)
         R1 = RIGTO(6*(IJ-1)+1)
         R2 = RIGTO(6*(IJ-1)+2)
         R3 = RIGTO(6*(IJ-1)+3)
         R4 = RIGTO(6*(IJ-1)+4)
         R5 = RIGTO(6*(IJ-1)+5)
         R6 = RIGTO(6*(IJ-1)+6)
         RIGNOE(6*(II-1)+1) = R1
         RIGNOE(6*(II-1)+2) = R2
         RIGNOE(6*(II-1)+3) = R3
         RIGNOE(6*(II-1)+4) = R4
         RIGNOE(6*(II-1)+5) = R5
         RIGNOE(6*(II-1)+6) = R6
         TABNOE(II) = NOMNOE
 51   CONTINUE
      NBNO = II
C
      CALL JEDETR('&&MASREP.FONGRO')
      CALL JEDETR('&&MASREP.COENO')
      CALL JEDETR('&&MASREP.COENXX')
      CALL JEDETR('&&MASREP.COENXY')
      CALL JEDETR('&&MASREP.COENYY')
      CALL JEDETR('&&MASREP.COENXZ')
      CALL JEDETR('&&MASREP.COENYZ')
      CALL JEDETR('&&MASREP.COENZZ')
      CALL JEDETR('&&MASREP.PARNO')
      CALL JEDETR('&&MASREP.SURMAI')
      CALL JEDETR('&&MASREP.SURMA1')
      CALL JEDETR('&&MASREP.SURMA2')
      CALL JEDETR('&&MASREP.SURMA3')
      CALL JEDETR('&&MASREP.SURMA4')
      CALL JEDETR('&&MASREP.SURMA5')
      CALL JEDETR('&&MASREP.SURMA6')
C
      CALL JEDEMA()
 1010 FORMAT(' MXX= ',1PE12.5,' MXY= ',1PE12.5,' MXZ= ',1PE12.5/,
     &       ' MYY= ',1PE12.5,' MYZ= ',1PE12.5,' MZZ= ',1PE12.5)
      END
