      SUBROUTINE IRGAGE(NCMPMX,NCMPGD,NBCMPT,NUCMPU,
     &                  NBVSCA,IPVSCA,NBVVEC,IPVVEC)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER           NCMPMX,       NBCMPT,NUCMPU(*)
      CHARACTER*(*)            NCMPGD(*)
      INTEGER           NBVSCA,IPVSCA(500),NBVVEC,IPVVEC(500,3)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_20
C--------------------------------------------------------------------
C BUT: PARCOURS ET RECONNAISSANCE DES COMPOSANTES D'UNE GRANDEUR ASTER
C      A IMPRIMER POUR DETERMINER LE TYPE (SCALAIRE OU VECTORIEL), LE
C      NOM ET LE NOMBRE DES VARIABLES ENSIGHT CORRESPONDANTES
C ENTREE:
C   NCMPMX: NOMBRE MAXI DE CMP DE LA GRANDEUR ASTER
C   NCMPGD: NOMS DES CMP DE LA GRANDEUR ASTER
C SORTIE:
C   NBVSCA: NBRE DE VARIABLES SCALAIRES ENSIGHT DEFINIES PAR LES CMP
C   IPVSCA: POSITION DE CES VARIABLES SCALAIRES DANS LA GRANDEUR ASTER
C   NBVVEC: NBRE DE VARIABLES VECTORIELLES ENSIGHT DEFINIES PAR LES CMP
C   IPVVEC: POSITION DES 3 COMPOSANTES DE CES VARIABLES VECTORIELLES
C           DANS LA GRANDEUR ASTER
C
C     ----------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL
      CHARACTER*8  ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
C     ------------------------------------------------------------------
      INTEGER ITRDEP(3),IVTDEP(3),IRODEP(3),NTRDEP,NRODEP,IVARI(500)
      INTEGER ISIGM(6),ISIGI(6),ISIGS(6),IEPSM(6),IEPSI(6),IEPSS(6)
      INTEGER NBSIGM,NBSIGI,NBSIGS,NBEPSM,NBEPSI,NBEPSS
      INTEGER IFLUX(3),IVFLUX(3),IFLUI(3),IVFLUI(3),IFLUS(3),IVFLUS(3)
      INTEGER NBFLUX,NBFLUI,NBFLUS,ITEMP,ITEMI,ITEMS,IPRES,NBCOMP,ICOMP
      LOGICAL LDEPL(6),LTEMP,LTEMI,LTEMS,LPRES,L2COMP
      LOGICAL LFLUX(3),LFLUI(3),LFLUS(3)
      LOGICAL LSIGM(6),LSIGI(6),LSIGS(6),LEPSM(6),LEPSI(6),LEPSS(6)
C
C  --- INITIALISATIONS ----
      NTRDEP = 0
      NRODEP = 0
      NBFLUX = 0
      NBFLUI = 0
      NBFLUS = 0
      NBSIGM = 0
      NBSIGI = 0
      NBSIGS = 0
      NBEPSM = 0
      NBEPSI = 0
      NBEPSS = 0
      NBVARI = 0
      NBVSCA = 0
      NBVVEC = 0
      DO 1 I=1,6
        LDEPL(I)=.FALSE.
        LSIGM(I)=.FALSE.
        LSIGI(I)=.FALSE.
        LSIGS(I)=.FALSE.
        LEPSM(I)=.FALSE.
        LEPSI(I)=.FALSE.
        LEPSS(I)=.FALSE.
 1    CONTINUE
      DO 2 I=1,3
        LFLUX(I)=.FALSE.
        LFLUI(I)=.FALSE.
        LFLUS(I)=.FALSE.
 2    CONTINUE
      LTEMP=.FALSE.
      LTEMI=.FALSE.
      LTEMS=.FALSE.
      LPRES=.FALSE.
      L2COMP=.FALSE.
C
C     --- RECONNAISSANCE, DECOMPTE DES COMPOSANTES DE LA GRANDEUR ASTER
      NBCOMP=NCMPMX
      IF(NBCMPT.GT.0) NBCOMP=NBCMPT
      DO 9 ICMAE=1,NBCOMP
        ICOMP=ICMAE
        IF(NBCMPT.GT.0) ICOMP=NUCMPU(ICMAE)
        IF (NCMPGD(ICOMP).EQ.'DX') THEN
          IF(LDEPL(1)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LDEPL(1)=.TRUE.
          NTRDEP= NTRDEP+1
          ITRDEP(NTRDEP)=ICOMP
          IVTDEP(1)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'DY') THEN
          IF(LDEPL(2)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LDEPL(2)=.TRUE.
          NTRDEP= NTRDEP+1
          ITRDEP(NTRDEP)=ICOMP
          IVTDEP(2)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'DZ') THEN
          IF(LDEPL(3)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LDEPL(3)=.TRUE.
          NTRDEP= NTRDEP+1
          ITRDEP(NTRDEP)=ICOMP
          IVTDEP(3)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'DRX') THEN
          IF(LDEPL(4)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LDEPL(4)=.TRUE.
          NRODEP= NRODEP+1
          IRODEP(NRODEP)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'DRY') THEN
          IF(LDEPL(5)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LDEPL(5)=.TRUE.
          NRODEP= NRODEP+1
          IRODEP(NRODEP)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'DRZ') THEN
          IF(LDEPL(6)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LDEPL(6)=.TRUE.
          NRODEP= NRODEP+1
          IRODEP(NRODEP)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'FLUX_INF') THEN
          IF(LFLUI(1)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LFLUI(1)=.TRUE.
          NBFLUI= NBFLUI+1
          IFLUI(NBFLUI)=ICOMP
          IVFLUI(1)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'FLUY_INF') THEN
          IF(LFLUI(2)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LFLUI(2)=.TRUE.
          NBFLUI= NBFLUI+1
          IFLUI(NBFLUI)=ICOMP
          IVFLUI(2)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'FLUZ_INF') THEN
          IF(LFLUI(3)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LFLUI(3)=.TRUE.
          NBFLUI= NBFLUI+1
          IFLUI(NBFLUI)=ICOMP
          IVFLUI(3)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'FLUX_SUP') THEN
          IF(LFLUS(1)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LFLUS(1)=.TRUE.
          NBFLUS= NBFLUS+1
          IFLUS(NBFLUS)=ICOMP
          IVFLUS(1)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'FLUY_SUP') THEN
          IF(LFLUS(2)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LFLUS(2)=.TRUE.
          NBFLUS= NBFLUS+1
          IFLUS(NBFLUS)=ICOMP
          IVFLUS(2)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'FLUZ_SUP') THEN
          IF(LFLUS(3)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LFLUS(3)=.TRUE.
          NBFLUS= NBFLUS+1
          IFLUS(NBFLUS)=ICOMP
          IVFLUS(3)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'FLUX') THEN
          IF(LFLUX(1)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LFLUX(1)=.TRUE.
          NBFLUX= NBFLUX+1
          IFLUX(NBFLUX)=ICOMP
          IVFLUX(1)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'FLUY') THEN
          IF(LFLUX(2)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LFLUX(2)=.TRUE.
          NBFLUX= NBFLUX+1
          IFLUX(NBFLUX)=ICOMP
          IVFLUX(2)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'FLUZ') THEN
          IF(LFLUX(3)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LFLUX(3)=.TRUE.
          NBFLUX= NBFLUX+1
          IFLUX(NBFLUX)=ICOMP
          IVFLUX(3)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'TEMP_INF') THEN
          IF(LTEMI) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LTEMI=.TRUE.
          ITEMI=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'TEMP_SUP') THEN
          IF(LTEMS) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LTEMS=.TRUE.
          ITEMS=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'TEMP') THEN
          IF(LTEMP) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LTEMP=.TRUE.
          ITEMP=ICOMP
        ELSEIF (NCMPGD(ICOMP)(1:4).EQ.'PRES') THEN
          IF(LPRES) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LPRES=.TRUE.
          IPRES=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'SIXX_INF') THEN
          IF(LSIGI(1)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LSIGI(1)=.TRUE.
          NBSIGI= NBSIGI+1
          ISIGI(NBSIGI)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'SIYY_INF') THEN
          IF(LSIGI(2)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LSIGI(2)=.TRUE.
          NBSIGI= NBSIGI+1
          ISIGI(NBSIGI)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'SIZZ_INF') THEN
          IF(LSIGI(3)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LSIGI(3)=.TRUE.
          NBSIGI= NBSIGI+1
          ISIGI(NBSIGI)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'SIXY_INF') THEN
          IF(LSIGI(4)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LSIGI(4)=.TRUE.
          NBSIGI= NBSIGI+1
          ISIGI(NBSIGI)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'SIXZ_INF')  THEN
          IF(LSIGI(5)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LSIGI(5)=.TRUE.
          NBSIGI= NBSIGI+1
          ISIGI(NBSIGI)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'SIYZ_INF') THEN
          IF(LSIGI(6)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LSIGI(6)=.TRUE.
          NBSIGI= NBSIGI+1
          ISIGI(NBSIGI)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'SIXX_SUP') THEN
          IF(LSIGS(1)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LSIGS(1)=.TRUE.
          NBSIGS= NBSIGS+1
          ISIGS(NBSIGS)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'SIYY_SUP') THEN
          IF(LSIGS(2)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LSIGS(2)=.TRUE.
          NBSIGS= NBSIGS+1
          ISIGS(NBSIGS)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'SIZZ_SUP') THEN
          IF(LSIGS(3)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LSIGS(3)=.TRUE.
          NBSIGS= NBSIGS+1
          ISIGS(NBSIGS)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'SIXY_SUP') THEN
          IF(LSIGS(4)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LSIGS(4)=.TRUE.
          NBSIGS= NBSIGS+1
          ISIGS(NBSIGS)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'SIXZ_SUP')  THEN
          IF(LSIGS(5)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LSIGS(5)=.TRUE.
          NBSIGS= NBSIGS+1
          ISIGS(NBSIGS)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'SIYZ_SUP') THEN
          IF(LSIGS(6)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LSIGS(6)=.TRUE.
          NBSIGS= NBSIGS+1
          ISIGS(NBSIGS)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'SIXX') THEN
          IF(LSIGM(1)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LSIGM(1)=.TRUE.
          NBSIGM= NBSIGM+1
          ISIGM(NBSIGM)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'SIYY') THEN
          IF(LSIGM(2)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LSIGM(2)=.TRUE.
          NBSIGM= NBSIGM+1
          ISIGM(NBSIGM)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'SIZZ') THEN
          IF(LSIGM(3)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LSIGM(3)=.TRUE.
          NBSIGM= NBSIGM+1
          ISIGM(NBSIGM)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'SIXY') THEN
          IF(LSIGM(4)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LSIGM(4)=.TRUE.
          NBSIGM= NBSIGM+1
          ISIGM(NBSIGM)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'SIXZ')  THEN
          IF(LSIGM(5)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LSIGM(5)=.TRUE.
          NBSIGM= NBSIGM+1
          ISIGM(NBSIGM)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'SIYZ') THEN
          IF(LSIGM(6)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LSIGM(6)=.TRUE.
          NBSIGM= NBSIGM+1
          ISIGM(NBSIGM)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'EPXX_INF') THEN
          IF(LEPSI(1)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LEPSI(1)=.TRUE.
          NBEPSI= NBEPSI+1
          IEPSI(NBEPSI)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'EPYY_INF') THEN
          IF(LEPSI(2)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LEPSI(2)=.TRUE.
          NBEPSI= NBEPSI+1
          IEPSI(NBEPSI)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'EPZZ_INF') THEN
          IF(LEPSI(3)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LEPSI(3)=.TRUE.
          NBEPSI= NBEPSI+1
          IEPSI(NBEPSI)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'EPXY_INF') THEN
          IF(LEPSI(4)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LEPSI(4)=.TRUE.
          NBEPSI= NBEPSI+1
          IEPSI(NBEPSI)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'EPXZ_INF') THEN
          IF(LEPSI(5)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LEPSI(5)=.TRUE.
          NBEPSI= NBEPSI+1
          IEPSI(NBEPSI)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'EPYZ_INF') THEN
          IF(LEPSI(6)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LEPSI(6)=.TRUE.
          NBEPSI= NBEPSI+1
          IEPSI(NBEPSI)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'EPXX_SUP') THEN
          IF(LEPSS(1)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LEPSS(1)=.TRUE.
          NBEPSS= NBEPSS+1
          IEPSS(NBEPSS)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'EPYY_SUP') THEN
          IF(LEPSS(2)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LEPSS(2)=.TRUE.
          NBEPSS= NBEPSS+1
          IEPSS(NBEPSS)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'EPZZ_SUP') THEN
          IF(LEPSS(3)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LEPSS(3)=.TRUE.
          NBEPSS= NBEPSS+1
          IEPSS(NBEPSS)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'EPXY_SUP') THEN
          IF(LEPSS(4)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LEPSS(4)=.TRUE.
          NBEPSS= NBEPSS+1
          IEPSS(NBEPSS)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'EPXZ_SUP') THEN
          IF(LEPSS(5)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LEPSS(5)=.TRUE.
          NBEPSS= NBEPSS+1
          IEPSS(NBEPSS)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'EPYZ_SUP') THEN
          IF(LEPSS(6)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LEPSS(6)=.TRUE.
          NBEPSS= NBEPSS+1
          IEPSS(NBEPSS)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'EPXX') THEN
          IF(LEPSM(1)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LEPSM(1)=.TRUE.
          NBEPSM= NBEPSM+1
          IEPSM(NBEPSM)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'EPYY') THEN
          IF(LEPSM(2)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LEPSM(2)=.TRUE.
          NBEPSM= NBEPSM+1
          IEPSM(NBEPSM)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'EPZZ') THEN
          IF(LEPSM(3)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LEPSM(3)=.TRUE.
          NBEPSM= NBEPSM+1
          IEPSM(NBEPSM)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'EPXY') THEN
          IF(LEPSM(4)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LEPSM(4)=.TRUE.
          NBEPSM= NBEPSM+1
          IEPSM(NBEPSM)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'EPXZ') THEN
          IF(LEPSM(5)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LEPSM(5)=.TRUE.
          NBEPSM= NBEPSM+1
          IEPSM(NBEPSM)=ICOMP
        ELSEIF (NCMPGD(ICOMP).EQ.'EPYZ') THEN
          IF(LEPSM(6)) THEN
            L2COMP=.TRUE.
            GO TO 9
          ENDIF
          LEPSM(6)=.TRUE.
          NBEPSM= NBEPSM+1
          IEPSM(NBEPSM)=ICOMP
        ELSE
          NBVARI= NBVARI+1
          IF (NBVARI.GT.500) CALL U2MESS('F','PREPOST2_49')
          IVARI(NBVARI)= ICOMP
        ENDIF
 9    CONTINUE
      IF(L2COMP) CALL U2MESS('A','PREPOST2_50')
C
C     --- DEFINITION DES VARIABLES SCALAIRES ET VECTORIELLES ENSIGHT:
C         TYPE ET NOMBRE DE VARIABLES DE CHAQUE TYPE, NOMS DES
C         COMPOSANTES DES VARIABLES, POSITION DANS LA GRANDEUR ASTER
      IF(NTRDEP.GT.0) THEN
        IF(NTRDEP.EQ.3) THEN
          NBVVEC = NBVVEC + 1
          IPVVEC(NBVVEC,1) = IVTDEP(1)
          IPVVEC(NBVVEC,2) = IVTDEP(2)
          IPVVEC(NBVVEC,3) = IVTDEP(3)
        END IF
        IF((NTRDEP.LT.3).OR.
     &       ((NBCMPT.EQ.0).AND.(NRODEP.GT.0))) THEN
          DO 10 I=1,NTRDEP
            NBVSCA = NBVSCA + 1
            IPVSCA(NBVSCA) = ITRDEP(I)
 10       CONTINUE
        ENDIF
      ENDIF
      IF(NRODEP.GT.0) THEN
        DO 11 I=1,NRODEP
          NBVSCA = NBVSCA + 1
          IPVSCA(NBVSCA) = IRODEP(I)
 11     CONTINUE
      ENDIF
      IF(NBFLUX.GT.0) THEN
        IF(NBFLUX.EQ.3) THEN
          NBVVEC = NBVVEC + 1
          IPVVEC(NBVVEC,1) = IVFLUX(1)
          IPVVEC(NBVVEC,2) = IVFLUX(2)
          IPVVEC(NBVVEC,3) = IVFLUX(3)
C        ELSE
         ENDIF
          DO 12 I=1,NBFLUX
            NBVSCA = NBVSCA + 1
            IPVSCA(NBVSCA) = IFLUX(I)
 12       CONTINUE
C        ENDIF
      ENDIF
      IF(NBFLUI.GT.0) THEN
        IF(NBFLUI.EQ.3) THEN
          NBVVEC = NBVVEC + 1
          IPVVEC(NBVVEC,1) = IVFLUI(1)
          IPVVEC(NBVVEC,2) = IVFLUI(2)
          IPVVEC(NBVVEC,3) = IVFLUI(3)
        ELSE
          DO 13 I=1,NBFLUI
            NBVSCA = NBVSCA + 1
            IPVSCA(NBVSCA) = IFLUI(I)
 13       CONTINUE
        ENDIF
      ENDIF
      IF(NBFLUS.GT.0) THEN
        IF(NBFLUS.EQ.3) THEN
          NBVVEC = NBVVEC + 1
          IPVVEC(NBVVEC,1) = IVFLUS(1)
          IPVVEC(NBVVEC,2) = IVFLUS(2)
          IPVVEC(NBVVEC,3) = IVFLUS(3)
        ELSE
          DO 14 I=1,NBFLUS
            NBVSCA = NBVSCA + 1
            IPVSCA(NBVSCA) = IFLUS(I)
 14       CONTINUE
        ENDIF
      ENDIF
      IF(LTEMP) THEN
        NBVSCA = NBVSCA + 1
        IPVSCA(NBVSCA) = ITEMP
      ENDIF
      IF(LTEMI) THEN
        NBVSCA = NBVSCA + 1
        IPVSCA(NBVSCA) = ITEMI
      ENDIF
      IF(LTEMS) THEN
        NBVSCA = NBVSCA + 1
        IPVSCA(NBVSCA) = ITEMS
      ENDIF
      IF(LPRES) THEN
        NBVSCA = NBVSCA + 1
        IPVSCA(NBVSCA) = IPRES
      ENDIF
      IF(NBSIGM.GT.0) THEN
        DO 15 I=1,NBSIGM
          NBVSCA = NBVSCA + 1
          IPVSCA(NBVSCA) = ISIGM(I)
 15    CONTINUE
      ENDIF
      IF(NBSIGS.GT.0) THEN
        DO 16 I=1,NBSIGS
          NBVSCA = NBVSCA + 1
          IPVSCA(NBVSCA) = ISIGS(I)
 16     CONTINUE
      ENDIF
      IF(NBSIGI.GT.0) THEN
        DO 17 I=1,NBSIGI
          NBVSCA = NBVSCA + 1
          IPVSCA(NBVSCA) = ISIGI(I)
 17     CONTINUE
      ENDIF
      IF(NBEPSM.GT.0) THEN
        DO 18 I=1,NBEPSM
          NBVSCA = NBVSCA + 1
          IPVSCA(NBVSCA) = IEPSM(I)
 18     CONTINUE
      ENDIF
      IF(NBEPSS.GT.0) THEN
        DO 19 I=1,NBEPSS
          NBVSCA = NBVSCA + 1
          IPVSCA(NBVSCA) = IEPSS(I)
 19     CONTINUE
      ENDIF
      IF(NBEPSI.GT.0) THEN
        DO 20 I=1,NBEPSI
          NBVSCA = NBVSCA + 1
          IPVSCA(NBVSCA) = IEPSI(I)
 20     CONTINUE
      ENDIF
      IF(NBVARI.NE.0) THEN
        DO 21 I=1,NBVARI
          NBVSCA = NBVSCA + 1
          IPVSCA(NBVSCA) = IVARI(I)
 21     CONTINUE
      ENDIF
      END
