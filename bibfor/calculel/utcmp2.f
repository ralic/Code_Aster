      SUBROUTINE UTCMP2(NOMGD,MCFAC,IOCC,DIM,NOMCMP,NUMCMP,NBCMP)
      IMPLICIT   NONE
      INTEGER IOCC,DIM,NBCMP,NUMCMP(*)
      CHARACTER*(*) NOMGD,MCFAC,NOMCMP(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 04/10/2010   AUTEUR PELLET J.PELLET 
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
C BUT :  SCRUTER LE MOT CLE MFAC/NOM_CMP ET RENDRE LA LISTE DES CMPS
C -----
C  ARGUMENTS :
C  -----------
C  NOMGD  IN  K8 : NOM DE LA GRANDEUR CONCERNEE
C  MCFAC  IN  K* : NOM DU MOT CLE FACTEUR A SCRUTER
C  IOCC   IN  I  : NUMERO DE L'OCCURRENCE DE MCFAC
C  DIM    IN  I  : LONGUEUR DES TABLEAUX NOMCMP ET NUMCMP

C  NOMCMP(*) OUT K8 : NOMS DES COMPOSANTES TROUVEES
C  NUMCMP(*) OUT I  : NUMEROS DES COMPOSANTES TROUVEES (SI VARI_R)
C  NBCMP     OUT I  : NOMBRE DE CMPS TROUVEES
C
C ----------------------------------------------------------------------
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
      CHARACTER*32 JEXNOM,JEXNUM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      INTEGER IBID,N2,JNOCP,I,J,II,NUCMP,IRET,JNOCMP,LGNCMP
      CHARACTER*8 K8B,NOCMP
      CHARACTER*24 VALK(2)
      CHARACTER*16 NOMCMD
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL GETRES(K8B,K8B,NOMCMD)


      CALL GETVTX(MCFAC,'NOM_CMP',IOCC,1,0,K8B,N2)
      NBCMP=-N2
      CALL ASSERT(DIM.GE.NBCMP)

      CALL GETVTX(MCFAC,'NOM_CMP',IOCC,1,NBCMP,NOMCMP,N2)


      IF (NOMGD(1:6).EQ.'VARI_R') THEN
C     -----------------------------------------
        DO 10 I=1,NBCMP
          NOCMP=NOMCMP(I)
          CALL ASSERT(NOCMP(1:1).EQ.'V')
          CALL LXLIIS(NOCMP(2:8),NUCMP,IRET)
          CALL ASSERT(IRET.EQ.0)
          NUMCMP(I)=NUCMP
   10   CONTINUE


C     -- CAS NOMGD /= VARI_R
C     -----------------------
      ELSE
        CALL JEVEUO(JEXNOM('&CATA.GD.NOMCMP',NOMGD),'L',JNOCMP)
        CALL JELIRA(JEXNOM('&CATA.GD.NOMCMP',NOMGD),'LONMAX',LGNCMP,K8B)
        CALL KNINCL(8,NOMCMP,NBCMP,ZK8(JNOCMP),LGNCMP,IRET)
        IF (IRET.NE.0) CALL U2MESK('F','CALCULEL5_6',1,NOMGD)
      ENDIF



      CALL JEDEMA()
      END
