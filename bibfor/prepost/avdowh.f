      SUBROUTINE AVDOWH( NBVEC, NBORDR, NOMMAT, NOMCRI, NCYCL, GDEQ,
     &                   DOMEL, NRUPT )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 18/09/2007   AUTEUR DURAND C.DURAND 
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
C RESPONSABLE F1BHHAJ J.ANGLES
      IMPLICIT     NONE
      INTEGER      NBVEC, NBORDR, NCYCL(NBVEC)
      REAL*8       GDEQ(NBVEC*NBORDR)
      REAL*8       NRUPT(NBVEC*NBORDR), DOMEL(NBVEC*NBORDR)
      CHARACTER*8  NOMMAT
      CHARACTER*16 NOMCRI
C ----------------------------------------------------------------------
C BUT: CALCULER LE DOMMAGE ELEMENTAIRE DE WOHLER POUR TOUS LES CYCLES
C      ELEMETAIRES DE CHAQUE VECTEUR NORMAL.
C ----------------------------------------------------------------------
C ARGUMENTS :
C  NBVEC    IN   I  : NOMBRE DE VECTEURS NORMAUX.
C  NBORDR   IN   I  : NOMBRE DE NUMEROS D'ORDRE.
C  NOMMAT   IN   K  : NOM DU MATERIAU.
C  NOMCRI   IN   K  : NOM DU CRITERE.
C  NCYCL    IN   I  : NOMBRE DE CYCLES ELEMENTAIRES POUR TOUS LES
C                     VECTEURS NORMAUX.
C  GDEQ     IN   R  : VECTEUR CONTENANT LES VALEURS DE LA GRANDEUR
C                     EQUIVALENTE (SIGEQ OU EPSEQ), POUR TOUS LES SOUS
C                     CYCLES DE CHAQUE VECTEUR NORMAL.
C  DOMEL    OUT  R  : VECTEUR CONTENANT LES VALEURS DES DOMMAGES
C                     ELEMENTAIRES, POUR TOUS LES SOUS CYCLES
C                     DE CHAQUE VECTEUR NORMAL.
C  NRUPT    OUT  R  : VECTEUR CONTENANT LES NOMBRES DE CYCLES
C                     ELEMENTAIRES, POUR TOUS LES SOUS CYCLES
C                     DE CHAQUE VECTEUR NORMAL.
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ------------------------------------------------------------------
      INTEGER       I, IVECT, ICYCL, ADRS
      REAL*8        R8MAEM
      CHARACTER*2   CODRET
      CHARACTER*16  PHENOM
      LOGICAL       LIMIT
C     ------------------------------------------------------------------

C234567                                                              012

      CALL JEMARQ()

      CALL RCCOME( NOMMAT, 'FATIGUE', PHENOM, CODRET )
      IF ( CODRET .EQ. 'NO' ) CALL U2MESS('F','FATIGUE1_24')

      IF ( NOMCRI(1:16) .EQ. 'FATESOCI_MODI_AV' ) THEN
         CALL RCPARE( NOMMAT, 'FATIGUE', 'MANSON_C', CODRET )
         DO 10 IVECT=1, NBVEC
            DO 20 ICYCL=1, NCYCL(IVECT)
               ADRS = (IVECT-1)*NBORDR + ICYCL

               IF ( CODRET .EQ. 'OK' ) THEN
                  CALL RCVALE(NOMMAT,'FATIGUE',1,'EPSI    ',GDEQ(ADRS),
     &                     1,'MANSON_C',NRUPT(ADRS),CODRET,'F')

                  CALL LIMEND( NOMMAT, GDEQ(ADRS), 'MANSON_C', LIMIT )
                  IF (LIMIT) THEN
                     NRUPT(ADRS)=R8MAEM()
                  ELSE
                     CALL RCVALE(NOMMAT,'FATIGUE',1,'EPSI    ',
     &                  GDEQ(ADRS),1,'MANSON_C',NRUPT(ADRS),CODRET,'F')
                  ENDIF
               ENDIF

               DOMEL(ADRS) = 1.0D0/NRUPT(ADRS)
               NRUPT(ADRS) = NINT(NRUPT(ADRS))

 20         CONTINUE
 10      CONTINUE

      ELSE

         CALL RCPARE( NOMMAT, 'FATIGUE', 'WOHLER', CODRET )
         DO 30 IVECT=1, NBVEC
            DO 40 ICYCL=1, NCYCL(IVECT)
               ADRS = (IVECT-1)*NBORDR + ICYCL

               IF ( CODRET .EQ. 'OK' ) THEN
                  CALL LIMEND( NOMMAT, GDEQ(ADRS), 'WOHLER', LIMIT)
                  IF (LIMIT) THEN
                     NRUPT(ADRS)=R8MAEM()
                  ELSE
                     CALL RCVALE(NOMMAT,'FATIGUE',1,'SIGM    ',
     &                  GDEQ(ADRS),1,'WOHLER  ',NRUPT(ADRS),CODRET,'F')
                  ENDIF
               ENDIF

               DOMEL(ADRS) = 1.0D0/NRUPT(ADRS)
               NRUPT(ADRS) = NINT(NRUPT(ADRS))

 40         CONTINUE
 30      CONTINUE

      ENDIF

      CALL JEDEMA()

      END
