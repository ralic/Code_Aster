      SUBROUTINE OPS014( ICMD , ICOND, IER )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER            ICMD , ICOND, IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 24/10/2006   AUTEUR DURAND C.DURAND 
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
C     PROCEDURE "INCLUDE_MATERIAU" PERMET LE DEBRANCHEMENT DE LA LECTURE
C     VERS UNE AUTRE UNITE LOGIQUE VERS UN FICHIER DU CATALOGUE MATERIAU
C     ASTER.
C     ------------------------------------------------------------------
      CHARACTER*16     CBID, NOMCMD
      CHARACTER*32     NAME
C     ------------------------------------------------------------------
      CHARACTER*8      UNITE,NOMSYM
      PARAMETER             (MXFILE=30)
      COMMON  /SUCC00/ UNITE(MXFILE), NAME(MXFILE)
      COMMON  /SUCN00/ IPASS,IFILE,JCMD
C     ------------------------------------------------------------------
      CHARACTER*72     NOMAFN,TYPMOD,VARIAN,TYPVAL
      INTEGER          LRM,LTY,LNA,LTYV
      CHARACTER*128    REP
      CHARACTER*128    NOMFM
      INTEGER          IOCCF
      CHARACTER*16     MCF,MCSMCS,MCSTPX
      REAL*8           TPEXT
      INTEGER          JARGV,ARGC
      CHARACTER*14     VALC
      CHARACTER*129    DIROU
      INTEGER          LDIROU
      CHARACTER*2      CUNIT
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
C     ------------------------------------------------------------------
      IF ((ICOND .EQ. 0).OR.(ICOND .EQ. 1)) THEN
        GOTO 9999
      ENDIF

      IF (ICOND .NE. -1) THEN
        CALL U2MESS('E','SUPERVIS_33')
        IER = 1
        GOTO 9999
      ENDIF

      CALL JEMARQ()
C
      IF ( IPASS .NE. 80191  ) THEN
         IPASS   = 80191
         IFILE   = 0
         NAME(1) = '   '
      ENDIF
C
      JCMD = ICMD
      CALL GETRES(CBID,CBID,NOMCMD)
      CALL LXINFU(IREAD,LREC,IWRITE,NOMSYM )

      IFILE = IFILE + 1
C     --- ON EMPILE LE NOM SYMBOLIQUE DE L'UNITE DE LECTURE COURANTE ---
      UNITE(IFILE) = NOMSYM
C
C     --- DEFINITION DE LA NOUVELLE UNITE LOGIQUE ---
      NOMSYM = 'INCMAT'
      IUNIT=99
      CALL LXUNIT(IUNIT,LREC,0,NOMSYM )
C
C     --- ANNULATION DE LA PROCEDURE ---
      CALL SMCDEL(ICMD,0,IER)
      ICMD = ICMD - 1

      CALL JEDEMA()

 9999 CONTINUE
      END
