      SUBROUTINE JEVEUO ( NOMLU , CEL  , JCTAB )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 13/11/2012   AUTEUR COURTOIS M.COURTOIS 
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
C RESPONSABLE LEFEBVRE J-P.LEFEBVRE
      IMPLICIT NONE
C
      INCLUDE 'jeveux.h'
      INCLUDE 'jeveux_private.h'
      INTEGER                            JCTAB
      CHARACTER*(*)       NOMLU , CEL
C     ==================================================================
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON 
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
C     ------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER IBACOL ,IBLONO ,INAT ,INATB ,IXDESO ,IXIADD ,IXLONO 
      INTEGER JCARA ,JDATE ,JDOCU ,JGENR ,JHCOD ,JIADD ,JIADM 
      INTEGER JLONG ,JLONO ,JLTYP ,JLUTI ,JMARQ ,JORIG ,JRNOM 
      INTEGER JTYPE ,LONOI ,LTYPI ,N 
C-----------------------------------------------------------------------
      PARAMETER  ( N = 5 )
      COMMON /JIATJE/  JLTYP(N), JLONG(N), JDATE(N), JIADD(N), JIADM(N),
     &                 JLONO(N), JHCOD(N), JCARA(N), JLUTI(N), JMARQ(N)
C
      COMMON /JKATJE/  JGENR(N), JTYPE(N), JDOCU(N), JORIG(N), JRNOM(N)
      INTEGER          NUMATR
      COMMON /IDATJE/  NUMATR
C     ------------------------------------------------------------------
      INTEGER          ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      COMMON /IATCJE/  ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      INTEGER         IZR,IZC,IZL,IZK8,IZK16,IZK24,IZK32,IZK80
      EQUIVALENCE    (IZR,ZR),(IZC,ZC),(IZL,ZL),(IZK8,ZK8),(IZK16,ZK16),
     &               (IZK24,ZK24),(IZK32,ZK32),(IZK80,ZK80)
C ----------------------------------------------------------------------
      CHARACTER*1      GENRI , TYPEI ,KCEL
      CHARACTER*8      NOML8
      CHARACTER*32     NOML32
      INTEGER          ICRE , IRET
      INTEGER         IDDESO     , IDIADD          ,
     &               IDLONO          
      PARAMETER    (  IDDESO = 1 , IDIADD = 2  ,
     &               IDLONO = 8   )
C     ==================================================================
      NOML32 = NOMLU
      NOML8  = NOML32(25:32)
      KCEL   = CEL
      IF ( KCEL .NE. 'L' .AND. KCEL .NE. 'E' ) THEN
         CALL U2MESK('F','JEVEUX1_27',1,KCEL)
      ENDIF
C
      ICRE = 0
      CALL JJVERN ( NOML32 , ICRE , IRET )
      INAT  = IRET
      INATB = IRET
      GOTO ( 10 , 20 , 30 ) ,IRET+1
 10   CONTINUE
C ----   IRET = 0
         CALL U2MESK('F','JEVEUX_26',1,NOML32(1:24))
         GOTO 100
 20   CONTINUE
C ----   IRET = 1
         GENRI =  GENR( JGENR(ICLAOS) + IDATOS )
         TYPEI =  TYPE( JTYPE(ICLAOS) + IDATOS )
         LTYPI =  LTYP( JLTYP(ICLAOS) + IDATOS )
         IF ( GENRI .EQ. 'N' ) THEN
           CALL U2MESK('F','JEVEUX1_20',1,NOML32)
         ENDIF
         GOTO 100
 30   CONTINUE
C ----   IRET = 2
         CALL JJALLC ( ICLACO , IDATCO , CEL , IBACOL )
         IXIADD = ISZON ( JISZON + IBACOL + IDIADD )
         IXDESO = ISZON ( JISZON + IBACOL + IDDESO )
         IF ( NOML8 .EQ. '$$XATR  ') THEN
            IXLONO = NUMATR
            IBLONO = IADM ( JIADM(ICLACO) + 2*IXLONO-1 )
            GENRI  = GENR ( JGENR(ICLACO) + IXLONO )
            LTYPI  = LTYP ( JLTYP(ICLACO) + IXLONO )
            LONOI  = LONO ( JLONO(ICLACO) + IXLONO ) * LTYPI
            CALL JXLOCS ( ZI, GENRI, LTYPI, LONOI, IBLONO, .FALSE.,
     &                    JCTAB)
            GOTO 1000
         ELSE
           IF ( NOML8 .NE. '        ') THEN
             INAT  = 3
             CALL JJCROC ( NOML8 , ICRE )
C            ------ CAS D'UN OBJET DE COLLECTION  ------
             IF ( IXIADD .NE. 0 ) INATB = 3
           ELSE
             IF ( IXIADD .NE. 0 ) THEN
C            ----------- COLLECTION DISPERSEE
                CALL U2MESK('F','JEVEUX1_21',1,NOML32)
             ENDIF
           ENDIF
           GENRI =  GENR( JGENR(ICLACO) + IXDESO )
           TYPEI =  TYPE( JTYPE(ICLACO) + IXDESO )
           LTYPI =  LTYP( JLTYP(ICLACO) + IXDESO )
         ENDIF
 100  CONTINUE
      CALL JJALTY ( TYPEI , LTYPI , CEL , INATB , JCTAB )
      IF ( INAT .EQ. 3 .AND. IXIADD .EQ. 0 ) THEN
        IXLONO = ISZON ( JISZON + IBACOL + IDLONO )
        IF ( IXLONO .GT. 0 ) THEN
          IBLONO = IADM  ( JIADM(ICLACO) + 2*IXLONO-1 )
          LONOI  = ISZON(JISZON+IBLONO-1+IDATOC+1) -
     &                      ISZON(JISZON+IBLONO-1+IDATOC  )
          IF ( LONOI .GT. 0 ) THEN
            JCTAB  = JCTAB +  (ISZON(JISZON+IBLONO-1+IDATOC) - 1)
          ELSE
            CALL U2MESK('F','JEVEUX1_22',1,NOML32)
          ENDIF
        ELSE
          JCTAB = JCTAB + LONG(JLONG(ICLACO)+IXDESO) * (IDATOC-1)
        ENDIF
      ENDIF
 1000 CONTINUE
      END
