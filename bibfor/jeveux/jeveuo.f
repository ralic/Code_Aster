      SUBROUTINE JEVEUO ( NOMLU , CEL  , JCTAB )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 27/03/2002   AUTEUR VABHHTS J.PELLET 
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
C TOLE CFT_720 CFT_726 CRP_18 CRS_508 CRS_512
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER                            JCTAB
      CHARACTER*(*)       NOMLU , CEL
C     ==================================================================
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
C     ------------------------------------------------------------------
      PARAMETER  ( N = 5 )
      INTEGER          LTYP    , LONG    , DATE    , IADD    , IADM    ,
     +                 LONO    , HCOD    , CARA    , LUTI    , IMARQ
      COMMON /IATRJE/  LTYP(1) , LONG(1) , DATE(1) , IADD(1) , IADM(1) ,
     +                 LONO(1) , HCOD(1) , CARA(1) , LUTI(1) , IMARQ(1)
      COMMON /JIATJE/  JLTYP(N), JLONG(N), JDATE(N), JIADD(N), JIADM(N),
     +                 JLONO(N), JHCOD(N), JCARA(N), JLUTI(N), JMARQ(N)
C
      CHARACTER*1      GENR    , TYPE
      CHARACTER*4      DOCU
      CHARACTER*8      ORIG
      CHARACTER*32     RNOM
      COMMON /KATRJE/  GENR(8) , TYPE(8) , DOCU(2) , ORIG(1) , RNOM(1)
      COMMON /JKATJE/  JGENR(N), JTYPE(N), JDOCU(N), JORIG(N), JRNOM(N)
      INTEGER          NUMATR
      COMMON /IDATJE/  NUMATR
C     ------------------------------------------------------------------
      INTEGER          ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      COMMON /IATCJE/  ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
C---------- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
C---------- FIN  COMMUNS NORMALISES  JEVEUX ----------------------------
      INTEGER         IZR,IZC,IZL,IZK8,IZK16,IZK24,IZK32,IZK80
      EQUIVALENCE    (IZR,ZR),(IZC,ZC),(IZL,ZL),(IZK8,ZK8),(IZK16,ZK16),
     &               (IZK24,ZK24),(IZK32,ZK32),(IZK80,ZK80)
C ----------------------------------------------------------------------
      CHARACTER*1      GENRI , TYPEI ,KCEL
      CHARACTER*8      NOML8
      CHARACTER*32     NOML32
      CHARACTER*75     CMESS
      INTEGER          ICRE , IRET
      INTEGER        IVNMAX     , IDDESO     ,IDIADD     , IDIADM     ,
     +               IDMARQ     , IDNOM      ,IDREEL     , IDLONG     ,
     +               IDLONO     , IDLUTI     ,IDNUM
      PARAMETER    ( IVNMAX = 0 , IDDESO = 1 ,IDIADD = 2 , IDIADM = 3 ,
     +               IDMARQ = 4 , IDNOM  = 5 ,IDREEL = 6 , IDLONG = 7 ,
     +               IDLONO = 8 , IDLUTI = 9 ,IDNUM  = 10 )
C     ==================================================================
C J#DEB

      NOML32 = NOMLU
      NOML8  = NOML32(25:32)
      KCEL   = CEL
      IF ( KCEL .NE. 'L' .AND. KCEL .NE. 'E' ) THEN
         CMESS = 'PARAMETRE D''ACCES '//KCEL//' INVALIDE'
         CALL JVMESS ( 'S' , 'JEVEUO00' , CMESS )
      ENDIF
C
      ICRE = 0
      CALL JJVERN ( NOML32 , ICRE , IRET )
      INAT  = IRET
      INATB = IRET
      GOTO ( 10 , 20 , 30 ) ,IRET+1
 10   CONTINUE
C ----   IRET = 0
         CMESS = 'OBJET INEXISTANT DANS LES BASES OUVERTES'
         CALL JVMESS ( 'S' , 'JEVEUO01' , CMESS )
         GOTO 100
 20   CONTINUE
C ----   IRET = 1
         GENRI =  GENR( JGENR(ICLAOS) + IDATOS )
         TYPEI =  TYPE( JTYPE(ICLAOS) + IDATOS )
         LTYPI =  LTYP( JLTYP(ICLAOS) + IDATOS )
         IF ( GENRI .EQ. 'N' ) THEN
           CMESS = 'ACCES A UN REPERTOIRE NON AUTORISE'
           CALL JVMESS ( 'S' , 'JEVEUO02' , CMESS )
         ENDIF
         GOTO 100
 30   CONTINUE
C ----   IRET = 2
         CALL JJALLC ( ICLACO , IDATCO , CEL , IBACOL )
         IXIADD = ISZON ( JISZON + IBACOL + IDIADD )
         IXDESO = ISZON ( JISZON + IBACOL + IDDESO )
         IF ( NOML8 .EQ. '$$XATR  ') THEN
            IXLONO = NUMATR
            IBLONO = IADM ( JIADM(ICLACO) + IXLONO )
            GENRI  = GENR ( JGENR(ICLACO) + IXLONO )
            LTYPI  = LTYP ( JLTYP(ICLACO) + IXLONO )
            LONOI  = LONO ( JLONO(ICLACO) + IXLONO ) * LTYPI
            CALL JXLOCS ( ZI, GENRI, LTYPI, LONOI, IBLONO, .FALSE.,
     +                    JCTAB)
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
                CMESS = 'COLLECTION DISPERSEE NON ACCESSIBLE EN BLOC'
                CALL JVMESS ( 'S' , 'JEVEUO03' , CMESS )
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
          IBLONO = IADM  ( JIADM(ICLACO) + IXLONO )
          LONOI  = ISZON(JISZON+IBLONO-1+IDATOC+1) -
     +                      ISZON(JISZON+IBLONO-1+IDATOC  )
          IF ( LONOI .GT. 0 ) THEN
            JCTAB  = JCTAB +  (ISZON(JISZON+IBLONO-1+IDATOC) - 1)
          ELSE
            CMESS = 'OBJET DE COLLECTION CONTIG DE LONGUEUR NULLE'
            CALL JVMESS ( 'S' , 'JEVEUO04' , CMESS )
          ENDIF
        ELSE
          JCTAB = JCTAB + LONG(JLONG(ICLACO)+IXDESO) * (IDATOC-1)
        ENDIF
      ENDIF
 1000 CONTINUE
C



C J#FIN
      END
