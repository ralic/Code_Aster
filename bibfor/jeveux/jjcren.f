      SUBROUTINE JJCREN ( NOMLU , ICRE , IRET )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 14/11/2000   AUTEUR D6BHHJP J.P.LEFEBVRE 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)        NOMLU
      INTEGER                      ICRE , IRET
C     ------------------------------------------------------------------
      PARAMETER  ( N = 5 )
      INTEGER          LTYP    , LONG    , DATE    , IADD    , IADM    ,
     +                 LONO    , HCOD    , CARA    , LUTI    , IMARQ
      COMMON /IATRJE/  LTYP(1) , LONG(1) , DATE(1) , IADD(1) , IADM(1) ,
     +                 LONO(1) , HCOD(1) , CARA(1) , LUTI(1) , IMARQ(1)
      COMMON /JIATJE/  JLTYP(N), JLONG(N), JDATE(N), JIADD(N), JIADM(N),
     +                 JLONO(N), JHCOD(N), JCARA(N), JLUTI(N), JMARQ(N)
      CHARACTER*1      GENR    , TYPE
      CHARACTER*4      DOCU
      CHARACTER*8      ORIG
      CHARACTER*32     RNOM
      COMMON /KATRJE/  GENR(8) , TYPE(8) , DOCU(2) , ORIG(1) , RNOM(1)
      COMMON /JKATJE/  JGENR(N), JTYPE(N), JDOCU(N), JORIG(N), JRNOM(N)
C     ------------------------------------------------------------------
      INTEGER          NRHCOD    , NREMAX    , NREUTI
      COMMON /ICODJE/  NRHCOD(N) , NREMAX(N) , NREUTI(N)
      CHARACTER*2      DN2
      CHARACTER*5      CLASSE
      CHARACTER*8                  NOMFIC    , KSTOUT    , KSTINI
      COMMON /KFICJE/  CLASSE    , NOMFIC(N) , KSTOUT(N) , KSTINI(N) ,
     +                 DN2(N)
      INTEGER          NBCLA
      COMMON /NFICJE/  NBCLA
      INTEGER          ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      COMMON /IATCJE/  ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      CHARACTER *24                     NOMCO
      CHARACTER *32    NOMUTI , NOMOS ,         NOMOC , BL32
      COMMON /NOMCJE/  NOMUTI , NOMOS , NOMCO , NOMOC , BL32
C     ------------------------------------------------------------------
      CHARACTER*32     CLEL , CLE , D32
      LOGICAL          LINSER , RINSER
      INTEGER          ICLAIN , IDATIN , IIN
      DATA             D32 /'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'/
C DEB ------------------------------------------------------------------
      IF (ICRE.NE.0) THEN
        IF (NOMLU(1:1).EQ.' ') CALL UTMESS('F','JJCREN','LE NOM D''OBJ'
     &    // 'ET COMMENCE PAR UN BLANC (DVLP)')
      END IF
  500 CONTINUE
      ICLASI = ICLAS
      IRET = 0
      LINSER = .FALSE.
      RINSER = .FALSE.
      NFIC = NBCLA
      LOREPA = 0
      DO 10 ICLA = 1 , NFIC
       IF ( CLASSE(ICLA:ICLA) .NE. ' ' ) THEN
        LOREP = NRHCOD(ICLA)
        CLEL  = NOMLU
        IF ( LOREP .NE. LOREPA ) THEN
          IREF = JXHCOD (CLEL,LOREP)
          LOREPA = LOREP
        ENDIF
        NE = 1
        I  = IREF
 5      CONTINUE
        IF ( HCOD(JHCOD(ICLA)+I) .EQ. 0 .AND. .NOT. RINSER ) THEN
          IF ( ICRE .EQ. 1 .OR. ICRE .EQ. 2 ) THEN
            IF ( ICLA .EQ. ICLAS ) THEN
              IF ( NREUTI(ICLA) .GE. NREMAX(ICLA) ) THEN
                CALL JJAREP ( ICLA , 2*NREMAX(ICLA) )
                GOTO 500
              ENDIF
              LINSER = .TRUE.
              J      = NREUTI(ICLA) + 1
              IDATIN = J
              ICLAIN = ICLA
              IIN    = I
              ISG    = 1
            END IF
          ELSE
            IF ( ICLA .EQ. NFIC ) THEN
              IRET = 0
            END IF
          END IF
        ELSE
          J = HCOD(JHCOD(ICLA)+I)
          CLE  = RNOM(JRNOM(ICLA)+ABS(J))
          IF ( CLE .EQ. CLEL ) THEN
            IF ( ICRE .EQ. 1 .OR. ICRE .EQ. 2 ) THEN
              CALL JVMESS ('S','JJCREN01','LE NOM DEMANDE EXISTE DEJA '
     +                     //' DANS LA BASE '//NOMFIC(ICLA))
            ELSE
              IF ( ICRE .EQ. -1 .OR. ICRE .EQ. -2 ) THEN
                HCOD(JHCOD(ICLA) + I ) = -J
                RNOM(JRNOM(ICLA) + J ) = '?'
              ENDIF
              IF ( GENR(JGENR(ICLA)+J) .NE. 'X' ) THEN
                ICLAOS  = ICLA
                IDATOS  = J
                IRET  = 1
              ELSE
                ICLACO  = ICLA
                IDATCO = J
                IRET  = 2
              ENDIF
              GOTO 15
            END IF
          ELSE
            IF ( J .LT. 0 .AND. .NOT. RINSER ) THEN
              IF ( ICRE .EQ. 1 .OR. ICRE .EQ. 2 ) THEN
                IF ( ICLA .EQ. ICLAS ) THEN
                  LINSER = .TRUE.
                  RINSER = .TRUE.
                  IDATIN = -J
                  ICLAIN = ICLA
                  IIN    = I
                  ISG    = 0
                ENDIF
              ENDIF
            ENDIF
            IF ( NE .EQ. 1 ) IN = JXHCOD (CLEL,LOREP-2)
            NE = NE + 1
            I = 1 + MOD (I+IN,LOREP)
 25         CONTINUE
            IF ( NE .LE. LOREP ) THEN
              J = HCOD(JHCOD(ICLA)+I)
              IF (J .EQ. 0 .AND. RINSER ) GOTO 10
              GOTO 5
            ELSE
              IF ( ICRE .EQ. 1 .OR. ICRE .EQ. 2 ) THEN
                IF ( ICLA .EQ. ICLAS ) THEN
                  CALL JJAREP ( ICLA , 2*NREMAX(ICLA) )
                  LOREP = NRHCOD(ICLA)
                  GOTO 500
                END IF
              ELSE IF ( ICLA .EQ. NFIC ) THEN
                IRET = 0
              END IF
            END IF
          END IF
        END IF
       END IF
   10 CONTINUE
      IF(LINSER) THEN
        ICLAS = ICLAIN
        IF ( ICRE .EQ. 1 ) THEN
          ICLAOS = ICLAIN
          IDATOS = IDATIN
          IRET   = 1
        ELSE IF ( ICRE .EQ. 2 ) THEN
          ICLACO = ICLAIN
          IDATCO = IDATIN
          IRET   = 2
        ENDIF
        NREUTI(ICLAS) = NREUTI(ICLAS) + ISG
        HCOD(JHCOD(ICLAS)+IIN)    = IDATIN
        RNOM(JRNOM(ICLAS)+IDATIN) = NOMLU
      END IF
  15  CONTINUE
      IF ( IRET .EQ. 1 ) THEN
        NOMOS = NOMLU
        IF ( ICLASI .NE. ICLAOS ) THEN
          NOMCO = D32
          NOMOC = D32
        ENDIF
      ELSE IF ( IRET .EQ. 2 ) THEN
        NOMCO = NOMLU
        NOMOC = D32
        IF ( ICLASI .NE. ICLACO ) THEN
          NOMOS = D32
        ENDIF
      ENDIF
C FIN ------------------------------------------------------------------
      END
