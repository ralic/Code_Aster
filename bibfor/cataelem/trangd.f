      SUBROUTINE TRANGD(INIT,FIN,IVAL,CVAL)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CATAELEM  DATE 12/05/97   AUTEUR JMBHH01 J.M.PROIX 
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
C
C     INCLUDE($CIMPERR)
C
      COMMON /CIMP/IMP,IULMES,IULIST,IULVIG
C
C     EXCLUDE($CIMPERR)
C
C
C   TABLE DE TRANSITION POUR LE CATALOGUE DES GRANDEURS
C
C
C
C
C GRANDEURS_1ERE    3
C               <1>   <4>
C
C   TEMPE          R8          1                TEMPE
C       <5>         <6>        <7>                  <8>
C
C   DEPLA          R8          7
C       <5>         <6>        <7>
C
C       UX       UY         UZ       RX       RY        RZ       LAGR
C         <8>       <8>        <8> .....
C
C   CONTR         R8           3
C       SIGXX     SIGYY          SIGXY
C
C
C
C GRANDEURS_2EME_MEMBRE               2
C                      <2>               <10>
C
C   FONOD             DEPLA
C      <10>               <11>
C
C   FLNOD             TEMPE
C
C
C
C GRANDEURS_ELEMENTAIRES              3
C                      <3>               <12>
C
C   VECTUU        1             DEPLA
C          <13>     <14>              <15>
C
C   MATUU         2             DEPLA      DEPLA       MS
C          <13>     <16>              <17>       <18>      <19>
C
C   MATTT         2             TEMPE      TEMPE       MS
C
C
C
C
C GRANDEURS_ELEMENTAIRES 0
C
C
C  FIN
C    -----------------------------------------
C    ! C ! C ! C  ! I ! E !TYPE! M  !TYPE! F !
C    ! L ! L ! L  ! N ! O !SCAL! O  !MATR! I !
C    ! E ! E ! E  ! T ! F !    ! T  !    ! N !
C    !(1)!(2)!(3) !   !   !    !    !    !   !
C    !   !   !    !   !   !    !    !    !   !
C --------------------------------------------
C  0 ! 1 ! 2 ! 3  !   ! 24!    !    !    !24 !
C --------------------------------------------
C  1 !   !   !    ! 4 !   !    !    !    !   !
C --------------------------------------------
C  2 !   !   !    ! 9 !   !    !    !    !   !
C --------------------------------------------
C  3 !   !   !    !12 !   !    !    !    !   !
C --------------------------------------------
C  4 !   !   !    !   ! 20!    ! 5  !    !20 !
C --------------------------------------------
C  5 !   !   !    !   !   ! 6  !    !    !   !
C --------------------------------------------
C  6 !   !   !    ! 7 !   !    !    !    !   !
C --------------------------------------------
C  7 !   !   !    !   ! 22!    ! 8  !    ! 22!
C --------------------------------------------
C  8 !   !   !    !   ! 23!    ! 8  !    ! 23!
C --------------------------------------------
C  9 !   !   !    !   ! 20!    ! 10 !    ! 20!
C --------------------------------------------
C 10 !   !   !    !   !   !    ! 11 !    !   !
C --------------------------------------------
C 11 !   !   !    !   ! 21!    ! 10 !    ! 21!
C --------------------------------------------
C 12 !   !   !    !   ! 20!    ! 13 !    ! 20!
C --------------------------------------------
C 13 !   !   !    !14/!   !    !    !    !   !
C    !   !   !    !16 !   !    !    !    !   !
C --------------------------------------------
C 14 !   !   !    !   !   !    ! 15 !    !   !
C --------------------------------------------
C 15 !   !   !    !   ! 21!    ! 13 !    ! 21!
C --------------------------------------------
C 16 !   !   !    !   !   !    ! 17 !    !   !
C --------------------------------------------
C 17 !   !   !    !   !   !    ! 18 !    !   !
C --------------------------------------------
C 18 !   !   !    !   !   !    !    ! 19 !   !
C --------------------------------------------
C 19 !   !   !    !   !21 !    ! 13 !    ! 21!
C --------------------------------------------
C 20 !   !   !    !   !   !    !    !    !   !
C --------------------------------------------
C
      INTEGER INIT,FIN,IVAL
      CHARACTER*(*) CVAL
C
      REAL*8 RVAL
      INTEGER ERRLEX,ERRGRA,LIRE
C
C     INCLUDE($CMOTGD)
      CHARACTER*24 CLEGD
      COMMON /CMTGDN/ NCLEGD
      COMMON /CMTGDC/ CLEGD(3)
C     EXCLUDE($CMOTGD)
C
C     INCLUDE($CDEBUG)
      CHARACTER*8 CLEDBG
      CHARACTER*24 OBJDMP
      INTEGER PASDMP,TYOBDM
      COMMON /CMODBG/CLEDBG
      COMMON /CDEBUG/ICCDBG
      COMMON /CBDMPC/OBJDMP(30)
      COMMON /CBDMPN/NDMP,PASDMP(30),TYOBDM(30)
C
C       NDMP : NOMBRE D OBJETS A DUMPER
C       PASDMP(IDMP)  : PASSE OU ON DUMPE L OBJET IDMP
C       OBJDMP(IDMP)  : NOM DE L OBJET IDMP
C       TYOBDM(IDMP)  : GENRE DE L OBJET IDMP :  0 OBJET SIMPLE
C                                                1 COLLECTION NUMEROTEE
C                                                2 COLLECTION NOMME
C
C     EXCLUDE($CDEBUG)
C
      CHARACTER*4 TYPSC(6),TYPM(2)
C
      PARAMETER (NTYPSC=6,NTYPM=2)

      TYPSC(1) = 'I'
      TYPSC(2) = 'K8'
      TYPSC(3) = 'K16'
      TYPSC(4) = 'K24'
      TYPSC(5) = 'R'
      TYPSC(6) = 'C'
      TYPM(1) = 'MS'
      TYPM(2) = 'MR'
C
      ASSIGN 500 TO LIRE
      ASSIGN 900 TO ERRLEX
      ASSIGN 901 TO ERRGRA
C
  500 CONTINUE
      CALL SNLIRE(ICLASS,IVAL,RVAL,CVAL)
      IF (ICLASS.EQ.0) GO TO ERRLEX
      IF (ICLASS.EQ.3) THEN
        IF (CVAL(1:IVAL).EQ.'FIN') THEN
          ICLASS = -1
        END IF

      END IF
C
      IF (ICLASS.EQ.3) THEN
        IF (CVAL(1:IVAL).EQ.CLEDBG) THEN
          CALL LCTDBG(ICLASS,IVAL,RVAL,CVAL,IRTET)
          IF ( IRTET.GT.0 ) GOTO (901), IRTET
          GO TO LIRE

        END IF

      END IF
C
      IF (INIT.EQ.0) THEN
        IF (ICLASS.EQ.-1) THEN
          FIN = 24
          GOTO 9999

        END IF

        IF (ICLASS.EQ.3) THEN
          IF (CVAL(1:IVAL).EQ.CLEGD(1)) THEN
            FIN = 1
            GOTO 9999

          ELSE IF (CVAL(1:IVAL).EQ.CLEGD(2)) THEN
            FIN = 2
            GOTO 9999

          ELSE IF (CVAL(1:IVAL).EQ.CLEGD(3)) THEN
            FIN = 3
            GOTO 9999

          ELSE
            GO TO ERRGRA

          END IF

        ELSE
          GO TO ERRGRA

        END IF

      ELSE
        GO TO (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19) INIT

      END IF
C
    1 CONTINUE
      IF (ICLASS.EQ.1) THEN
        FIN = 4
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
    2 CONTINUE
      IF (ICLASS.EQ.1) THEN
        FIN = 9
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
    3 CONTINUE
      IF (ICLASS.EQ.1) THEN
        FIN = 12
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
    4 CONTINUE
      IF (ICLASS.EQ.-1) THEN
        FIN = 20
        GOTO 9999

      ELSE IF (ICLASS.EQ.3) THEN
        FIN = 5
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
    5 CONTINUE
      IF (ICLASS.EQ.3) THEN
        DO 50 ITYPSC = 1,NTYPSC
          IF (CVAL(1:IVAL).EQ.TYPSC(ITYPSC)) THEN
            FIN = 6
            GOTO 9999

          END IF

   50   CONTINUE
      END IF

      GO TO ERRGRA
C
    6 CONTINUE
      IF (ICLASS.EQ.1) THEN
        FIN = 7
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
    7 CONTINUE
      IF (ICLASS.EQ.-1) THEN
        FIN = 22
        GOTO 9999

      ELSE IF (ICLASS.EQ.3) THEN
        FIN = 8
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
    8 CONTINUE
      IF (ICLASS.EQ.-1) THEN
        FIN = 23
        GOTO 9999

      ELSE IF (ICLASS.EQ.3) THEN
        FIN = 8
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
    9 CONTINUE
      IF (ICLASS.EQ.-1) THEN
        FIN = 20
        GOTO 9999

      ELSE IF (ICLASS.EQ.3) THEN
        FIN = 10
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   10 CONTINUE
      IF (ICLASS.EQ.3) THEN
        FIN = 11
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   11 CONTINUE
      IF (ICLASS.EQ.-1) THEN
        FIN = 21
        GOTO 9999

      ELSE IF (ICLASS.EQ.3) THEN
        FIN = 10
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   12 CONTINUE
      IF (ICLASS.EQ.-1) THEN
        FIN = 20
        GOTO 9999

      ELSE IF (ICLASS.EQ.3) THEN
        FIN = 13
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   13 CONTINUE
      IF (ICLASS.EQ.1) THEN
        IF (IVAL.EQ.1) THEN
          FIN = 14
          GOTO 9999

        ELSE IF (IVAL.EQ.2) THEN
          FIN = 16
          GOTO 9999

        ELSE
          GO TO ERRGRA

        END IF

      ELSE
        GO TO ERRGRA

      END IF
C
   14 CONTINUE
      IF (ICLASS.EQ.3) THEN
        FIN = 15
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   15 CONTINUE
      IF (ICLASS.EQ.-1) THEN
        FIN = 21
        GOTO 9999

      ELSE IF (ICLASS.EQ.3) THEN
        FIN = 13
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   16 CONTINUE
      IF (ICLASS.EQ.3) THEN
        FIN = 17
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   17 CONTINUE
      IF (ICLASS.EQ.3) THEN
        FIN = 18
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   18 CONTINUE
      IF (ICLASS.EQ.3) THEN
        DO 180 ITYPM = 1,NTYPM
          IF (CVAL(1:IVAL).EQ.TYPM(ITYPM)) THEN
            FIN = 19
            GOTO 9999

          END IF

  180   CONTINUE
      END IF

      GO TO ERRGRA
C
   19 CONTINUE
      IF (ICLASS.EQ.-1) THEN
        FIN = 21
        GOTO 9999

      ELSE IF (ICLASS.EQ.3) THEN
        FIN = 13
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
  900 CONTINUE
      WRITE (IULMES,9000)
      CALL UTMESS('F','TRANGD','PB. LECTURE CATALOGUES.')
C
  901 CONTINUE
      WRITE (IULMES,9001) INIT,FIN
      IF (ICLASS.EQ.1) THEN
        WRITE (IULMES,9002) IVAL

      ELSE IF (ICLASS.EQ.2) THEN
        WRITE (IULMES,9003) RVAL

      ELSE IF (ICLASS.EQ.3) THEN
        WRITE (IULMES,9004) IVAL,CVAL(1:IVAL)

      ELSE IF (ICLASS.EQ.4) THEN
        WRITE (IULMES,9005) IVAL,CVAL(1:IVAL)

      ELSE IF (ICLASS.EQ.5) THEN
        WRITE (IULMES,9006) CVAL(1:1)
      END IF

      CALL UTMESS('F','TRANGD','PB. LECTURE CATALOGUES.')
C
 9000 FORMAT (' ************* LECTURE CATALOGUE GRANDEURS **',/,
     +       ' ************* ERREUR LEXICALE **************',/)
 9001 FORMAT (' ************* LECTURE CATALOGUE GRANDEURS **',/,
     +       ' ************* ERREUR GRAMMAIRE *************',/,
     +       ' ETAT INITIAL ',I5,/,' ETAT FINAL   ',I5,/)
 9002 FORMAT (' ENTIER LU : ',I10)
 9003 FORMAT (' REAL   LU : ',D12.5)
 9004 FORMAT (' IDENTIFICATEUR LU : ',/,' LONGUEUR ',I4,/,2X,A72)
 9005 FORMAT (' TEXTE  LU : ',/,' LONGUEUR ',I4,/,2X,A72)
 9006 FORMAT (' SEPARATEUR LU : ',A1)

 9999 CONTINUE
      END
