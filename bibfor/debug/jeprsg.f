      SUBROUTINE JEPRSG(CUNIT,TGR,INFO)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF DEBUG  DATE 13/11/2012   AUTEUR COURTOIS M.COURTOIS 
C RESPONSABLE LEFEBVRE
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
      IMPLICIT NONE
      INCLUDE 'jeveux_private.h'
      CHARACTER*(*) CUNIT
      REAL*8 TGR
      INTEGER INFO
C ----------------------------------------------------------------------
C IMPRIME UNE IMAGE OU LA LISTE DES ESPACES DISPONIBLES EN MEMOIRE

C IN  CUNIT : NOM LOCAL DU FICHIER D'IMPRESSION
C IN  TGR   : TAILLE DE GRAIN EN MEGA MOTS (ENTIERS)
C IN  INFO  : =1 IMPRESSION D'UNE IMAGE
C             =2 IMPRESSION DE LA LISTE

C ----------------------------------------------------------------------
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON 
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
C ----------------------------------------------------------------------
      INTEGER          ISTAT
      COMMON /ISTAJE/  ISTAT(4)
      INTEGER          LBIS , LOIS , LOLS , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOR8 , LOC8
      INTEGER          IDINIT   ,IDXAXD   ,ITRECH,ITIAD,ITCOL,LMOTS,IDFR
      COMMON /IXADJE/  IDINIT(2),IDXAXD(2),ITRECH,ITIAD,ITCOL,LMOTS,IDFR
C ----------------------------------------------------------------------
      LOGICAL LAMOV
      CHARACTER*132 CHAINE,INIT,ENTE,DIESE
      INTEGER K,ID,IDA,NBC,IS,NN,NC
C DEB ------------------------------------------------------------------

C-----------------------------------------------------------------------
      INTEGER IUNIFI ,IZ ,JULIST ,MAPLAC ,NC0 
C-----------------------------------------------------------------------
      JULIST = IUNIFI(CUNIT)
      IF (JULIST.EQ.0) GO TO 90
      DO 10 K = 1,132
        INIT(K:K)  = '.'
        DIESE(K:K) = '#'
   10 CONTINUE
      ENTE = ' '
      DO 20 K = 1,132
        ENTE(K:K) = '-'
   20 CONTINUE
      DO 30 K = 1,132,10
        ENTE(K:K) = '+'
   30 CONTINUE
      DO 80 IZ = 1,2
        ID = IDINIT(IZ)
        IF (ID.EQ.0) GO TO 80
        IDA = IDINIT(IZ)
        LAMOV = .FALSE.
        IF (INFO.EQ.1) THEN
          WRITE (JULIST,'(A)')
     &      'REPRESENTATION CONDENSEE DE LA SEGMENTATION MEMOIRE'
          WRITE (JULIST,'(A,2(1PE11.2,A))') '1 CARACTERE = ',TGR,
     &      ' MEGA-MOTS (',TGR*LOIS,' MEGA-OCTETS)'
          WRITE (JULIST,'(A)') ' # -> ESPACE OCCUPE '
          WRITE (JULIST,'(A,/)') ' . -> ESPACE DISPONIBLE   '
        ELSE IF (INFO.EQ.2) THEN
          WRITE (JULIST,'(/,10X,A)') 'LISTE DES ESPACES DISPONIBLES : '
          WRITE (JULIST,'(A)')
     &      '----------------------------------------------------------'
          WRITE (JULIST,'(A)')
     &      '|ADRESSE DE DEPART |  ADDRESSE DE FIN  |       TAILLE    |'
          WRITE (JULIST,'(A)')
     &      '----------------------------------------------------------'
        END IF
        CHAINE = INIT
        NBC = 0
        NC  = 1
   40   CONTINUE
        NC0 = NC
        IS = ISZON(JISZON+ID)
        IF (IS.EQ.0) GO TO 60
        NN = NINT((IS*LOIS)/ (TGR*1024*1024*LOIS))
        NC = MOD(NN,132) + 1
        IF (NN/132.GT.NBC) THEN
          IF (INFO.EQ.1) THEN
            IF (ISZON(JISZON+ID+3).EQ.ISTAT(2)) THEN 
              CHAINE(NC0:) = DIESE
            ENDIF
            WRITE (JULIST,'(A,/,A,/,A,/)') ENTE,CHAINE,ENTE
            IF (ISZON(JISZON+ID+3).EQ.ISTAT(2)) THEN 
              CHAINE = DIESE
            ELSE
              CHAINE = INIT
            ENDIF
            DO 50 K = NBC + 1, (NN/132) - 1
              WRITE (JULIST,'(A,/,A,/,A,/)') ENTE,CHAINE,ENTE
   50       CONTINUE
            CHAINE = INIT
            IF (ISZON(JISZON+ID+3).EQ.ISTAT(2)) THEN 
              CHAINE(1:NC) = DIESE
            ENDIF
          END IF
          NBC = NN/132
        END IF
        IF (ISZON(JISZON+ID+3).EQ.ISTAT(2)) THEN 
          CHAINE(NC:NC) = '#'
        ELSE
          IF (.NOT.LAMOV) THEN
            LAMOV = .TRUE.
            IDA = ID
          END IF
          ID = IS
          GO TO 40
        END IF
   60   CONTINUE
        IF (LAMOV) THEN
          LAMOV = .FALSE.
          MAPLAC = ID - IDA - 8
          IF (INFO.EQ.2) THEN
            WRITE (JULIST,'(A1,4X,I10,4X,A1,6X,I10,3X,A1,6X,I10,1X,A1)')
     &        '|',IDA + 4,'|',ID - 4,'|',MAPLAC,'|'
          END IF
        END IF
        ID = IS
        IF (IS.NE.0) GO TO 40
        NN = NINT((LISZON*LOIS)/ (TGR*1024*1024*LOIS))
        NC = MOD(NN,132)
        IF (NN/132.GT.NBC) THEN
          IF (INFO.EQ.1) THEN
            DO 70 K = NBC, (NN/132) - 1
              WRITE (JULIST,'(A,/,A,/,A,/)') ENTE,CHAINE,ENTE
              CHAINE = INIT
   70       CONTINUE
          END IF
        END IF
        CHAINE(NC:132) = ' '
        CHAINE(NC:NC) = '|'
        IF (INFO.EQ.1) THEN
          WRITE (JULIST,'(A,/,A,/,A,/)') ENTE,CHAINE,ENTE
        ELSE IF (INFO.EQ.2) THEN
          WRITE (JULIST,'(A)')
     &      '----------------------------------------------------------'
        END IF
   80 CONTINUE
   90 CONTINUE
C FIN ------------------------------------------------------------------
      END
