      SUBROUTINE JEPREG ( CUNIT , CLAS , NUMERG , CMESS , INFO)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 17/11/2008   AUTEUR LEFEBVRE J-P.LEFEBVRE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_18 CRS_508
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)       CUNIT , CLAS , CMESS
      INTEGER             NUMERG, INFO
C ----------------------------------------------------------------------
C ROUTINE UTILISATEUR D'IMPRESSION DU CONTENU D'UN ENREGISTREMENT
C DU FICHIER D'ACCES DIRECT ASSOCIE A UNE BASE
C
C IN  CUNIT  : NOM LOCAL DU FICHIER DES IMPRESSIONS
C IN  CLAS   : CLASSE ASSOCIEE A LA BASE ( ' ' : TOUTES LES CLASSES )
C IN  NUMERG : NUMERO DE L'ENREGISTREMENT
C IN  CMESS  : MESSAGE D'INFORMATION
C IN  INFO   : NIVEAU DES IMPRESSIONS 
C              SI > 1 ALORS ON IMPRIME LE CONTENU DE L'ENREGISTREMENT
C              SINON ON IMPRIME UNIQUEMENT LE CHAINAGE
C ----------------------------------------------------------------------
      INTEGER          LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
      PARAMETER  ( N = 5 )
C
      CHARACTER*1      GENR    , TYPE
      CHARACTER*4      DOCU
      CHARACTER*8      ORIG
      CHARACTER*32     RNOM
      COMMON /KATRJE/  GENR(8) , TYPE(8) , DOCU(2) , ORIG(1) , RNOM(1)
      COMMON /JKATJE/  JGENR(N), JTYPE(N), JDOCU(N), JORIG(N), JRNOM(N)
C
      INTEGER          NBLMAX    , NBLUTI    , LONGBL    ,
     +                 KITLEC    , KITECR    ,             KIADM    ,
     +                 IITLEC    , IITECR    , NITECR    , KMARQ
      COMMON /IFICJE/  NBLMAX(N) , NBLUTI(N) , LONGBL(N) ,
     +                 KITLEC(N) , KITECR(N) ,             KIADM(N) ,
     +                 IITLEC(N) , IITECR(N) , NITECR(N) , KMARQ(N)
      COMMON /KUSADI/  IUSADI(1)
      COMMON /JUSADI/  JUSADI(N)
      CHARACTER*2      DN2
      CHARACTER*5      CLASSE
      CHARACTER*8                  NOMFIC    , KSTOUT    , KSTINI
      COMMON /KFICJE/  CLASSE    , NOMFIC(N) , KSTOUT(N) , KSTINI(N) ,
     +                 DN2(N)
      CHARACTER*8      NOMBAS
      COMMON /KBASJE/  NOMBAS(N)
      INTEGER          ISTAT
      COMMON /ISTAJE/  ISTAT(4)
      REAL *8          SVUSE,SMXUSE   
      COMMON /STATJE/  SVUSE,SMXUSE  
C ----------------------------------------------------------------------
      CHARACTER*1      KCLAS
      INTEGER          ITP,JITP,IADITP,IADYN,IDCO,IDOS,IDEC,ICOMP
      INTEGER          JI,NL,ND,IADDI(2),IDOSL,IDCOL,LGL,IC 
C DEB ------------------------------------------------------------------
      KCLAS = CLAS ( 1: MIN(1,LEN(CLAS)))
      JULIST = IUNIFI ( CUNIT )
      IF ( JULIST .EQ. 0 ) GOTO 9999
      IF ( KCLAS .EQ. ' ' ) THEN
         NCLA1 = 1
         NCLA2 = INDEX ( CLASSE , '$' ) - 1
         IF ( NCLA2 .LT. 0 ) NCLA2 = N
      ELSE
         NCLA1 = INDEX ( CLASSE , KCLAS )
         NCLA2 = NCLA1
      ENDIF
      DO 10 IC = NCLA1 , NCLA2
        LGBL=1024*LONGBL(IC)*LOIS
        WRITE (JULIST,'(    1X,4A)' ) ('--------------------',K=1,4)
        WRITE (JULIST,*)' '
        WRITE (JULIST,'(1X,A)') CMESS
        WRITE (JULIST,*)' NOM DE LA BASE                    : ',
     +        NOMBAS(IC)
        WRITE (JULIST,*)' NB D''ENREGISTREMENTS MAXIMUM      : ',
     +        NBLMAX(IC)
        WRITE (JULIST,*)' NB D''ENREGISTREMENTS UTILISES     : ',
     +        NBLUTI(IC)
        WRITE (JULIST,*)
     +                  ' LONGUEUR D''ENREGISTREMENT (OCTETS): ',LGBL
        WRITE(JULIST,*)'                                  '
        WRITE (JULIST,'(    1X,4A)' ) ('--------------------',K=1,4)

        IF ( NUMERG .LE. NBLUTI(IC) ) THEN
          LGBL = 1024*LONGBL(IC)*LOIS
          CALL JJALLS(LGBL,0,'V','I',LOIS,'INIT',ITP,JITP,IADITP,
     +                IADYN)
          ISZON(JISZON+IADITP-1) = ISTAT(2)
          ISZON(JISZON+ISZON(JISZON+IADITP-4)-4) = ISTAT(4)
          SVUSE = SVUSE + (ISZON(JISZON+IADITP-4) - IADITP + 4)
          IF (IADYN .NE. 0) SVUSE = SVUSE + 1
          IADDI(1) = NUMERG
          IADDI(2) = 0
          IADMO  = (IADITP - 1)*LOIS + ISZON(JISZON+IADITP- 3) + 1
          IF ( NUMERG .EQ. IITLEC(IC) ) THEN
            CALL JXDEPS (KITLEC(IC)+1 , IADMO , LGBL)
          ELSE IF ( NUMERG .EQ. IITECR(IC) ) THEN
            CALL JXDEPS (KITECR(IC)+1 , IADMO , LGBL)
          ELSE 
            CALL JXLIRO (IC, IADITP, IADDI, LGBL)
          ENDIF
          IF ( INFO .GT. 1 ) THEN  
            JI = JISZON + IADITP
            NL = LGBL / (10*LOIS)
            ND = MOD( LGBL , (10*LOIS) ) / LOIS
            WRITE(JULIST,*)'CONTENU BRUT DE L''ENREGISTREMENT ',NUMERG
            WRITE ( JULIST , 1001)
     &           (10*(L-1)+1,(ISZON( JI + 10*(L-1)+K-1),K=1,10),L=1,NL)
            IF ( ND .NE. 0 ) THEN
              WRITE ( JULIST , 1001)
     &                10*NL+1,(ISZON( JI + 10*NL+K-1),K=1,ND)
            ENDIF
          ENDIF
          IDCO = IUSADI(JUSADI(IC)+3*NUMERG-2)
          IDOS = IUSADI(JUSADI(IC)+3*NUMERG-1)
          IF ( IDOS .GT. 0 .OR. IDCO .GT. 0 ) THEN
C
C ------- L'ENREGISTREMENT CONTIENT UN OU UNE PARTIE D'UN "GROS" OBJET
C
            WRITE(JULIST,*)' '                            
            WRITE(JULIST,*)'ENREGISTREMENT NUMERO : ',NUMERG
            WRITE(JULIST,*)'ENREGISTREMENT AFFECTE A UN UNIQUE OBJET'
            IF ( IDCO .EQ. 0 ) THEN
              WRITE(JULIST,*) 'OBJET SIMPLE DE NOM :  ',
     +                        RNOM(JRNOM(IC)+IDOS)
            ELSE
              WRITE(JULIST,*) 'OBJET DE COLLECTION DE NOM : ',
     +                        RNOM(JRNOM(IC)+IDCO),' NUMERO ',IDOS
            ENDIF
          ELSE IF ( IDCO .LT. 0 .OR. IDOS .LT. 0 ) THEN
C
C ------- L'ENREGISTREMENT CORRESPOND A UN OBJET DETRUIT         
C
            WRITE(JULIST,*)' '                            
            WRITE(JULIST,*)'ENREGISTREMENT NUMERO : ',NUMERG
            WRITE(JULIST,*)'ENREGISTREMENT LIBERE',IDCO,IDOS
          ELSE IF ( IDCO .EQ. 0 .AND. IDOS .EQ. 0 ) THEN
C
C ------- L'ENREGISTREMENT CONTIENT DES PETITS OBJETS
C
            WRITE(JULIST,*)' '                            
            WRITE(JULIST,*)'CHAINAGE DE L''ENREGISTREMENT NUMERO :',
     +                     NUMERG
            IDEC  = 0
            ICOMP = 0
 300        CONTINUE
            ICOMP = ICOMP + 1
            IDCOL = ISZON(JISZON+IADITP+IDEC  )
            IDOSL = ISZON(JISZON+IADITP+IDEC+1)
            LGL   = ISZON(JISZON+IADITP+IDEC+2)
            IF ( IDCOL .EQ. 0 .AND. IDOSL .EQ. 0 ) THEN
              WRITE(JULIST,1002) ICOMP,IDCOL,IDOSL,LGL,
     +                        ' FIN DE L''ENREGISTREMENT ATTEINT'
              GOTO 350
            ELSE IF ( IDCOL .LT. 0 .OR. IDOSL .LT. 0 ) THEN
              WRITE(JULIST,1002) ICOMP,IDCOL,IDOSL,LGL,
     +                        ' OBJET DETRUIT'
              GOTO 320
            ENDIF
            IF ( IDCOL .EQ. 0 ) THEN
              WRITE(JULIST,1002) ICOMP,IDCOL,IDOSL,LGL,
     +                   ' OBJET SIMPLE DE NOM : ',RNOM(JRNOM(IC)+IDOSL)
            ELSE
              WRITE(JULIST,1002) ICOMP,IDCOL,IDOSL,LGL,
     +                    ' OBJET DE COLLECTION DE NOM : ',
     +                   RNOM(JRNOM(IC)+IDCOL),' ET DE NUMERO : ',IDOSL
            ENDIF
 320        CONTINUE
            IDEC = IDEC+LGL+3
            GOTO 300
 350        CONTINUE
          ENDIF
        ELSE
          WRITE(JULIST,*)'NUMERO D''ENREGISTREMENT INEXISTANT ',NUMERG
          GOTO 9999
        ENDIF
        WRITE (JULIST,'(    1X,4A)' ) ('--------------------',K=1,4)

        IF (IADYN .NE. 0 ) THEN
          CALL JJLIDY ( IADYN , IADITP )
        ELSE IF (IADITP .NE. 0) THEN
          CALL JJLIBP (IADITP)
        ENDIF
 10   CONTINUE
 9999 CONTINUE
C
 1001 FORMAT((I7,' - ',10(I12,1X)))
 1002 FORMAT(I6,1X,'ID COLLECTION:',I6,' ID OBJET:',I6,' LONGUEUR:',I6,
     +       A,A,I6)
C FIN -----------------------------------------------------------------
      END
