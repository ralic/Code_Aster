      SUBROUTINE JXECRO ( IC , IADMI , IADDI , LSO , IDCO , IDOS )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 17/10/2005   AUTEUR D6BHHJP J.P.LEFEBVRE 
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
C TOLE CFT_720 CFT_726 CRP_18 CRP_6  CRS_508
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             IC , IADMI , IADDI(2) , LSO , IDCO , IDOS
C ----------------------------------------------------------------------
C ECRITURE D'UN SEGMENT DE VALEUR
C
C IN  IC    : NOM DE LA CLASSE
C IN  IADMI : ADRESSE MEMOIRE DU SEGMENT DE VALEURS
C VAR IADDI : ADRESSE DISQUE DU SEGMENT DE VALEURS
C IN  LSO   : LONGUEUR EN OCTET DU SEGMENT DE VALEURS
C IN  IDCO  : IDENTIFICATEUR DE COLLECTION
C IN  IDOS  : IDENTIFICATEUR D'OBJET SIMPLE OU D'OBJET DE COLLECTION
C ----------------------------------------------------------------------
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
C     ------------------------------------------------------------------
      INTEGER          LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
C     ------------------------------------------------------------------
      PARAMETER  ( N = 5 )
      INTEGER          NBLMAX    , NBLUTI    , LONGBL    ,
     +                 KITLEC    , KITECR    , KINDEF    , KIADM    ,
     +                 IITLEC    , IITECR    , NITECR    , KMARQ
      COMMON /IFICJE/  NBLMAX(N) , NBLUTI(N) , LONGBL(N) ,
     +                 KITLEC(N) , KITECR(N) , KINDEF(N) , KIADM(N) ,
     +                 IITLEC(N) , IITECR(N) , NITECR(N) , KMARQ(N)
      LOGICAL          LITLEC
      COMMON /LFICJE/  LITLEC(N)
      INTEGER          IDN    , IEXT    , NBENRG
      COMMON /IEXTJE/  IDN(N) , IEXT(N) , NBENRG(N)
      COMMON /KINDJE/  INDEF(1)
      COMMON /JINDJE/  JINDEF(N)
      CHARACTER*8      NOMBAS
      COMMON /KBASJE/  NOMBAS(N)
      COMMON /KUSADI/  IUSADI(1)
      COMMON /JUSADI/  JUSADI(N)
C     ------------------------------------------------------------------
      CHARACTER*75     CMESS
      INTEGER          IADMO , KADD , LADD ,NDE ,LGBL
      LOGICAL          LPETIT
      PARAMETER      ( NDE = 6)
C ----------------------------------------------------------------------
C REMARQUE : LE PARAMETER NDE EST AUSSI DEFINI DANS JXLIRO
C ----------------------------------------------------------------------
C DESCRIPTION D'UN ENREGISTREMENT:
C ______________________________________________________________________
C I      I      I      I      I     I                       I      I
C I IDCO I IDOS I IDC1 I IDS1 I LS1 I     ............      I IDC2 I ...
C I______I______I______I______I_____I_______________________I______I____
C
C IDCO   : IDENTIFICATEUR DE COLLECTION
C IDOS   : IDENTIFICATEUR D'OBJET SIMPLE OU D'OBJET DE COLLECTION
C        - SI L'UN DES DEUX EST NON NUL L'ENREGISTREMENT CORRESPOND A
C          UN "GROS" OBJET
C        - SI LES DEUX SONT NULS L'ENREGISTREMENT CONTIENT PLUSIEURS
C          SEGMENTS DE VALEURS ASSOCIES A DE "PETITS" OBJETS
C          DANS CE CAS :
C IDC1   : IDENTIFICATEUR DE COLLECTION DE L'OBJET 1
C IDS1   : IDENTIFICATEUR D'OBJET SIMPLE OU D'OBJET DE COLLECTION DE
C          L'OBJET 1
C LS1    : LONGUEUR DU SEGMENT DE VALEURS ASSOCIE A L'OBJET 1
C          SUIVENT ENSUITE LES VALEURS
C          SI IDCI EST NUL, ON A ATTEINT LA FIN DE L'ENREGISTREMENT
C
C DEB ------------------------------------------------------------------
      IADM   = IADMI
      LADM   = ISZON(JISZON + IADM - 3 )
      KADD   = IADDI(1)
      LADD   = IADDI(2)
      IADMO  = ( IADM - 1 ) * LOIS + LADM + 1
      LGBL   = 1024*LONGBL(IC)*LOIS
      LPETIT = ( LSO .LT. LGBL-NDE*LOIS )
      IF ( IADDI(1) .EQ. 0 ) THEN
C
C ----- PREMIER DECHARGEMENT
C
        IF ( LPETIT ) THEN
C
C -------- PETIT OBJET
C
          IF ( NITECR(IC) + LSO +NDE*LOIS .GT. LGBL ) THEN
C
C --------- LE PETIT OBJET NE TIENT PAS DANS LE TAMPON D'ECRITURE
            IF ( IITECR(IC) .GT. 0 ) THEN
              JIECR = (JK1ZON+KITECR(IC)+NITECR(IC))/LOIS+1
C
C ----------- ON STOCKE LA LONGUEUR RESTANTE DE L'ENREGISTREMENT AU BOUT
              ISZON(JIECR  ) = 0
              ISZON(JIECR+1) = 0
              ISZON(JIECR+2) = (LGBL-NITECR(IC))/LOIS-3
              CALL JXECRB (IC, IITECR(IC), KITECR(IC)+1, LGBL,0,0)
            ENDIF
            DO 101 KD=1,NBLMAX(IC)
              LSADI = JUSADI(IC)+3*KD-2
              ISO = IUSADI(LSADI)+IUSADI(LSADI+1)
              IF ( ISO .GE. 0 ) GOTO 101
              NBLUTI(IC) = MAX (KD,NBLUTI(IC))
              NUMEXT = (NBLUTI(IC)-1)/NBENRG(IC)
              IF( NUMEXT .GT. IEXT(IC)-1 ) THEN
                NUMDEB = IEXT(IC)
                DO 103 K = NUMDEB,NUMEXT
                  LINDEF = JINDEF(IC)+K*(NBENRG(IC)/512+1)*512
                  CALL JXOUVR(IC,K+1,INDEF(LINDEF),NBENRG(IC) )
                  IEXT(IC) = IEXT(IC)+1
 103            CONTINUE
              ENDIF
              GOTO 104
 101        CONTINUE
            CMESS ='FICHIER SATURE, LE NOMBRE MAXIMUM D''ENREGISTREMENT'
            CALL JVDEBM ( 'S' , 'JXECRO01' , CMESS )
            CALL JVIMPI ( 'S' , ' ' , 1 , NBLMAX(IC) )
            CMESS ='DE LA BASE '//NOMBAS(IC)//' EST ATTEINT, '
     &            //'IL FAUT RELANCER LE CALCUL EN '
            CALL JVIMPK ( 'S' , ' ' , 1 , CMESS )
            CMESS = 'MODIFIANT LE PARAMETRE NMAX_ENRE DANS DEBUT'
            CALL JVIMPK ( 'S' , ' ' , 1 , CMESS )
            CMESS ='OU EN PASSANT UNE TAILLE MAXIMUM DE BASE SUR LA '
     &            //'LIGNE DE COMMANDE :'
            CALL JVIMPK ( 'L' , ' ' , 1 , CMESS )
            CMESS =' ARGUMENT "-max_base" SUIVI DE LA VALEUR EN Mo'
            CALL JVIMPK ( 'S' , ' ' , 1 , CMESS )
            CALL JVFINM ( )
 104        CONTINUE
            IITECR(IC) = KD
            IUSADI(JUSADI(IC)+ 3*IITECR(IC)-2) = 0
            IUSADI(JUSADI(IC)+ 3*IITECR(IC)-1) = 0
            IUSADI(JUSADI(IC)+ 3*IITECR(IC)  ) = 0
            NITECR(IC) = 0
          ELSE
            IF ( IITECR(IC) .EQ. 0 ) THEN
              DO 201 KD=1,NBLMAX(IC)
                LSADI = JUSADI(IC)+3*KD-2
                ISO = IUSADI(LSADI)+IUSADI(LSADI+1)
                IF ( ISO .GE. 0 ) GOTO 201
                NBLUTI(IC) = MAX (KD,NBLUTI(IC))
                NUMEXT = (NBLUTI(IC)-1)/NBENRG(IC)
                IF( NUMEXT .GT. IEXT(IC)-1 ) THEN
                  NUMDEB = IEXT(IC)
                  DO 203 K = NUMDEB,NUMEXT
                    LINDEF = JINDEF(IC)+K*(NBENRG(IC)/512+1)*512
                    CALL JXOUVR(IC,K+1,INDEF(LINDEF),NBENRG(IC) )
                    IEXT(IC) = IEXT(IC)+1
 203              CONTINUE
                ENDIF
                GOTO 204
 201          CONTINUE
              CMESS='FICHIER SATURE, LE NOMBRE MAXIMUM '//
     &              'D''ENREGISTREMENT'
              CALL JVDEBM ( 'S' , 'JXECRO02' , CMESS )
              CALL JVIMPI ( 'S' , ' ' , 1 , NBLMAX(IC) )
              CMESS ='DE LA BASE '//NOMBAS(IC)//' EST ATTEINT, '
     &              //'IL FAUT RELANCER LE CALCUL EN '
              CALL JVIMPK ( 'S' , ' ' , 1 , CMESS )
              CMESS ='OU EN PASSANT UNE TAILLE MAXIMUM DE BASE SUR LA '
     &               //'LIGNE DE COMMANDE :'
              CALL JVIMPK ( 'L' , ' ' , 1 , CMESS )
              CMESS =' ARGUMENT "-max_base" SUIVI DE LA VALEUR EN Mo'
              CALL JVIMPK ( 'S' , ' ' , 1 , CMESS )
              CALL JVFINM ( )
 204          CONTINUE
              IITECR(IC) = KD
              IUSADI(JUSADI(IC)+ 3*IITECR(IC)-2) = 0
              IUSADI(JUSADI(IC)+ 3*IITECR(IC)-1) = 0
              IUSADI(JUSADI(IC)+ 3*IITECR(IC)  ) = 0
              NITECR(IC) = 0
            ENDIF
          ENDIF
          JIECR = (JK1ZON+KITECR(IC)+NITECR(IC))/LOIS+1
          ISZON(JIECR  )              = IDCO
          ISZON(JIECR+1)              = IDOS
          ISZON(JIECR+2)              = LSO/LOIS
          ISZON(JIECR+2+(LSO/LOIS)+1) = 0
          ISZON(JIECR+2+(LSO/LOIS)+2) = 0
          ISZON(JIECR+2+(LSO/LOIS)+3) = 0
          CALL JXDEPS (IADMO, KITECR(IC)+NITECR(IC)+3*LOIS+1, LSO )
          IADDI(1) = IITECR(IC)
          IADDI(2) = NITECR(IC)+3*LOIS
          NITECR(IC) = NITECR(IC) + LSO + 3*LOIS
        ELSE
C
C ------- GROS OBJET
C
          NBL = LSO / LGBL
          IF ( MOD ( LSO , LGBL ) .NE. 0 ) NBL = NBL + 1
          KD = 1
 301      CONTINUE
          IF ( KD .LE. NBLMAX(IC)-NBL ) THEN
            LSADI = JUSADI(IC)+3*KD-2
            DO 302 KL=1,NBL
              LSA = LSADI+3*(KL-1)
              ISO = IUSADI(LSA)+IUSADI(LSA+1)
              IF( ISO .GE. 0 ) THEN
                KD = KD + KL
                GOTO 301
              ENDIF
 302        CONTINUE
            IADDI(1) = KD
            NBLUTI(IC) = MAX (KD+NBL-1,NBLUTI(IC))
            NUMEXT = (NBLUTI(IC)-1)/NBENRG(IC)
            IF( NUMEXT .GT. IEXT(IC)-1 ) THEN
              NUMDEB = IEXT(IC)
              DO 303 K = NUMDEB,NUMEXT
                LINDEF = JINDEF(IC)+K*(NBENRG(IC)/512+1)*512
                CALL JXOUVR(IC,K+1,INDEF(LINDEF),NBENRG(IC) )
                IEXT(IC) = IEXT(IC)+1
 303          CONTINUE
            ENDIF
            GOTO 304
          ENDIF
          CMESS ='FICHIER SATURE, LE NOMBRE MAXIMUM D''ENREGISTREMENT'
          CALL JVDEBM ( 'S' , 'JXECRO03' , CMESS )
          CALL JVIMPI ( 'S' , ' ' , 1 , NBLMAX(IC) )
          CMESS ='DE LA BASE '//NOMBAS(IC)//' EST ATTEINT, '
     &           //'IL FAUT RELANCER LE CALCUL EN '
          CALL JVIMPK ( 'S' , ' ' , 1 , CMESS )
          CMESS = 'MODIFIANT LE PARAMETRE NMAX_ENRE DANS DEBUT'
          CALL JVIMPK ( 'S' , ' ' , 1 , CMESS )
          CMESS ='OU EN PASSANT UNE TAILLE MAXIMUM DE BASE SUR LA '
     &           //'LIGNE DE COMMANDE :'
          CALL JVIMPK ( 'L' , ' ' , 1 , CMESS )
          CMESS =' ARGUMENT "-max_base" SUIVI DE LA VALEUR EN Mo'
          CALL JVIMPK ( 'S' , ' ' , 1 , CMESS )
          CALL JVFINM ( )
 304      CONTINUE
          CALL JXECRB (IC, KD, IADMO, LSO, IDCO, IDOS)
        ENDIF
      ELSE
C
C ----- DECHARGEMENTS ULTERIEURS
C
        IF ( LPETIT ) THEN
C
C -------- PETIT OBJET
C
          IF ( KADD .EQ. IITLEC(IC) ) THEN
            CALL JXDEPS (IADMO, KITLEC(IC)+LADD+1, LSO)
            LITLEC(IC) = .TRUE.
          ELSE IF ( KADD .EQ. IITECR(IC) ) THEN
            CALL JXDEPS (IADMO, KITECR(IC)+LADD+1, LSO)
          ELSE
            IF ( LITLEC(IC) ) THEN
              CALL JXECRB (IC, IITLEC(IC), KITLEC(IC)+1, LGBL,0,0)
            ENDIF
            CALL JXLIRB (IC, KADD, KITLEC(IC)+1, LGBL)
            CALL JXDEPS (IADMO, KITLEC(IC)+LADD+1, LSO)
            IITLEC(IC) = KADD
            LITLEC(IC) = .TRUE.
          ENDIF
        ELSE
C
C ------- GROS  OBJET
C
          CALL JXECRB (IC, KADD ,IADMO, LSO, IDCO, IDOS)
        ENDIF
      ENDIF
C FIN ------------------------------------------------------------------
      END
