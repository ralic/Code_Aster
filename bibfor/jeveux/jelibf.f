      SUBROUTINE JELIBF ( COND , CLAS )
C TOLE CRS_505 CRS_508 CRP_18
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 11/08/2009   AUTEUR LEFEBVRE J-P.LEFEBVRE 
C RESPONSABLE LEFEBVRE J-P.LEFEBVRE
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
      CHARACTER*(*)       COND , CLAS
C ----------------------------------------------------------------------
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
      INTEGER          LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
C ----------------------------------------------------------------------
      PARAMETER  ( N = 5 )
      INTEGER          LTYP    , LONG    , DATE    , IADD    , IADM    ,
     &                 LONO    , HCOD    , CARA    , LUTI    , IMARQ   
      COMMON /IATRJE/  LTYP(1) , LONG(1) , DATE(1) , IADD(1) , IADM(1) ,
     &                 LONO(1) , HCOD(1) , CARA(1) , LUTI(1) , IMARQ(1)
      COMMON /JIATJE/  JLTYP(N), JLONG(N), JDATE(N), JIADD(N), JIADM(N),
     &                 JLONO(N), JHCOD(N), JCARA(N), JLUTI(N), JMARQ(N)
C ----------------------------------------------------------------------
      INTEGER          NBLMAX    , NBLUTI    , LONGBL    ,
     &                 KITLEC    , KITECR    ,             KIADM    ,
     &                 IITLEC    , IITECR    , NITECR    , KMARQ
      COMMON /IFICJE/  NBLMAX(N) , NBLUTI(N) , LONGBL(N) ,
     &                 KITLEC(N) , KITECR(N) ,             KIADM(N) ,
     &                 IITLEC(N) , IITECR(N) , NITECR(N) , KMARQ(N)
      LOGICAL          LITLEC
      COMMON /LFICJE/  LITLEC(N)
C
      CHARACTER*1      GENR    , TYPE
      CHARACTER*4      DOCU
      CHARACTER*8      ORIG
      CHARACTER*32     RNOM
      COMMON /KATRJE/  GENR(8) , TYPE(8) , DOCU(2) , ORIG(1) , RNOM(1)
      COMMON /JKATJE/  JGENR(N), JTYPE(N), JDOCU(N), JORIG(N), JRNOM(N)
      INTEGER          NRHCOD    , NREMAX    , NREUTI
      COMMON /ICODJE/  NRHCOD(N) , NREMAX(N) , NREUTI(N)
C
      CHARACTER*2      DN2
      CHARACTER*5      CLASSE
      CHARACTER*8                  NOMFIC    , KSTOUT    , KSTINI
      COMMON /KFICJE/  CLASSE    , NOMFIC(N) , KSTOUT(N) , KSTINI(N) ,
     &                 DN2(N)
      CHARACTER*8      NOMBAS
      COMMON /KBASJE/  NOMBAS(N)
C
      INTEGER          ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      COMMON /IATCJE/  ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
C
      CHARACTER *24                     NOMCO
      CHARACTER *32    NOMUTI , NOMOS ,         NOMOC , BL32
      COMMON /NOMCJE/  NOMUTI , NOMOS , NOMCO , NOMOC , BL32
C
      COMMON /IACCED/  IACCE(1)
      COMMON /JIACCE/  JIACCE(N)
      COMMON /KUSADI/  IUSADI(1)
      COMMON /JUSADI/  JUSADI(N)
      INTEGER          LUNDEF,IDEBUG
      COMMON /UNDFJE/  LUNDEF,IDEBUG
C ----------------------------------------------------------------------
      INTEGER          NBENRG, LGENRG, NBENRV, LGENRV, NBENRL, LGENRL
      COMMON /STACOD/  NBENRG, LGENRG, NBENRV, LGENRV, NBENRL, LGENRL
C ----------------------------------------------------------------------
      INTEGER          LIDBAS      , LIDEFF
      PARAMETER      ( LIDBAS = 20 , LIDEFF = 15 )
      CHARACTER*1      KCLAS
      CHARACTER*8      KCOND,VALK(1),NOM
      CHARACTER*32     NOMCAR
      INTEGER          IADCAR,IADDAC(2),LGBL,VALI(7),IADCDY,IBID,
     &                 IADDAD(2),KLEC,KECR,INFO
      REAL*8           VALR(1)
C DEB ------------------------------------------------------------------
      KCOND = COND
      KCLAS = CLAS
      IC = INDEX ( CLASSE , KCLAS )
      ICLAOS = IC
      CALL ASSERT (IC .NE. 0)
      CALL ASSERT (     KCOND .EQ. '        ' .OR. KCOND .EQ. 'SAUVE   '
     &             .OR. KCOND .EQ. 'ERREUR  ' .OR. KCOND .EQ. 'DETRUIT '
     &             .OR. KCOND .EQ. 'LIBERE  ' )
      IF ( KCOND .EQ. '        '  ) THEN
        KCOND = KSTOUT(IC)
      ELSE IF ( KCOND .EQ. 'ERREUR  '  ) THEN
        KCOND = 'SAUVE'
      ENDIF
C
C     ----------- DECHARGER TOUTES LES COLLECTIONS -----------------
      DO 10 I = LIDBAS+1 , NREUTI(IC)
        IF ( GENR ( JGENR(IC) + I ) .EQ. 'X' ) THEN
          ICLACO = IC
          IBACOL = IADM( JIADM(IC) + 2*I-1 )
          IF ( IBACOL .NE. 0 ) THEN
            IDATCO = I
            NOMCO = RNOM ( JRNOM(IC) + I )
            CALL JJLIDE ( 'JELIBF' , NOMCO , 2 )
          END IF
        END IF
   10 CONTINUE
C     -- DECHARGER TOUS LES OBJETS SIMPLES Y COMPRIS AVEC $$ -------
      DO 20 I = LIDBAS+1 , NREUTI(IC)
        IADMI = IADM( JIADM(IC) + 2*I-1 )
        IF ( IADMI .NE. 0 ) THEN
          IDATOS = I
          NOMOS  = RNOM ( JRNOM(IC) + I )
          CALL JJLIDE ( 'JELIBF' , NOMOS , 1 )
        END IF
   20 CONTINUE
C     ----------- DECHARGER TOUS LES OBJETS SYSTEME ----------------
      IAD2   = IADM(JIADM(IC) + 2*2-1)
      IADCAR = IADM(JIADM(IC) + 2*1-1)
      IADCDY = IADM(JIADM(IC) + 2*1  )
      IADACC = IADM(JIADM(IC) + 2*LIDEFF-1)
      IADACY = IADM(JIADM(IC) + 2*LIDEFF  )
      K2  = IADM(JIADM(IC) + 2*2 )
      K16 = IADM(JIADM(IC) + 2*16)
      K18 = IADM(JIADM(IC) + 2*18)
      K18I = IADM(JIADM(IC) + 2*18-1)
      K19 = IADM(JIADM(IC) + 2*19)
      K19I = IADM(JIADM(IC) + 2*19-1)
      K20 = IADM(JIADM(IC) + 2*20)

      IF ( KCOND .NE. 'LIBERE  ' ) THEN

        NOMCAR = RNOM (JRNOM(IC) + 1)
        IADCAR = IADM (JIADM(IC) + 2*1-1)
        LACC   = LONO (JLONO(IC) + LIDEFF) * LTYP(JLTYP(IC)+LIDEFF)
        IADDAC(1) = IADD (JIADD(IC) + 2*LIDEFF-1)
        IADDAC(2) = IADD (JIADD(IC) + 2*LIDEFF  )

        IADADI = IADM (JIADM(IC) + 2*(LIDEFF-1)-1)
        IADADY = IADM (JIADM(IC) + 2*(LIDEFF-1)  )
        LADI   = LONO (JLONO(IC) + LIDEFF-1)* LTYP(JLTYP(IC)+LIDEFF-1)
        IADDAD(1) = IADD (JIADD(IC) + 2*(LIDEFF-1)-1)
        IADDAD(2) = IADD (JIADD(IC) + 2*(LIDEFF-1)  )
C       
        IDB    = IDEBUG
        IDEBUG = 0

        DO 30 I = LIDEFF-2 , 2 , -1
          IADMI = IADM( JIADM(IC) + 2*I-1 )
          IF ( IADMI .NE. 0 ) THEN
            IDATOS = I
            NOMOS  = RNOM ( JRNOM(IC) + I )
            CALL JJLIDE ( 'SYSTEM' , NOMOS , 1 )
          END IF
 30     CONTINUE
        DO 33 I = LIDBAS+1 , NREUTI(IC)
          IADMI = IADM( JIADM(IC) + 2*I-1 )
          IADYN = IADM( JIADM(IC) + 2*I   )
          IF ( IADYN .NE. 0 ) THEN
            CALL JJLIDY ( IADYN , IADMI )
          ELSE IF ( IADMI .NE. 0 ) THEN
            CALL JJLIBP ( IADMI )
          END IF
   33   CONTINUE
        IDEBUG = IDB
      ENDIF
C
      NBIO = 0
      DO 50 K=1,NBLMAX(IC)
        NBIO = NBIO + IACCE (JIACCE(IC)+K)
 50   CONTINUE
C
      IF ( KCOND .EQ. 'SAUVE   '  ) THEN
C       ----------- STATISTIQUES DU FICHIER
C       ----------- ACTUALISER CARA
        CARA(JCARA(IC)+1) = NREUTI(IC)
        CARA(JCARA(IC)+3) = NBLMAX(IC)
        CARA(JCARA(IC)+4) = NBLUTI(IC)
        CARA(JCARA(IC)+6) = IADD(JIADD(IC) + 3)
        CARA(JCARA(IC)+7) = IADD(JIADD(IC) + 4)
        IF ( IADCAR.NE. 0 ) THEN
           IDATOS = 1
           CALL JJLIDE ( 'JELIBF' , NOMCAR , 1 )
           IADCAR = 0
        END IF
      ENDIF
C
C ON PEUT MAINTENANT LIBERER LES ZONES MEMOIRES ASSOCIEES AUX
C OBJETS DU SYSTEME, ELLES NE SERONT PLUS UTILISEES (3-13):
C    $$GENR , $$TYPE , $$DOCU , $$ORIG , $$RNOM  , $$LTYP , $$LONG  , 
C    $$LONO , $$DATE , $$LUTI , $$HCOD 
C
      IF ( KCOND .NE. 'LIBERE  ' ) THEN
        DO 32 I = 3 , LIDEFF-2
          IADMI = IADM( JIADM(IC) + 2*I-1 )
          IADYN = IADM( JIADM(IC) + 2*I   )
          IF ( IADYN .NE. 0 ) THEN
            CALL JJLIDY ( IADYN , IADMI )
          ELSE IF ( IADMI .NE. 0 ) THEN
            CALL JJLIBP ( IADMI )
          END IF
   32   CONTINUE
      ENDIF
C      
C      
      IF ( KCOND .EQ. 'SAUVE   '  ) THEN
C       ----------- DECHARGER TAMPON D'ECRITURE ( PETITS OBJETS )
        LGBL = 1024*LONGBL(IC)*LOIS
        IF ( IITECR(IC) .GT. 0 ) THEN
           CALL JXECRB (IC, IITECR(IC), KITECR(IC)+1, LGBL, 0, 0)
        ENDIF
        IF ( LITLEC(IC) ) THEN
           CALL JXECRB (IC, IITLEC(IC), KITLEC(IC)+1, LGBL, 0, 0)
           LITLEC(IC) = .FALSE.
           IITLEC(IC) = 0
        ENDIF
C       ---- ON DECHARGE MAINTENANT LES STATISTIQUES SUR LES ACCES
        IDATOS = LIDEFF
        IACCE (JIACCE(IC)+IADDAC(1)) = IACCE (JIACCE(IC)+IADDAC(1))+1
        NBIO = NBIO + 1
        CALL JXECRO (IC,IADACC,IADDAC,LACC,0,LIDEFF)
        IITECR(IC) = 0
        IF ( LITLEC(IC) ) THEN
          CALL JXECRB ( IC,IITLEC(IC),KITLEC(IC)+1,LGBL,0,0)
        ENDIF
C
C     GLUTE STAT CODE
C
        IF    ( CLAS(1:1) .EQ. 'G' ) THEN
          NBENRG = NBLUTI(IC)
          LGENRG = 1024*LONGBL(IC)*LOIS
        ELSEIF( CLAS(1:1) .EQ. 'V' ) THEN
          NBENRV = NBLUTI(IC)
          LGENRV = 1024*LONGBL(IC)*LOIS
        ELSEIF( CLAS(1:1) .EQ. 'L' ) THEN
          NBENRL = NBLUTI(IC)
          LGENRL = 1024*LONGBL(IC)*LOIS
        ENDIF
      ENDIF
      IF ( KCOND .EQ. 'SAUVE   '  .OR. KCOND .EQ. 'DETRUIT  ' ) THEN
C       ---- ON DECHARGE MAINTENANT LA DESCRIPTION DES ENREGISTREMENTS
        LGBL = 1024*LONGBL(IC)*LOIS
        IDATOS = LIDEFF-1
        NBIO = NBIO + 1
        CALL JXECRO (IC,IADADI,IADDAD,LADI,0,LIDEFF-1)
        IITECR(IC) = 0
        IF ( LITLEC(IC) ) THEN
          CALL JXECRB ( IC,IITLEC(IC),KITLEC(IC)+1,LGBL,0,0)
        ENDIF
        IF ( IADADY .NE. 0 ) THEN
          CALL JJLIDY ( IADADY , IADADI )
          IADADY = 0
        ELSE IF ( IADADI .NE. 0 ) THEN
          CALL JJLIBP ( IADADI )
        ENDIF  
        IADADI = 0
C       ---- $$ACCE N'EST PLUS UTILISE, ON PEUT LE LIBERER
        IF ( IADACY .NE. 0 ) THEN
          CALL JJLIDY ( IADACY , IADACC )
          IADACY = 0
        ELSE IF ( IADACC.NE. 0 ) THEN
          CALL JJLIBP ( IADACC )
        ENDIF  
        IADACC = 0
      ENDIF
C
      VALK(1)= NOMBAS(IC)
      VALI(1)= NBLUTI(IC)
      VALI(2)= NBLMAX(IC)
      VALI(3)= 1024*LONGBL(IC)*LOIS
      VALI(4)= NBIO
      VALI(5)= NREUTI(IC)
      VALI(6)= NREMAX(IC)
      VALI(7)= (NREUTI(IC)*100)/NREMAX(IC)
C
      CALL U2MESG ('I','JEVEUX_22',1,VALK,7,VALI,0,VALR)
C
      IF ( KCOND .NE. 'LIBERE  ' ) THEN
        IF ( IADCAR.NE. 0 ) THEN
           IDATOS = 1
           IF (IADCDY .NE. 0 ) THEN 
             CALL JJLIDY ( IADCDY , IADCAR )
           ELSE
             CALL JJLIBP ( IADCAR )
           ENDIF
        END IF
        IF ( IADACY .NE. 0 ) THEN
          CALL JJLIDY ( IADADY , IADACC )
        ELSE IF ( IADACC.NE. 0 ) THEN
          CALL JJLIBP ( IADACC )
        ENDIF  
C       ----------- CLORE LE FICHIER
        IF ( KSTINI(IC) .NE. 'DUMMY   ' ) THEN
          CALL JXFERM ( IC )
        ENDIF
C       ----------- LIBERER PLACE
        IF (K18 .NE. 0) THEN
          CALL JJLIDY ( K18 , K18I )
        ELSE
          CALL JJLIBP ( 1 + KITLEC(IC) / LOIS )
        ENDIF
        IF (K2 .NE. 0) THEN
          CALL JJLIDY ( K2 , IAD2 )
        ELSE
          CALL JJLIBP ( IAD2 )
        ENDIF
        IF (K19 .NE. 0) THEN
          CALL JJLIDY ( K19 , K19I )
        ELSE
          CALL JJLIBP ( 1 + KITECR(IC) / LOIS )
        ENDIF
        IF (K16 .NE. 0) THEN
          CALL JJLIDY ( K16 , KMARQ(IC) )
        ELSE IF ( KMARQ(IC) .NE. 0 ) THEN
          CALL JJLIBP ( KMARQ(IC) )
        ENDIF
        IF (K20 .NE. 0) THEN
          CALL JJLIDY ( K20 , KIADM(IC) )
        ELSE IF ( KIADM(IC) .NE. 0 ) THEN
          CALL JJLIBP ( KIADM(IC) )
        ENDIF
C
        IF ( KSTOUT(IC)(1:7) .EQ. 'DETRUIT' ) THEN
          NOM = NOMFIC(IC)(1:4)//'.?  '
          CALL LXMINS (NOM)
          INFO = 0
          CALL RMFILE (NOM,INFO)
        ENDIF
C	
        CLASSE(IC:IC) = ' '
        NOMFIC(IC)    = ' '
        NOMBAS(IC)    = ' '
        KSTOUT(IC)    = ' '
        KSTINI(IC)    = ' '
        DN2(IC)       = ' '
        NRHCOD(IC)    = 0
        NREMAX(IC)    = 0
        NREUTI(IC)    = 0
        LITLEC(IC)    = .FALSE.
        NBLMAX(IC)    = 0
        NBLUTI(IC)    = 0
        LONGBL(IC)    = 0
        KITLEC(IC)    = 0
        KITECR(IC)    = 0
        KIADM(IC)     = 0
        IITLEC(IC)    = 0
        IITECR(IC)    = 0
        NITECR(IC)    = 0
      ENDIF
C FIN ------------------------------------------------------------------
      END
