      SUBROUTINE DYARCH ( NBPAS,LISINS, LISARC,NBARCH,ICH,NBEXCL,TYPE )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'
      INTEGER             NBPAS, NBARCH, ICH, NBEXCL
      CHARACTER*(*)       LISINS, LISARC, TYPE(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C ----------------------------------------------------------------------
C     SAISIE DU MOT CLE FACTEUR "ARCHIVAGE"
C
C IN  : NBPAS  : NOMBRE DE PAS DE CALCUL
C IN  : LISINS : NOM DE LA LISTE DES INSTANTS DE CALCUL
C IN  : LISARC : LISTE D'ARCHIVAGE DES PAS DE CALCUL
C OUT : NBARCH : NOMBRE DE PAS A ARCHIVER + CI
C IN  : ICH    : PRISE EN COMPTE DU MOT CLE "CHAM_EXCLU"
C OUT : NBEXCL : NOMBRE DE NOMS DES CHAMPS EXCLUS
C OUT : TYPE   : NOMS DES CHAMPS EXCLUS
C ----------------------------------------------------------------------
      INTEGER      JARCH, NBOCC, N1, JNUM, LNUM, K, IPACH, JINSC
      REAL*8       EPSI
      CHARACTER*8  K8B, RELA
      CHARACTER*19 NUMARC
      INTEGER      IARG
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      NBEXCL = 0
      CALL WKVECT ( LISARC, 'V V I', NBPAS, JARCH )
C
      CALL GETFAC ( 'ARCHIVAGE' , NBOCC )
C
      IF ( NBOCC .NE. 0 ) THEN
C
         
         CALL GETVID ( 'ARCHIVAGE', 'LIST_INST', 1,IARG,1, NUMARC, N1 )
         IF ( N1 .NE. 0 ) THEN
            CALL JEVEUO ( LISINS, 'L', JINSC )
            CALL GETVR8 ( 'ARCHIVAGE', 'PRECISION', 1,IARG,1, EPSI, N1 )
            CALL GETVTX ( 'ARCHIVAGE', 'CRITERE'  , 1,IARG,1, RELA, N1 )
            CALL JEVEUO ( NUMARC//'.VALE', 'L', JNUM )
            CALL JELIRA ( NUMARC//'.VALE', 'LONUTI', LNUM, K8B )
            CALL DYARC1 ( ZR(JINSC), NBPAS, ZR(JNUM), LNUM, ZI(JARCH),
     +                    EPSI, RELA  )
            ZI(JARCH+NBPAS-1) = 1
            GOTO 100
         ENDIF
C
         CALL GETVR8 ( 'ARCHIVAGE', 'INST', 1,IARG,0, EPSI, N1 )
         IF ( N1 .NE. 0 ) THEN
            CALL JEVEUO ( LISINS, 'L', JINSC )
            LNUM = -N1
            CALL GETVR8 ( 'ARCHIVAGE', 'PRECISION', 1,IARG,1, EPSI, N1 )
            CALL GETVTX ( 'ARCHIVAGE', 'CRITERE'  , 1,IARG,1, RELA, N1 )
            CALL WKVECT ( '&&DYARCH.VALE_INST', 'V V R', LNUM, JNUM )
            CALL GETVR8 ('ARCHIVAGE','INST',1,IARG,LNUM,
     &                   ZR(JNUM), N1 )
            CALL DYARC1 ( ZR(JINSC), NBPAS, ZR(JNUM), LNUM, ZI(JARCH),
     +                    EPSI, RELA  )
            CALL JEDETR ( '&&DYARCH.VALE_INST' )
            ZI(JARCH+NBPAS-1) = 1
            GOTO 100
         ENDIF
C
         CALL GETVIS ( 'ARCHIVAGE', 'PAS_ARCH', 1,IARG,1, IPACH, N1 )
         IF ( N1 .EQ. 0 ) IPACH = 1
C
         DO 10 K = IPACH , NBPAS , IPACH
            ZI(JARCH+K-1) = 1
 10      CONTINUE
         ZI(JARCH+NBPAS-1) = 1
C
 100     CONTINUE
C
         IF ( ICH .NE. 0 ) THEN
C
C        --- LES SORTIES ---
         CALL GETVTX ( 'ARCHIVAGE', 'CHAM_EXCLU' , 1,IARG,0, K8B, N1 )
         IF ( N1 .NE. 0 ) THEN
            NBEXCL = -N1
            CALL GETVTX('ARCHIVAGE','CHAM_EXCLU' ,1,IARG,NBEXCL,TYPE,N1)
         ENDIF
         ENDIF
C
      ELSE
C
        DO 30 K = 1,NBPAS
           ZI(JARCH+K-1) = 1
 30     CONTINUE
      ENDIF
C
C     --- 1 : CONDITIONS INITIALES ---
      NBARCH = 1
      DO 40 K = 1 , NBPAS
         NBARCH = NBARCH + ZI(JARCH+K-1)
 40   CONTINUE
C
      CALL JEDEMA()
      END
