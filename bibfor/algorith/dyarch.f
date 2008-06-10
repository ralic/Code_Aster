      SUBROUTINE DYARCH ( NBPAS,LISINS, LISARC,NBARCH,ICH,NBEXCL,TYPE )
      IMPLICIT   NONE
      INTEGER             NBPAS, NBARCH, ICH, NBEXCL
      CHARACTER*(*)       LISINS, LISARC, TYPE(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 27/10/98   AUTEUR CIBHHLV L.VIVAN 
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
C ----------------------------------------------------------------------
C     SAISIE DU MOT CLE FACTEUR "ARCHIVAGE"
C
C IN  : NBPAS  : NOMBRE DE PAS DE CALCUL
C IN  : LISINS : INSTANTS DE CALCUL
C IN  : LISARC : LISTE D'ARCHIVAGE DES PAS DE CALCUL
C OUT : NBARCH : NOMBRE DE PAS A ARCHIVER + CI
C IN  : ICH    : PRISE EN COMPTE DU MOT CLE "CHAM_EXCLU"
C OUT : NBEXCL : NOMBRE DE NOMS DES CHAMPS EXCLUS
C OUT : TYPE   : NOMS DES CHAMPS EXCLUS
C ----------------------------------------------------------------------
C     --- DEBUT DECLARATIONS NORMALISEES JEVEUX ------------------------
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
C     --- FIN DECLARATIONS NORMALISEES JEVEUX --------------------------
      INTEGER      JARCH, NBOCC, N1, JNUM, LNUM, K, IPACH, KARCH, JINSC
      REAL*8       EPSI
      CHARACTER*8  K8B, RELA
      CHARACTER*19 NUMARC
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
         CALL GETVID ( 'ARCHIVAGE', 'LIST_ARCH', 1,1,1, NUMARC, N1 )
         IF ( N1 .NE. 0 ) THEN
            CALL JEVEUO ( NUMARC//'.VALE', 'L', JNUM )
            CALL JELIRA ( NUMARC//'.VALE', 'LONUTI', LNUM, K8B )
            DO 20 K = 1 , LNUM
               KARCH = ZI(JNUM+K-1)
               IF ( KARCH .LE. 0 ) THEN
                  GOTO 20
               ELSEIF ( KARCH .GT. NBPAS ) THEN
                  GOTO 22
               ELSE
                  ZI(JARCH+KARCH-1) = 1
               ENDIF
 20         CONTINUE
 22         CONTINUE
            ZI(JARCH+NBPAS-1) = 1
            GOTO 100
         ENDIF
C
         IF ( LISINS(1:1) .NE.  ' ' ) THEN
         CALL JEVEUO ( LISINS(1:19)//'.VALE', 'L', JINSC )
         CALL GETVID ( 'ARCHIVAGE', 'LIST_INST', 1,1,1, NUMARC, N1 )
         IF ( N1 .NE. 0 ) THEN
            CALL GETVR8 ( 'ARCHIVAGE', 'PRECISION', 1,1,1, EPSI, N1 )
            CALL GETVTX ( 'ARCHIVAGE', 'CRITERE'  , 1,1,1, RELA, N1 )
            CALL JEVEUO ( NUMARC//'.VALE', 'L', JNUM )
            CALL JELIRA ( NUMARC//'.VALE', 'LONUTI', LNUM, K8B )
            CALL DYARC1 ( ZR(JINSC), NBPAS, ZR(JNUM), LNUM, ZI(JARCH),
     +                    EPSI, RELA  )
            GOTO 100
         ENDIF
C
         CALL GETVR8 ( 'ARCHIVAGE', 'INST', 1,1,0, EPSI, N1 )
         IF ( N1 .NE. 0 ) THEN
            LNUM = -N1
            CALL GETVR8 ( 'ARCHIVAGE', 'PRECISION', 1,1,1, EPSI, N1 )
            CALL GETVTX ( 'ARCHIVAGE', 'CRITERE'  , 1,1,1, RELA, N1 )
            CALL WKVECT ( '&&DYARCH.VALE_INST', 'V V R', LNUM, JNUM )
            CALL GETVR8 ( 'ARCHIVAGE', 'INST', 1,1,LNUM, ZR(JNUM), N1 )
            CALL DYARC1 ( ZR(JINSC), NBPAS, ZR(JNUM), LNUM, ZI(JARCH),
     +                    EPSI, RELA  )
            CALL JEDETR ( '&&DYARCH.VALE_INST' )
            GOTO 100
         ENDIF
         ENDIF
C
         CALL GETVIS ( 'ARCHIVAGE', 'PAS_ARCH', 1,1,1, IPACH, N1 )
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
         CALL GETVTX ( 'ARCHIVAGE', 'CHAM_EXCLU' , 1,1,0, K8B, N1 )
         IF ( N1 .NE. 0 ) THEN
            NBEXCL = -N1
            CALL GETVTX('ARCHIVAGE','CHAM_EXCLU' ,1,1,NBEXCL,TYPE,N1)
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
