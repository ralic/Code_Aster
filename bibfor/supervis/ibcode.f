      SUBROUTINE IBCODE ( IER )
C TOLE CRS_513 CRS_512
      IMPLICIT REAL*8 (A-H,O-Z)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 10/03/98   AUTEUR VABHHTS J.PELLET 
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
C     LECTURE DES MOT CLE SOUS LE MOT CLE FACTEUR CODE
C     ------------------------------------------------------------------
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C     LE COMMON IBCODC N'EST COMMUN QU'AUX 2 ROUTINES IBCODE ET GETVLI
C     IL NE SERT QU' A TRANSMETTRE A LA ROUTINE CALCUL
C     LES 2 INFORMATIONS PERMETTANT D'ECRIRE UN FICHIER .CODE
C
      CHARACTER*8      CATEST,FICHIE
      COMMON  /IBCODC/ CATEST,FICHIE
C
      CHARACTER*8     LOGNAM
      CHARACTER*16    CBID,   NOMCMD
      INTEGER         UL(3)
C
      LOGICAL EXIST
      INTEGER IVERI,GTENV
      LOGICAL BVERIF
C
      CALL JEMARQ()
      CALL GETRES(CBID,CBID,NOMCMD)
      CALL GCUOPR(0,ICMDU)
      IF ( NOMCMD .EQ. 'DEBUT' ) THEN
         CALL WKVECT('&&SYS   .CODE','G V K8',3,LCODE)
         IERENV = GTENV ( 'LOGNAME' , LOGNAM )
         ZK8(LCODE)   = LOGNAM
         ZK8(LCODE+1) = '       0'
         ISTAT        =        95
         ZK8(LCODE+2) = '      95'
C        EXECUTION OU VERIFICATION DE SYNTAXE
         IVERI=-1
         IF (IVERIF(IVERI) .GT. 0) THEN
            BVERIF=.TRUE.
         ELSE
            BVERIF=.FALSE.
         ENDIF
         CALL GETFAC('CODE',NBOCC)
         IF (( NBOCC .GT. 0 ) .OR. BVERIF) THEN
            IF (NBOCC .GT. 0) THEN
               CALL GETVTX('CODE','NOM',1,1,1,ZK8(LCODE),K)
               CALL GETLTX('CODE','NOM',1,1,1,ILONG     ,K)
            ELSE
               ZK8(LCODE)='VERIFSYN'
               ILONG=8
            ENDIF
            IF ( ILONG .GT. 8 ) CALL UTMESS('A',NOMCMD,'L''ARGUMENT '
     +              //'DU MOT CLE "NOM" SOUS LE MOT CLE FACTEUR "CODE" '
     +              //'EST TRONQUE A 8 CARACTERES. LE NOM DE CODE '
     +              //'EST DONC "'//ZK8(LCODE)//'".' )
            IUNIT = 15
            IF (NBOCC .GT. 0) THEN
               CALL GETVIS('CODE','UNITE',1,1,1,IUNIT,K)
            ENDIF
            CALL DEFUFI(IUNIT,'&SYSCODE')
            CALL CODENT(IUNIT,'D',ZK8(LCODE+1))
            FICHIE='&SYSCODE'
            CATEST=ZK8(LCODE)
         ELSE
            CALL DEFUFI( 0 ,'&SYSCODE')
         ENDIF
         CALL DEFUFI( ISTAT ,'&SYSSTAT')
      ELSE
         CALL JEEXIN('&&SYS   .CODE',IRET)
         IF ( IRET .EQ. 0 ) THEN
            CALL DEFUFI(0,'&SYSCODE')
            CALL DEFUFI(0,'&SYSSTAT')
         ELSE
            CALL JEVEUO('&&SYS   .CODE','L',LCODE)
            CALL LXLIIS(ZK8(LCODE+1),IUNIT,IER)
            CALL DEFUFI(IUNIT,'&SYSCODE')
C
C           SUPPRESSION CAR POSITIONNEMENT APRES LA FIN DE FICHIER
C           EST-CE QUE CELA SERT A QUELQUE CHOSE
C-QUE       IF ( IUNIT .GT. 0 ) THEN
C              --- ON POSITIONNE LE FICHIER ----
C   1           READ(IUNIT, '(A)', END=2) CBID(1:1)
C               GOTO 1
C   2           CONTINUE
C-QUE        ENDIF
C
            CALL LXLIIS(ZK8(LCODE+2),ISTAT,IER)
            CALL DEFUFI(ISTAT,'&SYSSTAT')
         ENDIF
      ENDIF
C
C     --- RE-ECRITURE DE LA COMMANDE DE DEMARRAGE ---
      UL(1) = IUNIFI('MESSAGE')
      UL(2) = IUNIFI('&SYSCODE')
      UL(3) = IUNIFI('&SYSSTAT')
      CALL GCECDU( UL ,ICMDU, NUMINT )
      CALL JEDEMA()
      END
