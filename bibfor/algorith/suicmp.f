      SUBROUTINE SUICMP(MOTFAC,MAILLA,NBSUIV,SUIVCO)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 24/08/2005   AUTEUR MABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
      IMPLICIT     NONE
      CHARACTER*24 MOTFAC
      CHARACTER*8  MAILLA
      INTEGER      NBSUIV
      CHARACTER*24 SUIVCO
C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : SUIINI
C ----------------------------------------------------------------------
C
C SAISIE DU MOT CLE FACTEUR "SUIVI"
C  CREATION DE LA SD DE SUIVI
C
C IN  MOTFAC : MOT-CLEF POUR SUIVI
C IN  MAILLA : NOM DU MAILLAGE
C IN  NBSUIV : NOMBRE DE SUIVIS A FAIRE
C OUT SUIVCO : SD SUIVIS EN TEMPS REEL
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C
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
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      INTEGER      ISUI,IBID,IOCC
      INTEGER      NBNO,NBMA,NBPO

      CHARACTER*8  K8B,NOMGD,NOMCMP,NOMMAI,NOMNOE
      CHARACTER*16 CHAM
      INTEGER      NUMCMP,NUMPOI
      CHARACTER*24 SUIMAI,SUICHA,SUICOM,SUICNP,SUINOE,SUIVMA,SUIPOI
      INTEGER      KKKMA,KCHAM,KCOMP,KNUCM,KNOEU,KMAIL,KPOIN
      CHARACTER*24 SUICNT,SUIINF
      INTEGER      KNUCT,KINFO
      INTEGER      NBNOCP,NBNUCP
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      SUIVCO = '&&SUICMP     '
      SUIMAI = SUIVCO(1:14)//'.MAILLA'
      SUIINF = SUIVCO(1:14)//'.INFO'
      CALL WKVECT(SUIMAI,'V V K8' ,1     ,KKKMA)
      CALL WKVECT(SUIINF,'V V I'  ,1     ,KINFO)

      ZK8(KKKMA) = MAILLA
      ZI (KINFO) = NBSUIV

      IF (NBSUIV.NE.0) THEN

        SUICHA = SUIVCO(1:14)//'.NOM_CHAM'
        SUICOM = SUIVCO(1:14)//'.NOM_CMP'
        SUICNP = SUIVCO(1:14)//'.NUME_CMP'
        SUICNT = SUIVCO(1:14)//'.TYPE_CMP'
        SUINOE = SUIVCO(1:14)//'.NOEUD'
        SUIVMA = SUIVCO(1:14)//'.MAILLE'
        SUIPOI = SUIVCO(1:14)//'.POINT'


        CALL WKVECT(SUICHA,'V V K16',NBSUIV,KCHAM)
        CALL WKVECT(SUICOM,'V V K8' ,NBSUIV,KCOMP)
        CALL WKVECT(SUICNP,'V V I'  ,NBSUIV,KNUCM)
        CALL WKVECT(SUICNT,'V V I'  ,NBSUIV,KNUCT)
        CALL WKVECT(SUINOE,'V V K8' ,NBSUIV,KNOEU)
        CALL WKVECT(SUIVMA,'V V K8' ,NBSUIV,KMAIL)
        CALL WKVECT(SUIPOI,'V V I'  ,NBSUIV,KPOIN)
      ENDIF
C
      DO 10 IOCC = 1 , NBSUIV
C
C --- NUMERO D'ORDRE DU SUIVI
C
        CALL GETVIS(MOTFAC,'NUME_SUIVI',IOCC,1,1,ISUI,IBID)

        IF ((ISUI.GT.4).OR.(ISUI.LT.1)) THEN
          CALL UTMESS('F','SUICMP',
     &      'NUME_SUIVI INCORRECT DANS SUIVI_DDL')
        ENDIF
C
C ---  TYPE DU CHAMP
C
         CALL GETVTX(MOTFAC,'NOM_CHAM',IOCC,1,1,CHAM,IBID)

         IF ( CHAM(1:4) .EQ. 'DEPL' ) THEN
            NOMGD = 'DEPL_R'
         ELSEIF ( CHAM(1:4) .EQ. 'VITE' ) THEN
            NOMGD = 'DEPL_R'
         ELSEIF ( CHAM(1:4) .EQ. 'ACCE' ) THEN
            NOMGD = 'DEPL_R'
         ELSEIF ( CHAM(1:9) .EQ. 'VALE_CONT' ) THEN
            NOMGD = 'INFC_R'
         ELSEIF ( CHAM(1:9) .EQ. 'FORC_NODA' ) THEN
            NOMGD = 'FORC_R'
         ELSEIF ( CHAM(1:9) .EQ. 'SIEF_ELGA' ) THEN
            NOMGD = 'SIEF_R'
         ELSEIF ( CHAM(1:9) .EQ. 'VARI_ELGA' ) THEN
            NOMGD = 'VARI_R'
         ENDIF

         ZK16(KCHAM-1+ISUI) = NOMGD
C
C --- COMPOSANTE DU CHAMP
C
         NBNOCP = 1
         NBNUCP = 1

         CALL UTCMP2(NOMGD,MOTFAC,IOCC,NOMCMP,NBNOCP,NUMCMP,NBNUCP)
         ZK8(KCOMP-1+ISUI) = NOMCMP
         ZI(KNUCM-1+ISUI)  = NUMCMP
C
C --- LIEU D'EXTRACTION
C
         CALL GETVID(MOTFAC,'NOEUD'  , IOCC,1,0, K8B ,NBNO )
         CALL GETVID(MOTFAC,'MAILLE' , IOCC,1,0, K8B ,NBMA )
         CALL GETVIS(MOTFAC,'POINT'  , IOCC,1,0, IBID,NBPO )
         IF ( NBNO .NE. 0 ) THEN
            CALL GETVID(MOTFAC,'NOEUD', IOCC,1,1,NOMNOE,IBID)
            ZK8(KNOEU-1+ISUI)   = NOMNOE
            ZI(KNUCT-1+ISUI)    = 1
         ENDIF
         IF ( NBMA .NE. 0 ) THEN
            CALL GETVID(MOTFAC,'MAILLE',IOCC,1,1,NOMMAI,IBID)
            ZK8(KMAIL-1+ISUI)   = NOMMAI
            ZI(KNUCT-1+ISUI)    = 2
         ENDIF
         IF ( NBPO .NE. 0 ) THEN
            CALL GETVIS(MOTFAC,'POINT', IOCC,1,1,NUMPOI,IBID)
            ZI(KPOIN-1+ISUI)    = NUMPOI
            ZI(KNUCT-1+ISUI)    = 2
         ENDIF
C
C --- QUELQUES VERIFICATIONS
C
         IF ( NOMGD(1:6) .EQ. 'DEPL_R' .OR.
     &        NOMGD(1:6) .EQ. 'FORC_R' .OR.
     &        NOMGD(1:6) .EQ. 'INFC_R' ) THEN
             
            IF ( NBNO.EQ.0) THEN
               CALL UTDEBM('F','SUICMP','ERREUR DANS LES DONNEES '//
     &                      'DE SUIVI')
               CALL UTIMPK('L','POUR "NOM_CHAM" ',1,NOMGD)
               CALL UTIMPK('S',' IL FAUT RENSEIGNER ',1,'NOEUD')
               CALL UTFINM()
            ENDIF
C
         ELSEIF ( NOMGD(1:6) .EQ. 'SIEF_R' .OR.
     &            NOMGD(1:6) .EQ. 'VARI_R' ) THEN
            IF ( NBMA*NBPO.EQ.0) THEN
               CALL UTDEBM('F','SUICMP','ERREUR DANS LES DONNEES '//
     &                      'DE SUIVI')
               CALL UTIMPK('L','POUR "NOM_CHAM" ',1,NOMGD)
               CALL UTIMPK('S',' IL FAUT RENSEIGNER ',1,'MAILLE')
               CALL UTIMPK('S',' ET ',1,'POINT')
               CALL UTFINM()
            ENDIF
         ENDIF
C
C
 10   CONTINUE
C
      CALL JEDEMA()
C
      END
