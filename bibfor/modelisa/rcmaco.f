      SUBROUTINE RCMACO ( CHMAT , INDMAT ,NBMAT, IMATE ,EOUN)
      IMPLICIT   NONE
      CHARACTER*1        EOUN
      CHARACTER*8        CHMAT
      INTEGER            INDMAT,NBMAT,IMATE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 19/02/2008   AUTEUR MACOCCO K.MACOCCO 
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
C RESPONSABLE                            D6BHHJP J.P.LEFEBVRE
C
C     BUT: CREER L'OBJET NOMMAT//'      .CODI' ,LE REMPLIR ET RENVOYER
C          SON ADRESSE PAR RAPPORT A ZI
C
C IN  CHMAT  : NOM DU CHAM_MATER
C IN  INDMAT : INDICE DU PREMIER MATERIAU DANS LA LISTE
C IN  NBMAT  : NOMBRE DE MATERIAUX DU NUMERO D'OCCURRENCE DE AFFE
C IN  IMATE  : NUMERO DU GROUPE
C     IGRP   : ADRESSE DU VECTEUR K8 CONTENANT LES NOMS DES MATERIAUX
C     INGRP  : ADRESSE DU VECTEUT ENTIER CONTENANT LE NOMBRE DE MATERIAU
C              POUR CHAQUE OCCURRENCE DE AFFE
C IN  EOUN  : / 'E' : MATERIAU PAR ELEMENTS
C             / 'N' : MATERIAU PAR NOEUDS
C
C    CODI(1)   : ADRESSE ZK16  DE '.MATERIAU.NOMRC'
C    CODI(2)   : NOMBRE DE TYPES DE COMPORTEMENT
C
C         P.I = CODI(2+I)
C
C    CODI(2+I)  :POINTEUR DANS .CODI DU IEME COMPORTEMENT
C    CODI(P.I)  :NOMBRE DE COEFFICIENTS REELS
C    CODI(P.I+1):NOMBRE DE COEFFICIENTS COMPLEXES
C    CODI(P.I+2):NOMBRE DE COEFFICIENTS FONCTIONS
C    CODI(P.I+3):ADRESSE ZK8 RELATIVE AU .VALK DES PARAMETRES (NOMS)
C    CODI(P.I+4):ADRESSE ZR  RELATIVE AU .VALR DES REELS
C    CODI(P.I+5):ADRESSE ZC  RELATIVE AU .VALC DES COMPLEXES
C
C         P.IF = P.I+6
C
C    CODI(P.IF+LFCT*(K-1))  :NOMBRE DE POINTS DE LA FONCTION ASSOCIEE
C    CODI(P.IF+LFCT*(K-1)+1):ADRESSE ZK16 DU .PROL
C    CODI(P.IF+LFCT*(K-1)+2):ADRESSE ZR  DU .VALE
C    CODI(P.IF+LFCT*(K-1)+3):ADRESSE ZI  DU POINTEUR DE LONGUEUR(NAPPE)
C    CODI(P.IF+LFCT*(K-1)+4):ADRESSE ZR  DU .PARA (NAPPE)
C    CODI(P.IF+LFCT*(K-1)+5):LONUTI  DU .PARA (NAPPE)
C    CODI(P.IF+LFCT*(K-1)+6):POINTEUR SUPPLEMENTAIRE (TRACTION,TRC)
C    CODI(P.IF+LFCT*(K-1)+7):INDICE DE L'INTERVALLE POUR INTERPOLATION
C    CODI(P.IF+LFCT*(K-1)+8):INDICE SUPPLEMENTAIRE
C
C         P.IFC = CODI(P.IF+LFCT*(K-1)+6))
C
C    CODI(P.IFC)  :ADRESSE ZK8 DU .&&RDEP.PROL
C    CODI(P.IFC+1):ADRESSE ZR  DU .&&RDEP.VALE
C
C ----------------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
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
      CHARACTER*32       JEXNUM , JEXNOM , JEXATR
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER            IRET,IRETF,IRETT,NBCM,JNOMRC,LMAT,LFCT,LSUP
      INTEGER            NBCMP,JNBCM,JDESC,L,NBV,NBTT,NBCOT,NBCMT,NBCO
      INTEGER        NBT,JDIM,K,KK,NBK,LGCODI,ISUNDF,ISNNEM,IDMA
      INTEGER        IMAT,IPI,IPIF,NBPTS,IPIFC,M,IGRP,IER,IRETC
      INTEGER        JCODI,JNOMR,JJDIM,JLCOD,IPI0
      REAL*8         TDEF,PREC
      CHARACTER *4       KNUMA1
      CHARACTER *3       KNUMA2
      CHARACTER *3       KNUMA3
      CHARACTER *8       K8B,NOMMAT,NOPARA,NOMGD,MATERI
      CHARACTER *19      CH19,CHMA,LISTR,CODI
C ----------------------------------------------------------------------
C PARAMETER ASSOCIE AU MATERIAU CODE
C
C --- LMAT   : NOMBRE DE PARAMETRES ASSOCIES AU COMPORTEMENT
C --- LFCT   : NOMBRE DE PARAMETRES ASSOCIES AUX FONCTIONS
C --- LSUP   : NOMBRE DE PARAMETRES SUPPLEMENTAIRE (COURBE &&RDEP)
      PARAMETER        ( LMAT = 7 , LFCT = 9 , LSUP = 2 )
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      PREC   = 1.0D0

      IF (EOUN.EQ.'E') THEN
        CALL JEVEUT(CHMAT(1:8)//'.MATE_CODE.GRP' ,'L',IGRP)
      ELSE IF (EOUN.EQ.'N') THEN
        CALL JEVEUT(CHMAT(1:8)//'.MATN_CODE.GRP' ,'L',IGRP)
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF

      MATERI=ZK8(IGRP+INDMAT)
      NOMMAT = MATERI
      CALL JEVEUO (CHMAT(1:8)//'.CHAMP_MAT .DESC', 'L', JDESC )
      CALL JENUNO(JEXNUM('&CATA.GD.NOMCMP',ZI(JDESC)),NOMGD)
      CALL DISMOI('F','NB_CMP_MAX',NOMGD,'GRANDEUR',NBCMP,K8B,IER)
      CALL CODENT ( IMATE, 'D0', KNUMA1 )
      CODI  = ' '
      CODI(1:8)  = NOMMAT
      CODI(9:13) = '.'//KNUMA1
      CALL JEEXIN(CODI//'.CODI',IRETC)
      IF ( IRETC .NE. 0 ) THEN
        CALL JEVEUT(CODI//'.CODI','L',JCODI)
        GOTO 999
      ENDIF

      CALL WKVECT ( '&&RCMACO.NBCM', 'V V I', NBMAT, JNBCM )
      CALL WKVECT ( '&&RCMACO.NOMR', 'V V I', NBMAT, JNOMR )
      CALL WKVECT ( '&&RCMACO.JDIM', 'V V I', NBMAT, JJDIM )
      CALL WKVECT ( '&&RCMACO.LCOD', 'V V I', NBMAT, JLCOD )
      DO 100 L=1,NBMAT
         NOMMAT=ZK8(IGRP+INDMAT+L-1)
         CALL JEEXIN(NOMMAT//'.MATERIAU.NOMRC',IRET)
         IF ( IRET .EQ. 0 ) THEN
             CALL U2MESK('F','MODELISA6_63',1,NOMMAT)
         ENDIF
         CALL JELIRA(NOMMAT//'.MATERIAU.NOMRC','LONMAX',
     &               ZI(JNBCM+L-1),K8B)
         CALL JEVEUT(NOMMAT//'.MATERIAU.NOMRC','L',ZI(JNOMR+L-1))
         NBV = 0
         IF ( ZK16(ZI(JNOMR+L-1)).EQ.'ELAS_COQMU' )  NBV =  1
         IF ( ZK16(ZI(JNOMR+L-1)+NBV).EQ.'THER_COQMU') NBV = NBV+1
         IF ( NBV .GT. 0 ) ZI(JNBCM+L-1) = NBV
 100  CONTINUE
C
      NBTT  = 0
      NBCOT = 0
      NBCMT=0
      DO 200 L=1,NBMAT
         NBCO  = 0
         NBT  = 0
         NOMMAT=ZK8(IGRP+INDMAT+L-1)
         NBCM=ZI(JNBCM+L-1)
         JNOMRC=ZI(JNOMR+L-1)
         CALL CODENT ( L, 'D0', KNUMA3 )
         CALL JEEXIN('&&RCMACO.DIM'//KNUMA3,IRET)
         IF (IRET.NE.0) THEN
            CALL JEDETR('&&RCMACO.DIM'//KNUMA3)
         ENDIF
         CALL WKVECT ('&&RCMACO.DIM'//KNUMA3,'V V I',LMAT*NBCM,
     &                ZI(JJDIM+L-1))
         JDIM=ZI(JJDIM+L-1)
         DO 10 K = 1 , NBCM
           KK = JDIM+LMAT*(K-1)
           CHMA = NOMMAT//'.'//ZK16(JNOMRC+K-1)(1:10)
           CALL CODENT(K, 'D0', KNUMA2)
C           CH19 = '&&'//CHMA(1:8)//'.'//KNUMA2//'.'//KNUMA1
           CH19 = CHMA(1:8)//'.'//KNUMA2//KNUMA1//KNUMA3
           CALL JEDUPC(' ', CHMA, 1, 'V', CH19, .FALSE.)
           CALL JELIRA ( CH19//'.VALR', 'LONUTI', ZI(KK)  , K8B )
           CALL JEVEUT ( CH19//'.VALR', 'L',      ZI(KK+1)      )
           CALL JELIRA ( CH19//'.VALC', 'LONUTI', ZI(KK+2), K8B )
           CALL JEVEUT ( CH19//'.VALC', 'L',      ZI(KK+3)      )
           CALL JELIRA ( CH19//'.VALK', 'LONUTI', NBK     , K8B )
           CALL JEVEUT ( CH19//'.VALK', 'L',      ZI(KK+5)      )
           ZI(KK+4) = ( NBK - ZI(KK) - ZI(KK+2) ) / 2
           NBCO = NBCO + ZI(KK+4)
           IF ((ZK16(JNOMRC+K-1)(1:8) .EQ. 'TRACTION') ) THEN
              ZI(KK+6) = 1
              NBT = NBT + 1
           ENDIF
           IF (ZK16(JNOMRC+K-1)(1:13) .EQ. 'META_TRACTION')THEN
              ZI(KK+6) = 1
              NBT = NBT + NBK/2
           ENDIF
 10      CONTINUE
         ZI(JLCOD+L-1)=2 + LMAT*NBCM+ LFCT*NBCO + LSUP*NBT
         NBCMT=NBCMT+NBCM
         NBTT = NBTT + NBT
         NBCOT = NBCOT + NBCO
 200  CONTINUE
C
      LGCODI= 2*NBMAT+1+ 2*NBMAT + LMAT*NBCMT + LFCT*NBCOT + LSUP*NBTT
      CALL WKVECT ( CODI//'.CODI', 'V V I',LGCODI , JCODI )
      CALL JEVEUT ( CODI//'.CODI' , 'E', JCODI )
      ISUNDF = ISNNEM()
      DO 12 K = 1, LGCODI
        ZI(JCODI + K-1) = ISUNDF
 12   CONTINUE
      ZI(JCODI  ) = NBMAT
      IPI0=2*NBMAT+1
      IDMA=IPI0
      DO 300 IMAT=1,NBMAT
        NOMMAT=ZK8(IGRP+INDMAT+IMAT-1)
        NBCM=ZI(JNBCM+IMAT-1)
        JNOMRC=ZI(JNOMR+IMAT-1)
        JDIM=ZI(JJDIM+IMAT-1)
        ZI(JCODI+IMAT)=IGRP+INDMAT+IMAT-1
        ZI(JCODI+IMAT+NBMAT)=IDMA
C
        ZI(JCODI+IDMA  ) = JNOMRC
        NBCM=ZI(JNBCM+IMAT-1)
        ZI(JCODI+IDMA+1) = NBCM
        IPI = JCODI+IDMA+2+NBCM
C
        DO 20 K = 1,NBCM
C
          CHMA = NOMMAT//'.'//ZK16(JNOMRC+K-1)(1:10)
C
          KK = JDIM+LMAT*(K-1)
          ZI(JCODI+IDMA+1+K) = IPI
          ZI(IPI)   = ZI(KK)
          ZI(IPI+1) = ZI(KK+2)
          ZI(IPI+2) = ZI(KK+4)
          ZI(IPI+3) = ZI(KK+5)
          ZI(IPI+4) = ZI(KK+1)
          ZI(IPI+5) = ZI(KK+3)
          IPIF = IPI+LMAT-1
C
C ---     BOUCLE SUR LE NOMBRE DE COEFFICIENTS REELS :
C         ------------------------------------------
          DO 21 L = 0,ZI(KK)-1
            CH19 = ZK8(ZI(KK+5)+L)
            IF (CH19.EQ.'PRECISIO') PREC  = ZR(ZI(KK+1)+L)
  21      CONTINUE
          DO 22 L = 0,ZI(KK)-1
            CH19 = ZK8(ZI(KK+5)+L)
            IF (CH19.EQ.'TEMP_DEF') THEN
               TDEF  = ZR(ZI(KK+1)+L)
C
C ---       BOUCLE SUR LES FONCTIONS :
C           ------------------------
               DO 23 M = 0,ZI(KK+4)-1
                 CH19   = ZK8(ZI(KK+5)+ZI(KK)+ZI(KK+2)+ZI(KK+4)+M)
                 NOPARA = ZK8(ZI(KK+5)+ZI(KK)+ZI(KK+2)+M)
                 IF (NOPARA(1:5).EQ.'ALPHA'.OR.
     &               NOPARA.EQ.'F_ALPHA '.OR.NOPARA.EQ.'C_ALPHA ') THEN
C
C ---        INTERPOLATION DES COEFFICIENTS DE DILATATION ALPHA
C ---        EN TENANT COMPTE DE LA TEMPERATURE DE DEFINITION TDEF :
C            -----------------------------------------------------
                    CALL ALFINT ( CHMAT, IMATE, NOMMAT, TDEF, NOPARA,
     &                                            K, PREC, CH19,EOUN)
                    ZK8(ZI(KK+5)+ZI(KK)+ZI(KK+2)+ZI(KK+4)+M) = CH19
                 ENDIF
  23           CONTINUE
C
            ENDIF
  22      CONTINUE
C
C ---     BOUCLE SUR LE NOMBRE DE COEFFICIENTS FONCTIONS :
C         ------------------------------------------------
          DO 25 L = 0 , ZI(KK+4)-1
            CH19   = ZK8(ZI(KK+5)+ZI(KK)+ZI(KK+2)+ZI(KK+4)+L)
            CALL EXISD ( 'FONCTION',CH19(1:8), IRETF )
            CALL EXISD ( 'TABLE'   ,CH19(1:8), IRETT )
C ---   DES FONCTIONS SONT CREEES SUR LA VOLATILE (ROUTINE ALFINT) ---
            IF ( IRETF .EQ. 1 ) THEN
               CALL JEVEUT ( CH19//'.PROL', 'L', ZI(IPIF+1) )
               ZI(IPIF+7) = 1
               ZI(IPIF+8) = 1
               IF ( ZK24(ZI(IPIF+1))(1:1) .EQ. 'C' .OR.
     &             ZK24(ZI(IPIF+1))(1:1) .EQ. 'F' ) THEN
                  CALL JEVEUT ( CH19//'.VALE', 'L', ZI(IPIF+2) )
                  CALL JELIRA ( CH19//'.VALE', 'LONMAX', NBPTS, K8B )
                  ZI(IPIF) = NBPTS/2
               ELSEIF ( ZK24(ZI(IPIF+1))(1:1) .EQ. 'N' ) THEN
                  CALL JEVEUT ( CH19//'.VALE', 'L', ZI(IPIF+2) )
                  CALL JEVEUT ( JEXATR(CH19//'.VALE','LONCUM'), 'L',
     &                                                     ZI(IPIF+3))
                  CALL JEVEUT ( CH19//'.PARA', 'L', ZI(IPIF+4) )
                  CALL JELIRA ( CH19//'.PARA', 'LONUTI',ZI(IPIF+5),K8B)
               ELSEIF ( ZK24(ZI(IPIF+1))(1:1) .EQ. 'I' ) THEN
               ELSE
                  CALL U2MESK('F','MODELISA6_64',1,ZK24(ZI(IPIF+1)))
               ENDIF
            ELSE IF ( IRETT .EQ. 1 ) THEN
              LISTR = '&&'//CH19(1:8)//'_LR8'
              CALL JEEXIN(LISTR//'.VALE',IRETC)
              IF ( IRETC .EQ. 0 ) THEN
                CALL TBEXLR ( CH19, LISTR, 'V' )
              ENDIF
              CALL JEVEUT ( LISTR//'.VALE' , 'L', ZI(IPIF) )
              ZI(IPIF+1) = 0
              ZI(IPIF+2) = 0
            ELSE
               CALL U2MESK('F','MODELISA6_64',1,CH19(1:8))
            ENDIF
C
            IF ( ZI(KK+6) .EQ. 1 ) THEN
               IF(( ZK16(JNOMRC+K-1)(1:8) .EQ. 'TRACTION' ). OR .
     &            (ZK16(JNOMRC+K-1)(1:13) .EQ. 'META_TRACTION'))THEN
                 IPIFC = IPIF+LFCT
                 ZI(IPIF+6) = IPIFC
                 CH19 = NOMMAT//'.&&RDEP'
                 CALL JEVEUT ( CH19//'.PROL', 'E', ZI(IPIFC)   )
                 CALL JEVEUT ( CH19//'.VALE', 'E', ZI(IPIFC+1) )
                 IPIF = IPIFC + LSUP
               ELSE
                 IPIF = IPIF + LFCT
               ENDIF
            ELSE
               IPIF = IPIF + LFCT
            ENDIF
 25       CONTINUE
          IPI = IPIF
 20     CONTINUE
        IDMA=IDMA+ZI(JLCOD+IMAT-1)
 300  CONTINUE
C
      CALL JEDETC('V','&&RCMACO.DIM',1)
      CALL JEDETR('&&RCMACO.NBCM')
      CALL JEDETR('&&RCMACO.NOMR')
      CALL JEDETR('&&RCMACO.JDIM')
      CALL JEDETR('&&RCMACO.LCOD')
C
 999  CONTINUE
      CALL JEDEMA()
C
      END
