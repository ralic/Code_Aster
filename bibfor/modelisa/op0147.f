      SUBROUTINE OP0147(IER)
      IMPLICIT REAL*8 (A-H,O-Z)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 05/10/2004   AUTEUR REZETTE C.REZETTE 
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
C-----------------------------------------------------------------------
C   CALCUL DES INTERSPECTRES DE REPONSE MODALE
C      LE CONCEPT PRODUIT EST TABL_INTSP
C   LE CONCEPT TABL_INTSP SE COMPOSE :
C      D UNE STRUCTURE TABLE QUI POINTE SUR UNE TABLE DE FONCTIONS
C      COMPLEXE
C-----------------------------------------------------------------------
C  OUT  : IER = 0 => TOUT S EST BIEN PASSE
C         IER > 0 => NOMBRE D ERREURS RENCONTREES
C-----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
C
      PARAMETER   ( NBPAR = 8 )
      REAL*8        R8B
      COMPLEX*16    C16B
      LOGICAL       CASINT
      CHARACTER*8   K8B, TABLE, NOMU, OPTION, TYPAR(NBPAR)
      CHARACTER*16  CONCEP, CMD, NOPAR(NBPAR), KVAL(2)
      CHARACTER*19  BASE
      CHARACTER*24  FREQ, MASG, VITE, NUMO, NOMOBJ
C
      DATA NOPAR / 'NOM_CHAM' , 'OPTION' , 'DIMENSION' , 
     +             'NUME_VITE_FLUI' , 'VITE_FLUIDE' ,
     +             'NUME_ORDRE_I' , 'NUME_ORDRE_J' , 'FONCTION' /
      DATA TYPAR / 'K16' , 'K16' , 'I' , 'I' , 'R' , 'I' , 'I' , 'K24' /
C
C-----------------------------------------------------------------------
      CALL JEMARQ()
      CALL INFMAJ()
C
      CALL GETRES(NOMU,CONCEP,CMD)
C
C --- 1.RECUPERATION DES OBJETS DE LA BASE MODALE PERTURBEE ---
C
      CALL GETVID(' ','BASE_ELAS_FLUI',0,1,1,BASE,ZI)
C
      MASG = BASE//'.MASG'
      VITE = BASE//'.VITE'
      FREQ = BASE//'.FREQ'
      NUMO = BASE//'.NUMO'
C
      CALL JEVEUO(MASG,'L',IMASG)
C
      CALL JEVEUO(VITE,'L',IVITE)
      CALL JELIRA(VITE,'LONUTI',NPV,K8B)
C
      CALL JEVEUO(FREQ,'L',IFREQ)
      CALL JELIRA(FREQ,'LONUTI',NBM,K8B)
      NBM = NBM / ( 2 * NPV )
C
      CALL JEVEUO(NUMO,'L',INUMO)
C
C --- 2.RECUPERATION DU NOM DE LA TABLE ---
C
      CALL GETVID('EXCIT ','INTE_SPEC_GENE',1,1,1,TABLE,ZI)
C
      NOMOBJ = '&&OP0147.TEMP.NUOR'
      CALL TBEXVE ( TABLE, 'NUME_ORDRE_I', NOMOBJ, 'V', NBMR, K8B )
      CALL JEVEUO ( NOMOBJ, 'L', JNUOR )
      CALL ORDIS  ( ZI(JNUOR) , NBMR )
      CALL WKVECT ( '&&OP0147.MODE', 'V V I', NBMR, INUOR )
      NNN = 1
      ZI(INUOR) = ZI(JNUOR)
      DO 20 I = 2 , NBMR
         IF ( ZI(JNUOR+I-1) .EQ. ZI(INUOR+NNN-1) ) GOTO 20
         NNN = NNN + 1
         ZI(INUOR+NNN-1) = ZI(JNUOR+I-1)
 20   CONTINUE
      NBMR = NNN
      DO 30 IM = 1,NBM
         IF (ZI(INUMO+IM-1).EQ.ZI(INUOR)) THEN
            IMOD1 = IM
            GOTO 31
         ENDIF
 30   CONTINUE
      CALL UTMESS('F','OP0147','PAS LE BON NUMERO DE MODE' )
 31   CONTINUE
C
C --- 3.RECUPERATION DE L'OPTION DE CALCUL ---
C
      CASINT = .TRUE.
      CALL GETVTX ( ' ', 'OPTION', 0,1,1, OPTION, IBID )
      IF ( OPTION(1:4) .EQ. 'DIAG' ) CASINT = .FALSE.
C
      CALL TBLIVA ( TABLE, 0, K8B, IBID, R8B, C16B, K8B, K8B, R8B, 
     +             'OPTION', K8B, IBID, R8B, C16B, K8B, IRET )
      IF ( IRET .NE. 0 ) CALL UTMESS('F','OP0147','Y A UN BUG 3' )
      IF ( K8B(1:4) .EQ. 'DIAG' .AND. CASINT ) THEN
        CALL UTMESS('F',CMD,'LE CALCUL DE TOUS LES INTERSPECTRES DE '//
     &              'REPONSE MODALE N EST PAS POSSIBLE CAR SEULS LES '//
     &              'AUTOSPECTRES D EXCITATION ONT ETE CALCULES.')
      ENDIF
C
C --- 4.CREATION DE LA STRUCTURE RESULTAT ET CALCUL DE LA REPONSE ---
C ---   PAR CALCSP                                                ---
C
      CALL TBCRSD ( NOMU, 'G' )
      CALL TBAJPA ( NOMU, NBPAR, NOPAR, TYPAR )
C
      KVAL(1) = 'DEPL_GENE'
      KVAL(2) = OPTION
      CALL TBAJLI ( NOMU, 3, NOPAR, NBMR, R8B, C16B, KVAL, 0 )
C
      CALL CALCSP ( CASINT, NOMU, TABLE, ZR(IFREQ), ZR(IMASG), NBM, NPV,
     &              NBMR, IMOD1, ZI(INUOR), ZR(IVITE) )
C
      CALL TITRE
C
C
      CALL JEDEMA()
      END
