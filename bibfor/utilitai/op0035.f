      SUBROUTINE OP0035 ( IER )
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 05/10/2004   AUTEUR REZETTE C.REZETTE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      INTEGER IER
            
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

C DECLARATION VARIABLES LOCALES      
      CHARACTER*8   RESULT,TYPVAL,K8BID, ETAT
      CHARACTER*16  TYPE, NOMCMD,K16BID
      CHARACTER*255 KFIC
      COMPLEX*16    CBID
      INTEGER       ULNUME, ULISOP, ULNOMF
      INTEGER       NBVAL,JKVAL,JTVAL,IBID,K,UL
      INTEGER       NBUNIT, NBFIC, ISOP
      REAL*8        RVAL,RBID
C
      CALL JEMARQ()
      CALL INFMAJ
      CALL GETRES(RESULT,TYPE,NOMCMD)

C=======================================================================
C- NOM DES PARAMETRES REELS A RECUPERER 
C=======================================================================

      CALL GETVTX(' ','LISTE_INFO',0,1,0,K16BID,NBVAL)
      NBVAL=-NBVAL
      CALL WKVECT('&&LISTE_INFO','V V K16',NBVAL,JKVAL)
      CALL WKVECT('&&TYPE_INFO','V V K8',NBVAL,JTVAL)

      CALL GETVTX(' ','LISTE_INFO',0,1,NBVAL,ZK16(JKVAL),IBID)
      DO 20 K=1,NBVAL
        IF ( ZK16(JKVAL+K-1) .EQ. 'CPU_RESTANT' ) THEN
          ZK8(JTVAL+K-1) = 'R'
        ELSEIF ( ZK16(JKVAL+K-1) .EQ. 'UNITE_LIBRE' ) THEN
          ZK8(JTVAL+K-1) = 'I'
        ELSEIF ( ZK16(JKVAL+K-1) .EQ. 'ETAT_UNITE' ) THEN
          ZK8(JTVAL+K-1) = 'K8'
        ENDIF
 20   CONTINUE

      CALL TBCRSD ( RESULT, 'G' )
      CALL TBAJPA ( RESULT, NBVAL, ZK16(JKVAL), ZK8(JTVAL) )

      DO 100 K=1,NBVAL
        IF ( ZK16(JKVAL+K-1) .EQ. 'CPU_RESTANT' ) THEN
          CALL UTTCPU (0,'    ',1,RVAL)
          CALL TBAJLI(RESULT,NBVAL,ZK16(JKVAL+K-1),IBID,RVAL,
     &                CBID,K8BID,0)
        ELSEIF ( ZK16(JKVAL+K-1) .EQ. 'UNITE_LIBRE' ) THEN
          UL = ULNUME ()
          CALL TBAJLI(RESULT,NBVAL,ZK16(JKVAL+K-1),UL,RBID,
     &                CBID,K8BID,0)
        ELSEIF ( ZK16(JKVAL+K-1) .EQ. 'ETAT_UNITE' ) THEN
          CALL GETVIS(' ','UNITE',      0,1,1,UL,NBUNIT)
          IF ( NBUNIT .EQ. 0 ) THEN
            CALL GETVTX(' ','FICHIER',0,1,1,KFIC,NBFIC)
            UL = ULNOMF (KFIC, K8BID, K8BID)
          ENDIF
          IF ( UL .LT. 0 ) THEN
            ISOP = 0
          ELSE
            ISOP = ULISOP (UL, K16BID)
          ENDIF
          IF ( ISOP .EQ. 0 ) THEN
            ETAT='FERME   '
          ELSE
            ETAT='OUVERT  '
          ENDIF
          CALL TBAJLI(RESULT,NBVAL,ZK16(JKVAL+K-1),IBID,RBID,
     &                CBID,ETAT,0)
        ENDIF 
 100  CONTINUE      

      CALL TITRE

      CALL JEDEMA()
      END
