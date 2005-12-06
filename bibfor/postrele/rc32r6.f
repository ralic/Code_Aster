      SUBROUTINE RC32R6 ( NOMRES )
      IMPLICIT   NONE
      CHARACTER*8         NOMRES
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 24/05/2004   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
C     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
C     STOCKAGE DES RESULTATS DANS LA TABLE DE SORTIE
C
C     ------------------------------------------------------------------
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
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32 JEXNOM,JEXNUM,JEXATR
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER       NPAR2, IM, IG, IS,
     +              NBSIGR, VALEI(2), JNUMGR, JNSITU, JNSG, 
     +              JPMPB, NBGR, IOC, NUMGR
      PARAMETER    ( NPAR2 = 5 )
      REAL*8        VALER(2)
      COMPLEX*16    C16B
      CHARACTER*4   LIEU(2)
      CHARACTER*8   K8B, TYPAR2(NPAR2)
      CHARACTER*16  NOPAR2(NPAR2)
      CHARACTER*24  K24B
C     ------------------------------------------------------------------
      DATA LIEU   / 'ORIG' , 'EXTR' /
C
      DATA NOPAR2 / 'NUME_GROUPE', 'LIEU', 'NUME_SITU' , 'SN', 'SN*' /
      DATA TYPAR2 / 'I', 'K8', 'I', 'R', 'R' /
C DEB ------------------------------------------------------------------
C
      CALL JELIRA ( '&&RC3200.SITU_NUME_GROUP', 'LONMAX', NBGR, K8B )
      CALL JEVEUO ( '&&RC3200.SITU_NUME_GROUP', 'L', JNUMGR )

      CALL JEVEUO ( '&&RC3200.SITU_NUMERO', 'L', JNSITU )
C
C     -----------------------------------------------------------------
C
        CALL TBAJPA ( NOMRES, NPAR2, NOPAR2, TYPAR2 )
C
        DO 400 IG = 1 , NBGR
          NUMGR = ZI(JNUMGR+IG-1)
          VALEI(1) = NUMGR
          CALL JELIRA(JEXNUM('&&RC3200.LES_GROUPES',NUMGR),'LONMAX',
     +                                                    NBSIGR,K8B)
          CALL JEVEUO(JEXNUM('&&RC3200.LES_GROUPES',NUMGR),'L',JNSG)

          DO 402 IS = 1 , NBSIGR
            IOC = ZI(JNSG+IS-1)
            VALEI(2) = ZI(JNSITU+IOC-1)
            DO 404 IM = 1 , 2
              K24B = '&&RC3200.PMPB       '//LIEU(IM)
              CALL JEVEUO ( JEXNUM(K24B,NUMGR), 'L', JPMPB )

              VALER(1) = ZR(JPMPB-1+6*(IS-1)+4)
              VALER(2) = ZR(JPMPB-1+6*(IS-1)+6)

              CALL TBAJLI ( NOMRES, NPAR2, NOPAR2, VALEI,
     +                      VALER, C16B, LIEU(IM), 0 )
C
 404        CONTINUE
 402      CONTINUE
 400    CONTINUE
C
      END
