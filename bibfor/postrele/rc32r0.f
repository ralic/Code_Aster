      SUBROUTINE RC32R0 ( NOMRES, PMPB, SN, SNET )
      IMPLICIT   NONE
      CHARACTER*8         NOMRES
      LOGICAL                     PMPB, SN, SNET
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 16/02/2009   AUTEUR GALENNE E.GALENNE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*32 JEXNUM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER       NPAR0, NPAR1, NPAR2, IM, IG, IS, NBSIGR, VALEI(2),
     +              JNUMGR, JNSITU, JNSG, JPMPBA, JPMPBS, NBGR, IOC,
     +              NUMGR, JVALE, IBID, N1
      PARAMETER    ( NPAR0 = 15 , NPAR1 = 7 , NPAR2 = 10 )
      REAL*8        VALER(5), R8VIDE
      COMPLEX*16    C16B
      CHARACTER*4   LIEU(2)
      CHARACTER*8   K8B, VALEK(3), TYPAR0(NPAR0), TYPAR1(NPAR1), TYPTAB
      CHARACTER*16  NOPAR0(NPAR0), NOPAR1(NPAR1), NOPAR2(NPAR2)
      CHARACTER*24  K24A, K24S
C     ------------------------------------------------------------------
      DATA LIEU   / 'ORIG' , 'EXTR' /
C
      DATA NOPAR0 / 'TYPE','SEISME', 'NUME_GROUPE', 'LIEU', 'NUME_SITU',
     +              'PM', 'PB', 'PMPB', 'SN', 'SN*', 'PM_MAX',
     +              'PB_MAX', 'PMPB_MAX', 'SN_MAX' , 'SN*_MAX' /
      DATA TYPAR0 / 'K8', 'K8', 'I', 'K8', 'I', 'R', 'R', 'R', 'R' ,
     +                               'R', 'R', 'R', 'R', 'R' , 'R'  /
C
      DATA NOPAR1 / 'TYPE', 'LIEU', 'PM_MAX', 'PB_MAX', 'PMPB_MAX',
     +                              'SN_MAX', 'SN*_MAX' /
      DATA TYPAR1 / 'K8', 'K8', 'R', 'R', 'R', 'R' , 'R'  /
C
      DATA NOPAR2 / 'TYPE', 'SEISME', 'NUME_GROUPE', 'LIEU',
     +              'NUME_SITU', 'PM', 'PB', 'PMPB', 'SN', 'SN*'  /
C DEB ------------------------------------------------------------------
C
      CALL GETVTX ( ' ', 'TYPE_RESU', 1,1,1, TYPTAB , N1 )
C
      CALL JELIRA ( '&&RC3200.SITU_NUME_GROUP', 'LONMAX', NBGR, K8B )
      CALL JEVEUO ( '&&RC3200.SITU_NUME_GROUP', 'L', JNUMGR )

      CALL JEVEUO ( '&&RC3200.SITU_NUMERO', 'L', JNSITU )
C
C     -----------------------------------------------------------------
C
      IF ( TYPTAB .EQ. 'VALE_MAX' ) THEN
         CALL TBAJPA ( NOMRES, NPAR1, NOPAR1, TYPAR1 )
      ELSE
         CALL TBAJPA ( NOMRES, NPAR0, NOPAR0, TYPAR0 )
      ENDIF
C
C     -----------------------------------------------------------------
C
C --- STOCKAGE DES MAXIMA DANS LA TABLE
C
      VALEK(1) = 'MAXI'
      DO 130 IM = 1 , 2
C
         VALEK(2) = LIEU(IM)
C
         CALL JEVEUO ( '&&RC3200.RESULTAT  .'//LIEU(IM), 'L', JVALE )
C
         IF ( PMPB ) THEN
            VALER(1) = ZR(JVALE)
            VALER(2) = ZR(JVALE+1)
            VALER(3) = ZR(JVALE+2)
         ELSE
            VALER(1) = R8VIDE()
            VALER(2) = R8VIDE()
            VALER(3) = R8VIDE()
         ENDIF
         IF ( SN ) THEN
            VALER(4) = ZR(JVALE+5)
         ELSE
            VALER(4) = R8VIDE()
         ENDIF
         IF ( SNET ) THEN
            VALER(5) = ZR(JVALE+6)
         ELSE
            VALER(5) = R8VIDE()
         ENDIF
C
         CALL TBAJLI ( NOMRES, NPAR1, NOPAR1, IBID, VALER,
     +                                              C16B, VALEK, 0 )
 130  CONTINUE
C
      IF ( TYPTAB .EQ. 'VALE_MAX' ) GOTO 9999
C
C     -----------------------------------------------------------------
C
C --- STOCKAGE DES GRANDEURS PAR SITUATION
C
      VALEK(1) = 'SITU'
      DO 200 IG = 1 , NBGR
          NUMGR = ABS(ZI(JNUMGR+IG-1))
          VALEI(1) = NUMGR
          CALL JELIRA(JEXNUM('&&RC3200.LES_GROUPES',NUMGR),'LONMAX',
     +                                                    NBSIGR,K8B)
          CALL JEVEUO(JEXNUM('&&RC3200.LES_GROUPES',NUMGR),'L',JNSG)

          DO 202 IM = 1 , 2
            VALEK(3) = LIEU(IM)
            K24A = '&&RC3200.AVEC_SEISME'//LIEU(IM)
            CALL JEVEUO ( JEXNUM(K24A,NUMGR), 'L', JPMPBA )
            K24S = '&&RC3200.SANS_SEISME'//LIEU(IM)
            CALL JEVEUO ( JEXNUM(K24S,NUMGR), 'L', JPMPBS )

            VALEK(2) = 'AVEC'
            DO 204 IS = 1 , NBSIGR
              IOC = ZI(JNSG+IS-1)
              VALEI(2) = ZI(JNSITU+IOC-1)

              CALL TBAJLI ( NOMRES, NPAR2, NOPAR2, VALEI, 
     +                    ZR(JPMPBA-1+10*(IS-1)+1), C16B, VALEK, 0 )
 204        CONTINUE
C
            VALEK(2) = 'SANS'
            DO 206 IS = 1 , NBSIGR
              IOC = ZI(JNSG+IS-1)
              VALEI(2) = ZI(JNSITU+IOC-1)

              CALL TBAJLI ( NOMRES, NPAR2, NOPAR2, VALEI,
     +                    ZR(JPMPBS-1+10*(IS-1)+1), C16B, VALEK, 0 )
 206        CONTINUE
 202      CONTINUE
 200    CONTINUE
C
 9999 CONTINUE
C
      END
