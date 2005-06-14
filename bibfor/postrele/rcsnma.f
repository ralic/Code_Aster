      SUBROUTINE RCSNMA ( SNMAX , NBOCC )
C
      IMPLICIT    NONE
      INTEGER     NBOCC
      CHARACTER*8 SNMAX
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 01/05/2002   AUTEUR CIBHHBC R.FERNANDES 
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
C ======================================================================
C BUT : (POST_RCCM) RECUPERER LES VALEURS MAXIMALES DE SN --------------
C ----- POUR TOUTES LES COMBINAISONS DE TRANSITOIRES -------------------
C ======================================================================
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
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C ======================================================================
      INTEGER      VI(2), IOCC1, IOCC2, NBSN, JSNO, JSNE, JSNUM, ISN
      INTEGER      IDSNOM, IDSNEM
      REAL*8       R8B, SNOMAX, SNEMAX
      COMPLEX*16   CBID
      CHARACTER*2  LCRPA(2)
      CHARACTER*8  K8B
      CHARACTER*10 LIPACR(2)
      CHARACTER*19 RCCMTA, VECSNO, VECSNE, VECNUM
C ======================================================================
      CALL JEMARQ()
C ======================================================================
      LIPACR(1) = 'TRANSIT1'
      LIPACR(2) = 'TRANSIT2'
      LCRPA(1)  = 'EQ'
      LCRPA(2)  = 'EQ'
      CALL        JEVEUO ( '&&OP0165.SNOMAX', 'E', IDSNOM  )
      CALL        JEVEUO ( '&&OP0165.SNEMAX', 'E', IDSNEM  )
C ======================================================================
C --- RECUPERATION DES SOUS-TABLES ASSOCIEES AUX DIFFERENTS TRANSITOIRES
C ======================================================================
      DO    10 IOCC1 = 1, NBOCC
         DO 20 IOCC2 = IOCC1, NBOCC
            RCCMTA    = '&&RCSNMA.SNOSNE'
            VI(1) = IOCC1
            VI(2) = IOCC2
            CALL TBEXTB (SNMAX, 'V', RCCMTA, 2, LIPACR, LCRPA,
     +                                    VI, R8B, CBID, K8B, R8B, K8B )
C ======================================================================
C --- RECUPERATION DES LISTES DES SNO ET SNE DE LA SOUS-TABLE RCCMTA ---
C ======================================================================
            VECSNO = '&&RCSNMA.VECSNO'
            VECSNE = '&&RCSNMA.VECSNE'
            CALL TBEXVE ( RCCMTA, 'SNOTRANS', VECSNO, 'V', NBSN, K8B )
            CALL TBEXVE ( RCCMTA, 'SNETRANS', VECSNE, 'V', NBSN, K8B )
            CALL JEVEUO ( VECSNO, 'L', JSNO  )
            CALL JEVEUO ( VECSNE, 'L', JSNE  )
C ======================================================================
C --- RECUPERATION DE LA LISTE DES INDICATEURS D'INSTANT DE ------------
C --- LA SOUS-TABLE RCCMTA ---------------------------------------------
C ======================================================================
            VECNUM = '&&RCSNMA.VECNUM'
            CALL TBEXVE ( RCCMTA, 'INDICAT' , VECNUM, 'V', NBSN, K8B )
            CALL JEVEUO ( VECNUM, 'L', JSNUM )
C ======================================================================
C --- INITIALISATION DES MAX DE SNO ET SNE -----------------------------
C ======================================================================
            SNOMAX = 0.0D0
            SNEMAX = 0.0D0
C ======================================================================
C --- RECHERCHE DES MAX DE SNO ET SNE ----------------------------------
C ======================================================================
            DO 30 ISN = 1, NBSN
               SNOMAX = MAX(SNOMAX,ZR(JSNO-1+ISN))
               SNEMAX = MAX(SNEMAX,ZR(JSNE-1+ISN))
 30         CONTINUE
C ======================================================================
C --- RECOPIE DES MAX DE SNO ET SNE ------------------------------------
C ======================================================================
            DO 40 ISN = 1, NBSN
               ZR(IDSNOM - 1 + ZI(JSNUM-1+ISN)) = SNOMAX
               ZR(IDSNEM - 1 + ZI(JSNUM-1+ISN)) = SNEMAX
 40         CONTINUE
C ======================================================================
C --- DESTRUCTION DES VECTEURS INUTILES --------------------------------
C ======================================================================
            CALL JEDETR (VECSNO)
            CALL JEDETR (VECSNE)
            CALL JEDETR (VECNUM)
            CALL DETRSD ('TABLE', RCCMTA)
 20      CONTINUE
 10   CONTINUE
C ======================================================================
      CALL JEDEMA()
C ======================================================================
      END
