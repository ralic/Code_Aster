      SUBROUTINE TBCOPI ( BASE, SD1, SD2 )
      IMPLICIT NONE
      CHARACTER*(*)       BASE, SD1, SD2
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 11/01/2000   AUTEUR D6BHHBQ B.QUINNEZ 
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
C
C   BUT:
C   DUPLIQUER UNE STRUCTURE DE DONNEES TABLE.
C
C     IN:
C     BASE     : 'G' , 'V' , ... : BASE DE CREATION DE SD2
C     SD1 (K*) : NOM DE LA SD A DUPPLIQUER
C     SD2 (K*) : NOM DE LA SD A CREER
C
C     OUT:
C     SD2 EST CREEE ET A LE MEME CONTENU QUE SD1
C
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
      INTEGER      I, J, NBPM, NBPU, JNJV, KNJV, KVALE, JVALE, NBPARA,
     +             JTBBA, KTBNP, NBLIGN, JTBNP, NDIM, JTBLP, KTBLP
      CHARACTER*1  BAS2, KBID
      CHARACTER*4  TYPE, KNUME
      CHARACTER*19 TAB1, TAB2
      CHARACTER*24 NOMJV
C
C DEB-------------------------------------------------------------------
C
      CALL JEMARQ()
      BAS2 = BASE
C
      TAB1 = SD1
      TAB2 = SD2
C
      CALL WKVECT ( TAB2//'.TBBA', BAS2//' V K8', 1, JTBBA )
      ZK8(JTBBA) = BAS2
      CALL JEVEUO(TAB1//'.TBNP' , 'L', KTBNP )
      NBPARA = ZI(KTBNP  )
      NBLIGN = ZI(KTBNP+1)
C
      CALL WKVECT(TAB2//'.TBNP',BAS2//' V I',2,JTBNP)
      ZI(JTBNP  ) = NBPARA
      ZI(JTBNP+1) = NBLIGN
      NDIM = 4 * NBPARA
      CALL JECREO ( TAB2//'.TBLP', BAS2//' V K24')
      CALL JEECRA ( TAB2//'.TBLP', 'LONMAX', NDIM, ' ')
      CALL JEECRA ( TAB2//'.TBLP', 'LONUTI', NDIM, ' ')
      CALL JEVEUO ( TAB2//'.TBLP' , 'E', JTBLP )
      CALL JEVEUO ( TAB1//'.TBLP' , 'L', KTBLP )
      DO 10 I = 1 , NBPARA
         ZK24(JTBLP+4*(I-1)  ) = ZK24(KTBLP+4*(I-1)  )
         TYPE = ZK24(KTBLP+4*(I-1)+1)
         ZK24(JTBLP+4*(I-1)+1) = TYPE
         NOMJV = ZK24(KTBLP+4*(I-1)+2)
         CALL JELIRA ( NOMJV, 'LONMAX', NBPM, KBID)
         CALL JELIRA ( NOMJV, 'LONUTI', NBPU, KBID)
         CALL CODENT ( I ,'D0',KNUME)
         NOMJV = TAB2(1:17)//'LG.'//KNUME
         ZK24(JTBLP+4*(I-1)+3) = NOMJV
         CALL JECREO ( NOMJV ,BAS2//' V L' )
         CALL JEECRA ( NOMJV , 'LONMAX' , NBPM , ' ' )
         CALL JEECRA ( NOMJV , 'LONUTI' , NBPU , ' ' )
         CALL JEVEUO ( NOMJV , 'E', JNJV )
         NOMJV = TAB1(1:17)//'LG.'//KNUME
         CALL JEVEUO ( NOMJV , 'L', KNJV )
         DO 12 J = 1 , NBPM
           ZL(JNJV+J-1) = ZL(KNJV+J-1)
 12      CONTINUE
         NOMJV = TAB1//'.'//KNUME
         CALL JEVEUO ( NOMJV , 'L', KVALE )
         NOMJV = TAB2//'.'//KNUME
         ZK24(JTBLP+4*(I-1)+2) = NOMJV
         CALL JECREO ( NOMJV , BAS2//' V '//TYPE )
         CALL JEECRA ( NOMJV , 'LONMAX' , NBPM , ' ' )
         CALL JEECRA ( NOMJV , 'LONUTI' , NBPU , ' ' )
         CALL JEVEUO ( NOMJV , 'E', JVALE )
         IF ( TYPE(1:1) .EQ. 'I' ) THEN
            DO 20 J = 1 , NBPM
               ZI(JVALE+J-1) = ZI(KVALE+J-1)
 20         CONTINUE
         ELSEIF ( TYPE(1:1) .EQ. 'R' ) THEN
            DO 21 J = 1 , NBPM
               ZR(JVALE+J-1) = ZR(KVALE+J-1)
 21         CONTINUE
         ELSEIF ( TYPE(1:1) .EQ. 'C' ) THEN
            DO 22 J = 1 , NBPM
               ZC(JVALE+J-1) = ZC(KVALE+J-1)
 22         CONTINUE
         ELSEIF ( TYPE(1:3) .EQ. 'K80' ) THEN
            DO 23 J = 1 , NBPM
               ZK80(JVALE+J-1) = ZK80(KVALE+J-1)
 23         CONTINUE
         ELSEIF ( TYPE(1:3) .EQ. 'K32' ) THEN
            DO 24 J = 1 , NBPM
               ZK32(JVALE+J-1) = ZK32(KVALE+J-1)
 24         CONTINUE
         ELSEIF ( TYPE(1:3) .EQ. 'K24' ) THEN
            DO 25 J = 1 , NBPM
               ZK24(JVALE+J-1) = ZK24(KVALE+J-1)
 25         CONTINUE
         ELSEIF ( TYPE(1:3) .EQ. 'K16' ) THEN
            DO 26 J = 1 , NBPM
               ZK16(JVALE+J-1) = ZK16(KVALE+J-1)
 26         CONTINUE
         ELSEIF ( TYPE(1:2) .EQ. 'K8' ) THEN
            DO 27 J = 1 , NBPM
               ZK8(JVALE+J-1) = ZK8(KVALE+J-1)
 27         CONTINUE
         ENDIF
         CALL JEECRA ( NOMJV , 'LONUTI' , NBPU , ' ' )
 10   CONTINUE
C
      CALL JEDEMA
      END
