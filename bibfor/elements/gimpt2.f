      SUBROUTINE GIMPT2 (RESU,NBRE,TRAV1,TRAV2,TRAV3,CHFOND,STOK4,
     &                   LOBJ2,IMPR)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C -----------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
C
C FONCTION REALISEE:
C
C     IMPRESSION DES OBJETS DECRIVANT LE CHAMP THETA
C
C     ENTREE :    RESU        -------> NOM DU CONCEPT CHAM_NO
C                 NBRE        -------> NOMBRE-1 DE CHAMPS THETA (=1)
C                 TRAV1       -------> RAYON INTERNE
C                                      SUR LE FOND DE FISSURE
C                 TRAV2       -------> RAYON EXTERNE
C                                      SUR LE FOND DE FISSURE
C                 TRAV3       -------> MODULE DE THETA
C                                      SUR LE FOND DE FISSURE
C                 CHFOND      -------> NOMS DES NOEUDS
C                                      SUR LE FOND DE FISSURE
C                 STOK4       -------> NORMALE
C                                      SUR LE FOND DE FISSURE
C                 LOBJ2       -------> NOMBRE DE NOEUDS
C                                      SUR LE FOND DE FISSURE
C                 IMPR        -------> TAUX D'IMPRESSION
C
C -----------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
C
      INTEGER      NBRE,IADRT1,IADRT2,IADRT3,IADRNO,IN2
      INTEGER      IRET,UNIT,LOBJ2,IMPR
      REAL*8       RINF,RSUP,THET,NX,NY,NZ
      CHARACTER*8  NOEUD,RESU
      CHARACTER*24 CHAMNO,TRAV1,TRAV2,TRAV3,STOK4,CHFOND
C -----------------------------------------------------------------
C
      IF ( IMPR .GE. 1 ) THEN
         CALL JEMARQ()
         UNIT = IUNIFI('RESULTAT')
         CALL JEVEUO ( TRAV1 , 'L', IADRT1 )
         CALL JEVEUO ( TRAV2 , 'L', IADRT2 )
         CALL JEVEUO ( TRAV3 , 'L', IADRT3 )
         CALL JEVEUO ( CHFOND, 'L', IADRNO )
         CALL JEVEUO ( STOK4 , 'L', IN2    )
         CALL JEVEUO ( RESU  , 'L', JRESU  )
         DO 10 J = 1 , NBRE+1
            WRITE(UNIT,50) J
            WRITE(UNIT,*)
            WRITE(UNIT,100)
            WRITE(UNIT,200)
            WRITE(UNIT,*)
            WRITE(UNIT,300)
            WRITE(UNIT,400)
            DO 20 I = 1 , LOBJ2
               NOEUD = ZK8(IADRNO+I-1)
               RINF = ZR(IADRT1+I-1)
               RSUP = ZR(IADRT2+I-1)
               THET = ZR(IADRT3+(J-1)*LOBJ2+I-1)
               NX   = ZR(IN2+(I-1)*3+1-1)
               NY   = ZR(IN2+(I-1)*3+2-1)
               NZ   = ZR(IN2+(I-1)*3+3-1)
               WRITE(UNIT,500) NOEUD,RINF,RSUP,THET,NX,NY,NZ
 20         CONTINUE
            WRITE(UNIT,*)
C
            IF ( IMPR .GT. 1 ) THEN
C
               CHAMNO = ZK24(JRESU+J-1)
               CALL JEEXIN(CHAMNO(1:19)//'.DESC',IRET)
               IF ( IRET .NE. 0 ) THEN
                 CALL U2MESK('F','ELEMENTS_84',1,CHAMNO)
               ENDIF
C
               CHAMNO(20:24) = '.VALE'
               CALL JEIMPO(UNIT,CHAMNO,' ','OBJET .VALE')
C
               CHAMNO(20:24) = '.DESC'
               CALL JEIMPO(UNIT,CHAMNO,' ','OBJET .DESC')
C
               CHAMNO(20:24) = '.REFE'
               CALL JEIMPO(UNIT,CHAMNO,' ','OBJET .REFE')
C
            ENDIF
 10      CONTINUE
         CALL JEDEMA()
      ENDIF
C
 50   FORMAT('CHAMP THETA NUMERO ',I3)
 100  FORMAT('FOND DE FISSURE')
 200  FORMAT(15('*'))
 300  FORMAT(1X,'NOEUD',7X,'RINF',6X,'RSUP',6X,'THET',6X,'DIRX',
     &           6X,'DIRY',6X,'DIRZ')
 400  FORMAT(1X,5('+'),7X,5(4('+'),6X),4('+'))
 500  FORMAT(1X,A8,2X,6(D8.2,2X))
C
      END
