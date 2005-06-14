      SUBROUTINE ASCELI ( MAILLA )
      IMPLICIT   NONE 
      CHARACTER*8         MAILLA
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 08/03/99   AUTEUR AUBHHMB M.BONNAMY 
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
C     OPERATEUR: "MODI_MAILLAGE" , MOTCLE FACTEUR "PLAQ_TUBE"
C     ELIMINE LES NOEUDS EN DOUBLE:
C             SURFACE BORD1 AVEC BORD2
C             
C        
C-----------------------------------------------------------------------
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
      CHARACTER*32     JEXNUM, JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C-----------------------------------------------------------------------
C
      INTEGER       INUMA, NBPT, JPOIN, INO, INOV, NBMAT, IFM, IUNIFI,
     +              NBNO, JGRN1, JGRN2, JVI1, JVI2, IRET, IGR,
     +              IDIME, NBNOT, JNEW, INOB, NBGRNO, ICOOR, NDIM, J
      CHARACTER*8   K8B,  NOGRN1, NOGRN2, NOMGRN
      CHARACTER*24  GRPNOE, CONNEX, LISO1, LISO2, DIME, COORD 
C     ------------------------------------------------------------------
C
      CALL JEMARQ ( )
C
      IFM   = IUNIFI('MESSAGE') 
C      
      GRPNOE = MAILLA//'.GROUPENO       '
      CONNEX = MAILLA//'.CONNEX         '
      DIME   = MAILLA//'.DIME           '
      COORD  = MAILLA//'.COORDO    .VALE'
C
      NOGRN1 = 'BORD1   '
      NOGRN2 = 'BORD2   '
      CALL JEEXIN ( JEXNOM(GRPNOE,NOGRN1), IRET )
      IF ( IRET .EQ. 0 ) THEN
         CALL UTMESS('F','ASCELI','GROUP_NO : '//NOGRN1//
     +                                      ' INCONNU DANS LE MAILLAGE')
      ENDIF
      CALL JEEXIN ( JEXNOM(GRPNOE,NOGRN2), IRET )
      IF ( IRET .EQ. 0 ) THEN
         CALL UTMESS('F','ASCELI','GROUP_NO : '//NOGRN2//
     +                                      ' INCONNU DANS LE MAILLAGE')
      ENDIF
      CALL JELIRA ( JEXNOM(GRPNOE,NOGRN1), 'LONMAX', NBNO, K8B )
      CALL JEVEUO ( JEXNOM(GRPNOE,NOGRN1), 'L', JGRN1 )
      CALL JEVEUO ( JEXNOM(GRPNOE,NOGRN2), 'L', JGRN2 )
      LISO1 = '&&ASCELI.NOEUD_1'
      LISO2 = '&&ASCELI.NOEUD_2'
      CALL PACOA1 ( ZI(JGRN1), ZI(JGRN2), NBNO, MAILLA, LISO1, LISO2 )
      CALL JEVEUO ( LISO1, 'L', JVI1 )
      CALL JEVEUO ( LISO2, 'L', JVI2 )
C
C     RENUMEROTATION DU MAILLAGE EN VUE DE L'ELIMINATION DES 
C     NOEUDS DU GROUPE "BORD2" : TABLEAU DE CORRESPONDANCE 
C     INO (ANCIENNE) --> INOV (NOUVELLE NUMEROTATION)
C
C     OBJET .VALE DE L'OBJET .COORDO : ON SAUTE LES NOEUDS ELIMINES
C
      CALL JEVEUO (COORD,'E',ICOOR)      
      CALL JEVEUO (DIME ,'E',IDIME)
      NBNOT = ZI(IDIME)
      NBMAT = ZI(IDIME+2)      
      NDIM  = ZI(IDIME+5)
      
      CALL WKVECT ( '&&ASCELI.RENUM  ', 'V V I', NBNOT, JNEW )
      INOV = 0
C      
      DO 50 INO = 1, NBNOT
        INOV = INOV+1
        DO 60 INOB = 1 , NBNO
            IF ( ZI(JVI2+INOB-1) .EQ. INO ) THEN
C               WRITE(IFM,*) 'NOEUD N',INO,' ELIMINE'
               INOV = INOV - 1
               GOTO 50
            ENDIF  
 60     CONTINUE   
        ZI(JNEW+INO-1) = INOV
        DO 55 J=1, NDIM
         ZR(ICOOR+NDIM*(INOV-1)+J-1) = ZR(ICOOR+NDIM*(INO-1)+J-1)
 55     CONTINUE
 50   CONTINUE   
C
C     OBJET .DIME : NBE DE NOEUDS PHYSIQUE CORRIGE
C
      WRITE(IFM,*) 'COUTURE - NOMBRE DE NOEUDS ELIMINES : ',NBNOT-INOV
      WRITE(IFM,*) 'NOMBRE DE NOEUDS DU MAILLAGE APRES COUTURE : ',INOV
C            
      ZI(IDIME) = INOV
C
C     OBJET .GROUPENO : ON CHANGE LES NUMEROS DE NOEUDS DES GROUPENO
C
      CALL JELIRA ( GRPNOE, 'NUTIOC', NBGRNO, K8B )
C      
      DO 70 IGR = 1,NBGRNO
         CALL JENUNO ( JEXNUM(GRPNOE,IGR),NOMGRN )
         CALL JELIRA ( JEXNOM(GRPNOE,NOMGRN), 'LONMAX', NBPT, K8B )
         CALL JEVEUO ( JEXNOM(GRPNOE,NOMGRN), 'E', JPOIN )
         DO 80 INO = 1 , NBPT
           DO 90 INOV = 1 , NBNO
              IF ( ZI(JVI2+INOV-1) .EQ. ZI(JPOIN+INO-1) ) THEN
                   ZI(JPOIN+INO-1) = ZI(JNEW+ZI(JVI1+INOV-1)-1)
                   GOTO 80
              END IF
 90        CONTINUE
           ZI(JPOIN+INO-1) = ZI(JNEW+ZI(JPOIN+INO-1)-1)
 80      CONTINUE
 70   CONTINUE        
C
C     OBJET .GROUPEMA : ON CHANGE LES CONNECTIVITES (.CONNEX)
C
         DO 200 INUMA = 1, NBMAT
            CALL JELIRA ( JEXNUM(CONNEX,INUMA), 'LONMAX', NBPT, K8B )
            CALL JEVEUO ( JEXNUM(CONNEX,INUMA), 'E', JPOIN )
            DO 202 INO = 1 , NBPT
               DO 204 INOV = 1 , NBNO
                  IF ( ZI(JVI2+INOV-1) .EQ. ZI(JPOIN+INO-1) ) THEN
                     ZI(JPOIN+INO-1) = ZI(JNEW+ZI(JVI1+INOV-1)-1)
                   GOTO 202
                  ENDIF
 204           CONTINUE
               ZI(JPOIN+INO-1) = ZI(JNEW+ZI(JPOIN+INO-1)-1) 
 202        CONTINUE
 200     CONTINUE
C 
      CALL JEDETC('V','&&ASCELI',1)  
C
      CALL JEDEMA ( )
C
      END
