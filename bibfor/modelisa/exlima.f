      SUBROUTINE EXLIMA ( MOTFAZ, BASE, MODELZ, RESUZ, LIGRE )
      IMPLICIT   NONE
      CHARACTER*(*)       MOTFAZ, BASE, MODELZ, RESUZ, LIGRE
C     -----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 30/09/2003   AUTEUR VABHHTS J.PELLET 
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
C IN  : MODELE : NOM DU MODELE
C IN  : RESU   : NOM D'UN RESULTAT
C OUT : LIGRE  : LIGREL A CREER
C     -----------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  -------------------------
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  -------------------------
C
      INTEGER         I, IB, NMOFAC, N1, N2, INDMOT, LONLIS, INUM, IRET,
     +                LG, LXLGUT
      CHARACTER*6     KNUM
      CHARACTER*8     MODELE, NOMA, K8BID
      CHARACTER*16    MOTFAC, MO16BL
      CHARACTER*19    LIGREL, LIGRMO
      CHARACTER*24    LISMAI,NOOJB
C     -----------------------------------------------------------------
C
      MOTFAC = MOTFAZ
      MODELE = MODELZ
C
C --- LIGREL DU MODELE
C     ----------------
      CALL DISMOI('F','NOM_LIGREL',MODELE,'MODELE',IB,LIGRMO,IB)
C
C --- MAILLAGE ASSOCIE AU MODELE
C     --------------------------
      CALL DISMOI('F','NOM_MAILLA',MODELE,'MODELE',IB,NOMA  ,IB)

      LISMAI = '&&EXLIMA.LISTE_MAILLES'
      MO16BL = '                '
C
      INDMOT = 0
C
C --- SI ON N'UTILISE PAS GROUP_MA OU MAILLE, LIGREL=LIGRMO :
C     ------------------------------------------------------
      IF ( MOTFAC .NE. MO16BL ) THEN
         LG = LXLGUT( MOTFAC )
         CALL GETFAC( MOTFAC(1:LG) , NMOFAC )
         IF ( NMOFAC .EQ. 0 ) THEN
            LIGREL = LIGRMO
            GOTO 9999
         ELSE
            DO 10 I = 1 , NMOFAC
               CALL GETVTX ( MOTFAC(1:LG), 'TOUT' , I,1,0, K8BID, N1 )
               IF ( N1 .NE. 0 ) THEN
                  LIGREL = LIGRMO
                  GOTO 9999
               ENDIF
               CALL GETVEM(NOMA,'GROUP_MA',MOTFAC(1:LG),'GROUP_MA',
     +                      I,1,0,K8BID,N1)
               CALL GETVEM(NOMA,'MAILLE',MOTFAC(1:LG),'MAILLE',
     +                    I,1,0,K8BID,N2)
               IF ( N1+N2 .EQ. 0 ) THEN
                  LIGREL = LIGRMO
                  GOTO 9999
               ENDIF
 10         CONTINUE
         ENDIF
      ELSE
         CALL GETVTX ( ' ', 'TOUT' , I,1,0, K8BID, N1 )
         IF (N1.NE.0)  THEN
            LIGREL = LIGRMO
            GOTO 9999
         ENDIF
         CALL GETVEM(NOMA,'GROUP_MA',' ','GROUP_MA',
     +       1,1,0,K8BID,N1)
         CALL GETVEM(NOMA,'MAILLE',' ','MAILLE',
     +     1,1,0,K8BID,N2)
         IF ( N1+N2 .EQ. 0 ) THEN
            LIGREL = LIGRMO
            GOTO 9999
         ENDIF
      ENDIF
C
C --- CREATION ET AFFECTATION DU VECTEUR DE K8 DE NOM LISMAI
C     CONTENANT LES NOMS DES MAILLES FORMANT LE LIGREL A CREER
C     --------------------------------------------------------
      CALL RECMAI ( MOTFAC, 1, INDMOT, NOMA, LISMAI, LONLIS)
C
C --- TRAITEMENT DU CAS OU L'ON N'A AUCUN DES MOTS-CLES :
C     MAILLE OU GROUP_MA,  ON SORT AVEC UNE LONGUEUR DE LISTE
C     LONLIS = 0
C     -------------------------------------------------------
      IF ( LONLIS .EQ. 0 ) THEN
         LIGREL = LIGRMO
      ELSE
         NOOJB='12345678.LIGR000000.LIEL'
         CALL GNOMSD ( NOOJB,14,19 )
         LIGREL=NOOJB(1:19)
C
C  ---   RECUPERATION DE LA TAILLE DES GRELS
C        -----------------------------------
C
C ---    CREATION ET AFFECTATION DU LIGREL
C        ---------------------------------
         CALL EXLIM1 ( LISMAI, LONLIS, MODELE, BASE, LIGREL)
      ENDIF
      CALL JEDETR ( LISMAI )
C
 9999 CONTINUE
C
      LIGRE = LIGREL
C
      END
