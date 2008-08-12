      SUBROUTINE QUADIM(UNIT,ISMED,QUADRA,NGRMA1,NGRMA2)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 11/08/2008   AUTEUR MEUNIER S.MEUNIER 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE MEUNIER S.MEUNIER
C
      IMPLICIT NONE
      CHARACTER*10  QUADRA
      CHARACTER*10  NGRMA1,NGRMA2
      LOGICAL       ISMED
      INTEGER       UNIT
C
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C
C IMPRESSION DE LA SD POUR LES INTEGRALES (QUADRATURES)
C
C ----------------------------------------------------------------------
C
C
C IN  UNIT   : UNITE D'IMPRESSION
C IN  ISMED  : VAUT .TRUE. SI ZONE DE COLLAGE EST LA ZONE MEDIATRICE
C IN  QUADRA : NOM DE LA STRUCTURE DE DONNEES QUADRATURES
C IN  NGRMA1 : NOM DE LA LISTE DES MAILLES DU PREMIER GROUPE
C IN  NGRMA2 : NOM DE LA LISTE DES MAILLES DU SECOND GROUPE
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
      CHARACTER*32       JEXNUM
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
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER      IRET
      INTEGER      IM1,IM2
      INTEGER      NFAM,IFAM,NCPL,ICPL,ICPLF
      INTEGER      JQDIME,JQNUME,JQTYPM,JQLIMA,JQMAMA,JQMAFA
      INTEGER      JGRMA1,JGRMA2,JLGRF
      CHARACTER*8  K8BID,NOMO,NOMA,NOMMA1,NOMMA2
      INTEGER      NUMMA1,NUMMA2,IOCC
      LOGICAL      LINCL1,LINCL2,LINCLU,LSSMAI
      INTEGER      NMAIN1,NMAIN2,NMASS
      INTEGER      NNOIN1,NNOIN2
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- RECUPERATION MAILLAGE
C
      CALL GETVID(' ','MODELE',0,1,1,NOMO,IOCC)
      IF (IOCC.EQ.0) THEN
        WRITE(UNIT,*) '<QUADRA  > AFFICHAGE IMPOSSIBLE (VOIR QUADIM) '
        GOTO 999
      ELSE
        WRITE(UNIT,*) '<QUADRA  > SD DE QUADRATURE (INTEGRATION '//
     &                'NUMERIQUE)...'
      ENDIF
      CALL JEVEUO(NOMO(1:8)//'.MODELE    .LGRF','L',JLGRF)
      NOMA = ZK8(JLGRF)
C
      CALL JEVEUO(NGRMA1(1:10)//'.GROUPEMA','L',JGRMA1)
      CALL JEVEUO(NGRMA2(1:10)//'.GROUPEMA','L',JGRMA2)
C
      CALL JEEXIN(QUADRA(1:10)//'.DIME',IRET)
      IF (IRET.EQ.0) THEN
        WRITE(UNIT,*) '<QUADRA  > SD QUADRATURES: <',
     &          QUADRA(1:10)//'.DIME','> N''EXISTE PAS'
      ELSE
        CALL JEVEUO(QUADRA(1:10)//'.DIME' ,'L',JQDIME)
        NFAM   = ZI(JQDIME + 1 - 1 )
        NCPL   = ZI(JQDIME + 2 - 1 )
        NMAIN1 = ZI(JQDIME + 3 - 1 )
        NMAIN2 = ZI(JQDIME + 4 - 1 )
        NMASS  = ZI(JQDIME + 5 - 1 )
        NNOIN1 = ZI(JQDIME + 6 - 1 )
        NNOIN2 = ZI(JQDIME + 7 - 1 )
        WRITE(UNIT,*) '<QUADRA  > ... NOMBRE DE COUPLES DE MAILLES '//
     &                'A INTEGRER :',NCPL
        WRITE(UNIT,*) '<QUADRA  > ...  DONT ',NMAIN1,
     &                ' COUPLES INTEGRES SUR MAILLE 1 (SOIT ',
     &                NNOIN1,' NOEUDS)'
        WRITE(UNIT,*) '<QUADRA  > ...  DONT ',NMAIN2,
     &                ' COUPLES INTEGRES SUR MAILLE 2 (SOIT ',
     &                NNOIN2,' NOEUDS)'
        WRITE(UNIT,*) '<QUADRA  > ...  DONT ',NMASS ,
     &                ' COUPLES INTEGRES PAR SOUS-MAILLES '

C
        CALL JEVEUO(QUADRA(1:10)//'.MAMA','L',JQMAMA)
        CALL JEVEUO(QUADRA(1:10)//'.MAFA','L',JQMAFA)
        WRITE(UNIT,*) '<QUADRA  > ... LISTE DES COUPLES DE MAILLES '//
     &                'A INTEGRER...'
C
        DO 70 ICPL = 1 , NCPL
C
          IF (ISMED) THEN
            IM1  = ZI(JQMAMA+2*(ICPL-1))
            IM2  = ZI(JQMAMA+2*(ICPL-1)+1)
          ELSE
            IM1  = ZI(JQMAMA+2*(ICPL-1)+1)
            IM2  = ZI(JQMAMA+2*(ICPL-1))
          ENDIF
C
          NUMMA1 = ZI(JGRMA1+ABS(IM1)-1)
          CALL JENUNO(JEXNUM(NOMA(1:8)//'.NOMMAI',NUMMA1),NOMMA1)
          NUMMA2 = ZI(JGRMA2+ABS(IM2)-1)
          CALL JENUNO(JEXNUM(NOMA(1:8)//'.NOMMAI',NUMMA2),NOMMA2)
C
C --- TYPE D'INTEGRATION
C
          CALL ARLTII(IM1   ,IM2   ,
     &                LINCL1,LINCL2,LINCLU,LSSMAI)
C
          IFAM = ZI(JQMAFA+ICPL-1)
C
          IF (LSSMAI) THEN
            WRITE(UNIT,*) '<QUADRA  > ...... COUPLE (',ICPL,' ): ',
     &                     NOMMA1,'/',NOMMA2

            IF (IM1.GT.0) THEN
              WRITE(UNIT,*) '<QUADRA  > ......... INTEGRE '//
     &                      'PAR SOUS-MAILLES SUR MAILLE 1 '//
     &                      'AVEC FAMILLE :',IFAM
            ELSE
              WRITE(UNIT,*) '<QUADRA  > ......... INTEGRE '//
     &                      'PAR SOUS-MAILLES SUR MAILLE 2 '//
     &                      'AVEC FAMILLE :',IFAM
            ENDIF
          ELSEIF (LINCL1) THEN
            WRITE(UNIT,*) '<QUADRA  > ...... COUPLE (',ICPL,' ): ',
     &                     NOMMA1,'/',NOMMA2
            WRITE(UNIT,*) '<QUADRA  > ......... INTEGRE '//
     &                    'SUR MAILLE 1 AVEC FAMILLE     :',IFAM
          ELSEIF (LINCL2) THEN
            WRITE(UNIT,*) '<QUADRA  > ...... COUPLE (',ICPL,' ): ',
     &                     NOMMA1,'/',NOMMA2
            WRITE(UNIT,*) '<QUADRA  > ......... INTEGRE '//
     &                    'SUR MAILLE 2 AVEC FAMILLE     :',IFAM
          ELSE
            WRITE(UNIT,*) '<QUADRA  > ...... COUPLE (',ICPL,' ): ',
     &                     NOMMA1,'/',NOMMA2,
     &                    ' - INTEGRATION IMPOSSIBLE !'
            GOTO 999
          ENDIF
C
 70     CONTINUE
      ENDIF
C
      CALL JEEXIN(QUADRA(1:10)//'.TYPEMA',IRET)
      IF (IRET.EQ.0) THEN
        WRITE(UNIT,*) '<QUADRA  > SD QUADRATURES: <',
     &          QUADRA(1:10)//'.TYPEMA','> N''EXISTE PAS'
      ELSE
        CALL JEVEUO(QUADRA(1:10)//'.TYPEMA','L',JQTYPM)
        CALL JEVEUO(QUADRA(1:10)//'.NUMERO','L',JQNUME)
        CALL JEVEUO(QUADRA(1:10)//'.LIMAMA','L',JQLIMA)
C
        WRITE(UNIT,*) '<QUADRA  > ... NOMBRE DE FAMILLES DE '//
     &                'QUADRATURES: ',NFAM
C
        WRITE(UNIT,*) '<QUADRA  > ... FAMILLES DE QUADRATURE...'
C
        DO 80 IFAM = 1, NFAM
          WRITE(UNIT,*) '<QUADRA  > ... FAMILLE (',IFAM,' ) '
          WRITE(UNIT,*) '<QUADRA  > ...... NUMERO FORMULE: ',
     &                      ZI(JQNUME+IFAM-1)
          WRITE(UNIT,*) '<QUADRA  > ...... TYPE DE MAILLE: ',
     &                      ZK8(JQTYPM+IFAM-1)
          CALL JELIRA(JEXNUM(QUADRA(1:10)//'.LIMAMA',IFAM),
     &                  'LONMAX',NCPL,K8BID)
          WRITE(UNIT,*) '<QUADRA  > ...... NOMBRE DE COUPLES : ',
     &                      NCPL
          CALL JEVEUO(JEXNUM(QUADRA(1:10)//'.LIMAMA',IFAM),'L',JQLIMA)
          DO 81 ICPLF = 1, NCPL
            ICPL = ZI(JQLIMA+ICPLF-1)
C
            IF (ISMED) THEN
              IM1  = ZI(JQMAMA+2*(ICPL-1))
              IM2  = ZI(JQMAMA+2*(ICPL-1)+1)
            ELSE
              IM1  = ZI(JQMAMA+2*(ICPL-1)+1)
              IM2  = ZI(JQMAMA+2*(ICPL-1))
            ENDIF
C
            NUMMA1 = ZI(JGRMA1+ABS(IM1)-1)
            CALL JENUNO(JEXNUM(NOMA(1:8)//'.NOMMAI',NUMMA1),NOMMA1)
            NUMMA2 = ZI(JGRMA2+ABS(IM2)-1)
            CALL JENUNO(JEXNUM(NOMA(1:8)//'.NOMMAI',NUMMA2),NOMMA2)
C
            WRITE(UNIT,*) '<QUADRA  > ...... FAMILLE ',IFAM,
     &                    ' SUR COUPLE ',NOMMA1,'/',NOMMA2
  81      CONTINUE
  80    CONTINUE
      ENDIF
C
  999 CONTINUE
C
      CALL JEDEMA()
      END
