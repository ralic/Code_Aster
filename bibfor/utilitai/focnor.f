      SUBROUTINE FOCNOR ( NOMFO1, SORTIE, BASE )
      IMPLICIT   NONE
      CHARACTER*1         BASE
      CHARACTER*19        NOMFO1, SORTIE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 16/07/2002   AUTEUR VABHHTS J.PELLET 
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
C      
C     CALCUL LA NORME L2 D'UNE FONCTION
C
C IN  : NOMFO1 : NOM DE LA FONCTION A TRAITER
C IN  : BASE   : BASE OU EST STOCKEE LA FONCTION PRODUITE
C OUT : SORTIE : NOM DE LA FONCTION PRODUITE
C
C ----------------------------------------------------------------------
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
      CHARACTER*32 JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER      NBPARF,NBVECT,I,LVAL,LPRO,LONVAL,NBVAL,IADR,IBID
      REAL*8       NORME
      COMPLEX*16   CBID
      CHARACTER*8  TYPREF, K8B, TYPARA
      CHARACTER*16 PARAM
      CHARACTER*24 PARA, PROL, VALE
C DEB ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      PARA(20:24) = '.PARA'
      PROL(20:24) = '.PROL'
      VALE(20:24) = '.VALE'
C
      PROL( 1:19) = NOMFO1
      PARA( 1:19) = NOMFO1
      VALE( 1:19) = NOMFO1
C
      CALL JEVEUO ( PROL, 'L', LPRO )
      TYPREF = ZK8(LPRO)

C     --- ON TRAITE UNE NAPPE ----

      IF ( TYPREF .EQ. 'NAPPE' ) THEN
         CALL JELIRA ( PARA, 'LONMAX', NBPARF, K8B )
         PARAM  = 'NORME'
         TYPARA = 'R'
         CALL TBCRSD ( SORTIE, BASE )
         CALL TBAJPA ( SORTIE, 1, PARAM, TYPARA )
         NBVECT = 1

C ------ BOUCLE SUR CHAQUE SECOND PARAMETRE DE LA NAPPE

        DO 10 I = 1 , NBPARF
           CALL JELIRA ( JEXNUM(VALE,I), 'LONUTI', LONVAL, K8B )
           CALL JEVEUO ( JEXNUM(VALE,I), 'L', LVAL )             
           NBVAL = LONVAL / 2
C
           CALL WKVECT ( '&&FOCNOR.VECTTRA', 'V V R', NBVAL, IADR )
C
           CALL NOR091 ( ZR(LVAL), ZR(LVAL+NBVAL), NBVECT, NBVAL,
     +                   NORME, ZR(IADR) )
C
           CALL TBAJLI ( SORTIE, 1, PARAM, IBID, NORME, CBID, K8B, 0 )
C
           CALL JEDETR ( '&&FORNOR.VECTTRA' ) 
C    
 10      CONTINUE
      ELSE
 
C     --- ON TRAITE UNE FONCTION ---
C
         CALL UTMESS('F','FOCNOR','NORME L2 D''UNE FONCTION NON '//
     +                            'IMPLEMENTEE')
      ENDIF
C
      CALL JEDEMA()
C
      END
