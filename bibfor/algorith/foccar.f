      SUBROUTINE FOCCAR ( NOMFON, EXPO1, SORTIE, BASE )
      IMPLICIT   NONE
      INTEGER             EXPO1
      CHARACTER*1         BASE
      CHARACTER*19        NOMFON, SORTIE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/07/2002   AUTEUR VABHHTS J.PELLET 
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
C      CALCUL F(X)=G(X)**N
C
C IN  : NOMFON : NOM DE LA FONCTION A TRAITER
C IN  : EXPO1  : EXPOSANT
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
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32     JEXNUM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER       INI,I,K,LPRO,NBPARF,LPAREF, NBVAL, LONVAL
      INTEGER       LVAL,LVAR,LPARA,LPROL,LONPAR,LONT
      REAL*8        SUM, INIT
      CHARACTER*8   CBID, TYPREF
      CHARACTER*24  PROL, PARA, VALE
C DEB ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      INI=1
      PARA(20:24) = '.PARA'
      PROL(20:24) = '.PROL'
      VALE(20:24) = '.VALE'
C
      PROL( 1:19) = NOMFON
      PARA( 1:19) = NOMFON
      VALE( 1:19) = NOMFON
      CALL JEVEUO(PROL,'L',LPRO)
      TYPREF =  ZK8(LPRO)
 
C     -- ON TRAITE UNE NAPPE ----
 
      IF ( TYPREF .EQ. 'NAPPE' ) THEN
         CALL JELIRA(VALE,'LONT',LONT,CBID)
         CALL JELIRA(PARA,'LONMAX',NBPARF,CBID)
         CALL JEVEUO(PARA,'L',LPAREF)

C        -- INITIALISATION DE LA NAPPE RESULTAT SI INI=1

         IF( INI.EQ.1) THEN
            PARA( 1:19) = SORTIE
            CALL WKVECT(PARA,BASE//' V R',NBPARF,LPARA)
            DO 10 I =1,NBPARF
               ZR(LPARA+I-1) = ZR(LPAREF+I-1)
10          CONTINUE
            PROL(1:19)=SORTIE
            LONPAR=6+2*NBPARF
            CALL WKVECT(PROL,BASE//' V K8',LONPAR,LPROL)
            DO 11 I=0,LONPAR-1
                ZK8(LPROL+I)   = ZK8(LPRO+I)
11          CONTINUE
            VALE( 1:19) = SORTIE
            CALL JECREC(VALE,BASE//' V R','NU','CONTIG',
     +                  'VARIABLE',NBPARF)
            CALL JEECRA(VALE,'LONT',LONT,' ')
         ENDIF

C        -- BOUCLE SUR CHAQUE SECOND PARAMETRE DE LA NAPPE

         DO 110 I=1,NBPARF
            VALE(1:19)= NOMFON
            CALL JELIRA(JEXNUM(VALE,I),'LONUTI',LONVAL,CBID)
            NBVAL=LONVAL/2
            CALL JEVEUO(JEXNUM(VALE,I),'L',LVAL)
            VALE( 1:19) = SORTIE
 
C           -- CREATION DES OBJ JEVEUX NECESSAIRES SI INI=1

            IF (INI.EQ.1) THEN
              CALL JECROC(JEXNUM(VALE,I))
              CALL JEECRA(JEXNUM(VALE,I) ,'LONMAX',LONVAL,' ')
              CALL JEECRA(JEXNUM(VALE,I) ,'LONUTI',LONVAL,' ')
              CALL JEVEUO(JEXNUM(VALE,I),'E',LVAR)
              DO 100 K=1,NBVAL
                 ZR(LVAR+K-1)=ZR(LVAL+K-1)
                 ZR(LVAR+NBVAL+K-1)= 0.D0
100           CONTINUE

C           -- SINON ON REUTILISE LES OBJ DEJA CREES
 
            ELSE
              CALL JEVEUO(JEXNUM(VALE,I),'E',LVAR)
            ENDIF 
C
            DO 102 K=1,NBVAL
                  INIT=ZR(LVAR+NBVAL+K-1)
                  SUM=ZR(LVAL+NBVAL+K-1)
                  ZR(LVAR+NBVAL+K-1)=INIT+SUM**EXPO1
102         CONTINUE
            
110      CONTINUE
      ELSE
 
C     -- ON TRAITE UNE FONCTION
C
         CALL UTMESS('F','FOCCAR','PUISSANCE D UNE FONCTION NON 
     +                             IMPLEMENTEE')
      ENDIF
C
      CALL JEDEMA()
C
      END
