      SUBROUTINE XPRCFL(MODEL,CNSVT,CFLPRO,LCMIN,CNSLC)
      IMPLICIT NONE
      REAL*8         CFLPRO,LCMIN
      CHARACTER*19   CNSVT,CNSLC
      CHARACTER*8    MODEL
  
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/05/2006   AUTEUR MASSIN P.MASSIN 
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
C RESPONSABLE MASSIN P.MASSIN
C     ------------------------------------------------------------------
C
C       XPRCFL   : X-FEM PROPAGATION ; CALCUL DES CONDITIONS CFL
C       ------     -     --                                  ---       
C    CALCUL DES CONDITIONS CFL DE PROPAGATION, ET DES LONGUEUR
C     CARACTERISTIQUES MINIMALE D'ELEMNTS
C
C    ENTREE
C        MODEL   : NOM DU CONCEPT MODELE
C        CNSVT   : CHAM_NO_S DE VITESSE DE PROPAGATION TANGENTIELLE
C
C    SORTIE
C        CFLPRO  : LIMITE MAXI CFL POUR LE PAS DE TEMPS DE PROPAGATION
C        LCMIN   : LONGUEUR CARACTERISTIQUE MINIMALE DU MAILLAGE
C        CNSLC   : CHAM_NO_S DE LONGUEUR CARACTERISTIQUE
C                  (ISSU DE CESCNS, C'EST LA MOYENNE DES PLUS PETITES
C                   ARETES DES ELEMENTS CONTENANT LE NOEUD)
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
      CHARACTER*32    JEXNUM,JEXATR
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------

      INTEGER        IBID,IFM,NIV
      CHARACTER*8    LPAIN(2),LPAOUT(2)
      CHARACTER*19   CNOVT,CELCFL,CELLC,CESLC
      CHARACTER*24   LIGREL,CHGEOM,LCHIN(2),LCHOUT(2)
      LOGICAL        EXIGEO
      COMPLEX*16     CBID
      
C-----------------------------------------------------------------------
C     DEBUT
C-----------------------------------------------------------------------

      CALL JEMARQ()
      CALL INFMAJ()
      CALL INFNIV(IFM,NIV)

      CNOVT='&&XPRCFL.CNOVT'
      CALL CNSCNO(CNSVT,' ','NON','V',CNOVT)
      
      CELCFL='&&XPRCFL.CELCFL'
      CELLC='&&XPRCFL.CELLC'
      CESLC='&&XPRCFL.CESLC'

      CALL MEGEOM(MODEL,' ',EXIGEO,CHGEOM)
      LIGREL=MODEL//'.MODELE'
      LPAIN(1)='PGEOMER'
      LCHIN(1)=CHGEOM
      LPAIN(2)='PVTNO'
      LCHIN(2)=CNOVT
      LPAOUT(1)='PLONCAR'
      LCHOUT(1)=CELLC
      LPAOUT(2)='PCFLVT'
      LCHOUT(2)=CELCFL
            
      CALL CALCUL('S','CFL_XFEM',LIGREL,2,LCHIN,LPAIN,2,LCHOUT,LPAOUT,
     &            'V')

      CALL JEDETR(CNOVT)

C   ON VA CHERCHER LE MINIMUM DE CELCFL SUR LES ELEMENTS -->  CFLPRO
      CALL MEMAX('MIN',CELCFL,1,1,CFLPRO,0,IBID)
      CALL JEDETR(CELCFL)
      
C   ON VA CHERCHER LE MINIMUM DE CELLC SUR LES ELEMENTS -->  LCMIN
      CALL MEMAX('MIN',CELLC,1,1,LCMIN,0,IBID)
      
      CALL CELCES(CELLC,'V',CESLC)
      CALL CESCNS(CESLC,' ','V',CNSLC)
      CALL JEDETR(CELLC)
      CALL JEDETR(CESLC)
      
      IF (NIV.GT.1) THEN
        WRITE(IFM,*)'CONDITION CFL POUR LA PROPAGATION DES LEVEL SETS :'
        WRITE(IFM,*)'    DELTA_T_CFL = ',CFLPRO
        WRITE(IFM,*)'CONDITION CFL POUR LA REINITIALISATION DES LEVEL '
     &             //'SETS :'
        WRITE(IFM,*)'    DELTA_T_CFL = ',LCMIN
        WRITE(IFM,*)' '
      ENDIF
      
C-----------------------------------------------------------------------
C     FIN
C-----------------------------------------------------------------------
      CALL JEDEMA()
      END   
