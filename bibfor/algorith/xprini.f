      SUBROUTINE XPRINI(MODEL,NOMA,FISPRE,FISS,CNSLN,CNSLT,CNSGLS,
     &                  NOESOM,NORESI)
      IMPLICIT NONE
      CHARACTER*8    MODEL,NOMA,FISPRE,FISS
      CHARACTER*19   CNSLN,CNSLT,CNSGLS,NOESOM,NORESI
  
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/08/2006   AUTEUR MASSIN P.MASSIN 
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
C       XPRINI   : X-FEM PROPAGATION : INITIALISATION DES PARAMETRES DE
C       ------     -     --            ---         XPRREI ET XPRREO
C
C    ENTREE
C        MODEL   : NOM DU CONCEPT MODELE
C        NOMA    : NOM DU CONCEPT MAILLAGE
C        FISPRE  : NOM DE LA FISSURE PRECEDENTE
C        FISS    : NOM DE LA FISSURE CALCULEE
C        CNSLT   : CHAM_NO_S DES VALEURS DE LEVEL SET NORMALE
C        CNSLT   : CHAM_NO_S DES VALEURS DE LEVEL SET TANGENTE
C        CNSGLS  : CHAM_NO_S DES VALEURS DU GRADIENT DE LS
C    SORTIE
C        NOESOM  : VECTEUR LOGIQUE INDIQUANT SI LE NOEUD EST SOMMET
C        NORESI  : VECTEUR LOGIQUE INDIQUANT SI LE RESIDU DOIT ETRE
C                  ESTIME SUR LE NOEUD
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

      INTEGER        I,INO,IMA,IFM,NIV,JGLSNO,IRET,IRET2,NBNO,JNOSOM,
     &               JNRESI,ADDIM,NBMA,JCOOR,JCONX1,JCONX2,JLNNO,JLTNO,
     &               NDIM,NBMAFF,NNORES,
     &               JMAIFF,NBNOMA,INOA,INOB,NUNOA,NUNOB,IBID
      CHARACTER*8    LPAIN(4),LPAOUT(2),K8B,METHOD
      CHARACTER*19   CELMT,MAIFF
      CHARACTER*24   LIGREL,LCHIN(1),LCHOUT(2)
      REAL*8         NORMGR,R8PREM,BARY(3),P(3),FF(3),DIST,PADIST,
     &               LSNA,LSNB,LSTA,LSTB,RAYON
      LOGICAL        COUPLN,COUPLT
      
C-----------------------------------------------------------------------
C     DEBUT
C-----------------------------------------------------------------------
      CALL JEMARQ()
      CALL INFMAJ()
      CALL INFNIV(IFM,NIV)
      
C  RECUPERATION DU MODELE ET DU MAILLAGE
      LIGREL = MODEL//'.MODELE'
      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNO,K8B,IRET)
      CALL DISMOI('F','NB_MA_MAILLA',NOMA,'MAILLAGE',NBMA,K8B,IRET)
      CALL JEVEUO(NOMA//'.COORDO    .VALE','L',JCOOR)
      CALL JEVEUO(NOMA//'.CONNEX','L',JCONX1)
      CALL JEVEUO(JEXATR(NOMA//'.CONNEX','LONCUM'),'L',JCONX2)
      CALL JEVEUO(NOMA//'.DIME','L',ADDIM)
      NDIM=ZI(ADDIM-1+6)
      
C   RECUPERATION DE LA METHODE DE REINITIALISATION A EMPLOYER
      CALL GETVTX(' ','METHODE',1,1,1,METHOD,IBID)
      
      IF (METHOD.EQ.'UPWIND')  CALL UTMESS('F','XPRREI','LA METHODE '
     &      //'"UPWIND" EST EN COURS D''IMPLEMENTATION.')
      
C  RECUPERATION DU RAYON DU TORE OU L'ON ESTIME LE RESIDU
      CALL GETVR8(' ','RAYON',1,1,1,RAYON,IRET)
      
C   RECUPERATION DES VALEURS DES LS ET DU GRADIENT DE LS
      CALL JEVEUO(CNSLN//'.CNSV','E',JLNNO)
      CALL JEVEUO(CNSLT//'.CNSV','E',JLTNO)
      CALL JEVEUO(CNSGLS//'.CNSV','E',JGLSNO)
      
C-----------------------------------------------------------------------
      IF (METHOD.EQ.'SIMPLEXE') THEN

         CALL JEEXIN(FISPRE//'.PRO.MES_EL',IRET)
         CALL JEEXIN(FISPRE//'.PRO.NORMAL',IRET2)
         IF (IRET.EQ.0.OR.IRET2.EQ.0) THEN
C------------------------------------------------------
C   CALCUL DE |T| ET DES DIRECTIONS NI SUR LES ELEMENTS
C------------------------------------------------------
            CELMT =  '&&XPRINI.CELMT'
            LPAIN(1)='PGEOMER'
            LCHIN(1)=NOMA//'.COORDO'
            LPAOUT(1)='PMEAST'
            LCHOUT(1)=CELMT
            LPAOUT(2)='PNIELNO'
            LCHOUT(2)=FISS//'.PRO.NORMAL'

            CALL CALCUL('S','XFEM_SMPLX_INIT',LIGREL,1,LCHIN,LPAIN,2,
     &                   LCHOUT,LPAOUT,'V')

            CALL CELCES (CELMT,'V',FISS//'.PRO.MES_EL')
            CALL JEDETR (CELMT)

         ELSE
            CALL JEDUPO(FISPRE//'.PRO.MES_EL','G',
     &                    FISS//'.PRO.MES_EL',.FALSE.)
            CALL JEDUPO(FISPRE//'.PRO.NORMAL','G',
     &                    FISS//'.PRO.NORMAL',.FALSE.)
         ENDIF

      ENDIF
C-----------------------------------------------------------------------

C------------------------------------------------------------------
C     ON REPERE LES NOEUDS SOMMETS (DONT LE GRADIENT DE LS EST NUL)
C------------------------------------------------------------------
      CALL WKVECT(NOESOM,'V V L',NBNO,JNOSOM)
      DO 100 INO=1,NBNO
         ZL(JNOSOM-1+INO) = .TRUE.
         NORMGR = SQRT( ZR(JGLSNO-1+NDIM*(INO-1)+1)**2 +
     &                  ZR(JGLSNO-1+NDIM*(INO-1)+2)**2 +
     &                  ZR(JGLSNO-1+NDIM*(INO-1)+3)**2 )

C  LES NOEUDS DONT LE GRADIENT DE LS EST NUL SONT DES NOEUDS MILIEUX
         IF (NORMGR.LT.R8PREM()) ZL(JNOSOM-1+INO) = .FALSE.
         
 100  CONTINUE
 
C-----------------------------------------------------------------
C     ON REPERE LES NOEUDS SUR LESQUELS LE RESIDU DOIT ETRE ESTIME
C-----------------------------------------------------------------
C  VECTEUR CONTENANT LES COORDONNEES DES MAILLES COUPEES (BARYCENTRES)
      MAIFF = '&&XPRINI.MAIFF'
      CALL WKVECT(MAIFF,'V V R',NBMA*3,JMAIFF)

      NBMAFF = 0
      DO 200 IMA=1,NBMA
         COUPLN = .FALSE.
         COUPLT = .FALSE.
         NBNOMA = ZI(JCONX2+IMA) - ZI(JCONX2+IMA-1)
C  ON PARCOURS LES ARETES DE L'ELEMENT
         DO 210 INOA=1,NBNOMA-1
            NUNOA = ZI(JCONX1-1+ZI(JCONX2+IMA-1)+INOA-1)
C  ON ECARTE LES NOEUDS MILIEUX
            IF (.NOT.ZL(JNOSOM-1+NUNOA)) GOTO 210
            LSNA = ZR(JLNNO-1+NUNOA)
            LSTA = ZR(JLTNO-1+NUNOA)

            DO 220 INOB = INOA+1,NBNOMA
               NUNOB = ZI(JCONX1-1+ZI(JCONX2+IMA-1)+INOB-1)
C  ON ECARTE LES NOEUDS MILIEUX
               IF (.NOT.ZL(JNOSOM-1+NUNOB)) GOTO 220
               LSNB = ZR(JLNNO-1+NUNOB)
               LSTB = ZR(JLTNO-1+NUNOB)

               IF ((LSNA*LSNB).LE.0.D0 )  COUPLN=.TRUE.
               IF ((LSTA*LSTB).LE.0.D0 )  COUPLT=.TRUE.

 220        CONTINUE
 210     CONTINUE

C  SI LA MAILLE EST COUPEE PAR LES ISOZEROS DE LS ET LT
         IF (COUPLN.AND.COUPLT) THEN
            NBMAFF = NBMAFF + 1

C  ON REPERE LE BARYCENTRE DES NOEUDS DE LA MAILLE
            DO 230 I=1,3
               ZR(JMAIFF-1+3*(NBMAFF-1)+I) = 0.D0
 230        CONTINUE
            DO 240 INOA=1,NBNOMA
               NUNOA = ZI(JCONX1-1+ZI(JCONX2+IMA-1)+INOA-1)
               DO 245 I=1,3
                  ZR(JMAIFF-1+3*(NBMAFF-1)+I) =
     &             ZR(JMAIFF-1+3*(NBMAFF-1)+I) +
     &             ZR(JCOOR-1+3*(NUNOA-1)+I) / NBNOMA
 245          CONTINUE
 240        CONTINUE
         ENDIF

 200  CONTINUE
 
      NNORES=0
      CALL WKVECT(NORESI,'V V L',NBNO,JNRESI)
      DO 250 INO=1,NBNO
         ZL(JNRESI-1+INO) = .FALSE.
C  ON ECARTE LES NOEUDS MILIEUX
         IF (.NOT.ZL(JNOSOM-1+INO)) GOTO 250
         DO 260 I=1,3
            P(I) = ZR(JCOOR-1+3*(INO-1)+I)
 260     CONTINUE
         DO 270 IMA=1,NBMAFF
            DO 275 I=1,3
               FF(I) = ZR(JMAIFF-1+3*(IMA-1)+I)
 275        CONTINUE
            DIST = PADIST(3,P,FF)
            IF (DIST.LE.RAYON) THEN
C  LE NOEUD EST PROCHE D'UNE MAILLE DU FOND DE FISSURE
               ZL(JNRESI-1+INO) = .TRUE.
               NNORES = NNORES+1
               GOTO 250
            ENDIF
 270     CONTINUE
 250  CONTINUE
C      IF (NIV.GT.1)  
      WRITE(IFM,*)'   NOMBRE DE NOEUDS POUR L'''
     &                         //'ESTIMATION DES RESIDUS :',NNORES
      
      CALL JEDETR(MAIFF)

C-----------------------------------------------------------------------
C     FIN
C-----------------------------------------------------------------------
      CALL JEDEMA()
      END   
