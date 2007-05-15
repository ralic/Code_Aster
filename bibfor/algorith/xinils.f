      SUBROUTINE XINILS(IFM,NOMA,METH,NFONF,NFONG,GEOFIS,A,B,C,COTE,
     &                                         VECTX,VECTY,CNSLT,CNSLN)
      IMPLICIT NONE
      INTEGER       IFM
      CHARACTER*8   NOMA,METH,NFONF,NFONG,GEOFIS,COTE
      CHARACTER*19  CNSLT,CNSLN
      REAL*8        A,B,C(3),VECTX(3),VECTY(3)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 15/05/2007   AUTEUR GENIAUT S.GENIAUT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE GENIAUT S.GENIAUT
C                      CALCUL INITIAL DES LEVEL-SETS
C
C    ENTREE :
C              IFM    :   FICHIER D'IMPRESSION
C              NOMA   :   OBJET MAILLAGE
C              METH   :   MÉTHODE DE CALUL DES LEVEL-SETS
C              NFONF  :   NOM DE LA FONCTION LEVEL SET TANGENTE
C              NFONG  :   NOM DE LA FONCTION LEVEL SET NORMALE
C              GEOFIS :   GEOMETRIE DE LA FISSURE
C              A      :   DEMI-GRAND AXE       (POUR FISSURE ELLIPTIQUE)
C              B      :   DEMI-PETIT AXE       (POUR FISSURE ELLIPTIQUE)
C              C      :   CENTRE               (POUR FISSURE ELLIPTIQUE)
C              COTE   :   COTE DE LA FISSURE ('IN' OU 'OUT')
C              VECTX  :   VECTEUR DU GRAND AXE (POUR FISSURE ELLIPTIQUE)
C              VECTY  :   VECTEUR DU PETIT AXE (POUR FISSURE ELLIPTIQUE)
C
C    SORTIE : 
C              CNSLN  :   LEVEL-SET NORMALE  (PLAN DE LA FISSURE)
C              CNSLT  :   LEVEL-SET TANGENTE (TRACE DE LA FISSURE)
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
C     
      INTEGER       IA,IMA,ITYPMA,JCONX1,JCONX2,JDLIMA,JDLISE,JMA
      INTEGER       NA,NB,NBAR,NBMA,NBSEF,NMAABS,NUNOA,NUNOB
      REAL*8        XLN,XLT
      INTEGER       DIMENS, ADDIM, DIMNO
      INTEGER       IBID,IRET,ME1,ME2,CLSM
      INTEGER       NBNO,INO,JCOOR,NBMAF,I
      INTEGER       JLTSV,JLTSL,JLNSV,JLNSL,AR(12,2)
      REAL*8        VALPU(3),P3D(3),PLOC(3),VECTZ(3),H
      REAL*8        NORME,LSNA,LSNB,D,LSTA,LSTB
      CHARACTER*8   K8BID,NOMPU(3),TYPMA,NOMNO
      CHARACTER*16  K16BID
      CHARACTER*19  MAI
      CHARACTER*24  LISMA,LISNO,LISSE
      REAL*8        R8PREM
C
      CALL JEMARQ()
C
      NOMPU(1)='X'
      NOMPU(2)='Y'
      NOMPU(3)='Z'
C
      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNO,K8BID,IRET)
      CALL DISMOI('F','NB_MA_MAILLA',NOMA,'MAILLAGE',NBMA,K8BID,IRET)
C
      CALL JEVEUO(NOMA//'.COORDO    .VALE','L',JCOOR)
C
      CALL JEVEUO(CNSLT//'.CNSV','E',JLTSV)
      CALL JEVEUO(CNSLT//'.CNSL','E',JLTSL)
      CALL JEVEUO(CNSLN//'.CNSV','E',JLNSV)
      CALL JEVEUO(CNSLN//'.CNSL','E',JLNSL)
     
      CALL JEVEUO(NOMA//'.CONNEX','L',JCONX1)
      CALL JEVEUO(JEXATR(NOMA//'.CONNEX','LONCUM'),'L',JCONX2)
     
      CALL JEVEUO(NOMA//'.DIME','L',ADDIM)
      DIMENS=ZI(ADDIM-1+6)

      IF (METH.EQ.'FONCTION') THEN

C-----------------------------------------------------------------------
C       DANS LE CAS OU ON DONNE FONC_LT ET FONC_LN
C-----------------------------------------------------------------------

        WRITE(IFM,*)'CALCUL DES LEVEL-SETS AVEC LA METHODE 1'
        DO 1 INO=1,NBNO
           DO 12 DIMNO=1, DIMENS
             VALPU(DIMNO)=ZR(JCOOR-1+3*(INO-1)+DIMNO)
 12        CONTINUE
           CALL FOINTE('F ',NFONF, DIMENS, NOMPU, VALPU, XLT, IBID )
           CALL FOINTE('F ',NFONG, DIMENS, NOMPU, VALPU, XLN, IBID )
           ZR(JLTSV-1+(INO-1)+1)=XLT
           ZR(JLNSV-1+(INO-1)+1)=XLN
           ZL(JLTSL-1+(INO-1)+1)=.TRUE.
           ZL(JLNSL-1+(INO-1)+1)=.TRUE.             
 1      CONTINUE

      ELSEIF (METH.EQ.'GROUP_MA') THEN

C-----------------------------------------------------------------------
C       DANS LE CAS OU ON DONNE GROUP_MA_FISS ET GROUP_MA_FOND
C-----------------------------------------------------------------------

        WRITE(IFM,*)'CALCUL DES LEVEL-SETS AVEC LA METHODE 2'  
        LISMA = '&&XINILS.LISTE_MA_FISSUR'    
        CALL RELIEM(' ',NOMA,'NU_MAILLE','DEFI_FISS',1,1,
     &              'GROUP_MA_FISS','GROUP_MA',LISMA,NBMAF)       
        CALL JEVEUO(LISMA,'L',JDLIMA)

        LISSE = '&&XINILS.LISTE_MA_FONFIS' 
        CALL RELIEM(' ',NOMA,'NU_MAILLE','DEFI_FISS',1,1,
     &              'GROUP_MA_FOND','GROUP_MA',LISSE,NBSEF)  
        CALL JEVEUO(LISSE,'L',JDLISE)

        IF (DIMENS .EQ. 3) THEN
         CALL XLS3D(JLTSV,JLTSL,JLNSV,JLNSL,NBNO,JCOOR,
     &   NBMAF,JDLIMA,NBSEF,JDLISE,JCONX1,JCONX2,NOMA)
        ELSE     
         CALL XLS2D(JLTSV,JLTSL,JLNSV,JLNSL,NBNO,JCOOR,
     &   NBMAF,JDLIMA,NBSEF,JDLISE,JCONX1,JCONX2)
        ENDIF

      ELSEIF (METH.EQ.'GEOMETRI') THEN

C-----------------------------------------------------------------------
C       DANS LE CAS OU ON DONNE LA GEOMETRIE DE LA FISSURE
C-----------------------------------------------------------------------

        WRITE(IFM,*)'CALCUL DES LEVEL-SETS AVEC LA METHODE 3'  

        IF (GEOFIS.EQ.'ELLIPSE') THEN

          DO 20 INO=1,NBNO

C           COORDONNEES 3D DU POINT DANS LE REPERE GLOBAL
            DO 21 DIMNO=1, DIMENS
              P3D(DIMNO)=ZR(JCOOR-1+3*(INO-1)+DIMNO)
 21         CONTINUE

C           BASE LOCALE : (VECTX,VECTY,VECTZ)
            CALL NORMEV(VECTX,NORME)
            CALL NORMEV(VECTY,NORME)
            CALL PROVEC(VECTX,VECTY,VECTZ)

C           PROJECTION DU POINT 3D DANS LE REPERE LOCAL LIE A L'ELLIPSE
            DO 22 I=1,3
              PLOC(I)=(P3D(1)-C(1))*VECTX(I)+(P3D(2)-C(2))*VECTY(I)
     &                                      +(P3D(3)-C(3))*VECTZ(I)
 22         CONTINUE

C           LEVEL SET NORMALE CORRESPOND A LA 3EME COORDONNEE LOCALE
            ZR(JLNSV-1+(INO-1)+1)=PLOC(3)
            ZL(JLNSL-1+(INO-1)+1)=.TRUE.

C           LEVEL SET TANGENTE CORRESPOND A LA DISTANCE DU POINT
C           A L'ELLIPSE DANS LE PLAN (VECTX,VECTY)
            CALL DISELL(PLOC,A,B,H)

C           SI LA FISSURE EST A L'EXTERIEUR DE L'ELLIPSE, ON PREND 
C           L'OPPOSEE DE H (PAR DEFAUT, LA FISSURE EST A L'INTERIEUR)
            IF (COTE.EQ.'OUT') H = -1.D0 * H

            ZR(JLTSV-1+(INO-1)+1)=H
            ZL(JLTSL-1+(INO-1)+1)=.TRUE.

 20       CONTINUE          
          
        ENDIF

      ENDIF     
      
C-----------------------------------------------------------------------
C     REAJUSTEMENT DE LSN (BOOK III 06/02/04) ET LST
C-----------------------------------------------------------------------

      CALL XAJULS(IFM,NOMA,NBMA,CNSLT,CNSLN,JCONX1,JCONX2,CLSM)

      WRITE(IFM,*)'NOMBRE DE LEVEL SET REAJUSTEES APRES CONTROLE:',CLSM
      WRITE(IFM,*)'FIN DU CALCUL DES LEVEL-SETS'

C-----------------------------------------------------------------------
C     FIN
C-----------------------------------------------------------------------

      CALL JEDEMA()
      END
