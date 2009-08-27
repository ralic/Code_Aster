      SUBROUTINE XINILS(IFM,NOMA,METH,NFONF,NFONG,GEOFIS,A,B,NOEUD,COTE,
     &                                         VECT1,VECT2,CNSLT,CNSLN)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 24/08/2009   AUTEUR GENIAUT S.GENIAUT 
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
C TOLE CRP_20
C
      IMPLICIT NONE
      INTEGER       IFM
      CHARACTER*8   NOMA,METH,NFONF,NFONG,COTE
      CHARACTER*16  GEOFIS
      CHARACTER*19  CNSLT,CNSLN
      REAL*8        A,B,NOEUD(3),VECT1(3),VECT2(3)

C ----------------------------------------------------------------------
C                      CALCUL INITIAL DES LEVEL-SETS
C
C ENTREE :
C  IFM    :  FICHIER D'IMPRESSION
C  NOMA   :  OBJET MAILLAGE
C  METH   :  MÉTHODE DE CALUL DES LEVEL-SETS
C  NFONF  :  NOM DE LA FONCTION LEVEL SET TANGENTE
C  NFONG  :  NOM DE LA FONCTION LEVEL SET NORMALE
C  GEOFIS :  GEOMETRIE DE LA FISSURE
C  A,B,NOEUD,COTE,VECT1,VECT2 : QUANTITES DEFINISSANT LA GEO DE LA FISS
C SORTIE : 
C      CNSLN  :  LEVEL-SET NORMALE  (PLAN DE LA FISSURE)
C      CNSLT  :  LEVEL-SET TANGENTE (TRACE DE LA FISSURE)
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
      INTEGER       NA,NB,NBAR,NBMA,NBSEF,NMAABS,NUNOA,NUNOB,JCLTSV
      INTEGER       JCNV,JCNL,JCTV,JCTL,NBCMP,JCND,JCTD
      REAL*8        XLN,XLT,MAT(3,3)
      INTEGER       DIMENS, ADDIM, DIMNO
      INTEGER       IBID,IRET,ME1,ME2,CLSM,ME4
      INTEGER       NBNO,INO,JCOOR,NBMAF,I,J
      INTEGER       JLTSV,JLTSL,JLNSV,JLNSL,AR(12,2)
      REAL*8        VALPU(3),P2D(2),P3D(3),PLOC(3),VECT3(3),H
      REAL*8        NORME,LSNA,LSNB,D,LSTA,LSTB
      REAL*8        NORI(3),NEXT(3),NMIL(3),VSEG(3),NSEG
      CHARACTER*8   FISS,K8BID,NOMPU(3),TYPMA,NOMNO,NCHAMN,NCHAMT
      CHARACTER*16  K16BID,TYPDIS,VALK(3)
      CHARACTER*19  MAI,CHSLSN,CHSLST
      CHARACTER*24  LISMA,LISNO,LISSE
      REAL*8        R8PREM
      LOGICAL       CALLST
C
      CALL JEMARQ()
C
      CALL GETRES(FISS,K16BID,K16BID)

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

      CALL DISMOI('F','TYPE_DISCONTINUITE',FISS,'FISS_XFEM',
     &                                                 IBID,TYPDIS,IRET)
      IF (TYPDIS.EQ.'INTERFACE') CALLST = .FALSE.
      IF (TYPDIS.EQ.'FISSURE')   CALLST = .TRUE.

      IF (METH.EQ.'FONCTION') THEN

C-----------------------------------------------------------------------
C       DANS LE CAS OU ON DONNE FONC_LT ET FONC_LN
C-----------------------------------------------------------------------

        WRITE(IFM,*)'CALCUL DES LEVEL-SETS AVEC LA METHODE 1'
        DO 1 INO=1,NBNO
          DO 12 DIMNO=1, DIMENS
            VALPU(DIMNO)=ZR(JCOOR-1+3*(INO-1)+DIMNO)
 12       CONTINUE
          CALL FOINTE('F ',NFONG, DIMENS, NOMPU, VALPU, XLN, IBID )
          IF (CALLST) THEN
            CALL FOINTE('F ',NFONF, DIMENS, NOMPU, VALPU, XLT, IBID )
          ELSE
            XLT = -1.D0
          ENDIF
          ZR(JLNSV-1+(INO-1)+1)=XLN
          ZR(JLTSV-1+(INO-1)+1)=XLT
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

        IF (CALLST) THEN
          LISSE = '&&XINILS.LISTE_MA_FONFIS' 
          CALL RELIEM(' ',NOMA,'NU_MAILLE','DEFI_FISS',1,1,
     &                'GROUP_MA_FOND','GROUP_MA',LISSE,NBSEF)  
          CALL JEVEUO(LISSE,'L',JDLISE)
        ENDIF

        IF (DIMENS .EQ. 3) THEN
         CALL XLS3D(CALLST,JLTSV,JLTSL,JLNSV,JLNSL,NBNO,JCOOR,
     &              NBMAF,JDLIMA,NBSEF,JDLISE,JCONX1,JCONX2,NOMA)
        ELSE     
         CALL XLS2D(CALLST,JLTSV,JLTSL,JLNSV,JLNSL,NBNO,JCOOR,
     &              NBMAF,JDLIMA,NBSEF,JDLISE,JCONX1,JCONX2)
        ENDIF

      ELSEIF (METH.EQ.'GEOMETRI') THEN

C-----------------------------------------------------------------------
C       DANS LE CAS OU ON DONNE LA GEOMETRIE DE LA FISSURE
C-----------------------------------------------------------------------

        WRITE(IFM,*)'CALCUL DES LEVEL-SETS AVEC LA METHODE 3'    

C       VERIFICATIONS (CAR REGLES INMPOSSIBLES DANS CAPY)
        IF (.NOT.CALLST) THEN
          IF (GEOFIS.EQ.'ELLIPSE'.OR.
     &        GEOFIS.EQ.'CYLINDRE'.OR.      
     &        GEOFIS.EQ.'DEMI_PLAN'.OR.
     &        GEOFIS.EQ.'SEGMENT'.OR.      
     &        GEOFIS.EQ.'DEMI_DROITE') THEN
            VALK(1) = 'INTERFACE'
            VALK(2) =  GEOFIS
            VALK(3) = 'FISSURE'
            CALL U2MESK('F','XFEM_23',3,VALK)
          ENDIF
        ELSEIF (CALLST) THEN
          IF (GEOFIS.EQ.'DROITE'.OR.      
     &        GEOFIS.EQ.'INCLUSION') THEN  
            VALK(1) = 'FISSURE'
            VALK(2) =  GEOFIS
            VALK(3) = 'INTERFACE'
            CALL U2MESK('F','XFEM_23',3,VALK)
          ENDIF
        ENDIF
                
        IF (GEOFIS.EQ.'ELLIPSE') THEN
            
C  VECT1  = VECT DU DEMI-GRAND AXE
C  VECT2  = VECT DU DEMI-PETIT AXE
C  NOEUD  = CENTRE DE L'ELLIPSE
C  A      =  DEMI-GRAND AXE       
C  B      =  DEMI-PETIT AXE       
C  COTE   =  COTE DE LA FISSURE ('IN' OU 'OUT' DE L'ELLIPSE)

          DO 20 INO=1,NBNO

C           COORDONNEES 3D DU POINT DANS LE REPERE GLOBAL
            DO 21 DIMNO=1, DIMENS
              P3D(DIMNO)=ZR(JCOOR-1+3*(INO-1)+DIMNO)
 21         CONTINUE

C           BASE LOCALE : (VECT1,VECT2,VECT3)
            CALL NORMEV(VECT1,NORME)
            CALL NORMEV(VECT2,NORME)
            CALL PROVEC(VECT1,VECT2,VECT3)

C           MATRICE DE PASSAGE LOC-GLOB
            DO 22 I=1,3
              MAT(1,I)=VECT1(I)
              MAT(2,I)=VECT2(I)
              MAT(3,I)=VECT3(I)
 22         CONTINUE

C           PROJECTION DU POINT 3D DANS LE REPERE LOCAL LIE A L'ELLIPSE
            DO 23 I=1,3
              PLOC(I)=0.D0
              DO 24 J=1,3
                PLOC(I) = PLOC(I) + MAT(I,J) * (P3D(J)-NOEUD(J))
 24           CONTINUE
 23         CONTINUE

C           LEVEL SET NORMALE CORRESPOND A LA 3EME COORDONNEE LOCALE
            ZR(JLNSV-1+(INO-1)+1)=PLOC(3)
            ZL(JLNSL-1+(INO-1)+1)=.TRUE.

C           LEVEL SET TANGENTE CORRESPOND A LA DISTANCE DU POINT
C           A L'ELLIPSE DANS LE PLAN (VECT1,VECT2)
            CALL DISELL(PLOC,A,B,H)

C           SI LA FISSURE EST A L'EXTERIEUR DE L'ELLIPSE, ON PREND 
C           L'OPPOSEE DE H (PAR DEFAUT, LA FISSURE EST A L'INTERIEUR)
            IF (COTE.EQ.'OUT') H = -1.D0 * H

            ZR(JLTSV-1+(INO-1)+1)=H
            ZL(JLTSL-1+(INO-1)+1)=.TRUE.

 20       CONTINUE          
        
        ELSEIF (GEOFIS.EQ.'CYLINDRE') THEN
           
C  VECT1  = VECT DU DEMI-GRAND AXE
C  VECT2  = VECT DU DEMI-PETIT AXE
C  NOEUD  = CENTRE DE L'ELLIPSE
C  A      =  DEMI-GRAND AXE       
C  B      =  DEMI-PETIT AXE       

          DO 30 INO=1,NBNO

C           COORDONNEES 3D DU POINT DANS LE REPERE GLOBAL
            DO 31 DIMNO=1, DIMENS
              P3D(DIMNO)=ZR(JCOOR-1+3*(INO-1)+DIMNO)
 31         CONTINUE

C           BASE LOCALE : (VECT1,VECT2,VECT3)
            CALL NORMEV(VECT1,NORME)
            CALL NORMEV(VECT2,NORME)
            CALL PROVEC(VECT1,VECT2,VECT3)

C           MATRICE DE PASSAGE LOC-GLOB
            DO 32 I=1,3
              MAT(1,I)=VECT1(I)
              MAT(2,I)=VECT2(I)
              MAT(3,I)=VECT3(I)
 32         CONTINUE

C           PROJECTION DU POINT 3D DANS LE REPERE LOCAL LIE A L'ELLIPSE
            DO 33 I=1,3
              PLOC(I)=0.D0
              DO 34 J=1,3
                PLOC(I) = PLOC(I) + MAT(I,J) * (P3D(J)-NOEUD(J))
 34           CONTINUE
 33         CONTINUE

C           LEVEL SET TANGENTE CORRESPOND A LA 3EME COORDONNEE LOCALE
            ZR(JLTSV-1+(INO-1)+1)= PLOC(3)
            ZL(JLTSL-1+(INO-1)+1)=.TRUE.

C           LEVEL SET NORMALE CORRESPOND A LA DISTANCE DU POINT
C           AU CYLINDRE
            CALL DISELL(PLOC,A,B,H)

            ZR(JLNSV-1+(INO-1)+1)=H
            ZL(JLNSL-1+(INO-1)+1)=.TRUE.

 30       CONTINUE          
        
        ELSEIF (GEOFIS.EQ.'DEMI_PLAN') THEN


C  VECT1 = VECT NORMAL AU PLAN DE FISSURE
C  VECT2 = VECT DANS LE PLAN DE FISSURE  
C          (NORMALE AU FOND : DIRECTION DE PROPAGATION POTENTIELLE)
C  NOEUD = NOEUD DU FOND DE FISSURE

          DO 29 INO=1,NBNO

C           COORDONNEES 3D DU POINT DANS LE REPERE GLOBAL
            DO 25 DIMNO=1, DIMENS
              P3D(DIMNO)=ZR(JCOOR-1+3*(INO-1)+DIMNO)
 25         CONTINUE

C           BASE LOCALE : (VECT2,VECT3,VECT1)
            CALL NORMEV(VECT1,NORME)
            CALL NORMEV(VECT2,NORME)
            CALL PROVEC(VECT1,VECT2,VECT3)

C           MATRICE DE PASSAGE LOC-GLOB
            DO 26 I=1,3
              MAT(1,I)=VECT2(I)
              MAT(2,I)=VECT3(I)
              MAT(3,I)=VECT1(I)
 26         CONTINUE

C           PROJECTION DU POINT 3D DANS LE REPERE LOCAL
            DO 27 I=1,3
              PLOC(I)=0.D0
              DO 28 J=1,3
                PLOC(I) = PLOC(I) + MAT(I,J) * (P3D(J)-NOEUD(J))
 28           CONTINUE
 27         CONTINUE

C           LEVEL SET NORMALE CORRESPOND A LA 3EME COORDONNEE LOCALE
            ZR(JLNSV-1+(INO-1)+1)= PLOC(3)
            ZL(JLNSL-1+(INO-1)+1)=.TRUE.        

C           LEVEL SET TANGENTE CORRESPOND A LA 1ERE COORDONNEE LOCALE
            ZR(JLTSV-1+(INO-1)+1)= PLOC(1)
            ZL(JLTSL-1+(INO-1)+1)=.TRUE.
            
 29       CONTINUE          
         
        ELSEIF (GEOFIS.EQ.'SEGMENT') THEN

C  VECT1 = NOEUD DU FOND ORIGINE
C  VECT2 = NOEUD DU FOND EXTREMITE

          DO 100 I=1,3
            NORI(I) = VECT1(I)
            NEXT(I) = VECT2(I)
            NMIL(I) = (NORI(I) + NEXT(I))/2
            VSEG(I) = NEXT(I)-NORI(I)
 100      CONTINUE   
 
          NSEG = SQRT(VSEG(1)**2 + VSEG(2)**2 + VSEG(3)**2)

          DO 50 INO=1,NBNO

C           COORDONNEES 2D DU POINT DANS LE REPERE GLOBAL
            DO 51 DIMNO=1, DIMENS
              P2D(DIMNO)=ZR(JCOOR-1+3*(INO-1)+DIMNO)
 51         CONTINUE

            VECT3(1) = 0.D0
            VECT3(2) = 0.D0
            VECT3(3) = 1.D0
            
C           BASE LOCALE : (VSEG,VECT2)
            CALL NORMEV(VSEG,NORME)
            CALL PROVEC(VECT3,VSEG,VECT2)

C           MATRICE DE PASSAGE LOC-GLOB
            DO 52 I=1,2
              MAT(1,I)=VSEG(I)
              MAT(2,I)=VECT2(I)
 52         CONTINUE

C           PROJECTION DU POINT 2D DANS LE REPERE LOCAL
C           POSITIONNE AU CENTRE DU SEGEMENT
            DO 53 I=1,2
              PLOC(I)=0.D0
              DO 54 J=1,2
                PLOC(I) = PLOC(I) + MAT(I,J) * (P2D(J)-NMIL(J))
 54           CONTINUE
 53         CONTINUE

C           LEVEL SET NORMALE CORRESPOND A LA 2EME COORDONNEE LOCALE
            ZR(JLNSV-1+(INO-1)+1)= PLOC(2)
            ZL(JLNSL-1+(INO-1)+1)=.TRUE.        

C           LEVEL SET TANGENTE EST DEFINIE PAR : 
            ZR(JLTSV-1+(INO-1)+1)= ABS(PLOC(1)) - NSEG/2
            ZL(JLTSL-1+(INO-1)+1)=.TRUE.
            
 50       CONTINUE          

          
        ELSEIF (GEOFIS.EQ.'DEMI_DROITE') THEN

C  VECT1 = VECTEUR DIRECTEUR DE LA DEMI DROITE 
C          DANS LA DIRECTION DE PROPA
C  NOEUD = NOEUD DU FOND DE FISSURE

          DO 35 INO=1,NBNO

C           COORDONNEES 2D DU POINT DANS LE REPERE GLOBAL
            DO 36 DIMNO=1, DIMENS
              P2D(DIMNO)=ZR(JCOOR-1+3*(INO-1)+DIMNO)
 36         CONTINUE
            
            VECT3(1) = 0.D0
            VECT3(2) = 0.D0
            VECT3(3) = 1.D0
            
C           BASE LOCALE : (VECT1,VECT2)
            CALL NORMEV(VECT1,NORME)
            CALL PROVEC(VECT3,VECT1,VECT2)

C           MATRICE DE PASSAGE LOC-GLOB
            DO 37 I=1,2
              MAT(1,I)=VECT1(I)
              MAT(2,I)=VECT2(I)
 37         CONTINUE

C           PROJECTION DU POINT 2D DANS LE REPERE LOCAL
            DO 38 I=1,2
              PLOC(I)=0.D0
              DO 39 J=1,2
                PLOC(I) = PLOC(I) + MAT(I,J) * (P2D(J)-NOEUD(J))
 39           CONTINUE
 38         CONTINUE

C           LEVEL SET NORMALE CORRESPOND A LA 2EME COORDONNEE LOCALE
            ZR(JLNSV-1+(INO-1)+1)= PLOC(2)
            ZL(JLNSL-1+(INO-1)+1)=.TRUE.        

C           LEVEL SET TANGENTE CORRESPOND A LA 1ERE COORDONNEE LOCALE
            ZR(JLTSV-1+(INO-1)+1)= PLOC(1)
            ZL(JLTSL-1+(INO-1)+1)=.TRUE.
            
 35       CONTINUE  


        ELSEIF (GEOFIS.EQ.'DROITE') THEN

C  VECT1 = VECTEUR DIRECTEUR DE LA  DROITE
C  NOEUD = UN POINT DE LA DROITE

          DO 40 INO=1,NBNO

C           COORDONNEES 2D DU POINT DANS LE REPERE GLOBAL
            DO 41 DIMNO=1, DIMENS
              P2D(DIMNO)=ZR(JCOOR-1+3*(INO-1)+DIMNO)
 41         CONTINUE
            
            VECT3(1) = 0.D0
            VECT3(2) = 0.D0
            VECT3(3) = 1.D0
            
C           BASE LOCALE : (VECT1,VECT2)
            CALL NORMEV(VECT1,NORME)
            CALL PROVEC(VECT3,VECT1,VECT2)

C           MATRICE DE PASSAGE LOC-GLOB
            DO 42 I=1,2
              MAT(1,I)=VECT1(I)
              MAT(2,I)=VECT2(I)
 42         CONTINUE

C           PROJECTION DU POINT 2D DANS LE REPERE LOCAL
            DO 43 I=1,2
              PLOC(I)=0.D0
              DO 44 J=1,2
                PLOC(I) = PLOC(I) + MAT(I,J) * (P2D(J)-NOEUD(J))
 44           CONTINUE
 43         CONTINUE


C           LEVEL SET NORMALE CORRESPOND A LA 2EME COORDONNEE LOCALE
            ZR(JLNSV-1+(INO-1)+1)= PLOC(2)
            ZL(JLNSL-1+(INO-1)+1)=.TRUE.        

C           LEVEL SET TANGENTE PAS DEFINIE 
            CALL ASSERT(.NOT.CALLST)
            ZR(JLTSV-1+(INO-1)+1)= -1.D0
            ZL(JLTSL-1+(INO-1)+1)=.TRUE.        
            
 40       CONTINUE  
        
        ELSEIF  (GEOFIS.EQ.'INCLUSION') THEN
           
C  VECT1  = VECT DU DEMI-GRAND AXE
C  VECT2  = VECT DU DEMI-PETIT AXE
C  NOEUD  = CENTRE DE L'ELLIPSE
C  A      =  DEMI-GRAND AXE       
C  B      =  DEMI-PETIT AXE       

          DO 45 INO=1,NBNO

C           COORDONNEES 3D DU POINT DANS LE REPERE GLOBAL
            DO 46 DIMNO=1, DIMENS
              P2D(DIMNO)=ZR(JCOOR-1+3*(INO-1)+DIMNO)
 46         CONTINUE

C           BASE LOCALE : (VECT1,VECT2,VECT3)
            CALL NORMEV(VECT1,NORME)
            CALL NORMEV(VECT2,NORME)

C           MATRICE DE PASSAGE LOC-GLOB
            DO 47 I=1,2
              MAT(1,I)=VECT1(I)
              MAT(2,I)=VECT2(I)
 47         CONTINUE

C           PROJECTION DU POINT 3D DANS LE REPERE LOCAL LIE A L'ELLIPSE
            DO 48 I=1,2
              PLOC(I)=0.D0
              DO 49 J=1,2
                PLOC(I) = PLOC(I) + MAT(I,J) * (P2D(J)-NOEUD(J))
 49           CONTINUE
 48         CONTINUE

C           LEVEL SET NORMALE CORRESPOND A LA DISTANCE DU POINT
C           AU CYLINDRE
            CALL DISELL(PLOC,A,B,H)

            ZR(JLNSV-1+(INO-1)+1)=H
            ZL(JLNSL-1+(INO-1)+1)=.TRUE.

C           LEVEL SET TANGENTE PAS DEFINIE 
            CALL ASSERT(.NOT.CALLST)
            ZR(JLTSV-1+(INO-1)+1)= -1.D0
            ZL(JLTSL-1+(INO-1)+1)=.TRUE.        

 45       CONTINUE          
                          
        ENDIF

      ELSE IF (METH.EQ.'CHAMP') THEN

C-----------------------------------------------------------------------
C       DANS LE CAS OU ON DONNE UN CHAMP DE LEVEL SET
C-----------------------------------------------------------------------

        WRITE(IFM,*)'CALCUL DES LEVEL-SETS AVEC LA METHODE 4'
        CALL GETVID('DEFI_FISS','CHAM_NO_LSN',1,1,1,NCHAMN,ME4)
        CALL GETVID('DEFI_FISS','CHAM_NO_LST',1,1,1,NCHAMT,IBID)

        CHSLSN='&&XINILS.CHAM_S_LSN'
        CHSLST='&&XINILS.CHAM_S_LST'
        CALL CNOCNS(NCHAMN,'V',CHSLSN)
        IF (CALLST) CALL CNOCNS(NCHAMT,'V',CHSLST)

C       ON VERIFIE LE NOMBRE DE COMPOSANTES = 1  (LSN OU LST)
        CALL JEVEUO(CHSLSN//'.CNSD','L',JCND)
        CALL ASSERT(ZI(JCND+1).EQ.1)
        IF (CALLST) CALL JEVEUO(CHSLST//'.CNSD','L',JCTD)
        IF (CALLST) CALL ASSERT(ZI(JCTD+1).EQ.1)

        CALL JEVEUO(CHSLSN//'.CNSV','L',JCNV)
        CALL JEVEUO(CHSLSN//'.CNSL','L',JCNL)
        IF (CALLST) CALL JEVEUO(CHSLST//'.CNSV','L',JCTV)
        IF (CALLST) CALL JEVEUO(CHSLST//'.CNSL','L',JCTL)

        DO 60 INO=1,NBNO
C           ON VERIFIE QUE LE NOEUD POSSEDE CETTE COMPOSANTE
            CALL ASSERT(ZL(JCNL+INO-1))
            IF (CALLST) CALL ASSERT(ZL(JCTL+INO-1))
            ZR(JLNSV-1+(INO-1)+1)=ZR(JCNV+INO-1)
            ZL(JLNSL-1+(INO-1)+1)=.TRUE. 
            IF (CALLST)      ZR(JLTSV-1+(INO-1)+1)=ZR(JCTV+INO-1)
            IF (.NOT.CALLST) ZR(JLTSV-1+(INO-1)+1)= -1.D0
            ZL(JLTSL-1+(INO-1)+1)=.TRUE.
 60     CONTINUE 

        CALL JEDETR(CHSLSN)
        IF (CALLST) CALL JEDETR(CHSLST)

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
