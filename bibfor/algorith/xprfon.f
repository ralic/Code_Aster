      SUBROUTINE   XPRFON(NOMA,FISS,NUMFON,NVIT,NBETA)

      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*8    FISS,NOMA

      CHARACTER*24   NVIT,NBETA
      INTEGER        NUMFON

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE COLOMBO D.COLOMBO

C       XPRFON   : X-FEM PROPAGATION :RENUMEROTATION DU FRONT DE FISSURE
C       ------     -     --                                  ---
C    RENUMEROTATION DU FRONT DE FISSURE DANS LE CAS DE L'UTILISATION DE
C    LA METHODE UPWIND AVEC PLUSIEURS FOND DE FISSURE     
C
C    ENTREE
C        NOMA    : NOM DU CONCEPT MAILLAGE
C        FISS    : NOM DU CONCEPT FISSURE X-FEM
C                  (FISSURE INITIALE DONT ON EXTRAIT LE FOND DE FISSURE)
C        NVIT    : VECTEUR DES VITESSES DE PROPAGATION POUR CHAQUE POINT
C                  DU FOND DE LA FISSURE (NOM DU CONCEPT)
C        NBETA   : VECTEUR DES ANGLES DE PROPAGATION POUR CHAQUE POINT
C                  DU FOND DE LA FISSURE (NOM DU CONCEPT)
C        NUMFON  : NOMBRE DE POINT DU FOND DE FISSURE
C
C     ------------------------------------------------------------------


      INTEGER        I,JCOOR,IRET,NBNO,JVIT,JBETA,NPOIN,COMPTE,
     &               JFONF,IFM,NIV,NFON,NFVPR             
      REAL*8         D,DMAX
      CHARACTER*8    K8B
      INTEGER        JBASEF,NBPTFF

      REAL*8         PI(3),PJ(3),PIJ(3)   
                  
C     MULTIPLE CRACK FRONTS
      INTEGER        JFMULT

C-----------------------------------------------------------------------
C     DEBUT
C-----------------------------------------------------------------------
     
C     Recuperation des points des caracteristique du maillage et du
C     fond de fissure

      CALL JEMARQ()
      CALL INFMAJ()
      CALL INFNIV(IFM,NIV)

C     RECUPERATION DES CARACTERISTIQUES DU MAILLAGE
      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNO,K8B,IRET)
      CALL JEVEUO(NOMA//'.COORDO    .VALE','L',JCOOR)

C     RECUPERATION DU FOND DE FISSURE
      CALL JEVEUO(FISS//'.FONDFISS','L',JFONF)
      CALL DISMOI('F','NB_POINT_FOND',FISS,'FISS_XFEM',NBPTFF,K8B,IRET)

C     RETRIEVE THE DIFFERENT PIECES OF THE CRACK FRONT
      CALL JEVEUO(FISS//'.FONDMULT','L',JFMULT)
      CALL DISMOI('F','NB_FOND',FISS,'FISS_XFEM',NUMFON,K8B,IRET)

C     RETRIEVE THE LOCAL REFERENCE SYSTEM FOR EACH NODE ON THE FRONT
      CALL JEVEUO(FISS//'.BASEFOND','E',JBASEF)

C     RETRIEVE THE CRACK'S SPEED AND PROPAGATION ANGLE FOR EACH NODE ON
C     THE FRONT
      CALL JEVEUO(NVIT,'E',JVIT)
      CALL JEVEUO(NBETA,'E',JBETA)

C     creation du vecteur ou l'on stocke les distances entre les
C     differents fond de fissure
      CALL WKVECT('&&XPRFON.DIST_FON','V V R8',
     &            NUMFON,NFON)  

C     CREATION DU VECTEUR CONTENANT LES NUMEROTATIONS DU FOND DE FISSURE
      CALL WKVECT('&&XPRFON.NUM_FON_VIRPR','V V I',
     &           (2*NUMFON),NFVPR)

C         On calcule la distance maximale entre les fonds de fissure 
C         (utile si le fond de fissure est ouvert uniquememt) 
            DO 1  I=1,(NUMFON-1)
C         On extrait le numero du dernier point du front considere      
                NPOIN=ZI(JFMULT-1+2*I)
C         On extrait les coordonnes du point du fond de fissure et du
C         premier point du fond suivant
              PI(1)=ZR(JFONF-1+4*(NPOIN-1)+1)
              PI(2)=ZR(JFONF-1+4*(NPOIN-1)+2)
              PI(3)=ZR(JFONF-1+4*(NPOIN-1)+3)

              PJ(1)=ZR(JFONF-1+4*(NPOIN)+1)
              PJ(2)=ZR(JFONF-1+4*(NPOIN)+2)
              PJ(3)=ZR(JFONF-1+4*(NPOIN)+3)
               
C            On calcule la distance et on la stocke
              PIJ(1)=PJ(1)-PI(1)
              PIJ(2)=PJ(2)-PI(2)
              PIJ(3)=PJ(3)-PI(3)
              ZR(NFON+I-1)=(PIJ(1)**2+PIJ(2)**2+PIJ(3)**2)
1        CONTINUE
         
C           cas particulier du dernier front de fissure  

C         On extrait les coordonnes du point du fond de fissure et du
C         premier point du fond suivant
C             dernier point de la liste
              PI(1)=ZR(JFONF-1+4*(NBPTFF-1)+1)
              PI(2)=ZR(JFONF-1+4*(NBPTFF-1)+2)
              PI(3)=ZR(JFONF-1+4*(NBPTFF-1)+3)
C             premier point de la liste
              PJ(1)=ZR(JFONF-1+1)
              PJ(2)=ZR(JFONF-1+2)
              PJ(3)=ZR(JFONF-1+3)
               
C            On calcule la distance et on la stocke
              PIJ(1)=PJ(1)-PI(1)
              PIJ(2)=PJ(2)-PI(2)
              PIJ(3)=PJ(3)-PI(3)
              ZR(NFON+NUMFON-1)=(PIJ(1)**2+PIJ(2)**2+PIJ(3)**2)


C         On definit quelle distance est la plus grande
            D=0.D0
            DMAX=0.D0
            COMPTE=0
            DO 10 I=1,NUMFON
               D= ZR(NFON+I-1)
               IF (D.GT.DMAX) THEN
                     DMAX=D
                     COMPTE=I
               ENDIF
10          CONTINUE
        
C           Si le trou le plus grand n est pas le dernier, les
C           differents fond de fissure sont mal numerote
C            On doit changer l'ordre des fonds de fissure 
            IF (COMPTE.NE.NUMFON) THEN
C                On change l´ordre des points du fonds de fissure en
C                utilisants une permutation circulaire
C                On commence par definir quel sera le nouveau point
C                d origine
                 NPOIN=ZI(JFMULT-1+2*COMPTE+1)

C                On procede a la permutation des noeuds
                 CALL PERMR8(ZR(JFONF), 4*(NPOIN-1)+1, 4*NBPTFF)
                 CALL PERMR8(ZR(JBASEF), 2*3*(NPOIN-1)+1 ,2*3*NBPTFF)
                 CALL PERMR8(ZR(JVIT), (NPOIN-1)+1,NBPTFF)
                 CALL PERMR8(ZR(JBETA),(NPOIN-1)+1 ,NBPTFF)      

C           Modification de JMULT

C           on calcule les nouvelles numerotation des points, que l on
C           rentre dans le vecteur JFMULT 
            DO 20 I=1,(2*NUMFON)
                IF (I.LT.(2*COMPTE+1)) THEN
                     ZI(NFVPR-1+I)=ZI(JFMULT-1+I)+NBPTFF-NPOIN+1
                ELSE
                     ZI(NFVPR-1+I)=ZI(JFMULT-1+I)+(1-NPOIN)
                ENDIF                           
20    CONTINUE 
                          
C           On permute ensuite l ordre des fissures     
            DO 40, I=1,(2*NUMFON)
                IF (I.LT.(2*COMPTE+1)) THEN
                    ZI(JFMULT-1+I+2*NUMFON+1-(2*COMPTE+1))=
     &              ZI(NFVPR-1+I)
                ELSE
                    ZI(JFMULT-1+I+1-(2*COMPTE+1))=ZI(NFVPR-1+I)
                ENDIF
40       CONTINUE
        ENDIF
       
      CALL JEDETR('&&XPRFON.DIST_FON')
      CALL JEDETR('&&XPRFON.NUM_FON_VIRPR')
C-----------------------------------------------------------------------
C     FIN
C-----------------------------------------------------------------------
      CALL JEDEMA()
      END
