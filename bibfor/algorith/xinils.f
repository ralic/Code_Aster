      SUBROUTINE XINILS(IFM,NOMA,METH,NFONF,NFONG,CNSLT,CNSLN)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER       IFM
      CHARACTER*8   NOMA,METH,NFONF,NFONG
      CHARACTER*19  CNSLT,CNSLN

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/07/2004   AUTEUR GENIAUT S.GENIAUT 
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
C RESPONSABLE CIBHHLV L.VIVAN
C                      CALCUL DES LEVEL-SETS
C
C    ENTREE :
C              IFM    :   FICHIER D'IMPRESSION
C              NOMA   :   OBJET MAILLAGE
C              METH   :   M…THODE DE CALUL DES LEVEL-SETS
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
      INTEGER       IBID,IRET,ME1,ME2,CLSM
      INTEGER       NBNO,INO,JCOOR,NBMAF,NUNO(4),NUNOSE(2),I
      INTEGER       JLTSV,JLTSL,JLNSV,JLNSL,AR(12,2)
      REAL*8        VALPU(3),AB(3),AC(3),AP(3),VN(3),VNT(3),BC(3)
      REAL*8        A(3),P(3),B(3),C(3),M(3),PM(3),VNREF(3)
      REAL*8        NORME,PS,PS1,PS2,LSNA,LSNB,D
      CHARACTER*8   K8BID,NOMPU(3),TYPMA 
      CHARACTER*16  K16BID
      CHARACTER*19  MAI
      CHARACTER*24  LISMA,LISNO,LISSE
      LOGICAL       MA2FF
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
C 
      CALL JEVEUO(NOMA//'.CONNEX','L',JCONX1)
      CALL JEVEUO(JEXATR(NOMA//'.CONNEX','LONCUM'),'L',JCONX2)

      IF (METH.EQ.'FONCTION') THEN

C-----------------------------------------------------------------------
C       DANS LE CAS OU ON DONNE FONC_LT ET FONC_LN
C-----------------------------------------------------------------------

        WRITE(IFM,*)'CALCUL DES LEVEL-SETS AVEC LA METHODE 1'
        DO 1 INO=1,NBNO
           X=ZR(JCOOR-1+3*(INO-1)+1)
           Y=ZR(JCOOR-1+3*(INO-1)+2)
           Z=ZR(JCOOR-1+3*(INO-1)+3)
           VALPU(1) = X
           VALPU(2) = Y
           VALPU(3) = Z
           CALL FOINTE('F ',NFONF, 3, NOMPU, VALPU, XLT, IBID )
           CALL FOINTE('F ',NFONG, 3, NOMPU, VALPU, XLN, IBID )
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

C       BOUCLE SUR TOUS LES NOEUDS P DU MAILLAGE  
C        write(6,*)'nbno ',NBNO
        DO 11 INO=1,NBNO
C          write(6,*)'ino ',INO
          P(1)=ZR(JCOOR-1+3*(INO-1)+1)
          P(2)=ZR(JCOOR-1+3*(INO-1)+2)
          P(3)=ZR(JCOOR-1+3*(INO-1)+3)

C         CALCUL DE LSN
C         -------------        
          DMIN=R8MAEM()
C         RECHERCHE DE LA MAILLE LA PLUS PROCHE : 
C         BOUCLE SUR NOEUDS DE MAFIS
          DO 2 IMAFIS=1,NBMAF
            NMAABS=ZI(JDLIMA-1+(IMAFIS-1)+1)
            NBNOMA=ZI(JCONX2+NMAABS) - ZI(JCONX2+NMAABS-1)
            IF (NBNOMA.EQ.4) NTRI=4  
            IF (NBNOMA.EQ.3) NTRI=1  

C           BOUCLE SUR LE NOMBRE DE TRIANGLES DE LA MAILLE
            DO 21 ITRI=1,NTRI          
       
              INOMA=1
              IF (ITRI.EQ.4) INOMA=4
              NUNO(INOMA)=ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+INOMA-1)
              A(1)=ZR(JCOOR-1+3*(NUNO(INOMA)-1)+1)
              A(2)=ZR(JCOOR-1+3*(NUNO(INOMA)-1)+2)
              A(3)=ZR(JCOOR-1+3*(NUNO(INOMA)-1)+3)
              
              INOMA=2
              IF (ITRI.EQ.2) INOMA=4
              NUNO(INOMA)=ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+INOMA-1)
              B(1)=ZR(JCOOR-1+3*(NUNO(INOMA)-1)+1)
              B(2)=ZR(JCOOR-1+3*(NUNO(INOMA)-1)+2)
              B(3)=ZR(JCOOR-1+3*(NUNO(INOMA)-1)+3)
              
              INOMA=3             
              IF (ITRI.EQ.3) INOMA=4
              NUNO(INOMA)=ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+INOMA-1)
              C(1)=ZR(JCOOR-1+3*(NUNO(INOMA)-1)+1)
              C(2)=ZR(JCOOR-1+3*(NUNO(INOMA)-1)+2)
              C(3)=ZR(JCOOR-1+3*(NUNO(INOMA)-1)+3)
              
              DO 211 I=1,3
                AB(I)=B(I)-A(I)
                BC(I)=C(I)-B(I)
                AP(I)=P(I)-A(I)
                AC(I)=C(I)-A(I)
 211          CONTINUE
 
C             CALCUL DE LA NORMALE ¿ LA MAILLE TRIA3
C             PROJECTION DE P SUR LA MAILLE VOIR R5.03.50-B 
              CALL PROVEC(AB,AC,VN)              
              CALL NORMEV(VN,NORME)      
              CALL PROVEC(AP,VN,VNT)
              CALL PSCAL(3,VNT,AC,PS)            
              EPS1=-1*PS/NORME              
              CALL PSCAL(3,VNT,AB,PS)
              EPS2=PS/NORME
              EPS3=1-EPS1-EPS2
             
C             SI M EST DS LE SECTEUR 1
              IF (EPS1.LT.0) THEN                
                CALL PSCAL(3,AC,AC,PS)
                CALL PSCAL(3,AB,AC,PS1)
                EPS2=EPS2+EPS1*PS1/PS
                EPS1=0 
              END IF
C             SI M EST DS LE SECTEUR 2
              IF (EPS2.LT.0) THEN               
                CALL PSCAL(3,AB,AB,PS)
                CALL PSCAL(3,AB,AC,PS1)            
                EPS1=EPS1+EPS2*PS1/PS
                EPS2=0
              END IF
C             SI M EST DS LE SECTEUR 3
              IF (EPS3.LT.0) THEN                
                CALL PSCAL(3,AB,AB,PS)
                CALL PSCAL(3,BC,BC,PS)
                CALL PSCAL(3,AB,BC,PS1)            
                CALL PSCAL(3,AC,BC,PS2) 
                EPS1=(-1*EPS1*PS1+(1-EPS2)*PS2)/PS
                EPS2=1-EPS1
              END IF             
             
C            ON FINIT DE RAMENER LES POINTS ENCORE DEHORS             
             IF (EPS1.LT.0) EPS1=0
             IF (EPS2.LT.0) EPS2=0
             IF (EPS1.GT.1) EPS1=1
             IF (EPS2.GT.1) EPS2=1
             
             DO 212 I=1,3
               M(I)=A(I)+EPS1*AB(I)+EPS2*AC(I)
               PM(I)=M(I)-P(I)
 212         CONTINUE                       
                          
C            CALCUL DE LA DISTANCE PM
             D=PADIST(3,P,M)
             
C            ON V…RIFIE QUE CETTE NORMALE EST ORIENTÈ…E COMME LA 
C            PR…CENDENTE, ¿ PART POUR LE 1ER TRIANGLE DE LA 1ER MAILLE! 
             IF ((IMAFIS.NE.1) .OR. (ITRI.NE.1))THEN
               CALL PSCAL(3,VN,VNREF,PS)     
               IF (PS.LT.0) VN(1)=-1*VN(1)
               IF (PS.LT.0) VN(2)=-1*VN(2)
               IF (PS.LT.0) VN(3)=-1*VN(3)
             END IF  
                        
C            ON GARDE CETTE NORMALE COMME R…F…R POUR LA MAILLE SUIVANTE
             DO 213 I=1,3
               VNREF(I)=VN(I) 
 213         CONTINUE                
                                                
C            MISE EN M…MOIRE DE LST POUR LA MAILLE LA PLUS PROCHE
             IF (D.LT.DMIN) THEN
               DMIN=D             
               CALL PSCAL(3,VN,PM,XLN)                         
             END IF 

 21        CONTINUE

 2       CONTINUE
       
         ZR(JLNSV-1+(INO-1)+1)=XLN
         ZL(JLNSL-1+(INO-1)+1)=.TRUE.

C        CALCUL DE LST
C        -------------   
          DMIN=R8MAEM()
C
C         RECHERCHE DU SEGMENT LE PLUS PROCHE : BOUCLE SUR SEG DE FONFIS
          DO 3 ISEFIS=1,NBSEF     

            NSEABS=ZI(JDLISE-1+(ISEFIS-1)+1)
            NBNOSE=ZI(JCONX2+NSEABS) - ZI(JCONX2+NSEABS-1)

            IF (NBNOSE.NE.2) THEN
              CALL UTMESS('F','XINILS','SEGMENT QUADRATIQUE SUR '//
     &                     'FOND_FISS.')
            ENDIF

            INOSE=1
            NUNOSE(INOSE)=ZI(JCONX1-1+ZI(JCONX2+NSEABS-1)+INOSE-1)
            INOSE=2
            NUNOSE(INOSE)=ZI(JCONX1-1+ZI(JCONX2+NSEABS-1)+INOSE-1)

C           BOUCLE SUR LES MAILLES DE MAFIS POUR TROUVER LA BONNE MAILLE
            MA2FF=.FALSE.
            DO 31 IMAFIS=1,NBMAF

              NMAABS=ZI(JDLIMA-1+(IMAFIS-1)+1)
              NBNOMA=ZI(JCONX2+NMAABS) - ZI(JCONX2+NMAABS-1)
C             ON R…CUP»RE LES NUMEROS DS NOEUDS DE LA MAILLE ET ON TESTE
              N1=0
              N2=0

              DO 32 INOMA=1,NBNOMA                
                NUM=ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+INOMA-1)
                IF (NUNOSE(1).EQ.NUM) N1=1
                IF (NUNOSE(2).EQ.NUM) N2=1
                IF ((NUNOSE(1).NE.NUM).AND.(NUNOSE(2).NE.NUM)) NUNOC=NUM
 32           CONTINUE

              IF ((N1*N2).EQ.1) THEN

                MA2FF=.TRUE.
                DO 33 I=1,3
                  A(I)=ZR(JCOOR-1+3*(NUNOSE(1)-1)+I)              
                  B(I)=ZR(JCOOR-1+3*(NUNOSE(2)-1)+I)
                  C(I)=ZR(JCOOR-1+3*(NUNOC-1)+I) 
                  AB(I)=B(I)-A(I)
                  AP(I)=P(I)-A(I)
                  AC(I)=C(I)-A(I)
 33             CONTINUE

C               CALCUL DE LA NORMALE ‡ LA MAILLE 
                CALL PROVEC(AB,AC,VN) 
                CALL NORMEV(VN,NORME)

C               CALCUL DE LA NORMALE INTERIEURE AU SEGMENT
                CALL PROVEC(AB,VN,VNT)
                CALL NORMEV(VNT,NORME)
                VN(1)=-1*VNT(1)
                VN(2)=-1*VNT(2)
                VN(3)=-1*VNT(3)

C               ON V…RIFIE QUE CETTE NORMALE EST ORIENT…E
C               COMME LA PR…CENDENTE (¿ PART POUR LE 1ER SEG!)
C               MAIS CA DEVRAIT JAMAIS ARRIVER
                IF (ISEFIS.NE.1) THEN
                  CALL PSCAL(3,VN,VNREF,PS)
                  IF (PS.LT.0) THEN
                    CALL UTMESS('A','XINILS','ORIENTATION '//
     &                   'DES NORMALES A FOND_FISS A VERIFIER.')
C                    VN(1)=-1*VN(1)
C                    VN(2)=-1*VN(2)
C                    VN(3)=-1*VN(3)
                  ENDIF  
                ENDIF

C               ON GARDE CETTE NORMALE COMME R…F POUR LE SEG SUIVANT
                VNREF(1)=VN(1) 
                VNREF(2)=VN(2)
                VNREF(3)=VN(3)

C               PROJECTION SUR LE SEGMENT
                CALL PSCAL(3,AP,AB,PS) 
                CALL PSCAL(3,AB,AB,PS1)          
                EPS=PS/PS1

C               ON RAM»NE M SUR LES BORDS S'IL LE FAUT
                IF (EPS.GT.1) EPS=1
                IF (EPS.LT.0) EPS=0

                DO 34 I=1,3
                  M(I)=A(I)+EPS*AB(I)
                  PM(I)=M(I)-P(I)
 34             CONTINUE                    

C               CALCUL DE LA DISTANCE PM
                D=PADIST(3,P,M)
C               MISE EN M…MOIRE DE LSN=PM.N POUR LE SEG LE PLUS PROCHE
                IF (D.LT.DMIN) THEN
                  DMIN=D
                  CALL PSCAL(3,VN,PM,XLT)
                END IF

              END IF

 31         CONTINUE 
 
            IF (.NOT.MA2FF) CALL UTMESS('F','XINILS','SEGMENT '//
     &              'DE FOND_FISS SANS MAILLE DE SURFACE RATTACHEE.')
 3        CONTINUE

         ZR(JLTSV-1+(INO-1)+1)=XLT
         ZL(JLTSL-1+(INO-1)+1)=.TRUE.

 11     CONTINUE

      ENDIF   

C-----------------------------------------------------------------------
C     REAJUSTEMENT DE LSN (BOOK III 06/02/04)
C-----------------------------------------------------------------------

C BUT : ON MODIFIE LES VALEURS DE LSN AUX NOEUDS SI TROP PROCHES DE 0
C ---   POUR …VITER LES ERREURS D'INT…GRATION
     
C     COMPTEUR DES LSN MODIFI…ES
      CLSM=0
      MAI=NOMA//'.TYPMAIL'
      CALL JEVEUO(MAI,'L',JMA)
C     BOUCLE SUR TOUTES LES MAILLES DU MAILLAGE
      DO 200 IMA=1,NBMA
        NMAABS=IMA
        ITYPMA=ZI(JMA-1+IMA)
        CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ITYPMA),TYPMA)
C      SI MAILLE NON VOLUMIQUE ON CONTINUE ¿ 200
        IF (TYPMA.NE.'HEXA8'.AND.TYPMA.NE.'PENTA6'.
     &       AND.TYPMA.NE.'TETRA4') GOTO 200  
C       BOUCLE SUR LES ARETES DE LA MAILLE VOLUMIQUE
        CALL CONARE(TYPMA,AR,NBAR)
        DO 210 IA=1,NBAR
          NA=AR(IA,1)
          NB=AR(IA,2)
          NUNOA=ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+NA-1)
          NUNOB=ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+NB-1)
          LSNA=ZR(JLNSV-1+(NUNOA-1)+1)
          LSNB=ZR(JLNSV-1+(NUNOB-1)+1)    
          IF (LSNA*LSNB.LT.0) THEN
            D=LSNA/(LSNA-LSNB)
            IF (D.LE.1.D-2) THEN
C              R…AJUSTEMENT DE LSNA
               ZR(JLNSV-1+(NUNOA-1)+1)=0.D0
               ZL(JLNSL-1+(NUNOA-1)+1)=.TRUE.
               CLSM=CLSM+1
            ENDIF
            IF (D.GE.(1.D0-1.D-2)) THEN
C              R…AJUSTEMENT DE LSNB
               ZR(JLNSV-1+(NUNOB-1)+1)=0.D0
               ZL(JLNSL-1+(NUNOB-1)+1)=.TRUE.
               CLSM=CLSM+1
            ENDIF
          ENDIF 
 210    CONTINUE
 200  CONTINUE
      WRITE(IFM,*)'NOMBRE DE LSN REAJUSTEES :',CLSM
      WRITE(IFM,*)'FIN DU CALCUL DES LEVEL-SETS'

C-----------------------------------------------------------------------
C     FIN
C-----------------------------------------------------------------------

      CALL JEDEMA()
      END
