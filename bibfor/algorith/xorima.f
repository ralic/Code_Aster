      SUBROUTINE XORIMA(NOMA,NBMAF,JDLIMA,JCONX1,JCONX2,JCOOR,SENS)

      IMPLICIT NONE

      INCLUDE 'jeveux.h'
      CHARACTER*8  NOMA
      CHARACTER*19 SENS
      INTEGER      NBMAF,JDLIMA,JCONX1,JCONX2,JCOOR

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
   
C  ---------------------------------------------------------------------
C
C       XORIMA   : X-FEM ORIENTATION MAILLES : ORIENTATION DE LA NORMALE
C       ------     -     ---         --  
C                  DES MAILLES DEFINISSANT LA SURFACE DE LA FISSURE
C
C  DANS LE CADRE DE LA DEFINITION X-FEM D'UNE FISSURE 3D PAR UN MAILLAGE
C  SURFACIQUE, LA NORMALE DE CHAQUE ELEMENT DE CE MAILLAGE DOIT ETRE
C  BIEN ORIENTEE PAR RAPPORT A CELLES DES AUTRES MAILLES AFIN DE BIEN
C  CALCULER LA SIGNE DE LA LEVEL SET NORMALE.
C
C  ENTREE
C  ------
C
C     NOMA   = NOM DU MAILLAGE SURFACIQUE DEFINISSANT LA FISSURE
C     NBMAF  = NOMBRE DE MAILLES DANS JDLIMA
C     JDLIMA = POINTER A L'OBJET JEVEUX QUI CONTIENT LA LISTE DES 
C              MAILLES DE NOMA QUI FORMENT LA SURFACE DE LA FISSURE 
C     JCONX1 = POINTER A L'OBJET JEVEUX NOMA//'.CONNEX'
C     JCONX2 = POINTER A L'OBJET JEVEUX LONGUEUR CUMULEE DE JCONX1
C     JCOOR  = POINTER A L'OBJET JEVEUX DES COORDONNES DES NOEUDS DU
C              MAILLAGE NOMA
C     SENS   = NOM DE L'OBJET JEVEUX A CREER (VOIR CI-DESSOUS)
C
C  SORTIE
C  ------
C
C     SENS   = OBJET JEVEUX REMPLI. IL S'AGIT D'UN VECTEUR D'ENTIERS DE
C              LONGUEUR NBMAF CONTENANT SEULEMENT DES 1 OU DES -1.
C              PENDANT LE CALCUL DE LA LEVEL SET NORMALE PAR PROJECTION
C              SUR LES ELEMENT DE JDLIMA, LA NORMALE A L'ELEMENT DOIT
C              ETRE MULTIPLIEE PAR LA VALEUR CONTENUE DANS SENS AFIN DE
C              REORIENTER LA NORMALE (-1) SI NECESSAIRE POUR BIEN
C              CALCULER LA SIGNE DE LA LEVEL SET NORMALE.


      REAL*8       A(3),B(3),C(3),AB(3),AC(3),VN(3),VNREF(3),PS,DDOT,
     &             NORME
      INTEGER      JSENS,NLAYER,LAYER,I,J,NMAABS,NBNOMA,INOMA,JELNO,
     &             NBELNO,ELJ,NUMELM,NMAASS
      CHARACTER*19 CNXINV
      CHARACTER*8  K8B

C-----------------------------------------------------------------------
      INTEGER NUNO 
C-----------------------------------------------------------------------
      CALL JEMARQ()

C     CREATE THE VECTOR WHERE THE ORIENTATION OF THE ELEMENT NORMAL WILL
C     BE STORED
      CALL WKVECT(SENS,'V V I',NBMAF,JSENS)

C     CREATE A TEMPORARY VECTOR WHERE THE LAYER TO WHICH EACH ELEMENT
C     BELONGS IS STORED
      CALL WKVECT('&&XORIMA.LAY','V V I',NBMAF,NLAYER)
      CALL JERAZO('&&XORIMA.LAY',NBMAF,1)
      CALL JEVEUO('&&XORIMA.LAY','E',NLAYER)

C     THE FIRST ELEMENT IS TAKEN AS THE REFERENCE
      ZI(NLAYER-1+1) = 1
      ZI(JSENS-1+1) = 1

C     CREATE THE INVERSE CONNECTIVITY OBJECT
      CNXINV = '&&XORIMA.CNCINV'
      CALL CNCINV(NOMA,ZI(JDLIMA),NBMAF,'V',CNXINV)

C     LOOP ON THE LAYER NUMBER
      DO 10 LAYER=1,NBMAF

C        LOOP ON THE ELEMENT LIST
         DO 20 I=1,NBMAF

C           SEARCH FOR THE ELEMENTS BELONGING TO THE CURRENT LAYER
            IF (ZI(NLAYER-1+I).EQ.LAYER) THEN

C              CALCULATE THE NORMAL TO THE ELEMENT. IT WILL BE USED
C              AS A REFERENCE FOR THE CONNECTED ELEMENTS.
               NMAABS = ZI(JDLIMA-1+I)
               NBNOMA = ZI(JCONX2+NMAABS)-ZI(JCONX2+NMAABS-1)

               INOMA=1
               NUNO=ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+INOMA-1)
               A(1)=ZR(JCOOR-1+3*(NUNO-1)+1)
               A(2)=ZR(JCOOR-1+3*(NUNO-1)+2)
               A(3)=ZR(JCOOR-1+3*(NUNO-1)+3)

               INOMA=2
               NUNO=ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+INOMA-1)
               B(1)=ZR(JCOOR-1+3*(NUNO-1)+1)
               B(2)=ZR(JCOOR-1+3*(NUNO-1)+2)
               B(3)=ZR(JCOOR-1+3*(NUNO-1)+3)

               INOMA=3
               NUNO=ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+INOMA-1)
               C(1)=ZR(JCOOR-1+3*(NUNO-1)+1)
               C(2)=ZR(JCOOR-1+3*(NUNO-1)+2)
               C(3)=ZR(JCOOR-1+3*(NUNO-1)+3)

               AB(1)=B(1)-A(1)
               AB(2)=B(2)-A(2)
               AB(3)=B(3)-A(3)

               AC(1)=C(1)-A(1)
               AC(2)=C(2)-A(2)
               AC(3)=C(3)-A(3)

               CALL PROVEC(AB,AC,VNREF)
               CALL NORMEV(VNREF,NORME)

               VNREF(1) = VNREF(1)*ZI(JSENS-1+I)
               VNREF(2) = VNREF(2)*ZI(JSENS-1+I)
               VNREF(3) = VNREF(3)*ZI(JSENS-1+I)

C              LOOP ON EACH NODE OF THE SELECTED ELEMENT
               DO 30 J=1,NBNOMA

                  NUNO=ZI(JCONX1-1+ZI(JCONX2+NMAABS-1)+J-1)

C                 SEARCH FOR THE ELEMENTS CONNECTED TO THE SELECTED ONE
C                 BY MEANS OF NODE J
                  CALL JELIRA(JEXNUM(CNXINV,NUNO),'LONMAX',NBELNO,K8B)
                  CALL JEVEUO(JEXNUM(CNXINV,NUNO),'L',JELNO)

C                 LOOP ON EACH ELEMENT CONNECTED TO NODE J
                  DO 40 ELJ=1,NBELNO

                     NUMELM=ZI(JELNO-1+ELJ)

C                    CHECK IF THE CONNECTED ELEMENT HAS ALREADY BEEN
C                    SELECTED
                     IF (ZI(NLAYER-1+NUMELM).EQ.0) THEN

C                       NO. ASSIGN IT THE LAYER NUMBER
                        ZI(NLAYER-1+NUMELM) = LAYER+1

C                       CALCULATE THE NORMAL TO THE ELEMENT USING THE
C                       FIRST THREE NODES DEFINING IT
                        NMAASS = ZI(JDLIMA-1+NUMELM)
                        NBNOMA = ZI(JCONX2+NMAASS)-ZI(JCONX2+NMAASS-1)

                        INOMA=1
                        NUNO=ZI(JCONX1-1+ZI(JCONX2+NMAASS-1)+INOMA-1)
                        A(1)=ZR(JCOOR-1+3*(NUNO-1)+1)
                        A(2)=ZR(JCOOR-1+3*(NUNO-1)+2)
                        A(3)=ZR(JCOOR-1+3*(NUNO-1)+3)

                        INOMA=2
                        NUNO=ZI(JCONX1-1+ZI(JCONX2+NMAASS-1)+INOMA-1)
                        B(1)=ZR(JCOOR-1+3*(NUNO-1)+1)
                        B(2)=ZR(JCOOR-1+3*(NUNO-1)+2)
                        B(3)=ZR(JCOOR-1+3*(NUNO-1)+3)

                        INOMA=3
                        NUNO=ZI(JCONX1-1+ZI(JCONX2+NMAASS-1)+INOMA-1)
                        C(1)=ZR(JCOOR-1+3*(NUNO-1)+1)
                        C(2)=ZR(JCOOR-1+3*(NUNO-1)+2)
                        C(3)=ZR(JCOOR-1+3*(NUNO-1)+3)

                        AB(1)=B(1)-A(1)
                        AB(2)=B(2)-A(2)
                        AB(3)=B(3)-A(3)

                        AC(1)=C(1)-A(1)
                        AC(2)=C(2)-A(2)
                        AC(3)=C(3)-A(3)

                        CALL PROVEC(AB,AC,VN)
                        CALL NORMEV(VN,NORME)

C                       CHECK THE ORIENTATION OF THE ELEMENT NORMAL
C                       WITH RESPECT TO THE REFERENCE NORMAL 
                        PS=DDOT(3,VN,1,VNREF,1)

                        IF (PS.LT.0.D0)THEN
                           ZI(JSENS-1+NUMELM)=-1
                        ELSE
                           ZI(JSENS-1+NUMELM)=1
                        ENDIF

                     ENDIF

40                CONTINUE

30             CONTINUE

            ENDIF

20       CONTINUE

10    CONTINUE

C     LOOP ON THE ELEMENT LIST TO CHECK THAT EACH ELEMENT HAS BEEN
C     PROCESSED
      DO 100 I=1,NBMAF
         IF(ZI(NLAYER-1+I).EQ.0) CALL U2MESS('F','XFEM_9')
100   CONTINUE

C     CLEAN THE TEMPORARY JEVEUX OBJECTS
      CALL JEDETR('&&XORIMA.LAY')
      CALL JEDETR(CNXINV)

      CALL JEDEMA()
      END
