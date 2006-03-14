      SUBROUTINE CUTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,
     &                  RESOCU,TYPOPE,POSIT,LIAISO)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/03/2006   AUTEUR MABBAS M.ABBAS 
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
C
      IMPLICIT     NONE
      INTEGER      INDIC
      INTEGER      NBLIAC
      INTEGER      AJLIAI
      INTEGER      SPLIAI
      INTEGER      POSIT
      INTEGER      LIAISO
      CHARACTER*1  TYPOPE
      CHARACTER*24 RESOCU
C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : ALGOCU
C ----------------------------------------------------------------------
C
C MISE A JOUR DES VECTEURS DE LIAISONS 
C LE NOMBRE DE LIAISONS EST MIS A JOUR DANS LA ROUTINE
C
C OUT INDIC  :+1 ON A RAJOUTE UNE LIAISON 
C             -1 ON A ENLEVE UNE LIAISON 
C I/O NBLIAC : NOMBRE DE LIAISONS ACTIVES
C I/O AJLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
C              LIAISON CORRECTE DU CALCUL 
C              DE LA MATRICE DE CONTACT ACM1AT
C I/O SPLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
C              LIAISON AYANT ETE CALCULEE POUR LE VECTEUR CM1A
C IN  RESOCU : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C IN  TYPOPE : TYPE D'OPERATION DANS LE VECTEUR DES LIAISONS
C                'A' : AJOUTER UNE LIAISON
C                'S' : SUPPRIMER UNE LIAISON
C IN  POSIT  : POSITION POUR AJOUTER UNE LIAISON DANS LE 
C              VECTEUR DES LIAISONS ACTIVES
C IN  LIAISO : INDICE DE LA LIAISON A AJOUTER OU SUPPRIMER
C
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
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
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER       II
      CHARACTER*1   TYPEAJ,TYPESP
      CHARACTER*19  LIAC
      INTEGER       JLIAC
C
C ======================================================================
C
      CALL JEMARQ ()

      LIAC   = RESOCU(1:14)//'.LIAC'
      CALL JEVEUO (LIAC,  'E',JLIAC )
C 
C --- INITIALISATION DES VARIABLES TYPE DE CONTACT 
C 
      TYPEAJ = 'A'
      TYPESP = 'S'
C
      IF (TYPOPE.EQ.TYPEAJ) THEN
C 
C --- ON AJOUTE UNE LIAISON
C 
         INDIC  = 1
         ZI(JLIAC-1+POSIT) = LIAISO
         NBLIAC = NBLIAC + 1
      ELSE IF (TYPOPE.EQ.TYPESP) THEN
C 
C --- ON SUPPRIME UNE LIAISON 
C 
         INDIC  = -1                
         DO 30 II = POSIT, NBLIAC - 1
           ZI(JLIAC-1+II) = ZI(JLIAC-1+II+1)
 30      CONTINUE
         NBLIAC = NBLIAC - 1
         AJLIAI = AJLIAI - 1
      ENDIF
C 
C --- MISE A JOUR DE L'INDICATEUR POUR LA FACTORISATION DE LA MATRICE 
C --- DE CONTACT 
C
      SPLIAI = MIN(SPLIAI,POSIT-1)
      SPLIAI = MIN(SPLIAI,NBLIAC)
      IF (AJLIAI.LT.0) THEN
        AJLIAI = 0
      ENDIF  
C ======================================================================
      CALL JEDEMA ()
C ======================================================================
      END
