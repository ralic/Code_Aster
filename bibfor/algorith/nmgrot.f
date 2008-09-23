      SUBROUTINE NMGROT(IRAN  ,DELDET,THETA ,CHAMAJ)
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/09/2008   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      REAL*8        THETA(3),DELDET(3)
      INTEGER       IRAN(3)
      REAL*8        CHAMAJ(*)
C 
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME - UTILITAIRE - DYNAMIQUE)
C
C MISE A JOUR DES DEPLACEMENTS EN GRANDES ROTATIONS
C POUR POU_D_GD
C      
C ----------------------------------------------------------------------
C 
C
C IN  POUGD  : VARIABLE CHAPEAU POUR POUTRES EN GRANDES ROTATIONS
C IN  THETA  : VALEUR DE LA ROTATION PRECEDENTE
C IN  IRAN   : NUMEROS ABSOLUS D'EQUATION DES DDL DE ROTATION DANS LES
C                 CHAM_NO
C OUT CHAMAJ : CHAMP MISE A JOUR
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
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
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      INTEGER      IC
      REAL*8       QUAPRO(4),QUAROT(4),DELQUA(4)
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()             
                  
C
C --- QUATERNION DE LA ROTATION PRECEDENTE
C
      CALL VROQUA(THETA ,QUAROT)
C
C --- QUATERNION DE L'INCREMENT DE ROTATION
C      
      CALL VROQUA(DELDET,DELQUA)                
C
C --- CALCUL DE LA NOUVELLE ROTATION
C      
      CALL PROQUA(DELQUA,QUAROT,QUAPRO)       
      CALL QUAVRO(QUAPRO,THETA )
C
C --- MISE A JOUR DE LA ROTATION
C        
      DO 15 IC = 1,3
        CHAMAJ(IRAN(IC)) = THETA (IC)
15    CONTINUE

C            
      CALL JEDEMA()
      END
